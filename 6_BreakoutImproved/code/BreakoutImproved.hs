{-# LANGUAGE Arrows #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import JavaScript
import Collision
import WireUtils
import Data.IORef
import Control.Wire
import Prelude hiding ((.), id)
import Data.VectorSpace
import qualified Data.Function as F
import qualified Data.Traversable as T
import Data.Maybe
import Data.List
import qualified Data.Map as M
import Data.Monoid
import Control.Monad.Identity (Identity)
import Control.Monad.Fix (MonadFix)

-- Game Types
-- Input events
data InputEvent = KeyUp Int | KeyDown Int | Update
  deriving (Eq)

-- state of game objects
data Paddle     = Paddle { xPos    :: Double }
data Gun        = Gun    { ammo    :: Int    }
data Ball       = Ball   { ballPos   :: Vector,
                           ballSpeed :: Vector}
data Block      = Block  { blockType :: BlockType, blockPos :: Vector, blockState :: BlockState}
data BlockState = Alive Int | Dying Double
data BlockType  = NormalBlock | PowerBlock deriving (Eq)

data Bullet     = Bullet { bulletPos :: Vector }
data GameState  = GameState {
                     paddle  :: Paddle,
                     gun     :: Gun,
                     ball    :: Ball,
                     blocks  :: [Block],
                     bullets :: [Bullet]}
                  | StartScreen String

-- constants
screenWidth      = 600.0
screenHeight     = 400.0

paddleColor      = Color 0.0 0.0 0.0 1.0
paddleYPos       = screenHeight - paddleHeight
paddleHeight     = 15.0
paddleWidth      = 50.0
paddleRadius     = 7.0
paddleSpeed      = 7.0
initPaddleXPos   = (screenWidth - paddleWidth) / 2.0
initPaddle       = Paddle initPaddleXPos

initGun          = Gun 1

ballColor        = Color 1.0 0.0 0.0 1.0
ballRadius       = 5.0
initBallSpeed    = (3.0, -3.0)
initBallPos      = (screenWidth / 2.0, screenHeight - 50.0)
initBall         = Ball initBallPos initBallSpeed

blockWidth       = 60.0
blockHeight      = 20.0
blockRadius      = 5.0
normalBlockColor = [Color 0.0 0.0 1.0 1.0, Color 0.0 0.0 0.5 1.0]
powerBlockColor  = [Color 0.0 0.5 0.0 1.0]
initBlocks       = [Block t (x,y) (Alive l) | x <- [20.0, 140.0, 240.0, 340.0, 440.0, 520.0], (y,t,l) <- [(60.0, PowerBlock, 1), (100.0, NormalBlock,2), (140.0,NormalBlock,1), (180.0,NormalBlock,2), (260.0,NormalBlock,2)]]

bulletRadius     = 3.0
bulletSpeed      = (0.0, -10.0)
bulletColor      = Color 0.0 0.5 0.0 1.0

-- technical constants
leftKeyCode  = 37
rightKeyCode = 39
startKeyCode = 13
fireKeyCode  = 32
canvasName   = "canvas4"
                

-- type of the main wire
data GameEnd = Win | Loose | None
instance Monoid GameEnd where
  mempty = None
  mappend x None = x
  mappend None x = x
  mappend _ Win = Win
  mappend Win _ = Win
  mappend _ _ = Loose

type MainWireType = Wire GameEnd Identity InputEvent (Maybe GameState)

-- startup
main = setOnLoad initilize

initilize :: IO ()
initilize = do
  wire <- newIORef mainWire
  setOnKeyDown canvasName (onKeyDown wire)
  setOnKeyUp   canvasName (onKeyUp   wire)
  setInterval 20.0 (update wire)

-- reactions to input events
onKeyDown :: IORef MainWireType -> Int -> IO ()
onKeyDown wire code = do
  w <- readIORef wire
  let (_, w') = stepWireP w 0.0 (KeyDown code)
  writeIORef wire w'
 
onKeyUp :: IORef MainWireType -> Int -> IO ()
onKeyUp wire code = do
  w <- readIORef wire
  let (_, w') = stepWireP w 0.0 (KeyUp code)
  writeIORef wire w'

-- update
update :: IORef MainWireType -> IO ()
update wire = do
  w <- readIORef wire
  let (res, w') = stepWireP w 1.0 Update
  case res of
    Left err -> alert "Error!"
    Right (Just gs) -> draw gs
  writeIORef wire w'

-- Circles and rectangles of game objects
instance CircleShaped Ball where
  circle (Ball p _) = Just $ Circle p ballRadius

instance CircleShaped Bullet where
  circle (Bullet p) = Just $ Circle p bulletRadius

instance RoundedRectShaped Paddle where
  roundedRect (Paddle x) = Just $ RoundedRect (x,paddleYPos) (x+paddleWidth,paddleYPos+paddleHeight) paddleRadius

instance RoundedRectShaped Block where
  roundedRect (Block _ _ (Dying _))    = Nothing
  roundedRect (Block _ (x,y) (Alive _)) = Just $ RoundedRect (x,y) (x+blockWidth,y+blockHeight) blockRadius

-- drawing function, draw a game state
draw :: GameState -> IO ()
draw (StartScreen msg) = do
  ctx <- getContext2d canvasName
  clear ctx
  setFillColor ctx $ Color 0.0 0.0 0.0 1.0
  fillText ctx msg (screenWidth / 2.0 - 100.0) (screenHeight /2.0)

draw (GameState paddle gun ball blocks bullets) = do
  ctx <- getContext2d canvasName
  clear ctx
  setFillColor ctx paddleColor
  fillRoundedRect ctx (xPos paddle) paddleYPos paddleWidth paddleHeight paddleRadius
  mapM_ (drawBlock ctx) $ blocks
  mapM_ (drawBullet ctx) $ bullets
  setFillColor ctx ballColor
  let Ball (x,y) _ = ball
  fillCircle ctx x y ballRadius
  setFillColor ctx $ Color 0.0 0.0 0.0 1.0
  fillText ctx ("Ammo: " ++ (show . ammo $ gun)) 0 10.0
   

drawBlock :: Context2D -> Block -> IO ()
drawBlock ctx (Block t (x,y) (Alive lives)) = do
  setFillColor ctx $ (if t == PowerBlock then powerBlockColor else normalBlockColor) !! (lives -1)
  fillRoundedRect ctx x y blockWidth blockHeight blockRadius

drawBlock ctx (Block t (x,y) (Dying f)) = do
  setFillColor ctx $ ((if t == PowerBlock then powerBlockColor else normalBlockColor) !! 0) { alpha = f }
  fillRoundedRect ctx x y blockWidth blockHeight blockRadius
 
drawBullet :: Context2D -> Bullet -> IO ()
drawBullet ctx (Bullet (x,y)) = do
  setFillColor ctx bulletColor 
  fillCircle ctx x y bulletRadius

-- collsion detection
fromMaybeList :: Ord a => [(a,Maybe b)] -> M.Map a b
fromMaybeList [] = M.empty
fromMaybeList ((k,Nothing):xs) = fromMaybeList xs
fromMaybeList ((k,Just v):xs) = M.insert k v (fromMaybeList xs)

calcBallBlockColls :: Ball -> [(Int,Block)] -> M.Map Int Collision
calcBallBlockColls ball = fromMaybeList . map (\(id,block) -> (id,circleRectCollision ball block))

calcBallWallColls :: Ball -> [Collision]
calcBallWallColls (Ball (bx,by) _) = map snd $ filter (fst) $ [
  (bx <= 0          , Collision (1.0 , 0.0)),
  (bx >= screenWidth, Collision (-1.0, 0.0)),
  (by <= 0          , Collision (0.0 , 1.0))
  ]

calcBallPaddleColls :: Ball -> Paddle -> [Collision]
calcBallPaddleColls b p = 
  catMaybes [circleRectCollision b p]

pairUp :: [a] -> [b] -> [(a,b)]
pairUp as bs = [(a,b) | a <- as, b <- bs]

calcBlockBulletColls :: [(Int,Block)] -> [(Int,Bullet)] -> (M.Map Int Collision,M.Map Int Collision)
calcBlockBulletColls blocks bullets = foldl' buildColls (M.empty, M.empty) $ pairUp blocks bullets
  where
  buildColls (blList, buList) ((blId,block), (buId, bullet)) = case circleRectCollision bullet block of
                                                                    Nothing -> (blList, buList)
                                                                    Just c  -> (M.insert blId c blList, M.insert buId c buList)

createBullet :: Paddle -> Bullet
createBullet (Paddle x) = Bullet (x + paddleWidth / 2.0, paddleYPos)

-- Wires
-- key wires
keyDown :: (Monad m, Monoid e) => Int -> Event e m InputEvent
keyDown code = when (==KeyDown code)

keyUp :: (Monad m, Monoid e) => Int -> Event e m InputEvent
keyUp code = when (==KeyUp code)

-- main wire
startScreenWire :: String -> MainWireType
startScreenWire msg = pure (Just $ StartScreen msg) . notE (keyDown startKeyCode)

mainWire = switchBy start (start None)
  where
  start None = startScreenWire "Press Enter to start (click canvas to focus)" --> mainGameWire
  start Win  = startScreenWire "Congratulations, you won! Press Enter to restart." --> mainGameWire
  start Loose = startScreenWire "Sorry, you loose! Press Enter to restart." --> mainGameWire

mainGameWire :: MainWireType
mainGameWire = proc input -> do

  paddle <- paddleWire -< input

  let newFR old
       | input == Update              = []
       | input == KeyDown fireKeyCode = (createBullet paddle):old
       | otherwise                    = old

  fireRequests <- accum (flip ($)) [] -< newFR

  if input == Update then do
    rec
      let validCollDir (Collision n) = n <.> ballSpeed oldBall < 0.0
          ballBlockColls                      = M.filter validCollDir $ calcBallBlockColls  oldBall oldBlocks
          ballWallColls                       = calcBallWallColls   oldBall
          ballPaddleColls                     = filter validCollDir $ calcBallPaddleColls oldBall paddle
          (blockBulletColls,bulletBlockColls) = calcBlockBulletColls oldBlocks oldBullets

      ball <- ballWire -< ballWallColls ++ ballPaddleColls ++ (M.elems ballBlockColls)
      oldBall <- delay initBall -< ball
      _ <- looseWire -< oldBall

      (newAmmo,blocks)    <- blocksWire -< ballBlockColls `M.union` blockBulletColls
      oldBlocks <- delay $ [] -< blocks
      _ <- winWire -< (map snd oldBlocks)

      (newBullets,gun) <- gunWire -< (fireRequests,newAmmo)
      
      bullets    <- bulletsWire -< (bulletBlockColls,newBullets)
      oldBullets <- delay $ []  -< bullets
    returnA -< Just $ GameState paddle gun ball (map snd blocks) (map snd bullets)
  else
    returnA -< Nothing

-- induvidial game objects
looseWire :: (Monad m) => Wire GameEnd m Ball Ball
looseWire = unless ballOut --> inhibit Loose
  where
  ballOut (Ball (x,y) _) = y > screenHeight

winWire :: (Monad m) => Wire GameEnd m [Block] [Block]
winWire = (once --> unless null) --> inhibit Win

paddleWire :: (Monad m, Monoid e) => Wire e m InputEvent Paddle
paddleWire = Paddle <$> (integralLim1_ bound initPaddleXPos <<< (paddleSpeedWire &&& pure ()))
  where
  bound _ _ pos = max 0.0 $ min (screenWidth-paddleWidth) pos

paddleSpeedWire :: (Monad m, Monoid e) => Wire e m InputEvent Double 
paddleSpeedWire = (valueFromKeyDown leftKeyCode 0.0 (-paddleSpeed))
                  +
                  (valueFromKeyDown rightKeyCode 0.0 paddleSpeed)
  where
  valueFromKeyDown :: (Monad m, Monoid e) => Int -> a -> a -> Wire e m InputEvent a
  valueFromKeyDown code upValue downValue = F.fix (\start ->
                                                   pure upValue   . notE (keyDown code) -->
                                                   pure downValue . notE (keyUp code) -->
                                                   start)

gunWire :: (MonadFix m) => Wire e m ([Bullet],Int) ([Bullet],Gun)
gunWire = proc (bs,new) -> do
  rec
    let fires = take ammo bs
    ammo <- accum (+) (ammo initGun) -< new - (length fires)
  returnA -< (fires,Gun ammo)

accum1Fold :: (Monad m) => (b -> a -> b) -> b -> Wire e m [a] b
accum1Fold f init = accum1 step init
  where
  step last as = foldl' f last as

ballSpeedWire :: (Monad m) => Wire e m [Collision] Vector
ballSpeedWire = accum1Fold (collide) initBallSpeed
  where
  collide v0 (Collision n) = v0 - (2.0 * (n <.> v0)) *^ n

ballWire :: (Monad m) => Wire e m [Collision] Ball
ballWire = (Ball <$> integral1_ initBallPos) . ballSpeedWire <*> ballSpeedWire

blockWire :: (Monad m, Monoid e) => Block -> Wire e m (Maybe Collision) Block
blockWire init = while blockAlive . accum1 update init -->
                 Block (blockType init) (blockPos init) <$> (Dying <$> (pure 1.0) - (time / (pure 30.0))) . for 30.0
  where
  update old Nothing = old
  update old@(Block _ _ (Alive l)) _ = old { blockState = Alive (l - 1) }
  blockAlive (Block _ _ (Alive l)) = l > 0

blockAmmoWire :: (Monad m, Monoid e) => Block -> Wire e m (Maybe Collision) Int
blockAmmoWire (Block PowerBlock _ _) = (pure 0) . while (isNothing) --> once . (pure 1) --> pure 0
blockAmmoWire _ = (pure 0)

blockWithAmmoWire :: (Monad m, Monoid e) => Block -> Wire e m (Maybe Collision) (Int,Block)
blockWithAmmoWire b = blockAmmoWire b &&& blockWire b

blocksWire :: (Monad m, Monoid e) => Wire e m (M.Map Int Collision) (Int,[(Int,Block)])
blocksWire = (shrinkingMap $ map blockWithAmmoWire initBlocks) >>> (arr reorder)
  where
  reorder as = (sum $ map (fst . snd) as, map (\j -> (fst j, snd $ snd j)) as)

bulletWire :: (Monad m, Monoid e) => Bullet -> Wire e m (Maybe Collision) Bullet
bulletWire (Bullet init) = while bulletAlive . (Bullet <$> (pure bulletSpeed >>> integral1_ init)) . while (isNothing)
  where
  bulletAlive (Bullet (x,y)) = y > 0.0

bulletsWire :: (Monad m, Monoid e) => Wire e m (M.Map Int Collision,[Bullet]) [(Int,Bullet)]
bulletsWire = dynamicSetMap bulletWire []
