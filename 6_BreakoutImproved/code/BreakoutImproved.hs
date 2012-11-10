{-# LANGUAGE Arrows #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import JavaScript
import Data.IORef
import Control.Wire
import Prelude hiding ((.), id)
import Data.VectorSpace
import qualified Control.Monad as CM
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

-- GameData
type Vector = (Double, Double) -- thanks to vector-space we can do ^+^ and similar

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

-- Information about collision
data Collision   = Collision { normal :: Vector } deriving (Show)

type Radius = Double
data Circle = Circle { circlePos :: Vector, circleRadius :: Radius}
data Rectangle   = Rectangle Vector Vector
data RoundedRect = RoundedRect { rectMin :: Vector, rectMax :: Vector, rectRadius :: Radius}

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
canvasName   = "canvas5"

-- wire util
dynamicSet :: (Monad m) => (c -> Wire e m a b) -> [Wire e m a b] ->  Wire e m (a, [c]) [b]
dynamicSet creator ws' = mkGen $ \dt (i,new) -> do
            res <- mapM (\w -> stepWire w dt i) ws'
            let filt (Right a, b) = Just (a,b)
                filt _            = Nothing
                resx = mapMaybe filt res
            return (Right $ (fmap fst resx), dynamicSet creator $ (fmap snd resx) ++ (map creator new))

staticQueue :: (Monad m) => [a] -> Wire e m Int [a]
staticQueue set = unfold give set
  where
  give s n = (take n s, drop n s)

pairListsWith :: (Monad m) => [p] -> Wire e m [a] [(p,a)]
pairListsWith pairs = proc as -> do
  p <- staticQueue pairs  -< length as
  returnA -< zip p as

dynamicSetMap :: (Monad m) => (c -> Wire e m (Maybe a) b) -> [Wire e m (Maybe a) b] -> Wire e m (M.Map Int a, [c]) [(Int,b)]
dynamicSetMap creator ws = dynamicSet creator' ws' . (second $ pairListsWith restKeys)
  where
  wireWithLookupAndKey :: (Monad m) => Int -> Wire e m (Maybe a) b -> Wire e m (M.Map Int a) (Int,b)
  wireWithLookupAndKey i w = (pure i) &&& (w . (arr (M.lookup i)))
  keys           = [0,1..]
  restKeys       = drop (length ws) keys
  ws'            = map (uncurry wireWithLookupAndKey) $ zip keys ws
  creator' (i,c) = wireWithLookupAndKey i (creator c)
  

shrinking :: (Monad m) => [Wire e m a b] -> Wire e m a [b]
shrinking ws = dynamicSet undefined ws <<< arr (\a -> (a,[]))

shrinkingMap :: (Monad m) => [Wire e m (Maybe a) b] -> Wire e m (M.Map Int a) [(Int,b)]
shrinkingMap ws = dynamicSetMap undefined ws <<< arr (\a -> (a,[]))
                
{-manager ws' = mkGen $ \dt xs' -> do
            res <- mapM (\(w,x) -> stepWire w dt x) $ zip ws' xs'
            let filt (Right a, b) = Just (a, b)
                filt _           = Nothing
                resx = mapMaybe filt res
            return (Right $ (fmap fst) resx,manager (fmap snd resx))-}
            

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

-- Collision utils
class CircleShaped a where
  circle      :: a -> Maybe Circle
class RoundedRectShaped a where
  roundedRect :: a -> Maybe RoundedRect

circleCollision :: (CircleShaped a, CircleShaped b) => a -> b -> Maybe Collision
circleCollision a b = do
  (Circle p1 r1) <- circle a
  (Circle p2 r2) <- circle b
  let centerDiff = p2 ^-^ p1
  CM.guard (centerDiff <.> centerDiff <= (r1 + r2) * (r1 + r2))
  return $ Collision $ normalized centerDiff

pointInRectangle    :: Vector -> Rectangle -> Bool
pointInRectangle (px,py) (Rectangle (minX,minY) (maxX,maxY))
  | px > maxX = False
  | px < minX = False
  | py > maxY = False
  | py < minY = False
  | otherwise = True

circleRectCollision :: (CircleShaped a, RoundedRectShaped b) => a -> b -> Maybe Collision
circleRectCollision c r = do
  circle <- circle c
  rect   <- roundedRect r
  circleRectCollision' circle rect
  where
    circleRectCollision' circle@(Circle (cx,cy) cr) (RoundedRect (minX,minY) (maxX,maxY) rr)
      -- test the corners
      | cx <= innerMinX && cy <= innerMinY = circleCollision (Circle (innerMinX, innerMinY) rr) circle
      | cx >= innerMaxX && cy <= innerMinY = circleCollision (Circle (innerMaxX, innerMinY) rr) circle
      | cx >= innerMaxX && cy >= innerMaxY = circleCollision (Circle (innerMaxX, innerMaxY) rr) circle
      | cx <= innerMinX && cy <= innerMinY = circleCollision (Circle (innerMinX, innerMaxY) rr) circle
      -- test if collision with rectangle occured
      | not $ pointInRectangle (cx,cy) (Rectangle ((minX-cr), (minY-cr)) ((maxX+cr), (maxY+cr))) = Nothing
      -- collision definitly occured, find correct normal
      | otherwise = Just $ fst $ minimumBy (\(_,a) (_,b) -> compare a b)
                          [
                          (Collision (-1.0,0.0), cx - minX),
                          (Collision (1.0, 0.0), maxX - cx),
                          (Collision (0.0,-1.0), cy - minY),
                          (Collision (0.0, 1.0), maxY - cy)
                          ]
      where
        innerMinX = minX + rr
        innerMinY = minY + rr
        innerMaxX = maxX - rr
        innerMaxY = maxY - rr


-- Circles and rectangles of game objects
instance CircleShaped Circle where
  circle c = Just c

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

gameLost :: Maybe GameState -> Bool
gameLost (Just (GameState _ _ (Ball (_,y) _) _ _)) = y > screenHeight
gameLost _ = False

gameWon :: Maybe GameState -> Bool
gameWon (Just (GameState _ _ _ [] _)) = True
gameWon _ = False

-- Wires
-- key wires
keyDown :: (Monad m, Monoid e) => Int -> Event e m InputEvent
keyDown code = when (==KeyDown code)

keyUp :: (Monad m, Monoid e) => Int -> Event e m InputEvent
keyUp code = when (==KeyUp code)

-- main wire
startScreenWire :: String -> MainWireType
startScreenWire msg = pure (Just $ StartScreen msg) . notE (keyDown startKeyCode)

mainWire = F.fix (\start -> 
             startScreenWire "Press Enter to start (click canvas to focus)" -->
             (unless gameWon . ((unless gameLost . mainGameWire) --> (startScreenWire "You LOST!") --> start)) --> (startScreenWire "You WON!") -->
             start)
{-lostWire = startScreenWire "YouLost" --> inhibit [mainGameWire']
wonWire  = startScreenWire "YouWon"  --> inhibit [mainGameWire']
mainGameWire' = (unless gameWon --> inhibit [wonWire]) . (unless gameLost --> [lostWire]) mainGameWire

mainWire = F.fix (\start -> 
             startScreenWire "Press Enter to start (click canvas to focus)" -->
             switchBy head mainGameWire'
             start)-}

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

      (newAmmo,blocks)    <- blocksWire -< ballBlockColls `M.union` blockBulletColls
      oldBlocks <- delay $ [] -< blocks

      (newBullets,gun) <- gunWire -< (fireRequests,newAmmo)
      
      bullets    <- bulletsWire -< (bulletBlockColls,newBullets)
      oldBullets <- delay $ []  -< bullets
    returnA -< Just $ GameState paddle gun ball (map snd blocks) (map snd bullets)
  else
    returnA -< Nothing

-- induvidial game objects
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
