
module Collision where

import Data.VectorSpace
import Data.List
import Control.Monad

-- Information about collision
type Vector = (Double, Double) -- thanks to vector-space we can do ^+^ and similar

data Collision   = Collision { normal :: Vector } deriving (Show)

type Radius = Double
data Circle = Circle { circlePos :: Vector, circleRadius :: Radius}
data Rectangle   = Rectangle Vector Vector
data RoundedRect = RoundedRect { rectMin :: Vector, rectMax :: Vector, rectRadius :: Radius}



class CircleShaped a where 
  circle      :: a -> Maybe Circle
class RoundedRectShaped a where
  roundedRect :: a -> Maybe RoundedRect

instance CircleShaped Circle where
  circle c = Just c

circleCollision :: (CircleShaped a, CircleShaped b) => a -> b -> Maybe Collision
circleCollision a b = do
  (Circle p1 r1) <- circle a
  (Circle p2 r2) <- circle b
  let centerDiff = p2 ^-^ p1
  guard (centerDiff <.> centerDiff <= (r1 + r2) * (r1 + r2))
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
      --test the corners
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


