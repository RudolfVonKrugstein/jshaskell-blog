{-# LANGUAGE Arrows #-}

module WireUtils where

import Control.Wire
import Prelude hiding ((.),id)
import Data.Maybe
import qualified Data.Map as M

-- dynamic set of wires. Wires are created with the creator function and the [c] parameter
-- Wires that inhibit are deleted
dynamicSet :: (Monad m) => (c -> Wire e m a b) -> [Wire e m a b] ->  Wire e m (a, [c]) [b]
dynamicSet creator ws' = mkGen $ \dt (i,new) -> do
            res <- mapM (\w -> stepWire w dt i) ws'
            let filt (Right a, b) = Just (a,b)
                filt _            = Nothing
                resx = mapMaybe filt res
            return (Right $ (fmap fst resx), dynamicSet creator $ (fmap snd resx) ++ (map creator new))

-- queue for the objects in the list given as parameter
-- The Int argument says how many objects should be returned
staticQueue :: (Monad m) => [a] -> Wire e m Int [a]
staticQueue set = unfold give set
  where
  give s n = (take n s, drop n s)

-- Pairs the input list with the given list, which is assumed to be infinite
pairListsWith :: (Monad m) => [p] -> Wire e m [a] [(p,a)]
pairListsWith pairs = proc as -> do
  p <- staticQueue pairs  -< length as
  returnA -< zip p as

-- Same as dynamic set, but pairs all wires with a key.  The input map is than loouped with these keys to determine the input for the indubidual wires
dynamicSetMap :: (Monad m) => (c -> Wire e m (Maybe a) b) -> [Wire e m (Maybe a) b] -> Wire e m (M.Map Int a, [c]) [(Int,b)]
dynamicSetMap creator ws = dynamicSet creator' ws' . (second $ pairListsWith restKeys)
  where
  wireWithLookupAndKey :: (Monad m) => Int -> Wire e m (Maybe a) b -> Wire e m (M.Map Int a) (Int,b)
  wireWithLookupAndKey i w = (pure i) &&& (w . (arr (M.lookup i)))
  keys           = [0,1..]
  restKeys       = drop (length ws) keys
  ws'            = map (uncurry wireWithLookupAndKey) $ zip keys ws
  creator' (i,c) = wireWithLookupAndKey i (creator c)


-- same as dynamicSet, only that it can not grow
shrinking :: (Monad m) => [Wire e m a b] -> Wire e m a [b]
shrinking ws = dynamicSet undefined ws <<< arr (\a -> (a,[]))

-- same as dynamicSetMap, only that it can not grow
shrinkingMap :: (Monad m) => [Wire e m (Maybe a) b] -> Wire e m (M.Map Int a) [(Int,b)]
shrinkingMap ws = dynamicSetMap undefined ws <<< arr (\a -> (a,[]))

