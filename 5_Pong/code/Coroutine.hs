module Coroutine where

import Prelude hiding (id, (.))

import Control.Arrow
import Control.Category
import Control.Applicative
import Data.List

newtype Coroutine i o = Coroutine { runC :: i -> (o, Coroutine i o) }


instance Functor (Coroutine i) where
    fmap f co = Coroutine $ \i ->
        let (o, co') = runC co i
        in (f o, fmap f co')

instance Applicative (Coroutine i) where
    pure x = Coroutine $ const (x, pure x)

    cof <*> cox = Coroutine $ \i ->
        let (f, cof') = runC cof i
            (x, cox') = runC cox i
        in (f x, cof' <*> cox')

instance Category Coroutine where
    id = Coroutine $ \i -> (i, id)

    cof . cog = Coroutine $ \i ->
        let (x, cog') = runC cog i
            (y, cof') = runC cof x
        in (y, cof' . cog')

instance Arrow Coroutine where
    arr f = Coroutine $ \i -> (f i, arr f)

    first co = Coroutine $ \(a,b) ->
        let (c, co') = runC co a
        in ((c,b), first co')

-- Events
type Event a = [a]

-- | Map events into different kinds of events
mapE :: (e -> e') -> Coroutine (Event e) (Event e')
mapE = arr . map

-- | Filter events based on a predicate function
filterE :: (e -> Bool) -> Coroutine (Event e) (Event e)
filterE = arr . filter

-- | Replace every event occurence with a fixed event
constE :: e -> Coroutine (Event e') (Event e)
constE = mapE . const

-- | Merge two time varying values using a combining function
zipWithC :: (a -> b -> c) -> Coroutine (a, b) c
zipWithC = arr . uncurry

-- | Merge two event streams together
zipE :: Coroutine (Event e, Event e) (Event e)
zipE = zipWithC (++)

scanE :: (a -> e -> a) -> a -> Coroutine (Event e) a
scanE f i = Coroutine $ step i where
    step a e = let a' = foldl' f a e in (a', scanE f a')

-- | Split a value into (current value, previous value) using the given
--   initial value as the previous value during first call.
withPrevious :: a -> Coroutine a (a, a)
withPrevious first = Coroutine $ \i -> ((i, first), step i) where
    step old = Coroutine $ \i -> ((i, old), step i)

-- | Delay the value by a single time-step, using the given initial value for
--   the first call.
delay :: a -> Coroutine a a
delay a = withPrevious a >>> arr snd

-- | Integrate a numerical value over time
integrate :: Num a => a -> Coroutine a a
integrate = scan (+)

-- | Derivate a numerical value over time (i.e. return the delta between current
--   and previous time-step.
derivate :: Num a => Coroutine a a
derivate = withPrevious 0 >>> zipWithC (-)

-- | Trigger an event whenever the value satisfies the given predicate function
watch :: (a -> Bool) -> Coroutine a (Event a)
watch f = Coroutine $ \i ->
    if f i
        then ([i], watch f)
        else ([], watch f)

scan :: (a -> b -> a) -> a -> Coroutine b a
scan f i = Coroutine $ step i where
    step a b = let a' = f a b in (a', scan f a')
