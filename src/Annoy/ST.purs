module Annoy.ST
  ( build
  , build_
  , new
  , push
  , unsafeFreeze
  ) where

import Prelude

import Annoy.Types (Annoy, Metric, STAnnoy, strMetric)
import Annoy.Unsafe (getNItems, unsafeAddItem, unsafeBuild, unsafeNew)
import Data.Maybe (Maybe(..), fromJust)
import Data.Typelevel.Num (class Nat, class Pos, toInt)
import Data.Vec (Vec, toArray)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

-- | `build { trees } m` executes `m` which creates Annoy and adds vectors, then builds it using `trees` (more info in original [annoy](https://github.com/spotify/annoy))
-- | ```purescript
-- | a = build { trees: d1 } (do
-- |   a <- new { size: d2 , metric: Manhattan }
-- |   push (1.0 +> 2.0 +> empty) a
-- |   push (3.0 +> 4.0 +> empty) a
-- |   push (5.0 +> 6.0 +> empty) a
-- |   pure a)
-- | ```
build
  :: forall s t o
   . Nat s
  => Pos t
  => { trees :: t | o }
  -> (forall h. Effect (STAnnoy h s))
  -> Annoy s
build { trees } m = unsafePartial $ fromJust $ build_ { trees: toInt trees } m

-- | Similar to `build` but `trees` argument is an Int, so return type is Maybe
build_
  :: forall s o
   . Nat s
  => { trees :: Int | o }
  -> (forall h. Effect (STAnnoy h s))
  -> Maybe (Annoy s)
build_ { trees } m = if trees < 1 then Nothing
  else Just $ unsafePerformEffect (do
    a <- m
    unsafeBuild (unsafeCoerce a) trees
    unsafeFreeze a)

-- | `new { size , metric }` creates mutable annoy that stores vectors of `size` and uses given `metric`
new
  :: forall h s o
   . Nat s
  => { size :: s , metric :: Metric | o }
  -> Effect (STAnnoy h s)
new { size , metric } = unsafeCoerce $ unsafeNew (toInt size) $ strMetric metric

-- | `push annoy v` adds vector `v` at index 'len' which is equal to the number of currently stored vectors
push
  :: forall h s
   . Nat s
  => STAnnoy h s
  -> Vec s Number
  -> Effect Unit
push annoy v = do
  let a = unsafeCoerce annoy
  len <- getNItems a
  unsafeAddItem a len (toArray v)

unsafeFreeze
  :: forall h s
   . STAnnoy h s
  -> Effect (Annoy s)
unsafeFreeze = pure <<< unsafeCoerce
