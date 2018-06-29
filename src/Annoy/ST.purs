module Annoy.ST
  ( build
  , new
  , push
  , unsafeFreeze
  ) where

import Prelude

import Annoy.Types (Annoy, Metric, STAnnoy, strMetric)
import Annoy.Unsafe (getNItems, unsafeAddItem, unsafeBuild, unsafeNew)
import Control.Monad.Eff (Eff, runPure)
import Control.Monad.ST (ST, runST)
import Data.Maybe (Maybe(..), fromJust)
import Data.Typelevel.Num (class Nat, class Pos, toInt)
import Data.Vec (Vec, toArray)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

-- | `build trees m` executes `m` which creates Annoy and adds vectors, then builds it using `trees` (more info in original [annoy](https://github.com/spotify/annoy))
-- | ```purescript
-- | a = build d1 (do
-- |   a <- new d2 Manhattan
-- |   push (1.0 +> 2.0 +> empty) a
-- |   push (3.0 +> 4.0 +> empty) a
-- |   push (5.0 +> 6.0 +> empty) a
-- |   pure a)
-- | ```
build
  :: forall s t
   . Nat s
  => Pos t
  => t
  -> (forall h. Eff ( st :: ST h ) (STAnnoy h s))
  -> Annoy s
build trees m = unsafePartial $ fromJust $ build_ (toInt trees) m

-- | Similar to `build` but `trees` argument is an Int, so return type is Maybe
build_
  :: forall s t
   . Nat s
  => Int
  -> (forall h. Eff ( st :: ST h ) (STAnnoy h s))
  -> Maybe (Annoy s)
build_ trees m | trees < 1 = Nothing
build_ trees m | otherwise = Just $ runPure (runST (do 
  a <- m
  unsafeBuild trees $ unsafeCoerce a
  unsafeFreeze a))

-- | `new s metric` creates mutable annoy that stores vectors of size `s` and uses given `metric`
new
  :: forall h r s
   . Nat s
  => s
  -> Metric
  -> Eff ( st :: ST h | r ) (STAnnoy h s)
new s metric = unsafeCoerce $ unsafeNew (toInt s) $ strMetric metric

-- | `push v annoy` adds vector `v` at index 'len' which is equal to the number of currently stored vectors
push
  :: forall h r s
   . Nat s
  => Vec s Number
  -> STAnnoy h s
  -> Eff ( st :: ST h | r ) Unit
push v annoy = do
  let a = unsafeCoerce annoy
  len <- getNItems a
  unsafeAddItem len (toArray v) a

unsafeFreeze
  :: forall h r s
   . STAnnoy h s
  -> Eff ( st :: ST h | r ) (Annoy s)
unsafeFreeze = pure <<< unsafeCoerce
