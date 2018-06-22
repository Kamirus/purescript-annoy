module Annoy.Unsafe where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST)

foreign import data Annoy :: Type

-- | `unsafeNewAnnoy f metric`
-- | Creates new `Annoy` that stores vector of `f` dimensions with `metric`. Does not validate input.
foreign import unsafeNewAnnoy
  :: forall h r
  .  Int
  -> String
  -> Eff (st :: ST h | r) Annoy

-- | Inserts vector at given index. No checks for: not frozen `Annoy`, matching dimension, negative index
foreign import unsafeAddItem 
  :: forall h r
  .  Int 
  -> Array Number
  -> Annoy
  -> Eff (st :: ST h | r) Unit

-- | Builds a forest of given number of trees. After calling, no more items can be added.
foreign import unsafeBuild
  :: forall h r
  .  Int
  -> Annoy
  -> Eff (st :: ST h | r) Unit

-- | Returns vector under given index. No bounds checks are performed.
foreign import unsafeGetItem
  :: Int
  -> Annoy
  -> Array Number
