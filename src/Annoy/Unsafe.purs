module Annoy.Unsafe where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST)
import Node.FS (FS)

foreign import data STAnnoy :: Type -> Type

-- | `unsafeNewAnnoy f metric`
-- | Creates new `STAnnoy` that stores vector of `f` dimensions with `metric`. Does not validate input.
foreign import unsafeNewAnnoy
  :: forall h r
  .  Int
  -> String
  -> Eff (st :: ST h | r) (STAnnoy h)

-- | Inserts vector at given index. No checks for: not frozen `STAnnoy`, matching dimension, negative index.
foreign import unsafeAddItem 
  :: forall h r
  .  Int 
  -> Array Number
  -> STAnnoy h
  -> Eff (st :: ST h | r) Unit

-- | Builds a forest of given number of trees. After calling, no more items can be added.
foreign import unsafeBuild
  :: forall h r
  .  Int
  -> STAnnoy h
  -> Eff (st :: ST h | r) Unit

-- | Saves the index to disk.
foreign import save
  :: forall h r
  .  String
  -> STAnnoy h
  -> Eff (st :: ST h, fs :: FS | r) Boolean

-- | Loads an index from disk.
foreign import load
  :: forall h r
  .  String
  -> STAnnoy h
  -> Eff (st :: ST h, fs :: FS | r) Boolean

-- | Returns vector under given index. No bounds checks are performed.
foreign import unsafeGetItem
  :: forall h r
  .  Int
  -> STAnnoy h
  -> Eff (st :: ST h | r) (Array Number)
