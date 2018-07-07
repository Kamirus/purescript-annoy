module Annoy.Unsafe where

import Prelude

import Annoy.Types (STPrimAnnoy)
import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST)
import Node.FS (FS)

-- | `unsafeNew f metric`
-- | Creates new `STPrimAnnoy` that stores vector of `f` dimensions with `metric`. Does not validate input.
foreign import unsafeNew
  :: forall h r
   . Int
  -> String
  -> Eff ( st :: ST h | r ) (STPrimAnnoy h)

-- | Inserts vector at given index. No checks for: not frozen `STPrimAnnoy`, matching dimension, negative index.
foreign import unsafeAddItem 
  :: forall h r
   . STPrimAnnoy h
  -> Int
  -> Array Number
  -> Eff ( st :: ST h | r ) Unit

-- | Builds a forest of given number of trees. After calling, no more items can be added.
foreign import unsafeBuild
  :: forall h r
   . STPrimAnnoy h
  -> Int
  -> Eff ( st :: ST h | r ) Unit

-- | Saves the index to disk.
foreign import save
  :: forall h r
   . STPrimAnnoy h
  -> String
  -> Eff ( st :: ST h, fs :: FS | r ) Boolean

-- | Loads an index from disk.
foreign import unsafeLoad
  :: forall h r
   . STPrimAnnoy h
  -> String
  -> Eff ( st :: ST h, fs :: FS | r ) Boolean

foreign import unload
  :: forall h r
   . STPrimAnnoy h
  -> Eff ( st :: ST h, fs :: FS | r ) Unit

-- | Returns vector under given index. No bounds checks are performed.
foreign import unsafeGetItem
  :: forall h r
   . STPrimAnnoy h
  -> Int
  -> Eff ( st :: ST h | r ) (Array Number)

-- | `unsafeGetNNsByItem a i n search_k`
-- | Returns `n` closest items to the `i`-th vector. No bounds checks are performed.
foreign import unsafeGetNNsByItem
  :: forall h r
   . STPrimAnnoy h
  -> Int
  -> Int
  -> Int
  -> Eff ( st :: ST h | r ) (Array Int)

-- | Like `unsafeGetNNsByItem` but returns also distances
foreign import unsafeGetNNsByItem_
  :: forall h r
   . STPrimAnnoy h
  -> Int
  -> Int
  -> Int
  -> Eff ( st :: ST h | r ) { neighbors :: Array Int, distances :: Array Int }

-- | `unsafeGetNNsByVector a v n search_k`
-- | Like above but query by vector v instead of index.
foreign import unsafeGetNNsByVector
  :: forall h r
   . STPrimAnnoy h
  -> Array Number
  -> Int
  -> Int
  -> Eff ( st :: ST h | r ) (Array Int)

-- | Like `unsafeGetNNsByVector` but returns also distances
foreign import unsafeGetNNsByVector_
  :: forall h r
   . STPrimAnnoy h
  -> Array Number
  -> Int
  -> Int
  -> Eff ( st :: ST h | r ) { neighbors :: Array Int, distances :: Array Int }

-- | Returns number of (allocated!) elements in Annoy.
foreign import getNItems
  :: forall h r
   . STPrimAnnoy h
  -> Eff ( st :: ST h | r ) Int

-- | Returns the distance between items at positions `i` and `j`. No bounds checks are performed.
foreign import unsafeGetDistance
  :: forall h r
   . STPrimAnnoy h
  -> Int
  -> Int
  -> Eff ( st :: ST h | r ) Number
