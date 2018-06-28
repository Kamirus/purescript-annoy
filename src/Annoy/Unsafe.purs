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
   . Int 
  -> Array Number
  -> STPrimAnnoy h
  -> Eff ( st :: ST h | r ) Unit

-- | Builds a forest of given number of trees. After calling, no more items can be added.
foreign import unsafeBuild
  :: forall h r
   . Int
  -> STPrimAnnoy h
  -> Eff ( st :: ST h | r ) Unit

-- | Saves the index to disk.
foreign import save
  :: forall h r
   . String
  -> STPrimAnnoy h
  -> Eff ( st :: ST h, fs :: FS | r ) Boolean

-- | Loads an index from disk.
foreign import load
  :: forall h r
   . String
  -> STPrimAnnoy h
  -> Eff ( st :: ST h, fs :: FS | r ) Boolean

foreign import unload
  :: forall h r
   . STPrimAnnoy h
  -> Eff ( st :: ST h, fs :: FS | r ) Unit

-- | Returns vector under given index. No bounds checks are performed.
foreign import unsafeGetItem
  :: forall h r
   . Int
  -> STPrimAnnoy h
  -> Eff ( st :: ST h | r ) (Array Number)

-- | `unsafeGetNNsByItem i n search_k`
-- | Returns `n` closest items to the `i`-th vector. Pass default -1 for `search_k`, for more info visit [original annoy](https://github.com/spotify/annoy). No bounds checks are performed.
foreign import unsafeGetNNsByItem
  :: forall h r
   . Int
  -> Int
  -> Int
  -> STPrimAnnoy h
  -> Eff ( st :: ST h | r ) (Array Int)

-- | Like `unsafeGetNNsByItem` but returns also distances
foreign import unsafeGetNNsByItem_
  :: forall h r
   . Int
  -> Int
  -> Int
  -> STPrimAnnoy h
  -> Eff ( st :: ST h | r ) { neighbors :: Array Int, distances :: Array Int }

-- | `unsafeGetNNsByVector v n search_k`
-- | Like above but query by vector v instead of index.
foreign import unsafeGetNNsByVector
  :: forall h r
   . Array Number
  -> Int
  -> Int
  -> STPrimAnnoy h
  -> Eff ( st :: ST h | r ) (Array Int)

-- | Like `unsafeGetNNsByVector` but returns also distances
foreign import unsafeGetNNsByVector_
  :: forall h r
   . Array Number
  -> Int
  -> Int
  -> STPrimAnnoy h
  -> Eff ( st :: ST h | r ) { neighbors :: Array Int, distances :: Array Int }

-- | Returns number of (allocated!) elements in Annoy.
foreign import getNItems
  :: forall h r
   . STPrimAnnoy h
  -> Eff ( st :: ST h | r ) Int

-- | Returns the distance between items at positions `i` and `j`. No bounds checks are performed.
foreign import unsafeGetDistance
  :: forall h r
   . Int
  -> Int
  -> STPrimAnnoy h
  -> Eff ( st :: ST h | r ) Number
