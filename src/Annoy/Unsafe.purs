module Annoy.Unsafe where

import Prelude

import Annoy.Types (STPrimAnnoy)
import Effect (Effect)

-- | `unsafeNew f metric`
-- | Creates new `STPrimAnnoy` that stores vector of `f` dimensions with `metric`. Does not validate input.
foreign import unsafeNew
  :: forall h
   . Int
  -> String
  -> Effect (STPrimAnnoy h)

-- | Inserts vector at given index. No checks for: not frozen `STPrimAnnoy`, matching dimension, negative index.
foreign import unsafeAddItem
  :: forall h
   . STPrimAnnoy h
  -> Int
  -> Array Number
  -> Effect Unit

-- | Builds a forest of given number of trees. After calling, no more items can be added.
foreign import unsafeBuild
  :: forall h
   . STPrimAnnoy h
  -> Int
  -> Effect Unit

-- | Saves the index to disk.
foreign import save
  :: forall h
   . STPrimAnnoy h
  -> String
  -> Effect Boolean

-- | Loads an index from disk.
foreign import unsafeLoad
  :: forall h
   . STPrimAnnoy h
  -> String
  -> Effect Boolean

foreign import unload
  :: forall h
   . STPrimAnnoy h
  -> Effect Unit

-- | Returns vector under given index. No bounds checks are performed.
foreign import unsafeGetItem
  :: forall h
   . STPrimAnnoy h
  -> Int
  -> Effect (Array Number)

-- | `unsafeGetNNsByItem a i n search_k`
-- | Returns `n` closest items to the `i`-th vector. No bounds checks are performed.
foreign import unsafeGetNNsByItem
  :: forall h
   . STPrimAnnoy h
  -> Int
  -> Int
  -> Int
  -> Effect (Array Int)

-- | Like `unsafeGetNNsByItem` but returns also distances
foreign import unsafeGetNNsByItem_
  :: forall h
   . STPrimAnnoy h
  -> Int
  -> Int
  -> Int
  -> Effect { neighbors :: Array Int, distances :: Array Int }

-- | `unsafeGetNNsByVector a v n search_k`
-- | Like above but query by vector v instead of index.
foreign import unsafeGetNNsByVector
  :: forall h
   . STPrimAnnoy h
  -> Array Number
  -> Int
  -> Int
  -> Effect (Array Int)

-- | Like `unsafeGetNNsByVector` but returns also distances
foreign import unsafeGetNNsByVector_
  :: forall h
   . STPrimAnnoy h
  -> Array Number
  -> Int
  -> Int
  -> Effect { neighbors :: Array Int, distances :: Array Int }

-- | Returns number of (allocated!) elements in Annoy.
foreign import getNItems
  :: forall h
   . STPrimAnnoy h
  -> Effect Int

-- | Returns the distance between items at positions `i` and `j`. No bounds checks are performed.
foreign import unsafeGetDistance
  :: forall h
   . STPrimAnnoy h
  -> Int
  -> Int
  -> Effect Number
