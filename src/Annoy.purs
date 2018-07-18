module Annoy
  ( get
  , unsafeGet
  , length
  , save
  , unsafeLoad
  , nnsByItem
  , nnsByItem_
  , nnsByVec
  , nnsByVec_
  , distance
  , unsafeDistance
  , fromVectors
  , fromVectors_
  ) where

import Prelude

import Annoy.ST (build_, new, push, unsafeFreeze)
import Annoy.Types (Annoy, Metric, STPrimAnnoy)
import Annoy.Unsafe as U
import Data.Foldable (class Foldable, traverse_)
import Data.Maybe (Maybe(..), fromJust)
import Data.Typelevel.Num (class Nat, class Pos, toInt)
import Data.Vec (Vec, fromArray, toArray)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Foreign.NullOrUndefined (undefined)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

-- | `fromVectors options@{ trees , metric } vectors`
-- | Creates new Annoy with `metric`, adds given vectors and executes `build` using `trees`.
fromVectors
  :: forall s t f o
   . Nat s
  => Foldable f
  => Pos t
  => { trees :: t , metric :: Metric | o }
  -> f (Vec s Number)
  -> Annoy s
fromVectors ops vectors = unsafePartial $ fromJust $
  fromVectors_ (ops { trees = toInt ops.trees }) vectors

-- | Similar to `fromVectors` but `trees` :: Int.
fromVectors_
  :: forall s f o
   . Nat s
  => Foldable f
  => { trees :: Int , metric :: Metric | o }
  -> f (Vec s Number)
  -> Maybe (Annoy s)
fromVectors_ ops@{ metric } vectors = build_ ops (do
  a <- new { size: (unsafeCoerce unit :: s) , metric }
  traverse_ (\v -> push a v) vectors
  pure a)

-- | `get annoy i` returns `i`-th vector. Performs bounds check.
get :: forall s. Nat s => Annoy s -> Int -> Maybe (Vec s Number)
get a i =
  if 0 <= i && i < length a
  then Just $ unsafeGet a i
  else Nothing

-- | Similar to `get` but no bounds checks are performed.
unsafeGet :: forall s. Nat s => Annoy s -> Int -> Vec s Number
unsafeGet a i = unsafeFromArray $ unsafePerformEffect (U.unsafeGetItem (unsafeCoerce a) i)

-- | `length annoy` returns number of stored vectors
length :: forall s. Annoy s -> Int
length a = unsafePerformEffect (U.getNItems $ unsafeCoerce a)

-- | `save annoy path` dumps annoy to the file. Boolean indicates succes or failure.
save :: forall s. Annoy s -> String -> Effect Boolean
save a path = U.save (unsafeCoerce a) path

-- | `unsafeLoad { size , metric } path` creates `STAnnoy` using `size` and `metric`, then loads annoy using `path`
-- | Unsafe aspect is that it does not check loaded vector sizes against `size`
unsafeLoad
  :: forall s o
   . Nat s
  => { size :: s , metric :: Metric | o }
  -> String
  -> Effect (Maybe (Annoy s))
unsafeLoad ops path = (do
  stAnnoy <- new ops
  isOk <- U.unsafeLoad (unsafeCoerce stAnnoy) path
  if isOk then Just <$> unsafeFreeze stAnnoy else pure Nothing)

-- | `nnsByItem a i n update`
-- | Works like `nnsByVec`, but requires index instead of vector
nnsByItem
  :: forall s
   . Nat s
  => Annoy s
  -> Int
  -> Int
  -> ({ searchK :: Int } -> { searchK :: Int })
  -> Maybe (Array Int)
nnsByItem a i n update = nnsByVec a <$> (get a i) <@> n <@> update

-- | `nnsByItem_ a i n update`
-- | Works like `nnsByVec_`, but requires index instead of vector
nnsByItem_
  :: forall s
   . Nat s
  => Annoy s
  -> Int
  -> Int
  -> ({ searchK :: Int } -> { searchK :: Int })
  -> Maybe { neighbors :: Array Int , distances :: Array Int }
nnsByItem_ a i n update = nnsByVec_ a <$> (get a i) <@> n <@> update

-- | `nnsByVec a v n update`
-- | Returns `n` closest items to the `v` vector. `update` is used to set **search_k** parameter.
-- | `nnsByVec a v n id` to use default `searchK`
-- | `nnsByVec a v n (_ { searchK = 123 })` to set `searchK`
nnsByVec
  :: forall s
   . Nat s
  => Annoy s
  -> Vec s Number
  -> Int
  -> ({ searchK :: Int } -> { searchK :: Int })
  -> Array Int
nnsByVec = _nnsByVec U.unsafeGetNNsByVector

-- | Works like `nnsByVec` but also returns distances
nnsByVec_
  :: forall s
   . Nat s
  => Annoy s
  -> Vec s Number
  -> Int
  -> ({ searchK :: Int } -> { searchK :: Int })
  -> { neighbors :: Array Int , distances :: Array Int }
nnsByVec_ = _nnsByVec U.unsafeGetNNsByVector_

_nnsByVec
  :: forall s a
   . Nat s
  => (forall h. STPrimAnnoy h -> Array Number -> Int -> Int -> Effect a)
  -> Annoy s
  -> Vec s Number
  -> Int
  -> ({ searchK :: Int } -> { searchK :: Int })
  -> a
_nnsByVec f a v n update =
  unsafePerformEffect (f (unsafeCoerce a) (toArray v) n ops.searchK)
  where
  ops :: { searchK :: Int }
  ops = update { searchK: unsafeCoerce undefined }

distance :: forall s. Nat s => Annoy s -> Int -> Int -> Maybe Number
distance a i j = if i < 0 || j < 0 || i >= n || j >= n then Nothing
  else Just $ unsafeDistance a i j
  where n = length a

unsafeDistance :: forall s. Nat s => Annoy s -> Int -> Int -> Number
unsafeDistance a i j = unsafePerformEffect (U.unsafeGetDistance (unsafeCoerce a) i j)

unsafeFromArray :: forall a s. Nat s => Array a -> Vec s a
unsafeFromArray arr = unsafePartial $ fromJust $ fromArray arr
