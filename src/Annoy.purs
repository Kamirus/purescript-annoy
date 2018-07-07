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
import Control.Monad.Eff (Eff, runPure)
import Control.Monad.ST (ST, runST)
import Data.Foldable (class Foldable, traverse_)
import Data.Foreign.NullOrUndefined (undefined)
import Data.Maybe (Maybe(..), fromJust)
import Data.Typelevel.Num (class Nat, class Pos, toInt)
import Data.Vec (Vec, fromArray, toArray)
import Node.FS (FS)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

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

fromVectors_
  :: forall s f o
   . Nat s
  => Foldable f
  => { trees :: Int , metric :: Metric | o }
  -> f (Vec s Number)
  -> Maybe (Annoy s)
fromVectors_ ops@{ metric } vectors = build_ ops (do
  a <- new { size: (unsafeCoerce unit :: s) , metric }
  traverse_ (\v -> push v a) vectors
  pure a)

-- | `get i annoy` returns `i`-th vector. Performs bounds check.
get :: forall s. Nat s => Int -> Annoy s -> Maybe (Vec s Number)
get i a = 
  if 0 <= i && i < length a
  then Just $ unsafeGet i a
  else Nothing

-- | Similar to `get` but no bounds checks are performed.
unsafeGet :: forall s. Nat s => Int -> Annoy s -> Vec s Number
unsafeGet i a = unsafeFromArray $ runPure (runST (U.unsafeGetItem i $ unsafeCoerce a))

-- | `length annoy` returns number of stored vectors
length :: forall s. Annoy s -> Int
length a = runPure (runST (U.getNItems $ unsafeCoerce a))

-- | `save path annoy` dumps annoy to the file. Boolean indicates succes or failure.
save :: forall s r. String -> Annoy s -> Eff ( fs :: FS | r ) Boolean
save path a = runST (U.save path $ unsafeCoerce a)

-- | `unsafeLoad path { size , metric }` creates `STAnnoy` using `size` and `metric`, then loads annoy using `path`
-- | Unsafe aspect is that it does not check loaded vector sizes against `size`
unsafeLoad
  :: forall r s o
   . Nat s
  => String
  -> { size :: s , metric :: Metric | o }
  -> Eff ( fs :: FS | r ) (Maybe (Annoy s))
unsafeLoad path ops = runST (do
  stAnnoy <- new ops
  isOk <- U.unsafeLoad path $ unsafeCoerce stAnnoy
  if isOk then Just <$> unsafeFreeze stAnnoy else pure Nothing)

-- | `nnsByItem i n update a`
-- | Works like `nnsByVec`, but requires index instead of vector
nnsByItem
  :: forall s
   . Nat s
  => Int
  -> Int
  -> ({ searchK :: Int } -> { searchK :: Int })
  -> Annoy s
  -> Maybe (Array Int)
nnsByItem i n update a = nnsByVec <$> (get i a) <@> n <@> update <@> a

-- | `nnsByItem_ i n update a`
-- | Works like `nnsByVec_`, but requires index instead of vector
nnsByItem_
  :: forall s
   . Nat s
  => Int
  -> Int
  -> ({ searchK :: Int } -> { searchK :: Int })
  -> Annoy s
  -> Maybe { neighbors :: Array Int , distances :: Array Int }
nnsByItem_ i n update a = nnsByVec_ <$> (get i a) <@> n <@> update <@> a

-- | `nnsByVec v n update a`
-- | Returns `n` closest items to the `v` vector. `update` is used to set **search_k** parameter.
-- | `nnsByVec v n id a` to use default `searchK`
-- | `nnsByVec v n (_ { searchK = 123 }) a` to set `searchK`
nnsByVec
  :: forall s
   . Nat s
  => Vec s Number
  -> Int
  -> ({ searchK :: Int } -> { searchK :: Int })
  -> Annoy s
  -> Array Int
nnsByVec = _nnsByVec U.unsafeGetNNsByVector

-- | `nnsByVec_ v n update a`
-- | Returns `n` closest items to the `v` vector with distances. `update` is used to set **search_k** parameter.
-- | `nnsByVec_ v n id a` to use default `searchK`
-- | `nnsByVec_ v n (_ { searchK = 123 }) a` to set `searchK`
nnsByVec_
  :: forall s
   . Nat s
  => Vec s Number
  -> Int ->
  ({ searchK :: Int } -> { searchK :: Int })
  -> Annoy s
  -> { neighbors :: Array Int , distances :: Array Int }
nnsByVec_ = _nnsByVec U.unsafeGetNNsByVector_

_nnsByVec
  :: forall s a
   . Nat s
  => (forall h r. Array Number -> Int -> Int -> STPrimAnnoy h -> Eff ( st :: ST h | r ) a)
  -> Vec s Number
  -> Int
  -> ({ searchK :: Int } -> { searchK :: Int })
  -> Annoy s
  -> a
_nnsByVec f v n update a = 
  runPure (runST (f (toArray v) n ops.searchK (unsafeCoerce a)))
  where
  ops :: { searchK :: Int }
  ops = update { searchK: unsafeCoerce undefined }

distance :: forall s. Nat s => Int -> Int -> Annoy s -> Maybe Number
distance i j a = if i < 0 || j < 0 || i >= n || j >= n then Nothing
  else Just $ unsafeDistance i j a
  where n = length a

unsafeDistance :: forall s. Nat s => Int -> Int -> Annoy s -> Number
unsafeDistance i j a = runPure (runST (U.unsafeGetDistance i j $ unsafeCoerce a))

unsafeFromArray :: forall a s. Nat s => Array a -> Vec s a
unsafeFromArray arr = unsafePartial $ fromJust $ fromArray arr
