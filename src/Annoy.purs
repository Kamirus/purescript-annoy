module Annoy
  ( get
  , unsafeGet
  , length
  , save
  , unsafeLoad
  -- , nnsByItem
  -- , nnsByItem_
  -- , nnsByVec
  -- , nnsByVec_
  -- , distance
  , fromVectors
  , fromVectors_
  ) where

import Prelude

import Annoy.ST (build_, new, push, unsafeFreeze)
import Annoy.Types (Annoy, Metric)
import Annoy.Unsafe as U
import Control.Monad.Eff (Eff, runPure)
import Control.Monad.ST (runST)
import Data.Foldable (class Foldable, traverse_)
import Data.Maybe (Maybe(..), fromJust)
import Data.Typelevel.Num (class Nat, class Pos, toInt)
import Data.Typelevel.Undefined (undefined)
import Data.Vec (Vec, fromArray)
import Node.FS (FS)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

fromVectors
  :: forall s t f
   . Nat s
  => Foldable f
  => Pos t
  => t
  -> Metric
  -> f (Vec s Number)
  -> Annoy s
fromVectors trees metric vectors = unsafePartial $ fromJust $ fromVectors_ (toInt trees) metric vectors

fromVectors_
  :: forall s f
   . Nat s
  => Foldable f
  => Int
  -> Metric
  -> f (Vec s Number)
  -> Maybe (Annoy s)
fromVectors_ trees metric vectors = build_ trees (do
  a <- new (undefined :: s) metric
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

-- | `unsafeLoad path s metric` creates `STAnnoy` using `s` and `metric`, then loads annoy using `path`
-- | Unsafe aspect is that it does not check loaded vector sizes against `s`
unsafeLoad
  :: forall r s
   . Nat s
  => String
  -> s
  -> Metric
  -> Eff ( fs :: FS | r ) (Maybe (Annoy s))
unsafeLoad path s metric = runST (do
  stAnnoy <- new s metric
  isOk <- U.unsafeLoad path $ unsafeCoerce stAnnoy
  if isOk then Just <$> unsafeFreeze stAnnoy else pure Nothing)

-- nnsByItem

-- nnsByItem_

-- nnsByVec

-- nnsByVec_

-- distance

unsafeFromArray :: forall a s. Nat s => Array a -> Vec s a
unsafeFromArray arr = unsafePartial $ fromJust $ fromArray arr
