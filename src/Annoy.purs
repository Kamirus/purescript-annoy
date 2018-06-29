module Annoy
  ( get
  , length
  -- , save
  -- , load
  -- , nnsByItem
  -- , nnsByItem_
  -- , nnsByVec
  -- , nnsByVec_
  -- , distance
  ) where

import Prelude

import Annoy.Types (Annoy)
import Annoy.Unsafe as U
import Control.Monad.Eff (Eff, runPure)
import Control.Monad.ST (runST)
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..), fromJust)
import Data.Typelevel.Num (class Nat)
import Data.Vec (Vec, fromArray)
import Node.FS (FS)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

-- fromVectors
--   :: forall t s
--    . Foldable t
--   => Nat s
--   => t (Vec s Number)
--   -> Annoy s
-- fromVectors = ...

get :: forall s. Nat s => Int -> Annoy s -> Maybe (Vec s Number)
get i a = 
  if 0 <= i && i < length a
  then Just $ unsafeFromArray $ runPure (runST (U.unsafeGetItem i $ unsafeCoerce a))
  else Nothing

length :: forall s. Annoy s -> Int
length a = runPure (runST (U.getNItems $ unsafeCoerce a))

save :: forall s r. String -> Annoy s -> Eff ( fs :: FS | r ) Boolean
save path a = runST (U.save path $ unsafeCoerce a)

-- load

-- nnsByItem

-- nnsByItem_

-- nnsByVec

-- nnsByVec_

-- distance

unsafeFromArray :: forall a s. Nat s => Array a -> Vec s a
unsafeFromArray arr = unsafePartial $ fromJust $ fromArray arr
