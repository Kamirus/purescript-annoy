module Annoy
  ( Annoy
  , get
  , length
  ) where

import Prelude

import Annoy.Unsafe (STPrimAnnoy, getNItems, unsafeGetItem)
import Control.Monad.Eff (Eff, runPure)
import Control.Monad.ST (ST, runST)
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..), fromJust)
import Data.Typelevel.Num (class Lt, class Nat, toInt)
import Data.Vec (Vec, fromArray)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

toPrim :: forall s h. Nat s => Annoy s -> STPrimAnnoy h
toPrim = unsafeCoerce

unsafeFromArray :: forall a s. Nat s => Array a -> Vec s a
unsafeFromArray arr = unsafePartial $ fromJust $ fromArray arr

-- | `Annoy s` where `s` is size of vectors
foreign import data Annoy :: Type -> Type

get
  :: forall s
   . Nat s
  => Int
  -> Annoy s
  -> Maybe (Vec s Number)
get i a = 
  if 0 <= i && i < length a
  then Just $ unsafeFromArray $ runPure (runST (unsafeGetItem i $ toPrim a))
  else Nothing

length
  :: forall s
   . Annoy s
  -> Int
length a = runPure (runST (getNItems $ unsafeCoerce a))

-- fromVectors
--   :: forall t s
--    . Foldable t
--   => Nat s
--   => t (Vec s Number)
--   -> Annoy s
-- fromVectors = ...


