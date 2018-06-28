module Annoy where

import Prelude

import Annoy.Unsafe (getNItems, unsafeGetItem)
import Control.Monad.Eff (runPure)
import Control.Monad.ST (runST)
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (class Lt, class Nat, toInt)
import Unsafe.Coerce (unsafeCoerce)

-- | `Annoy s` where `s` is size of vectors
foreign import data Annoy :: Type -> Type

get
  :: forall s
   . Int
  -> Annoy s
  -> Maybe (Array Number)
get i a = 
  if 0 <= i && i < length a
  then Just $ runPure (runST (unsafeGetItem i $ unsafeCoerce a))
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


