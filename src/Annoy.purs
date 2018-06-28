module Annoy
  ( get
  , length
  ) where

import Prelude

import Annoy.Types (Annoy)
import Annoy.Unsafe (getNItems, unsafeGetItem)
import Control.Monad.Eff (runPure)
import Control.Monad.ST (runST)
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..), fromJust)
import Data.Typelevel.Num (class Nat)
import Data.Vec (Vec, fromArray)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

unsafeFromArray :: forall a s. Nat s => Array a -> Vec s a
unsafeFromArray arr = unsafePartial $ fromJust $ fromArray arr

get
  :: forall s
   . Nat s
  => Int
  -> Annoy s
  -> Maybe (Vec s Number)
get i a = 
  if 0 <= i && i < length a
  then Just $ unsafeFromArray $ runPure (runST (unsafeGetItem i $ unsafeCoerce a))
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


