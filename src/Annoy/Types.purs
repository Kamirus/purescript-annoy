module Annoy.Types where

import Prelude

-- | `STPrimAnnoy h` foreign type used in Unsafe module.
foreign import data STPrimAnnoy :: Type -> Type

-- | `Annoy s` where `s` is size of vectors.
-- | Built/Immutable Annoy.
foreign import data Annoy :: Type -> Type

-- | `STAnnoy h s` where s is allowed size of vectors.
-- | Similar to `STPrimAnnoy` but keeps track of vectors size `s`.
foreign import data STAnnoy :: Type -> Type -> Type

data Metric = Angular | Manhattan | Euclidean

instance showMetric :: Show Metric where
  show Angular = "Angular"
  show Manhattan = "Manhattan"
  show Euclidean = "Euclidean"
