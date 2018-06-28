module Annoy.ST
  ( build
  , new
  , push
  ) where

import Prelude

import Annoy.Types (Annoy, STAnnoy)
import Annoy.Unsafe (getNItems, unsafeAddItem, unsafeBuild, unsafeNew)
import Control.Monad.Eff (Eff, runPure)
import Control.Monad.ST (ST, runST)
import Data.Typelevel.Num (class Nat, toInt)
import Data.Vec (Vec, toArray)
import Unsafe.Coerce (unsafeCoerce)

build
  :: forall s t
   . Nat s
  => Nat t
  => t
  -> (forall h. Eff ( st :: ST h ) (STAnnoy h s))
  -> Annoy s
build trees m = runPure (runST (do 
  a <- m
  unsafeBuild (toInt trees) $ unsafeCoerce a
  unsafeFreeze a))

new
  :: forall h r s
   . Nat s
  => s
  -> String
  -> Eff ( st :: ST h | r ) (STAnnoy h s)
new s metric = unsafeCoerce $ unsafeNew (toInt s) metric

push
  :: forall h r s
   . Nat s
  => Vec s Number
  -> STAnnoy h s
  -> Eff ( st :: ST h | r ) Unit
push v annoy = do
  let a = unsafeCoerce annoy
  len <- getNItems a
  unsafeAddItem len (toArray v) a

unsafeFreeze
  :: forall h r s
   . STAnnoy h s
  -> Eff ( st :: ST h | r ) (Annoy s)
unsafeFreeze = pure <<< unsafeCoerce
