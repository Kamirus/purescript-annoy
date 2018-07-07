module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Node.FS (FS)
import Test.Integration (integration)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Test.Units (units)

main
  :: forall eff
   . Eff
     ( console :: CONSOLE
     , testOutput :: TESTOUTPUT
     , avar :: AVAR
     , fs :: FS
     | eff
     ) Unit
main = runTest do
  units
  integration
