module Test.Units.Unsafe where

import Prelude

import Annoy.Unsafe (unsafeAddItem, unsafeGetItem, unsafeNew)
import Control.Monad.Eff (runPure)
import Control.Monad.Free (Free)
import Control.Monad.ST (runST)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as A

testUnsafe :: forall t. Free (TestF t) Unit
testUnsafe = suite "Unsafe" do
  suite "simple calls" do
    test "new add get" do
      let v = [0.25, 0.5, 0.25]
      let v' = newAddGet v
      A.assert "xd" $ v == v'
  where
    newAddGet v = runPure (runST (do 
      a <- unsafeNew 3 ""
      unsafeAddItem a 0 v
      unsafeGetItem a 0))
