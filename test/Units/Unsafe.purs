module Test.Units.Unsafe where

import Prelude

import Annoy.Unsafe (unsafeAddItem, unsafeGetItem, unsafeNew)
import Control.Monad.Free (Free)
import Effect.Unsafe (unsafePerformEffect)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as A

testUnsafe :: Free TestF Unit
testUnsafe = suite "Unsafe" do
  suite "simple calls" do
    test "new add get" do
      let v = [0.25, 0.5, 0.25]
      let v' = newAddGet v
      A.assert "xd" $ v == v'
  where
    newAddGet v = unsafePerformEffect (do
      a <- unsafeNew 3 ""
      unsafeAddItem a 0 v
      unsafeGetItem a 0)
