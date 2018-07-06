module Test.Units.ST where

import Prelude

import Annoy (unsafeGet)
import Annoy.ST (build, new, push)
import Annoy.Types (Annoy, Metric(..))
import Control.Monad.Free (Free)
import Data.Typelevel.Num (class Pos, D2, d1, d2)
import Data.Vec (Vec, empty, (+>))
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert (equal)

testST :: forall t. Free (TestF t) Unit
testST = suite "ST" do
  suite "creations" do 
    test "new 3xpush build 3xget" do
      let a = newPush3Build d1
      equal v0 $ unsafeGet 0 a
      equal v1 $ unsafeGet 1 a
      equal v2 $ unsafeGet 2 a

v0 :: Vec D2 Number
v0 = 1.0 +> 2.0 +> empty

v1 :: Vec D2 Number
v1 = 3.0 +> 4.0 +> empty

v2 :: Vec D2 Number
v2 = 5.0 +> 6.0 +> empty

newPush3Build :: forall t. Pos t => t -> Annoy D2
newPush3Build trees = build trees (do
  a <- new d2 Manhattan
  push v0 a
  push v1 a
  push v2 a
  pure a)

