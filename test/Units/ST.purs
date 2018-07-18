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

testST :: Free TestF Unit
testST = suite "ST" do
  suite "creations" do
    test "new 3xpush build 3xget" do
      let a = newPush3Build d1
      equal v0 $ unsafeGet a 0
      equal v1 $ unsafeGet a 1
      equal v2 $ unsafeGet a 2

v0 :: Vec D2 Number
v0 = 1.0 +> 2.0 +> empty

v1 :: Vec D2 Number
v1 = 3.0 +> 4.0 +> empty

v2 :: Vec D2 Number
v2 = 5.0 +> 6.0 +> empty

newPush3Build :: forall t. Pos t => t -> Annoy D2
newPush3Build trees = build { trees } (do
  a <- new { size: d2 , metric: Manhattan }
  push a v0
  push a v1
  push a v2
  pure a)

