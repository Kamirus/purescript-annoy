module Test.Units where

import Prelude

import Annoy (length)
import Control.Monad.Free (Free)
import Data.Typelevel.Num (d10)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert (equal)
import Test.Units.ST (newPush3Build, testST)
import Test.Units.Unsafe (testUnsafe)

units :: Free TestF Unit
units = suite "Units" do
  testUnsafe
  testST
  suite "Pure" $ do
    test "length 3" do
      let a = newPush3Build d10
      equal 3 $ length a
