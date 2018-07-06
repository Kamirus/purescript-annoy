module Test.Main where

import Prelude

import Annoy (length)
import Data.Typelevel.Num (d10)
import Test.Integration as I
import Test.ST (newPush3Build, testST)
import Test.Unit (suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Main (runTest)
import Test.Unsafe (testUnsafe)


units = runTest do
  suite "Unsafe" testUnsafe
  suite "ST" testST
  suite "Pure" $ do
    test "length 3" do
      let a = newPush3Build d10
      equal 3 $ length a

main = do
  units
  I.main
