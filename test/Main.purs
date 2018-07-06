module Test.Main where

import Prelude

import Test.ST (testST)
import Test.Unit (suite)
import Test.Unit.Main (runTest)
import Test.Unsafe (testUnsafe)


main = runTest do
  suite "Unsafe" testUnsafe
  suite "ST" testST
