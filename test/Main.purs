module Test.Main where

import Test.Unit (suite)
import Test.Unit.Main (runTest)
import Test.Unsafe (testUnsafe)

main = runTest do
  suite "Unsafe" testUnsafe
