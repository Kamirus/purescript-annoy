module Test.Main where

import Prelude

import Test.Integration (integration)
import Test.Unit.Main (runTest)
import Test.Units (units)

main = runTest do
  units
  integration
