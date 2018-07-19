module Test.Main where

import Prelude

import Effect (Effect)
import Test.Integration (integration)
import Test.Unit.Main (runTest)
import Test.Units (units)

main :: Effect Unit
main = runTest do
  units
  integration
