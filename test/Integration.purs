module Test.Integration where

import Prelude

import Annoy (fromVectors, length, save, unsafeGet, unsafeLoad)
import Annoy.Types (Annoy, Metric(..))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Free (Free)
import Data.Array ((..))
import Data.Foldable (traverse_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (class Nat, class Pos, d10, d5)
import Data.Vec (replicate)
import Node.FS (FS)
import Test.Unit (TestF, failure, suite, test)
import Test.Unit.Assert (assert, equal)

path :: String
path = ".path.txt"

integration :: forall r. Free (TestF ( fs :: FS | r )) Unit
integration = suite "integration" $ do
  test "save a; a' <- load; a == a'" $ do
    let ops = { trees: d10, size: d5, metric: Angular }
    let a = createByReplicate ops $ 1 .. 100
    isSaved <- liftEff $ save path a
    assert "is saved" isSaved
    maybeA <- liftEff $ unsafeLoad path ops.size ops.metric
    case maybeA of
      Nothing -> failure "load failed"
      Just a' -> equalAnnoys a a'

equalAnnoys :: forall s e. Nat s => Annoy s -> Annoy s -> Aff e Unit
equalAnnoys a a' = if n /= length a' - 1 then failure "lengths not equal"
  else traverse_ f $ 0 .. n
  where
  n = length a - 1
  f i = equal (unsafeGet i a) (unsafeGet i a')

createByReplicate
  :: forall t s r
   . Nat s
  => Pos t
  => { trees :: t, size :: s, metric :: Metric | r }
  -> Array Int
  -> Annoy s
createByReplicate { trees, size, metric } arr =
  let vs = arr # map (replicate size <<< toNumber) in
  fromVectors trees metric vs
