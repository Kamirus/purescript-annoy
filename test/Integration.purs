module Test.Integration where

import Prelude

import Annoy (distance, fromVectors, length, nnsByItem, nnsByItem_, nnsByVec, nnsByVec_, save, unsafeGet, unsafeLoad)
import Annoy.Types (Annoy, Metric(..))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Free (Free)
import Data.Array ((..))
import Data.Array as Array
import Data.Foldable (traverse_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Typelevel.Num (class Nat, class Pos, d10, d5)
import Data.Vec (empty, replicate, (+>))
import Node.FS (FS)
import Partial.Unsafe (unsafePartial)
import Test.Unit (TestF, failure, suite, test)
import Test.Unit.Assert (assert, equal)

path :: String
path = ".path.txt"

integration :: forall r. Free (TestF ( fs :: FS | r )) Unit
integration = suite "integration" $ do
  test "save a; a' <- load; a == a'; nns's; distance" $ do
    let ops = { trees: d10, size: d5, metric: Euclidean }
    let a = createByReplicate ops $ 1 .. 500
    isSaved <- liftEff $ save path a
    assert "save failed" isSaved
    maybeA <- liftEff $ unsafeLoad path ops
    case maybeA of
      Nothing -> failure "load failed"
      Just a' -> do
        equalAnnoys a a'
        let v = (0.0 +> 0.0 +> 0.0 +> 0.0 +> 0.0 +> empty)
        equal 10 $ Array.length $ nnsByVec v 10 id a'
        equal 10 $ Array.length $ nnsByVec v 10 (_ { searchK = 1 }) a'
        equal 10 $ Array.length (nnsByVec_ v 10 id a').distances
        equal 10 $ Array.length (nnsByVec_ v 10 id a').neighbors
        assert "distance not calculated" $ isJust $ distance 0 5 a
        equal 10 $ Array.length $ unsafePartial $ fromJust $ nnsByItem 0 10 id a'
        equal 10 $ Array.length $ unsafePartial $ fromJust $ nnsByItem 0 10 (_ { searchK = 1 }) a'
        equal 10 $ Array.length (unsafePartial $ fromJust $ nnsByItem_ 0 10 id a').distances
        equal 10 $ Array.length (unsafePartial $ fromJust $ nnsByItem_ 0 10 id a').neighbors

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
  let vs = arr # map (replicate size <<< (_ / 100.0) <<< toNumber) in
  fromVectors { trees , metric } vs
