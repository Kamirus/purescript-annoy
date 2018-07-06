module Test.Integration where

import Prelude

import Annoy (fromVectors, length, save, unsafeGet, unsafeLoad)
import Annoy.Types (Annoy, Metric(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Array ((..))
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (class Nat, class Pos, d10, d5)
import Data.Vec (replicate)
import Node.FS (FS)

type Log = Array String

type EWEff r = ExceptT String (WriterT Log (Eff ( fs :: FS | r ))) Unit

path :: String
path = ".path.txt"

test :: forall r. EWEff r
test = do
  tell ["Integration"]
  let ops = { trees: d10, size: d5, metric: Angular }
  let a = createByReplicate ops $ 1 .. 100
  isSaved <- liftEff $ save path a
  if isSaved then tell ["OK: saved"] else throwError "save failed"
  maybeA <- liftEff $ unsafeLoad path ops.size ops.metric
  a' <- case maybeA of
    Nothing -> throwError "load failed"
    Just a -> do
      tell ["OK: loaded"]
      pure a
  equalAnnoys a a'
  tell ["OK: both annoys equal"]

equalAnnoys :: forall s m. Nat s => Applicative m => MonadThrow String m => Annoy s -> Annoy s -> m Unit
equalAnnoys a a' = if n /= length a' - 1 then throwError "lengths not equal"
  else traverse_ f $ 0 .. n
  where
  n = length a - 1
  f i = equal (unsafeGet i a) (unsafeGet i a')

equal :: forall m v. Eq v => Applicative m => MonadThrow String m => Show v => v -> v -> m Unit
equal x y = if x == y then pure unit else throwError $ show x <> " /= " <> show y

main :: forall r. Eff ( fs :: FS, console :: CONSOLE | r ) Unit
main = do 
  Tuple res lines <- runWriterT $ runExceptT test
  traverse_ log lines
  case res of
    Left e -> log $ "ERROR: " <> e
    Right _ -> log "Passed"

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

logExcept :: forall a r. Either String a -> Eff ( console :: CONSOLE | r ) Unit
logExcept (Left e) = log $ "ERROR: " <> e
logExcept (Right _) = log "OK"

assert :: forall e m. MonadThrow e m => Boolean -> e -> m Unit
assert b msg = if not b then throwError msg else pure unit
