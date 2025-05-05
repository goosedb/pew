module Pew.Logger.Interface.Implicit where

import Data.Text (Text)
import Data.Text.Builder.Linear qualified as T
import Pew.Logger.General qualified as G

type WithLogger time severity m = (?pew_implicit_logger :: G.LoggerHandle m time severity, ?pew_get_now :: m time)

getLogger :: (WithLogger time severity m) => G.LoggerHandle m time severity
getLogger = ?pew_implicit_logger

withLogger :: G.LoggerHandle m time severity -> m time -> ((WithLogger time severity m) => a) -> a
withLogger logger getTime a =
  let ?pew_implicit_logger = logger
      ?pew_get_now = getTime
   in a

pushNamespace :: (WithLogger time severity m) => [Text] -> ((WithLogger time severity m) => a) -> a
pushNamespace namespaces = withLogger (G.pushNamespace namespaces getLogger) ?pew_get_now

withLabels :: (WithLogger time severity m) => [G.Label] -> ((WithLogger time severity m) => a) -> a
withLabels labels = withLogger (G.addLabels labels getLogger) ?pew_get_now

withLabel :: (WithLogger time severity m) => G.Label -> ((WithLogger time severity m) => a) -> a
withLabel l = withLabels [l]

logMsg :: (WithLogger time severity m, Monad m) => severity -> T.Builder -> m ()
logMsg sev msg = do
  now <- ?pew_get_now
  G.writeLog getLogger sev now msg
