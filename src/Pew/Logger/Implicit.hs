module Pew.Logger.Implicit where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Aeson as J
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as T
import Data.Time (getCurrentTime)
import Pew.Logger (LoggerHandle, addLabels, dropLabels, writeLog)

type WithLogger_ sev = (?pew_implicit_logger :: LoggerHandle sev)

getLogger :: WithLogger_ sev => LoggerHandle sev
getLogger = ?pew_implicit_logger

withLogger :: LoggerHandle sev -> (WithLogger_ sev => a) -> a
withLogger logger a =
  let ?pew_implicit_logger = logger
   in a

withLabels :: WithLogger_ sev => [(T.Text, J.Value)] -> (WithLogger_ sev => a) -> a
withLabels labels = withLogger (addLabels labels getLogger)

withLabel :: J.ToJSON v => WithLogger_ sev => T.Text -> v -> (WithLogger_ sev => a) -> a
withLabel k v = withLabels [(k, J.toJSON v)]

withoutLabels :: WithLogger_ sev => [T.Text] -> (WithLogger_ sev => a) -> a
withoutLabels labelKeys = withLogger (dropLabels labelKeys getLogger)

withoutLabel :: WithLogger_ sev => T.Text -> (WithLogger_ sev => a) -> a
withoutLabel labelKey = withoutLabels [labelKey]

logMsg :: (MonadIO m, WithLogger_ sev) => sev -> T.Builder -> m ()
logMsg sev msg = do
  now <- liftIO getCurrentTime
  liftIO $ writeLog getLogger sev now msg
