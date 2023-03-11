{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}

module Pew.Logger.Implicit where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Aeson as J
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as T
import Data.Time (getCurrentTime)
import Pew.Logger (LoggerHandle, addLabels, dropLabels, writeLog)

type WithLogger_ sev = (?logger :: LoggerHandle sev)

withLogger :: LoggerHandle sev -> (WithLogger_ sev => a) -> a
withLogger logger a = let ?logger = logger in a

withLabels :: WithLogger_ sev => [(T.Text, J.Value)] -> (WithLogger_ sev => a) -> a
withLabels labels a = let ?logger = addLabels labels ?logger in a

withLabel :: J.ToJSON v => WithLogger_ sev => T.Text -> v -> (WithLogger_ sev => a) -> a
withLabel k v a = let ?logger = addLabels [(k, J.toJSON v)] ?logger in a

withoutLabels :: WithLogger_ sev => [T.Text] -> (WithLogger_ sev => a) -> a
withoutLabels labelKeys a = let ?logger = dropLabels labelKeys ?logger in a

withoutLabel :: WithLogger_ sev => T.Text -> (WithLogger_ sev => a) -> a
withoutLabel labelKeys a = let ?logger = dropLabels [labelKeys] ?logger in a

logMsg :: (MonadIO m, WithLogger_ sev) => sev -> T.Builder -> m ()
logMsg sev msg = do
  now <- liftIO getCurrentTime
  liftIO $ writeLog ?logger sev now msg
