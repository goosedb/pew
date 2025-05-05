{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Pew.QuickStart.Interface.Monad (
  module Pew.QuickStart.Interface.Monad,
  (G..>),
)
where

import Data.Text (Text)
import Data.Text.Builder.Linear qualified as T
import Data.Time (UTCTime)
import Pew.Logger.General (Label)
import Pew.Logger.General qualified as G
import Pew.Logger.Interface.Monad as M
import Pew.Severity (Severity (..))

withLabels :: (MonadLogger UTCTime Severity m, Monad m) => [Label] -> m a -> m a
withLabels = M.withLabels @UTCTime @Severity

withLabel :: (MonadLogger UTCTime Severity m, Monad m) => Label -> m a -> m a
withLabel = M.withLabel @UTCTime @Severity

pushNamespace :: (MonadLogger UTCTime Severity m) => [Text] -> m a -> m a
pushNamespace = M.pushNamespace @UTCTime @Severity

debug, info, notice, warn, err, critical, alert, emergency :: (MonadLogger UTCTime Severity m) => T.Builder -> m ()
debug = logMsg @UTCTime Debug
info = logMsg @UTCTime Info
notice = logMsg @UTCTime Notice
warn = logMsg @UTCTime Warning
err = logMsg @UTCTime Error
critical = logMsg @UTCTime Critical
alert = logMsg @UTCTime Alert
emergency = logMsg @UTCTime Emergency
