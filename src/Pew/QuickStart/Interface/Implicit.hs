module Pew.QuickStart.Interface.Implicit (
  module Pew.QuickStart.Interface.Implicit,
  I.withLogger,
  (G..>),
) where

import Data.Text (Text)
import Data.Text.Builder.Linear qualified as T
import Data.Time (UTCTime)
import Pew.Logger.General
import Pew.Logger.General qualified as G
import Pew.Logger.Interface.Implicit qualified as I
import Pew.Severity (Severity (..))

type WithLogger m = I.WithLogger UTCTime Severity m

withLabels :: (WithLogger m, Monad m) => [Label] -> ((WithLogger m) => a) -> a
withLabels = I.withLabels

withLabel :: (WithLogger m, Monad m) => Label -> ((WithLogger m) => a) -> a
withLabel = I.withLabel

pushNamespace :: (WithLogger m, Monad m) => [Text] -> ((WithLogger m) => a) -> a
pushNamespace = I.pushNamespace

debug, info, notice, warn, err, critical, alert, emergency :: (WithLogger m, Monad m) => T.Builder -> m ()
debug = I.logMsg Debug
info = I.logMsg Info
notice = I.logMsg Notice
warn = I.logMsg Warning
err = I.logMsg Error
critical = I.logMsg Critical
alert = I.logMsg Alert
emergency = I.logMsg Emergency
