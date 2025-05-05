module Pew.QuickStart.Output.Json where

import Data.Aeson qualified as J
import Data.Aeson.Encoding qualified as J
import Data.Aeson.Key qualified as J
import Data.Foldable qualified as F
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.Text.Builder.Linear qualified as T
import Data.Time (UTCTime)
import Pew.Logger.General (Label (..), LoggerHandle (..), LoggerHandleContext (LoggerHandleContext), LoggerHandleSettings (..), PutLog)
import Pew.Severity (Severity, severityToText)

data ContextLocation = InsideObject J.Key | TopLevel

data JsonLoggerConfig time severity = JsonLoggerConfig
  { formatTime :: time -> J.Series
  , formatSeverity :: severity -> J.Encoding
  , contextLocation :: ContextLocation
  }

defaultJsonLoggerConfig :: ContextLocation -> JsonLoggerConfig UTCTime Severity
defaultJsonLoggerConfig loc =
  JsonLoggerConfig
    { formatTime = J.pair "timestamp" . J.utcTime
    , formatSeverity = J.text . severityToText
    , contextLocation = loc
    }

mkJsonLogger :: JsonLoggerConfig time severity -> PutLog IO -> LoggerHandle IO time severity
mkJsonLogger JsonLoggerConfig{..} logAction =
  let attachMessage rdn rdl msg stamp severity =
        let message =
              J.fromEncoding . J.pairs $
                J.pair "message" (J.text $ T.runBuilder msg)
                  <> formatTime stamp
                  <> J.pair "severity" (formatSeverity severity)
                  <> rdn
                  <> case contextLocation of
                    InsideObject key -> J.pair key (J.pairs rdl)
                    TopLevel -> rdl
         in message <> "\n"
   in LoggerHandle
        { settings =
            LoggerHandleSettings
              { putLog = \namespace labels msg stamp level -> logAction $ attachMessage namespace labels msg stamp level
              , renderLabels = foldMap (\Label{..} -> J.pair (J.fromText key) (J.value value))
              , renderNamespace = \n -> if Seq.null n then mempty else J.pair "namespace" . J.text . Text.intercalate "." . F.toList $ n
              }
        , context = LoggerHandleContext mempty mempty mempty mempty
        }
