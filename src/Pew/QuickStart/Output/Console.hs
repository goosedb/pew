{-# LANGUAGE ScopedTypeVariables #-}

module Pew.QuickStart.Output.Console where

import Data.Aeson qualified as J
import Data.Aeson.Text qualified as J
import Data.ByteString.Builder qualified as Bytes
import Data.Foldable (Foldable (..))
import Data.Foldable qualified as F
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Text.Builder.Linear qualified as B
import Data.Text.Lazy qualified as TL
import Data.Time qualified as Time
import Pew.Logger.General (Label (..), LoggerHandle (..), LoggerHandleContext (LoggerHandleContext), LoggerHandleSettings (..), PutLog)
import System.Console.ANSI (SGR (..), setSGRCode, ConsoleLayer (..), ColorIntensity (..), Color (..))
import Pew.Severity (Severity(..), severityToText)
import qualified Data.Text.Builder.Linear as Text.Linear
import Data.Time (getCurrentTimeZone)
import Control.Monad.IO.Class (MonadIO (..))

data ConsoleLoggerConfig time severity = ConsoleLoggerConfig
  { severityColor :: severity -> [SGR]
  , timestampColor :: [SGR]
  , namespaceColor :: [SGR]
  , labelKeyColor :: [SGR]
  , labelValueColor :: [SGR]
  , formatTime :: time -> Maybe B.Builder
  , formatSeverity :: severity -> B.Builder
  }

defaultConsoleLoggerConfig :: MonadIO m => String -> m (ConsoleLoggerConfig Time.UTCTime Severity)
defaultConsoleLoggerConfig timestampFormat = liftIO do
  timezome <- getCurrentTimeZone
  let timeFormat = Just . utcTimeFormat timezome timestampFormat
  pure $ ConsoleLoggerConfig severityColor timestampColor namespaceColor labelKeyColor labelValueColor timeFormat showSeverity
  where  
    namespaceColor = [SetColor Foreground Dull Magenta]
    timestampColor = [SetColor Foreground Vivid White]
    labelKeyColor = [SetColor Foreground Vivid Blue]
    labelValueColor = [SetColor Foreground Vivid Blue]
    showSeverity = Text.Linear.fromText . severityToText
    severityColor = pure . uncurry (SetColor Foreground) . \case
      Debug -> (Dull, Black)
      Info -> (Vivid, Green) 
      Notice -> (Vivid, Green) 
      Warning -> (Vivid, Yellow)
      Error -> (Vivid, Red)
      Critical-> (Vivid, Red)
      Alert-> (Vivid, Red)
      Emergency-> (Vivid, Red)
    utcTimeFormat tz format = B.fromText . T.pack . Time.formatTime Time.defaultTimeLocale format . Time.utcToLocalTime tz

mkConsoleLogger :: forall time severity. ConsoleLoggerConfig time severity -> PutLog IO -> LoggerHandle IO time severity
mkConsoleLogger ConsoleLoggerConfig{..} logAction =
  let
    attachMessage :: B.Builder -> B.Builder -> B.Builder -> time -> severity -> Bytes.Builder
    attachMessage rdn rdl msg stamp severity =
      Bytes.byteString . B.runBuilderBS $
        fold
          [ maybe "" (\t -> printTimestamp $ "[" <> t <> "]") (formatTime stamp)
          , " "
          , printSeverity severity
          , rdn
          , rdl
          , " "
          , msg
          , "\n"
          ]
   in
    LoggerHandle
      { settings =
          LoggerHandleSettings
            { putLog = \namespace labels msg stamp level ->
                logAction $ attachMessage namespace labels msg stamp level
            , renderNamespace = \n -> " " <> (formatNamespace . B.fromText . Text.intercalate "/" $ F.toList n)
            , renderLabels = \m ->
                if null m
                  then " "
                  else " " <> (fold . Seq.intersperse " " $ renderLabels m)
            }
      , context = LoggerHandleContext mempty mempty mempty mempty
      }
 where
  v2t (J.String t) = B.fromText t
  v2t v = B.fromText $ TL.toStrict $ J.encodeToLazyText v
  renderLabels = fmap (\Label{..} -> "#" <> printKey key <> "=" <> printValue value)

  formatNamespace ns = format namespaceColor <> ns <> reset

  printSeverity :: severity -> B.Builder
  printSeverity sev = format (severityColor sev) <> "[" <> formatSeverity sev <> "]" <> reset

  formatTimestamp = format timestampColor
  printTimestamp ts = formatTimestamp <> ts <> reset

  formatKey = format labelKeyColor
  printKey k = formatKey <> B.fromText k <> reset

  formatValue = format labelValueColor
  printValue v = formatValue <> v2t v <> reset

  format :: [SGR] -> B.Builder
  format sgr = B.fromText $ T.pack (setSGRCode sgr)

  reset :: B.Builder
  reset = B.fromText $ T.pack $ setSGRCode [Reset]
