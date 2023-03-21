module Pew.Logger.Output.Console where

import Control.Monad (when)
import qualified Data.Aeson as J
import qualified Data.Aeson.Text as J
import qualified Data.ByteString.Builder as BS
import Data.Foldable (Foldable (..))
import Data.List (intersperse)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Time as Time
import Pew.Logger (LoggerHandle (..))
import Pew.Logger.Config (LoggerConfig (..))
import System.Console.ANSI (SGR (..), setSGRCode)

data ConsoleLoggerConfig severity = ConsoleLoggerConfig
  { severityColor :: severity -> [SGR]
  , timestampColor :: [SGR]
  , labelKeyColor :: [SGR]
  , labelValueColor :: [SGR]
  , timestampFormat :: String
  }

mkConsoleTextLogger :: ConsoleLoggerConfig severity -> LoggerConfig severity -> (BS.Builder -> IO ()) -> IO (LoggerHandle severity)
mkConsoleTextLogger ConsoleLoggerConfig{..} LoggerConfig{..} logAction = do
  let attachMessage rd msg stamp severity =
        let timeFormat = Time.formatTime Time.defaultTimeLocale timestampFormat
         in fold
              [ printTimestamp $ "[" <> T.fromText (T.pack (timeFormat stamp)) <> "]"
              , " "
              , rd
              , printSeverity severity
              , " "
              , msg
              , "\n"
              ]
  pure
    LoggerHandle
      { logAction
      , putLog = \labels msg stamp level -> when (filterSeverity level) do
          logAction $ T.encodeUtf8Builder $ T.toLazyText $ attachMessage labels msg stamp level
      , labels = mempty
      , render = \m ->
          if null m
            then ""
            else "{" <> (fold . intersperse "," $ renderLabels m) <> "} "
      , rendered = mempty
      }
 where
  v2t (J.String t) = T.fromText t
  v2t v = T.fromLazyText $ J.encodeToLazyText v
  renderLabels = map (\(k, v) -> "#" <> printKey k <> "=" <> printValue v) . Map.toList

  printSeverity sev = format (severityColor sev) <> "[" <> T.fromText (showSeverity sev) <> "]" <> reset

  formatTimestamp = format timestampColor
  printTimestamp ts = formatTimestamp <> ts <> reset

  formatKey = format labelKeyColor
  printKey k = formatKey <> T.fromText k <> reset

  formatValue = format labelValueColor
  printValue v = formatValue <> v2t v <> reset

  format sgr = T.fromText $ T.pack (setSGRCode sgr)
  reset = T.fromText $ T.pack $ setSGRCode [Reset]
