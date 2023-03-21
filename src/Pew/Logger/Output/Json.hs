module Pew.Logger.Output.Json where

import Control.Monad (when)
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as J
import qualified Data.Aeson.Key as J
import qualified Data.ByteString.Builder as BS
import qualified Data.Map as Map
import qualified Data.Text.Lazy.Builder as T
import Pew.Logger (LoggerHandle (..))
import Pew.Logger.Config (LoggerConfig (..))

mkJsonLogger :: LoggerConfig severity -> (BS.Builder -> IO ()) -> IO (LoggerHandle severity)
mkJsonLogger LoggerConfig{..} logAction = do
  let attachMessage rd msg stamp severity =
        let message =
              J.fromEncoding . J.pairs $
                J.pair "message" (J.lazyText $ T.toLazyText msg)
                  <> J.pair "timestamp" (J.utcTime stamp)
                  <> J.pair "severity" (J.text $ showSeverity severity)
                  <> J.pair "data" (J.pairs rd)
         in message <> "\n"
  pure
    LoggerHandle
      { logAction
      , putLog = \labels msg stamp level -> when (filterSeverity level) do
          logAction $ attachMessage labels msg stamp level
      , labels = mempty
      , render = foldMap (\(k, v) -> J.pair (J.fromText k) (J.value v)) . Map.toList
      , rendered = mempty
      }
