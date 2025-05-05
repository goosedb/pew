{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}

module Pew.QuickStart.Backend.Pure where

import Control.Monad.Writer (MonadWriter (tell))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text.Builder.Linear qualified as B
import Pew.Logger.General (Label, LoggerHandle (..), LoggerHandleContext (..), LoggerHandleSettings (..))

data Message time severity = Message
  { namespace :: Seq Text
  , labels :: Seq Label
  , time :: time
  , message :: Text
  , severity :: severity
  }
  deriving (Eq, Ord, Show)

withPureLogger :: (MonadWriter (Seq (Message time severity)) m) => (LoggerHandle m time severity -> m a) -> m a
withPureLogger action =
  action
    LoggerHandle
      { settings =
          LoggerHandleSettings
            { putLog = \namespace labels (B.runBuilder -> message) time severity -> tell (Seq.singleton Message{..})
            , renderNamespace = id
            , renderLabels = id
            }
      , context = LoggerHandleContext mempty mempty mempty mempty
      }
