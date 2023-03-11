{-# LANGUAGE GADTs #-}

module Pew.Logger.Config where

import qualified Data.Text as T

data LoggerConfig severity where
  LoggerConfig ::
    { showSeverity :: severity -> T.Text
    , filterSeverity :: severity -> Bool
    } ->
    LoggerConfig severity
