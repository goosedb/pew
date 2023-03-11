{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Pew.Severity where

import qualified Data.Text as T

data Severity = Debug | Info | Warning | Error
  deriving (Eq, Ord, Show, Read, Enum)

severityToText :: Severity -> T.Text
severityToText = \case
  Debug -> debugText
  Info -> infoText
  Warning -> warningText
  Error -> errorText

debugText, infoText, warningText, errorText :: T.Text
debugText = "Debug"
infoText = "Info"
warningText = "Warning"
errorText = "Error"
