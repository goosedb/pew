{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}

module Pew.Logger.General where

import Data.Aeson qualified as J
import Data.ByteString.Builder qualified as Bytes
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as B

type PutLog m = Bytes.Builder -> m ()

data Label = Label
  { key :: T.Text
  , value :: J.Value
  }
  deriving (Eq, Ord, Show)

(.>) :: (J.ToJSON a) => T.Text -> a -> Label
(.>) k v = Label k (J.toJSON v)

class ToLabel a where
  toLabel :: a -> Label

data LoggerHandleSettings rdn rdl m time severity = LoggerHandleSettings
  { putLog :: rdn -> rdl -> B.Builder -> time -> severity -> m ()
  , renderNamespace :: Seq T.Text -> rdn
  , renderLabels :: Seq Label -> rdl
  }

data LoggerHandleContext rdn rdl = LoggerHandleContext
  { labels :: Seq Label
  , namespace :: Seq T.Text
  , renderedNamespace :: rdn
  , renderedLabels :: rdl
  }

data LoggerHandle m time severity where
  LoggerHandle ::
    { settings :: LoggerHandleSettings rdn rdl m time severity
    , context :: LoggerHandleContext rdn rdl
    } ->
    LoggerHandle m time severity

{-# INLINEABLE addLabels #-}
addLabels :: [Label] -> LoggerHandle m t s -> LoggerHandle m t s
addLabels ls LoggerHandle{context = LoggerHandleContext{..}, ..} =
  let newLabels = labels <> Seq.fromList ls
   in LoggerHandle{context = LoggerHandleContext{labels = newLabels, renderedLabels = settings.renderLabels newLabels, ..}, ..}

{-# INLINEABLE pushNamespace #-}
pushNamespace :: [T.Text] -> LoggerHandle m t s -> LoggerHandle m t s
pushNamespace ns LoggerHandle{context = LoggerHandleContext{..}, ..} =
  let newNamespace = namespace <> Seq.fromList ns
   in LoggerHandle{context = LoggerHandleContext{namespace = newNamespace, renderedNamespace = settings.renderNamespace newNamespace, ..}, ..}

{-# INLINE writeLog #-}
writeLog :: LoggerHandle m time severity -> severity -> time -> B.Builder -> m ()
writeLog LoggerHandle{..} s t m = settings.putLog context.renderedNamespace context.renderedLabels m t s
