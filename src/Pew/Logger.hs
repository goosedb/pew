module Pew.Logger where

import qualified Data.Aeson as J
import Data.Foldable (Foldable (..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Time as Time

data LoggerHandle severity where
  LoggerHandle ::
    { putLog :: rd -> T.Builder -> Time.UTCTime -> severity -> IO ()
    , labels :: Map.Map T.Text J.Value
    , render :: Map.Map T.Text J.Value -> rd
    , rendered :: rd
    } ->
    LoggerHandle severity

addLabels :: [(T.Text, J.Value)] -> LoggerHandle s -> LoggerHandle s
addLabels ls LoggerHandle{..} =
  let newLabelsMap = foldl' (\m (k, v) -> Map.insert k v m) labels ls
   in LoggerHandle{labels = newLabelsMap, rendered = render newLabelsMap, ..}

dropLabels :: [T.Text] -> LoggerHandle s -> LoggerHandle s
dropLabels ls LoggerHandle{..} =
  let newLabelsMap = foldl' (flip Map.delete) labels ls
   in LoggerHandle{labels = newLabelsMap, rendered = render newLabelsMap, ..}

writeLog :: LoggerHandle s -> s -> Time.UTCTime -> T.Builder -> IO ()
writeLog LoggerHandle{..} s t m = putLog rendered m t s
