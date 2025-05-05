module Pew.QuickStart.Backend.Async where

import Control.Concurrent (forkFinally, killThread)
import Control.Concurrent.STM (flushTBQueue, newTBQueueIO, writeTBQueue, writeTVar)
import Control.Exception (bracket, finally, uninterruptibleMask_)
import Control.Exception.Base (mask_)
import Control.Monad (forever)
import Data.Bool (bool)
import Data.Foldable (for_)
import GHC.Conc (TVar, ThreadId, atomically, newTVarIO, readTVar, retry)
import Numeric.Natural (Natural)
import Pew.Logger.General (PutLog)

withAsyncPutLog :: BackgroundLoggerConfig -> PutLog IO -> IO () -> (Worker -> PutLog IO -> IO a) -> IO a
withAsyncPutLog cfg putLog flush action =
  bracket
    do forkBackgroundLogger cfg putLog flush
    do \(_, worker) -> killBackgroundLogger worker
    do \(asyncPutLog, worker) -> action worker asyncPutLog

data BackgroundLoggerConfig = BackgroundLoggerConfig
  { capacity :: Natural
  , waitAllLogsFlushed :: Bool
  }

data Worker = Worker {threadId :: ThreadId, isAlive :: TVar Bool}

killBackgroundLogger :: Worker -> IO ()
killBackgroundLogger Worker{..} = do
  killThread threadId
  atomically $ readTVar isAlive >>= bool (pure ()) retry

forkBackgroundLogger :: BackgroundLoggerConfig -> PutLog IO -> IO () -> IO (PutLog IO, Worker)
forkBackgroundLogger BackgroundLoggerConfig {..} putLog flush = do
  queue <- newTBQueueIO capacity
  isAlive <- newTVarIO True
  let maskWorker = if waitAllLogsFlushed then uninterruptibleMask_ else id
  let writeBatch = maskWorker do
        msgs <- atomically $ fetch queue
        for_ msgs (mask_ . putLog)
        flush
  tid <- forkFinally
    do forever writeBatch
    do \_ -> writeBatch `finally` atomically (writeTVar isAlive False)
  pure (atomically . writeTBQueue queue, Worker tid isAlive)
 where
  fetch = flushTBQueue
