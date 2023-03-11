# Pew

Pew is backend agnostic structured logger. There are usage examples below

```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
import Pew.Logger.Output.Json
import Pew.Logger.Implicit
import Pew.Logger.Config 
import Pew.Severity
import Data.ByteString.Builder
import System.IO

main :: IO ()
main = do
  logger <- mkJsonLogger 
    (LoggerConfig severityToText (const True)) 
    (hPutBuilder stdout)

  withLogger logger do
    logMsg Debug "kek"
    withLabel "foo" (1 :: Int) do 
      logMsg Info "lol"

```
Produces
```
{"message":"kek","timestamp":"2023-03-11T16:51:39.229087532Z","severity":"Debug","data":{}}
{"message":"lol","timestamp":"2023-03-11T16:51:39.229150711Z","severity":"Info","data":{"foo":1}}
```
Sometimes JSON logs aren't very convenient. For example when you are running program on local machine and you aren't able to use _some json logs viewer_. In such case you can use logger from `Console` module:
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
import Pew.Logger.Output.Console
import Pew.Logger.Implicit
import Pew.Logger.Config 
import Pew.Severity
import Data.ByteString.Builder
import System.IO

main :: IO ()
main = do
  -- you can configure colors if you want 
  logger <- mkConsoleTextLogger 
    (ConsoleLoggerConfig (const []) [] [] [] "%H:%M:%S") 
    (LoggerConfig severityToText (const True)) 
    (hPutBuilder stdout)

  withLogger logger do
    logMsg Debug "kek"
    withLabel "foo" (1 :: Int) do 
      logMsg Info "lol"
```
Produces
```
[17:01:50] [Debug] kek
[17:01:50] {#foo=1} [Info] lol
```