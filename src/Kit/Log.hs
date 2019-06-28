module Kit.Log where

import Control.Monad
import qualified Control.Concurrent.Lock as Lock (new, with)
import Data.Time
import Data.Time.Format
import Data.Time.LocalTime
import System.Console.ANSI
import System.IO
import System.IO.Unsafe

data LogLevel
  = Notice
  | Debug
  | Warning
  | Error

normal = SetConsoleIntensity NormalIntensity
bold = SetConsoleIntensity BoldIntensity
color = SetColor Foreground Vivid

logColor lv = case lv of
  Notice  -> White
  Debug   -> Blue
  Warning -> Yellow
  Error   -> Red

maybeSetSGR a b = do
  supports <- hSupportsANSI a
  when supports $ hSetSGR a b

logLock = unsafePerformIO (Lock.new)

logPrefix :: LogLevel -> IO ()
logPrefix lv = do
  maybeSetSGR stderr [color (logColor lv), normal]
  case lv of
    Notice  -> ePutStr "===> "
    Debug   -> ePutStr "DBG: "
    Warning -> ePutStr "WRN: "
    Error   -> ePutStr "ERR: "

logMsg :: Maybe LogLevel -> String -> IO ()
logMsg lv msg = Lock.with logLock $ do
  maybeSetSGR stderr [color Yellow, normal]
  ePutStr "["
  maybeSetSGR stderr [color Cyan, normal]
  t <- getZonedTime
  ePutStr $ formatTime defaultTimeLocale "%F %T.%4q" t
  maybeSetSGR stderr [color Yellow, normal]
  ePutStr "]"
  maybeSetSGR stderr [Reset]
  ePutStr " "
  case lv of
    Just lv -> logPrefix lv
    Nothing -> return ()
  let intensity = case lv of
        Just Notice -> bold
        _           -> normal
  case lv of
    Just lv -> maybeSetSGR stderr [color (logColor lv), intensity]
    Nothing -> maybeSetSGR stderr [Reset]
  ePutStrLn msg
  maybeSetSGR stderr [Reset]
  hFlush stderr

traceLog = logMsg Nothing
printLog = logMsg (Just Notice)
printDebugLog = logMsg (Just Debug)
warningLog = logMsg (Just Warning)
errorLog = logMsg (Just Error)

ePutStr = hPutStr stderr
ePutStrLn = hPutStrLn stderr
