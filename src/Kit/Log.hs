module Kit.Log where

import Control.Monad
import Data.Time
import Data.Time.Format
import Data.Time.LocalTime
import System.Console.ANSI
import System.IO

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

logPrefix :: LogLevel -> IO ()
logPrefix lv = do
  hSetSGR stderr [color (logColor lv), normal]
  case lv of
    Notice  -> ePutStr "===> "
    Debug   -> ePutStr "DBG: "
    Warning -> ePutStr "WRN: "
    Error   -> ePutStr "ERR: "

logMsg :: Maybe LogLevel -> String -> IO ()
logMsg lv msg = do
  hSetSGR stderr [color Yellow, normal]
  ePutStr "["
  hSetSGR stderr [color Cyan, normal]
  t <- getZonedTime
  ePutStr $ formatTime defaultTimeLocale "%F %T.%6q" t
  hSetSGR stderr [color Yellow, normal]
  ePutStr "]"
  hSetSGR stderr [Reset]
  ePutStr " "
  case lv of
    Just lv -> logPrefix lv
    Nothing -> return ()
  let intensity = case lv of
        Just Notice -> bold
        _           -> normal
  case lv of
    Just lv -> hSetSGR stderr [color (logColor lv), intensity]
    Nothing -> hSetSGR stderr [Reset]
  ePutStrLn msg
  hSetSGR stderr [Reset]

traceLog = logMsg Nothing
printLog = logMsg (Just Notice)
warningLog = logMsg (Just Warning)
errorLog = logMsg (Just Error)

ePutStr = hPutStr stderr
ePutStrLn = hPutStrLn stderr
