module Kit.Log where

  import Control.Exception
  import Control.Monad
  import Data.Time
  import Data.Time.Format
  import System.Console.ANSI
  import System.IO
  import Kit.Parser.Span
  import Kit.Str

  data LogLevel
    = Notice
    | Debug
    | Warning
    | Error

  normal = SetConsoleIntensity NormalIntensity
  bold = SetConsoleIntensity BoldIntensity
  color = SetColor Foreground Vivid

  logColor lv = case lv of
    Notice -> White
    Debug -> Blue
    Warning -> Yellow
    Error -> Red

  logPrefix :: LogLevel -> IO ()
  logPrefix lv = do
    hSetSGR stderr [color (logColor lv), normal]
    case lv of
      Notice -> hPutStr stderr "===> "
      Debug -> hPutStr stderr "DBG: "
      Warning -> hPutStr stderr "WRN: "
      Error -> hPutStr stderr "ERR: "

  logMsg :: LogLevel -> String -> IO ()
  logMsg lv msg = do
    hSetSGR stderr [color Yellow, normal]
    hPutStr stderr "["
    hSetSGR stderr [color Cyan, normal]
    t <- getCurrentTime
    hPutStr stderr $ formatTime defaultTimeLocale "%F %T.%6q" t
    hSetSGR stderr [color Yellow, normal]
    hPutStr stderr "] "
    logPrefix lv
    let intensity = case lv of
                      Notice -> bold
                      _ -> normal
    hSetSGR stderr [color (logColor lv), intensity]
    hPutStrLn stderr msg
    hSetSGR stderr [Reset]

  printLog = logMsg Notice
  warningLog = logMsg Warning
  errorLog = logMsg Error
