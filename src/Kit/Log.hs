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
    | Info
    | Warning
    | Error

  logColor lv = case lv of
    Notice -> White
    Debug -> Blue
    Info -> Cyan
    Warning -> Yellow
    Error -> Red

  logPrefix :: LogLevel -> IO ()
  logPrefix lv = do
    hSetSGR stderr [SetColor Foreground Vivid (logColor lv), SetConsoleIntensity BoldIntensity]
    case lv of
      Notice -> hPutStr stderr "===> "
      Debug -> hPutStr stderr "DBG: "
      Info -> hPutStr stderr "INF: "
      Warning -> hPutStr stderr "WRN: "
      Error -> hPutStr stderr "ERR: "

  logMsg :: LogLevel -> String -> IO ()
  logMsg lv msg = do
    hSetSGR stderr [SetColor Foreground Vivid Blue, SetConsoleIntensity NormalIntensity]
    hPutStr stderr "["
    hSetSGR stderr [SetColor Foreground Vivid Cyan, SetConsoleIntensity NormalIntensity]
    t <- getCurrentTime
    hPutStr stderr $ formatTime defaultTimeLocale "%F %T.%6q" t
    hSetSGR stderr [SetColor Foreground Vivid Blue, SetConsoleIntensity NormalIntensity]
    hPutStr stderr "] "
    logPrefix lv
    let intensity = case lv of
                      Notice -> BoldIntensity
                      _ -> NormalIntensity
    hSetSGR stderr [SetColor Foreground Vivid White, SetConsoleIntensity intensity]
    hPutStrLn stderr msg
    hSetSGR stderr [Reset]

  printLog = logMsg Notice
  infoLog = logMsg Info
  warningLog = logMsg Warning
  errorLog = logMsg Error
