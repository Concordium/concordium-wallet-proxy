{-# LANGUAGE ScopedTypeVariables #-}

module Logging where

import qualified Control.Monad.Logger as Logger
import qualified Data.Text as Text

-- |The level of a log entry.
data LogLevel
  = LLOff
  | LLError
  | LLWarning
  | LLInfo
  | LLDebug
  | LLTrace
  deriving (Eq, Ord)

instance Show LogLevel where
  show LLOff = "off"
  show LLError = "error"
  show LLWarning = "warning"
  show LLInfo = "info"
  show LLDebug = "debug"
  show LLTrace = "trace"

logLevelFromString :: String -> Either String LogLevel
logLevelFromString s
    | s == show LLOff = Right LLOff
    | s == show LLError = Right LLError
    | s == show LLWarning = Right LLWarning
    | s == show LLInfo = Right LLInfo
    | s == show LLDebug = Right LLDebug
    | s == show LLTrace = Right LLTrace
    | otherwise = Left $ "Unrecognized log level '" ++ show s ++ "'. Valid options are 'off', 'error', 'warning', 'info', 'debug' and 'trace'."

convertLogLevel :: Logger.LogLevel -> Logging.LogLevel 
convertLogLevel ll = case ll of
  Logger.LevelDebug -> LLDebug 
  Logger.LevelInfo -> LLInfo
  Logger.LevelWarn -> LLWarning
  Logger.LevelError -> LLError
  Logger.LevelOther s ->
    if Text.toUpper s == Text.pack "trace"
    then LLTrace
    else LLOff

filterL :: Logging.LogLevel -> Logger.LoggingT m a -> Logger.LoggingT m a
filterL ll = Logger.filterLogger (\_ l -> convertLogLevel l <= ll)
