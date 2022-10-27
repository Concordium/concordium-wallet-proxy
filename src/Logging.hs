{-# LANGUAGE ScopedTypeVariables #-}

module Logging(
      Logging.LogLevel(..)
    , filterL
) where

import           Control.Monad.Logger
import           Data.Char
import           Data.Text                  hiding (map)
import           Data.String

-- basically lifted from Concordium.Logger
data LogLevel
  = LLOff
  | LLError
  | LLWarning
  | LLInfo
  | LLDebug
  | LLTrace
  deriving (Eq, Ord)

instance Show Logging.LogLevel where
  show LLOff = "OFF"
  show LLError = "ERROR"
  show LLWarning = "WARNING"
  show LLInfo = "INFO"
  show LLDebug = "DEBUG"
  show LLTrace = "TRACE"

instance IsString Logging.LogLevel where
  fromString s
    | sU == show LLOff = LLOff
    | sU == show LLError = LLError
    | sU == show LLWarning = LLWarning
    | sU == show LLInfo = LLInfo
    | sU == show LLDebug = LLDebug
    | sU == show LLTrace = LLTrace
    | otherwise = LLOff
    where sU = map Data.Char.toUpper s

convertLogLevel :: Control.Monad.Logger.LogLevel -> Logging.LogLevel 
convertLogLevel ll = case ll of
  LevelDebug -> LLDebug 
  LevelInfo -> LLInfo
  LevelWarn -> LLWarning
  LevelError -> LLError
  LevelOther s ->
    if   Data.Text.toUpper s == Data.Text.pack "TRACE"
    then LLTrace
    else LLOff

filterL :: Logging.LogLevel -> LoggingT m a -> LoggingT m a
filterL ll = filterLogger (\_ l -> convertLogLevel l <= ll)