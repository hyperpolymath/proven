-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeLog - Safe structured logging operations
|||
||| This module provides structured logging with levels,
||| safe string interpolation, and log filtering.
module Proven.SafeLog
import Data.String
import Data.List

import public Proven.Core

%default total

--------------------------------------------------------------------------------
-- Log Levels
--------------------------------------------------------------------------------

||| Logging severity levels
public export
data LogLevel : Type where
  Trace : LogLevel
  Debug : LogLevel
  Info : LogLevel
  Warn : LogLevel
  Error : LogLevel
  Fatal : LogLevel

public export
Eq LogLevel where
  Trace == Trace = True
  Debug == Debug = True
  Info == Info = True
  Warn == Warn = True
  Error == Error = True
  Fatal == Fatal = True
  _ == _ = False

||| Convert log level to numeric priority (higher = more severe)
public export
levelPriority : LogLevel -> Nat
levelPriority Trace = 0
levelPriority Debug = 1
levelPriority Info = 2
levelPriority Warn = 3
levelPriority Error = 4
levelPriority Fatal = 5

public export
Ord LogLevel where
  compare a b = compare (levelPriority a) (levelPriority b)

public export
Show LogLevel where
  show Trace = "TRACE"
  show Debug = "DEBUG"
  show Info = "INFO"
  show Warn = "WARN"
  show Error = "ERROR"
  show Fatal = "FATAL"

--------------------------------------------------------------------------------
-- Log Entry
--------------------------------------------------------------------------------

||| A structured log entry
public export
record LogEntry where
  constructor MkEntry
  timestamp : Nat
  level : LogLevel
  message : String
  logger : String            -- Logger name/category
  context : List (String, String)  -- Key-value pairs

||| Create a log entry
public export
entry : (timestamp : Nat) -> LogLevel -> String -> String -> LogEntry
entry ts lvl msg lgr = MkEntry ts lvl msg lgr []

||| Add context to a log entry
public export
withContext : (key : String) -> (value : String) -> LogEntry -> LogEntry
withContext k v e = MkEntry e.timestamp e.level e.message e.logger ((k, v) :: e.context)

||| Add multiple context pairs
public export
withContextMany : List (String, String) -> LogEntry -> LogEntry
withContextMany pairs e = MkEntry e.timestamp e.level e.message e.logger (pairs ++ e.context)

--------------------------------------------------------------------------------
-- Logger Configuration
--------------------------------------------------------------------------------

||| Logger configuration
public export
record LoggerConfig where
  constructor MkConfig
  name : String
  minLevel : LogLevel
  maxEntries : Nat          -- Max entries to keep in memory

||| Default logger configuration
public export
defaultConfig : String -> LoggerConfig
defaultConfig name = MkConfig name Info 1000

--------------------------------------------------------------------------------
-- Logger State
--------------------------------------------------------------------------------

||| Logger state with entries
public export
record Logger where
  constructor MkLogger
  config : LoggerConfig
  entries : List LogEntry
  entryCount : Nat

||| Create a new logger
public export
newLogger : LoggerConfig -> Logger
newLogger cfg = MkLogger cfg [] 0

||| Create a logger with default config
public export
defaultLogger : String -> Logger
defaultLogger name = newLogger (defaultConfig name)

||| Check if a log level should be logged
public export
shouldLog : Logger -> LogLevel -> Bool
shouldLog logger lvl = lvl >= logger.config.minLevel

||| Add an entry to the logger
public export
log : LogEntry -> Logger -> Logger
log e logger =
  if shouldLog logger e.level
    then let newEntries = take logger.config.maxEntries (e :: logger.entries)
         in MkLogger logger.config newEntries (S logger.entryCount)
    else logger

||| Log a message at a specific level
public export
logMessage : (timestamp : Nat) -> LogLevel -> String -> Logger -> Logger
logMessage ts lvl msg logger =
  log (entry ts lvl msg logger.config.name) logger

||| Convenience functions for each level
public export
trace : (timestamp : Nat) -> String -> Logger -> Logger
trace ts = logMessage ts Trace

public export
debug : (timestamp : Nat) -> String -> Logger -> Logger
debug ts = logMessage ts Debug

public export
info : (timestamp : Nat) -> String -> Logger -> Logger
info ts = logMessage ts Info

public export
warn : (timestamp : Nat) -> String -> Logger -> Logger
warn ts = logMessage ts Warn

public export
error : (timestamp : Nat) -> String -> Logger -> Logger
error ts = logMessage ts Error

public export
fatal : (timestamp : Nat) -> String -> Logger -> Logger
fatal ts = logMessage ts Fatal

--------------------------------------------------------------------------------
-- Log Filtering
--------------------------------------------------------------------------------

||| Filter entries by level
public export
filterByLevel : LogLevel -> Logger -> List LogEntry
filterByLevel lvl logger = filter (\e => e.level >= lvl) logger.entries

||| Filter entries by logger name
public export
filterByLogger : String -> Logger -> List LogEntry
filterByLogger name logger = filter (\e => e.logger == name) logger.entries

||| Filter entries by time range
public export
filterByTimeRange : (from : Nat) -> (to : Nat) -> Logger -> List LogEntry
filterByTimeRange from to logger =
  filter (\e => e.timestamp >= from && e.timestamp <= to) logger.entries

||| Get recent entries
public export
recentEntries : (count : Nat) -> Logger -> List LogEntry
recentEntries n logger = take n logger.entries

||| Get entries containing a string in message
public export
search : String -> Logger -> List LogEntry
search term logger = filter (\e => isInfixOf term e.message) logger.entries
  where
    isInfixOf : String -> String -> Bool
    isInfixOf needle haystack =
      let ns = unpack needle
          hs = unpack haystack
      in checkInfix ns hs
    
    checkInfix : List Char -> List Char -> Bool
    checkInfix [] _ = True
    checkInfix _ [] = False
    checkInfix ns (h :: hs) =
      startsWith ns (h :: hs) || checkInfix ns hs
    
    startsWith : List Char -> List Char -> Bool
    startsWith [] _ = True
    startsWith _ [] = False
    startsWith (n :: ns) (h :: hs) = n == h && startsWith ns hs

--------------------------------------------------------------------------------
-- Statistics
--------------------------------------------------------------------------------

||| Count entries by level
public export
countByLevel : LogLevel -> Logger -> Nat
countByLevel lvl logger = length (filter (\e => e.level == lvl) logger.entries)

||| Get error count
public export
errorCount : Logger -> Nat
errorCount logger = countByLevel Error logger + countByLevel Fatal logger

||| Get warning count
public export
warningCount : Logger -> Nat
warningCount logger = countByLevel Warn logger

||| Check if logger has errors
public export
hasErrors : Logger -> Bool
hasErrors logger = errorCount logger > 0

||| Clear all entries
public export
clear : Logger -> Logger
clear logger = MkLogger logger.config [] 0

||| Set minimum log level
public export
setLevel : LogLevel -> Logger -> Logger
setLevel lvl logger =
  MkLogger (MkConfig logger.config.name lvl logger.config.maxEntries)
           logger.entries
           logger.entryCount

--------------------------------------------------------------------------------
-- Formatting
--------------------------------------------------------------------------------

||| Format a log entry as string
public export
formatEntry : LogEntry -> String
formatEntry e =
  "[" ++ show e.timestamp ++ "] " ++
  show e.level ++ " " ++
  "[" ++ e.logger ++ "] " ++
  e.message ++
  formatContext e.context
  where
    formatContext : List (String, String) -> String
    formatContext [] = ""
    formatContext ctx = " {" ++ formatPairs ctx ++ "}"
    
    formatPairs : List (String, String) -> String
    formatPairs [] = ""
    formatPairs [(k, v)] = k ++ "=" ++ v
    formatPairs ((k, v) :: rest) = k ++ "=" ++ v ++ ", " ++ formatPairs rest

public export
Show LogEntry where
  show = formatEntry

public export
Show Logger where
  show logger = "Logger(" ++ logger.config.name ++
                ", entries=" ++ show (length logger.entries) ++ ")"

