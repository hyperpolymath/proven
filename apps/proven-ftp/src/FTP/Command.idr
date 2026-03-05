-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- FTP Commands (RFC 959 Section 4.1)
--
-- All FTP commands are a closed sum type. Each constructor carries its
-- required parameters. Unrecognised command strings parse to Nothing
-- rather than crashing the server.

module FTP.Command

%default total

-- ============================================================================
-- FTP Commands (RFC 959 Section 4.1)
-- ============================================================================

||| FTP commands as defined in RFC 959.
public export
data Command : Type where
  ||| USER: Identify the user (Section 4.1.1).
  USER : (username : String) -> Command
  ||| PASS: Supply the user's password (Section 4.1.1).
  PASS : (password : String) -> Command
  ||| ACCT: Supply an account for billing (Section 4.1.1).
  ACCT : (account : String) -> Command
  ||| CWD: Change working directory (Section 4.1.1).
  CWD  : (path : String) -> Command
  ||| CDUP: Change to parent directory (Section 4.1.1).
  CDUP : Command
  ||| QUIT: Logout and close control connection (Section 4.1.1).
  QUIT : Command
  ||| PASV: Enter passive mode for data transfer (Section 4.1.2).
  PASV : Command
  ||| PORT: Specify data connection address and port (Section 4.1.2).
  PORT : (hostPort : String) -> Command
  ||| TYPE: Set transfer type (Section 4.1.2).
  TYPE : (typeCode : String) -> Command
  ||| RETR: Retrieve (download) a file (Section 4.1.3).
  RETR : (path : String) -> Command
  ||| STOR: Store (upload) a file (Section 4.1.3).
  STOR : (path : String) -> Command
  ||| DELE: Delete a file (Section 4.1.3).
  DELE : (path : String) -> Command
  ||| RMD: Remove a directory (Section 4.1.3).
  RMD  : (path : String) -> Command
  ||| MKD: Make a directory (Section 4.1.3).
  MKD  : (path : String) -> Command
  ||| PWD: Print working directory (Section 4.1.3).
  PWD  : Command
  ||| LIST: List directory contents (Section 4.1.3).
  LIST : (path : Maybe String) -> Command
  ||| NLST: Name list of directory contents (Section 4.1.3).
  NLST : (path : Maybe String) -> Command
  ||| SYST: Identify the server system (Section 4.1.3).
  SYST : Command
  ||| STAT: Return server status or file status (Section 4.1.3).
  STAT : (path : Maybe String) -> Command
  ||| NOOP: No operation (Section 4.1.3).
  NOOP : Command
  ||| RNFR: Rename from — first half of rename (Section 4.1.3).
  RNFR : (path : String) -> Command
  ||| RNTO: Rename to — second half of rename (Section 4.1.3).
  RNTO : (path : String) -> Command
  ||| SIZE: Return file size (RFC 3659).
  SIZE : (path : String) -> Command

public export
Show Command where
  show (USER u)    = "USER " ++ u
  show (PASS _)    = "PASS ****"
  show (ACCT a)    = "ACCT " ++ a
  show (CWD p)     = "CWD " ++ p
  show CDUP        = "CDUP"
  show QUIT        = "QUIT"
  show PASV        = "PASV"
  show (PORT hp)   = "PORT " ++ hp
  show (TYPE t)    = "TYPE " ++ t
  show (RETR p)    = "RETR " ++ p
  show (STOR p)    = "STOR " ++ p
  show (DELE p)    = "DELE " ++ p
  show (RMD p)     = "RMD " ++ p
  show (MKD p)     = "MKD " ++ p
  show PWD         = "PWD"
  show (LIST p)    = "LIST" ++ maybe "" (" " ++) p
  show (NLST p)    = "NLST" ++ maybe "" (" " ++) p
  show SYST        = "SYST"
  show (STAT p)    = "STAT" ++ maybe "" (" " ++) p
  show NOOP        = "NOOP"
  show (RNFR p)    = "RNFR " ++ p
  show (RNTO p)    = "RNTO " ++ p
  show (SIZE p)    = "SIZE " ++ p

-- ============================================================================
-- Command classification
-- ============================================================================

||| The verb (keyword) of a command, without parameters.
public export
commandVerb : Command -> String
commandVerb (USER _) = "USER"
commandVerb (PASS _) = "PASS"
commandVerb (ACCT _) = "ACCT"
commandVerb (CWD _)  = "CWD"
commandVerb CDUP     = "CDUP"
commandVerb QUIT     = "QUIT"
commandVerb PASV     = "PASV"
commandVerb (PORT _) = "PORT"
commandVerb (TYPE _) = "TYPE"
commandVerb (RETR _) = "RETR"
commandVerb (STOR _) = "STOR"
commandVerb (DELE _) = "DELE"
commandVerb (RMD _)  = "RMD"
commandVerb (MKD _)  = "MKD"
commandVerb PWD      = "PWD"
commandVerb (LIST _) = "LIST"
commandVerb (NLST _) = "NLST"
commandVerb SYST     = "SYST"
commandVerb (STAT _) = "STAT"
commandVerb NOOP     = "NOOP"
commandVerb (RNFR _) = "RNFR"
commandVerb (RNTO _) = "RNTO"
commandVerb (SIZE _) = "SIZE"

||| Whether a command requires authentication.
public export
requiresAuth : Command -> Bool
requiresAuth (USER _) = False
requiresAuth (PASS _) = False
requiresAuth (ACCT _) = False
requiresAuth QUIT     = False
requiresAuth NOOP     = False
requiresAuth SYST     = False
requiresAuth _        = True

||| Whether a command uses the data connection.
public export
usesDataConnection : Command -> Bool
usesDataConnection (RETR _) = True
usesDataConnection (STOR _) = True
usesDataConnection (LIST _) = True
usesDataConnection (NLST _) = True
usesDataConnection _        = False

-- ============================================================================
-- Command parsing
-- ============================================================================

||| Parse errors for FTP commands.
public export
data CommandParseError : Type where
  EmptyCommand   : CommandParseError
  UnknownCommand : (verb : String) -> CommandParseError
  MissingParam   : (verb : String) -> CommandParseError
  LineTooLong    : (len : Nat) -> CommandParseError

public export
Show CommandParseError where
  show EmptyCommand       = "Empty command line"
  show (UnknownCommand v) = "Unknown command: " ++ v
  show (MissingParam v)   = "Missing parameter for " ++ v
  show (LineTooLong n)    = "Command line too long: " ++ show n ++ " chars"

||| Extract the verb and parameter from a command line.
extractVerbAndParam : String -> (String, String)
extractVerbAndParam s =
  let parts = break (== ' ') s
  in (toUpper (fst parts), ltrim (snd parts))

||| Parse an FTP command line into a typed Command.
||| Returns Left for malformed or unrecognised commands.
public export
parseCommand : String -> Either CommandParseError Command
parseCommand s =
  if length s == 0 then Left EmptyCommand
  else if length s > 512 then Left (LineTooLong (length s))
  else
    let (verb, param) = extractVerbAndParam s
        hasParam = length param > 0
        optParam = if hasParam then Just param else Nothing
    in case verb of
         "USER" => if hasParam then Right (USER param)
                   else Left (MissingParam "USER")
         "PASS" => if hasParam then Right (PASS param)
                   else Left (MissingParam "PASS")
         "ACCT" => if hasParam then Right (ACCT param)
                   else Left (MissingParam "ACCT")
         "CWD"  => if hasParam then Right (CWD param)
                   else Left (MissingParam "CWD")
         "CDUP" => Right CDUP
         "QUIT" => Right QUIT
         "PASV" => Right PASV
         "PORT" => if hasParam then Right (PORT param)
                   else Left (MissingParam "PORT")
         "TYPE" => if hasParam then Right (TYPE param)
                   else Left (MissingParam "TYPE")
         "RETR" => if hasParam then Right (RETR param)
                   else Left (MissingParam "RETR")
         "STOR" => if hasParam then Right (STOR param)
                   else Left (MissingParam "STOR")
         "DELE" => if hasParam then Right (DELE param)
                   else Left (MissingParam "DELE")
         "RMD"  => if hasParam then Right (RMD param)
                   else Left (MissingParam "RMD")
         "MKD"  => if hasParam then Right (MKD param)
                   else Left (MissingParam "MKD")
         "PWD"  => Right PWD
         "LIST" => Right (LIST optParam)
         "NLST" => Right (NLST optParam)
         "SYST" => Right SYST
         "STAT" => Right (STAT optParam)
         "NOOP" => Right NOOP
         "RNFR" => if hasParam then Right (RNFR param)
                   else Left (MissingParam "RNFR")
         "RNTO" => if hasParam then Right (RNTO param)
                   else Left (MissingParam "RNTO")
         "SIZE" => if hasParam then Right (SIZE param)
                   else Left (MissingParam "SIZE")
         _      => Left (UnknownCommand verb)
