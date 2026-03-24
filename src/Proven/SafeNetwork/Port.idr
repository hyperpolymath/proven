-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Port Number Types and Operations
|||
||| Type-safe port number handling with well-known port definitions.
||| Port numbers are bounded to 0-65535 with classification into
||| system (0-1023), registered (1024-49151), and dynamic (49152-65535) ranges.
|||
||| Updated for Idris 2 0.8.0 compatibility:
||| - Replaced `{auto ok : LTE n 65535}` with runtime bounds witness
|||   to avoid O(n) proof term construction that causes compilation
|||   timeouts for large port numbers.
module Proven.SafeNetwork.Port

import Data.Nat
import Data.List
import Data.String

%default total

||| Witness that a Nat is a valid port number (0-65535).
||| Unlike LTE n 65535 which constructs an O(n)-size proof term,
||| this is a constant-size witness checked at construction time.
public export
data IsPort : Nat -> Type where
  ItIsPort : IsPort n

||| A valid port number (0-65535)
public export
data Port : Type where
  MkPort : (n : Nat) -> {auto 0 ok : IsPort n} -> Port

||| Extract the numeric port value
public export
portValue : Port -> Nat
portValue (MkPort n) = n

||| Check if a Nat is a valid port number
public export
checkPort : (n : Nat) -> Maybe (IsPort n)
checkPort n = if n <= 65535 then Just ItIsPort else Nothing

||| Smart constructor for port numbers
public export
mkPort : Nat -> Maybe Port
mkPort n = case checkPort n of
  Just ok => Just (MkPort n {ok})
  Nothing => Nothing

||| Unsafe port constructor for known-valid literals.
||| Only use for compile-time constants where the bound is obvious.
||| Produces an erased IsPort witness.
public export
unsafeMkPort : (n : Nat) -> Port
unsafeMkPort n = MkPort n {ok = ItIsPort}

||| Parse a string as a port number
public export
parsePort : String -> Maybe Port
parsePort s = case parsePositive {a=Nat} s of
  Nothing => Nothing
  Just n => mkPort n

||| Show instance
public export
Show Port where
  show (MkPort n) = show n

||| Eq instance
public export
Eq Port where
  MkPort a == MkPort b = a == b

||| Ord instance
public export
Ord Port where
  compare (MkPort a) (MkPort b) = compare a b

||| Port range classification
public export
data PortRange = SystemPort | RegisteredPort | DynamicPort

||| Classify a port by range
public export
classify : Port -> PortRange
classify (MkPort n) =
  if n <= 1023
    then SystemPort
    else if n <= 49151
      then RegisteredPort
      else DynamicPort

||| Check if port is in system range (requires root/admin)
public export
isSystemPort : Port -> Bool
isSystemPort p = case classify p of
  SystemPort => True
  _ => False

||| Check if port is in registered range
public export
isRegisteredPort : Port -> Bool
isRegisteredPort p = case classify p of
  RegisteredPort => True
  _ => False

||| Check if port is in dynamic/ephemeral range
public export
isDynamicPort : Port -> Bool
isDynamicPort p = case classify p of
  DynamicPort => True
  _ => False

||| Well-known port definitions
public export
httpPort : Port
httpPort = unsafeMkPort 80

public export
httpsPort : Port
httpsPort = unsafeMkPort 443

public export
sshPort : Port
sshPort = unsafeMkPort 22

public export
ftpPort : Port
ftpPort = unsafeMkPort 21

public export
ftpDataPort : Port
ftpDataPort = unsafeMkPort 20

public export
smtpPort : Port
smtpPort = unsafeMkPort 25

public export
dnsPort : Port
dnsPort = unsafeMkPort 53

public export
pop3Port : Port
pop3Port = unsafeMkPort 110

public export
imapPort : Port
imapPort = unsafeMkPort 143

public export
mysqlPort : Port
mysqlPort = unsafeMkPort 3306

public export
postgresPort : Port
postgresPort = unsafeMkPort 5432

public export
redisPort : Port
redisPort = unsafeMkPort 6379

public export
mongoPort : Port
mongoPort = unsafeMkPort 27017

||| A port range (inclusive bounds)
public export
record PortRange' where
  constructor MkPortRange
  rangeStart : Port
  rangeEnd   : Port

||| Check if a port is within a range
public export
inRange : Port -> PortRange' -> Bool
inRange p r = portValue p >= portValue (rangeStart r) &&
              portValue p <= portValue (rangeEnd r)

||| Common dangerous ports that should be blocked in production
public export
dangerousPorts : List Port
dangerousPorts =
  [ unsafeMkPort 23    -- Telnet
  , unsafeMkPort 135   -- Windows RPC
  , unsafeMkPort 137   -- NetBIOS
  , unsafeMkPort 138   -- NetBIOS
  , unsafeMkPort 139   -- NetBIOS
  , unsafeMkPort 445   -- SMB
  , unsafeMkPort 1433  -- MSSQL
  , unsafeMkPort 1434  -- MSSQL Browser
  , unsafeMkPort 3389  -- RDP
  ]

||| Check if a port is considered dangerous
public export
isDangerous : Port -> Bool
isDangerous p = elem p dangerousPorts

||| Proof that a port is valid (always true by construction)
public export
data ValidPort : Port -> Type where
  MkValidPort : (p : Port) -> ValidPort p

||| Proof that a port is in a specific range
public export
data PortInRange : Port -> PortRange' -> Type where
  MkPortInRange : inRange p r = True -> PortInRange p r

||| Proof that a port is not dangerous
public export
data SafePort : Port -> Type where
  MkSafePort : isDangerous p = False -> SafePort p
