-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| Port Number Types and Operations
|||
||| Type-safe port number handling with well-known port definitions.
module Proven.SafeNetwork.Port

import Proven.Core
import Data.List

%default total

--------------------------------------------------------------------------------
-- Port Type
--------------------------------------------------------------------------------

||| A validated port number (0-65535)
public export
record Port where
  constructor MkPort
  value : Nat
  {auto valid : value <= 65535}

public export
Show Port where
  show (MkPort p) = show p

public export
Eq Port where
  (MkPort a) == (MkPort b) = a == b

public export
Ord Port where
  compare (MkPort a) (MkPort b) = compare a b

||| Parse port number from string
public export
parsePort : String -> Maybe Port
parsePort s =
  case parsePositive s of
    Just n => if n <= 65535 then Just (MkPort n) else Nothing
    Nothing => Nothing

||| Create port from Nat (safe)
public export
mkPort : (n : Nat) -> {auto prf : n <= 65535} -> Port
mkPort n = MkPort n

||| Try to create port from Nat
public export
toPort : Nat -> Maybe Port
toPort n = if n <= 65535 then Just (MkPort n) else Nothing

--------------------------------------------------------------------------------
-- Port Ranges
--------------------------------------------------------------------------------

||| Port categories by range
public export
data PortCategory
  = SystemPort       -- 0-1023 (well-known, requires root/admin)
  | UserPort         -- 1024-49151 (registered)
  | DynamicPort      -- 49152-65535 (ephemeral/private)

public export
Show PortCategory where
  show SystemPort = "System (0-1023)"
  show UserPort = "User (1024-49151)"
  show DynamicPort = "Dynamic (49152-65535)"

||| Get category of a port
public export
portCategory : Port -> PortCategory
portCategory (MkPort p) =
  if p <= 1023 then SystemPort
  else if p <= 49151 then UserPort
  else DynamicPort

||| Check if port is in system range (requires privileges)
public export
isSystemPort : Port -> Bool
isSystemPort p = portCategory p == SystemPort
  where
    Eq PortCategory where
      SystemPort == SystemPort = True
      UserPort == UserPort = True
      DynamicPort == DynamicPort = True
      _ == _ = False

||| Check if port is in user/registered range
public export
isUserPort : Port -> Bool
isUserPort p = portCategory p == UserPort
  where
    Eq PortCategory where
      SystemPort == SystemPort = True
      UserPort == UserPort = True
      DynamicPort == DynamicPort = True
      _ == _ = False

||| Check if port is in dynamic/ephemeral range
public export
isDynamicPort : Port -> Bool
isDynamicPort p = portCategory p == DynamicPort
  where
    Eq PortCategory where
      SystemPort == SystemPort = True
      UserPort == UserPort = True
      DynamicPort == DynamicPort = True
      _ == _ = False

--------------------------------------------------------------------------------
-- Well-Known Ports
--------------------------------------------------------------------------------

||| FTP data (20)
public export
ftpData : Port
ftpData = MkPort 20

||| FTP control (21)
public export
ftpControl : Port
ftpControl = MkPort 21

||| SSH (22)
public export
ssh : Port
ssh = MkPort 22

||| Telnet (23)
public export
telnet : Port
telnet = MkPort 23

||| SMTP (25)
public export
smtp : Port
smtp = MkPort 25

||| DNS (53)
public export
dns : Port
dns = MkPort 53

||| DHCP server (67)
public export
dhcpServer : Port
dhcpServer = MkPort 67

||| DHCP client (68)
public export
dhcpClient : Port
dhcpClient = MkPort 68

||| TFTP (69)
public export
tftp : Port
tftp = MkPort 69

||| HTTP (80)
public export
http : Port
http = MkPort 80

||| Kerberos (88)
public export
kerberos : Port
kerberos = MkPort 88

||| POP3 (110)
public export
pop3 : Port
pop3 = MkPort 110

||| NTP (123)
public export
ntp : Port
ntp = MkPort 123

||| NetBIOS Name Service (137)
public export
netbiosNs : Port
netbiosNs = MkPort 137

||| NetBIOS Session Service (139)
public export
netbiosSs : Port
netbiosSs = MkPort 139

||| IMAP (143)
public export
imap : Port
imap = MkPort 143

||| SNMP (161)
public export
snmp : Port
snmp = MkPort 161

||| SNMP Trap (162)
public export
snmpTrap : Port
snmpTrap = MkPort 162

||| LDAP (389)
public export
ldap : Port
ldap = MkPort 389

||| HTTPS (443)
public export
https : Port
https = MkPort 443

||| SMB/CIFS (445)
public export
smb : Port
smb = MkPort 445

||| SMTP over TLS (465)
public export
smtps : Port
smtps = MkPort 465

||| Syslog (514)
public export
syslog : Port
syslog = MkPort 514

||| SMTP submission (587)
public export
submission : Port
submission = MkPort 587

||| LDAPS (636)
public export
ldaps : Port
ldaps = MkPort 636

||| IMAP over TLS (993)
public export
imaps : Port
imaps = MkPort 993

||| POP3 over TLS (995)
public export
pop3s : Port
pop3s = MkPort 995

||| Microsoft SQL Server (1433)
public export
mssql : Port
mssql = MkPort 1433

||| Oracle Database (1521)
public export
oracle : Port
oracle = MkPort 1521

||| MySQL (3306)
public export
mysql : Port
mysql = MkPort 3306

||| RDP (3389)
public export
rdp : Port
rdp = MkPort 3389

||| PostgreSQL (5432)
public export
postgresql : Port
postgresql = MkPort 5432

||| VNC (5900)
public export
vnc : Port
vnc = MkPort 5900

||| Redis (6379)
public export
redis : Port
redis = MkPort 6379

||| HTTP alternate (8080)
public export
httpAlt : Port
httpAlt = MkPort 8080

||| HTTPS alternate (8443)
public export
httpsAlt : Port
httpsAlt = MkPort 8443

||| MongoDB (27017)
public export
mongodb : Port
mongodb = MkPort 27017

--------------------------------------------------------------------------------
-- Port Service Lookup
--------------------------------------------------------------------------------

||| Service information
public export
record ServiceInfo where
  constructor MkServiceInfo
  name : String
  description : String
  secure : Bool  -- Whether this is a secure/encrypted variant

||| Get service info for well-known ports
public export
serviceInfo : Port -> Maybe ServiceInfo
serviceInfo (MkPort 20) = Just (MkServiceInfo "ftp-data" "FTP Data" False)
serviceInfo (MkPort 21) = Just (MkServiceInfo "ftp" "FTP Control" False)
serviceInfo (MkPort 22) = Just (MkServiceInfo "ssh" "Secure Shell" True)
serviceInfo (MkPort 23) = Just (MkServiceInfo "telnet" "Telnet" False)
serviceInfo (MkPort 25) = Just (MkServiceInfo "smtp" "Simple Mail Transfer" False)
serviceInfo (MkPort 53) = Just (MkServiceInfo "dns" "Domain Name System" False)
serviceInfo (MkPort 67) = Just (MkServiceInfo "dhcp" "DHCP Server" False)
serviceInfo (MkPort 68) = Just (MkServiceInfo "dhcp" "DHCP Client" False)
serviceInfo (MkPort 80) = Just (MkServiceInfo "http" "Hypertext Transfer Protocol" False)
serviceInfo (MkPort 110) = Just (MkServiceInfo "pop3" "Post Office Protocol v3" False)
serviceInfo (MkPort 123) = Just (MkServiceInfo "ntp" "Network Time Protocol" False)
serviceInfo (MkPort 143) = Just (MkServiceInfo "imap" "Internet Message Access" False)
serviceInfo (MkPort 161) = Just (MkServiceInfo "snmp" "Simple Network Management" False)
serviceInfo (MkPort 389) = Just (MkServiceInfo "ldap" "Lightweight Directory Access" False)
serviceInfo (MkPort 443) = Just (MkServiceInfo "https" "HTTP over TLS" True)
serviceInfo (MkPort 445) = Just (MkServiceInfo "smb" "Server Message Block" False)
serviceInfo (MkPort 465) = Just (MkServiceInfo "smtps" "SMTP over TLS" True)
serviceInfo (MkPort 514) = Just (MkServiceInfo "syslog" "System Logging" False)
serviceInfo (MkPort 587) = Just (MkServiceInfo "submission" "SMTP Submission" False)
serviceInfo (MkPort 636) = Just (MkServiceInfo "ldaps" "LDAP over TLS" True)
serviceInfo (MkPort 993) = Just (MkServiceInfo "imaps" "IMAP over TLS" True)
serviceInfo (MkPort 995) = Just (MkServiceInfo "pop3s" "POP3 over TLS" True)
serviceInfo (MkPort 1433) = Just (MkServiceInfo "mssql" "Microsoft SQL Server" False)
serviceInfo (MkPort 1521) = Just (MkServiceInfo "oracle" "Oracle Database" False)
serviceInfo (MkPort 3306) = Just (MkServiceInfo "mysql" "MySQL Database" False)
serviceInfo (MkPort 3389) = Just (MkServiceInfo "rdp" "Remote Desktop Protocol" False)
serviceInfo (MkPort 5432) = Just (MkServiceInfo "postgresql" "PostgreSQL Database" False)
serviceInfo (MkPort 5900) = Just (MkServiceInfo "vnc" "Virtual Network Computing" False)
serviceInfo (MkPort 6379) = Just (MkServiceInfo "redis" "Redis Database" False)
serviceInfo (MkPort 8080) = Just (MkServiceInfo "http-alt" "HTTP Alternate" False)
serviceInfo (MkPort 8443) = Just (MkServiceInfo "https-alt" "HTTPS Alternate" True)
serviceInfo (MkPort 27017) = Just (MkServiceInfo "mongodb" "MongoDB Database" False)
serviceInfo _ = Nothing

||| Get service name for port
public export
serviceName : Port -> Maybe String
serviceName p = map name (serviceInfo p)

||| Check if port uses a secure protocol
public export
isSecurePort : Port -> Bool
isSecurePort p = maybe False secure (serviceInfo p)

--------------------------------------------------------------------------------
-- Port Ranges
--------------------------------------------------------------------------------

||| A range of ports (inclusive)
public export
record PortRange where
  constructor MkPortRange
  startPort : Port
  endPort : Port
  {auto valid : startPort.value <= endPort.value}

||| Create port range safely
public export
mkPortRange : Port -> Port -> Maybe PortRange
mkPortRange start end =
  if start.value <= end.value
    then Just (MkPortRange start end)
    else Nothing

||| Check if port is in range
public export
inRange : PortRange -> Port -> Bool
inRange (MkPortRange start end) p =
  p.value >= start.value && p.value <= end.value

||| Count ports in range
public export
rangeSize : PortRange -> Nat
rangeSize (MkPortRange start end) = end.value `minus` start.value + 1

||| System port range (0-1023)
public export
systemRange : PortRange
systemRange = MkPortRange (MkPort 0) (MkPort 1023)

||| User/registered port range (1024-49151)
public export
userRange : PortRange
userRange = MkPortRange (MkPort 1024) (MkPort 49151)

||| Dynamic/ephemeral port range (49152-65535)
public export
dynamicRange : PortRange
dynamicRange = MkPortRange (MkPort 49152) (MkPort 65535)

--------------------------------------------------------------------------------
-- Port List Operations
--------------------------------------------------------------------------------

||| Common database ports
public export
databasePorts : List Port
databasePorts = [mysql, postgresql, mssql, oracle, mongodb, redis]

||| Common web ports
public export
webPorts : List Port
webPorts = [http, https, httpAlt, httpsAlt]

||| Common mail ports
public export
mailPorts : List Port
mailPorts = [smtp, smtps, submission, pop3, pop3s, imap, imaps]

||| Common remote access ports
public export
remoteAccessPorts : List Port
remoteAccessPorts = [ssh, telnet, rdp, vnc]

||| Insecure ports (should be avoided in production)
public export
insecurePorts : List Port
insecurePorts = [telnet, ftpControl, ftpData, pop3, imap, smtp, http]

||| Check if port is commonly attacked
public export
isHighRiskPort : Port -> Bool
isHighRiskPort p = p `elem` [ssh, rdp, smb, mssql, mysql, postgresql, telnet]
  where
    elem : Port -> List Port -> Bool
    elem _ [] = False
    elem x (y :: ys) = x == y || elem x ys

