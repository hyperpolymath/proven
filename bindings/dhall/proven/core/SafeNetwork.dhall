-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath

{-
SafeNetwork - Safe network primitives

Provides IP address, CIDR, port, and network configuration types.
All operations are type-safe and bounds-checked.

Part of the Proven safety primitives library.
-}

let Prelude = https://prelude.dhall-lang.org/v23.0.0/package.dhall
    sha256:eb693342eb769f782174157eba9b5924cf8ac6793897fc36a31ccbd6f56dafe2

let Natural/lessThan = Prelude.Natural.lessThan
let Natural/greaterThan = Prelude.Natural.greaterThan

-- IPv4 octet (0-255)
let Octet = { value : Natural }

-- Create validated octet
let mkOctet
    : Natural -> Optional Octet
    = \(n : Natural) ->
        if Natural/greaterThan n 255
        then None Octet
        else Some { value = n }

-- IPv4 address
let IPv4 = { a : Natural, b : Natural, c : Natural, d : Natural }

-- Create IPv4 address
let mkIPv4
    : Natural -> Natural -> Natural -> Natural -> Optional IPv4
    = \(a : Natural) -> \(b : Natural) -> \(c : Natural) -> \(d : Natural) ->
        if      Natural/greaterThan a 255
        then    None IPv4
        else if Natural/greaterThan b 255
        then    None IPv4
        else if Natural/greaterThan c 255
        then    None IPv4
        else if Natural/greaterThan d 255
        then    None IPv4
        else    Some { a = a, b = b, c = c, d = d }

-- Format IPv4 address
let formatIPv4
    : IPv4 -> Text
    = \(ip : IPv4) ->
        Natural/show ip.a ++ "." ++ Natural/show ip.b ++ "." ++ Natural/show ip.c ++ "." ++ Natural/show ip.d

-- IPv6 segment (0-65535)
let IPv6Segment = { value : Natural }

-- Create IPv6 segment
let mkIPv6Segment
    : Natural -> Optional IPv6Segment
    = \(n : Natural) ->
        if Natural/greaterThan n 65535
        then None IPv6Segment
        else Some { value = n }

-- IPv6 address (8 segments)
let IPv6 = {
    s1 : Natural, s2 : Natural, s3 : Natural, s4 : Natural,
    s5 : Natural, s6 : Natural, s7 : Natural, s8 : Natural
}

-- IP address union
let IPAddress = < V4 : IPv4 | V6 : IPv6 >

-- Port number (1-65535)
let Port = { port : Natural }

-- Create validated port
let mkPort
    : Natural -> Optional Port
    = \(n : Natural) ->
        if Natural/lessThan n 1
        then None Port
        else if Natural/greaterThan n 65535
        then None Port
        else Some { port = n }

-- Common ports
let CommonPorts = {
    http = { port = 80 },
    https = { port = 443 },
    ssh = { port = 22 },
    ftp = { port = 21 },
    ftps = { port = 990 },
    sftp = { port = 22 },
    dns = { port = 53 },
    smtp = { port = 25 },
    smtps = { port = 465 },
    pop3 = { port = 110 },
    pop3s = { port = 995 },
    imap = { port = 143 },
    imaps = { port = 993 },
    mysql = { port = 3306 },
    postgres = { port = 5432 },
    redis = { port = 6379 },
    mongodb = { port = 27017 },
    elasticsearch = { port = 9200 }
}

-- CIDR notation
let CIDR = {
    ip : IPv4,
    prefix : Natural
}

-- Create validated CIDR
let mkCIDR
    : IPv4 -> Natural -> Optional CIDR
    = \(ip : IPv4) -> \(prefix : Natural) ->
        if Natural/greaterThan prefix 32
        then None CIDR
        else Some { ip = ip, prefix = prefix }

-- Format CIDR
let formatCIDR
    : CIDR -> Text
    = \(c : CIDR) ->
        formatIPv4 c.ip ++ "/" ++ Natural/show c.prefix

-- Common private network ranges
let PrivateNetworks = {
    classA = { ip = { a = 10, b = 0, c = 0, d = 0 }, prefix = 8 },
    classB = { ip = { a = 172, b = 16, c = 0, d = 0 }, prefix = 12 },
    classC = { ip = { a = 192, b = 168, c = 0, d = 0 }, prefix = 16 },
    localhost = { ip = { a = 127, b = 0, c = 0, d = 0 }, prefix = 8 },
    linkLocal = { ip = { a = 169, b = 254, c = 0, d = 0 }, prefix = 16 }
}

-- Network endpoint (host + port)
let Endpoint = {
    host : Text,
    port : Port
}

-- Create endpoint
let mkEndpoint
    : Text -> Port -> Endpoint
    = \(host : Text) -> \(port : Port) ->
        { host = host, port = port }

-- Format endpoint
let formatEndpoint
    : Endpoint -> Text
    = \(e : Endpoint) ->
        e.host ++ ":" ++ Natural/show e.port.port

-- Protocol type
let Protocol = < TCP | UDP | SCTP | ICMP >

-- Network interface
let NetworkInterface = {
    name : Text,
    ip : Optional IPv4,
    netmask : Optional IPv4,
    gateway : Optional IPv4
}

-- DNS record types
let DNSRecordType = < A | AAAA | CNAME | MX | TXT | NS | SOA | PTR | SRV >

-- DNS record
let DNSRecord = {
    name : Text,
    recordType : DNSRecordType,
    value : Text,
    ttl : Natural
}

-- Firewall rule
let FirewallAction = < Allow | Deny | Drop >

let FirewallRule = {
    action : FirewallAction,
    protocol : Protocol,
    sourceIP : Optional CIDR,
    destIP : Optional CIDR,
    sourcePort : Optional Port,
    destPort : Optional Port
}

-- MAC address type
let MACAddress = {
    octets : List Natural  -- 6 octets
}

-- Create MAC address
let mkMACAddress
    : Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> Optional MACAddress
    = \(o1 : Natural) -> \(o2 : Natural) -> \(o3 : Natural) ->
      \(o4 : Natural) -> \(o5 : Natural) -> \(o6 : Natural) ->
        let valid = \(n : Natural) -> Natural/lessThan n 256
        in if valid o1 && valid o2 && valid o3 && valid o4 && valid o5 && valid o6
           then Some { octets = [ o1, o2, o3, o4, o5, o6 ] }
           else None MACAddress

in {
    -- Types
    Octet,
    IPv4,
    IPv6,
    IPv6Segment,
    IPAddress,
    Port,
    CIDR,
    Endpoint,
    Protocol,
    NetworkInterface,
    DNSRecordType,
    DNSRecord,
    FirewallAction,
    FirewallRule,
    MACAddress,

    -- Constructors
    mkOctet,
    mkIPv4,
    mkIPv6Segment,
    mkPort,
    mkCIDR,
    mkEndpoint,
    mkMACAddress,

    -- Formatting
    formatIPv4,
    formatCIDR,
    formatEndpoint,

    -- Constants
    CommonPorts,
    PrivateNetworks
}
