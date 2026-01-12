// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

namespace Proven

/// Safe network validation and operations.
module SafeNetwork =
    open System
    open System.Text.RegularExpressions

    /// Classification of IP addresses.
    type IpClassification =
        | Loopback
        | Private
        | Reserved
        | Public
        | Invalid

    /// Parsed IPv4 address.
    type IPv4Address = {
        A: int
        B: int
        C: int
        D: int
    }

    /// Create an IPv4Address.
    let createIPv4 a b c d = { A = a; B = b; C = c; D = d }

    /// Parse an IPv4 address string.
    let parseIPv4 (address: string) : IPv4Address option =
        let parts = address.Split('.')
        if parts.Length <> 4 then None
        else
            let octets = parts |> Array.choose (fun part ->
                if String.IsNullOrEmpty(part) then None
                elif not (Regex.IsMatch(part, @"^[0-9]+$")) then None
                elif part.Length > 1 && part.StartsWith("0") then None
                else
                    match Int32.TryParse(part) with
                    | true, n when n >= 0 && n <= 255 -> Some n
                    | _ -> None
            )
            if octets.Length = 4 then Some(createIPv4 octets.[0] octets.[1] octets.[2] octets.[3])
            else None

    /// Convert IPv4Address to integer representation.
    let ipv4ToInt (ip: IPv4Address) : int64 =
        ((int64 ip.A &&& 0xFFL) <<< 24) |||
        ((int64 ip.B &&& 0xFFL) <<< 16) |||
        ((int64 ip.C &&& 0xFFL) <<< 8) |||
        (int64 ip.D &&& 0xFFL)

    /// Check if IPv4 is a loopback address.
    let isLoopback (ip: IPv4Address) : bool = ip.A = 127

    /// Check if IPv4 is a private address (RFC 1918).
    let isPrivate (ip: IPv4Address) : bool =
        ip.A = 10 ||
        (ip.A = 172 && ip.B >= 16 && ip.B <= 31) ||
        (ip.A = 192 && ip.B = 168)

    /// Check if IPv4 is a reserved address.
    let isReserved (ip: IPv4Address) : bool =
        ip.A = 0 ||
        (ip.A = 100 && ip.B >= 64 && ip.B <= 127) ||
        (ip.A = 169 && ip.B = 254) ||
        (ip.A = 192 && ip.B = 0 && ip.C = 0) ||
        (ip.A = 192 && ip.B = 0 && ip.C = 2) ||
        (ip.A = 198 && ip.B = 51 && ip.C = 100) ||
        (ip.A = 203 && ip.B = 0 && ip.C = 113) ||
        (ip.A >= 224 && ip.A <= 239) ||
        ip.A >= 240

    /// Check if IPv4 is a public address.
    let isPublic (ip: IPv4Address) : bool =
        not (isLoopback ip) && not (isPrivate ip) && not (isReserved ip)

    /// Classify an IPv4 address.
    let classify (ip: IPv4Address) : IpClassification =
        if isLoopback ip then Loopback
        elif isPrivate ip then Private
        elif isReserved ip then Reserved
        else Public

    /// Check if IPv4 is in a CIDR range.
    let isInRange (ip: IPv4Address) (network: IPv4Address) (prefixLength: int) : bool =
        if prefixLength < 0 || prefixLength > 32 then false
        else
            let mask = if prefixLength = 0 then 0L else (0xFFFFFFFFL <<< (32 - prefixLength))
            (ipv4ToInt ip &&& mask) = (ipv4ToInt network &&& mask)

    /// Format IPv4Address as string.
    let formatIPv4 (ip: IPv4Address) : string =
        sprintf "%d.%d.%d.%d" ip.A ip.B ip.C ip.D

    /// Check if string is a valid IPv4 address.
    let isValidIPv4 (address: string) : bool =
        (parseIPv4 address).IsSome

    /// Check if string is a valid IPv6 address.
    let isValidIPv6 (address: string) : bool =
        try
            let parsed = Net.IPAddress.Parse(address)
            parsed.AddressFamily = Net.Sockets.AddressFamily.InterNetworkV6
        with
        | _ -> false

    /// Check if string is any valid IP address.
    let isValidIP (address: string) : bool =
        isValidIPv4 address || isValidIPv6 address

    /// Classify an IPv4 address string.
    let classifyIPv4 (address: string) : IpClassification =
        match parseIPv4 address with
        | Some ip -> classify ip
        | None -> Invalid

    /// Check if port number is valid (1-65535).
    let isValidPort (port: int) : bool = port >= 1 && port <= 65535

    /// Check if port is privileged (< 1024).
    let isPrivilegedPort (port: int) : bool = port >= 1 && port < 1024

    /// Check if string is a valid hostname.
    let isValidHostname (hostname: string) : bool =
        if String.IsNullOrEmpty(hostname) || hostname.Length > 253 then false
        else
            let labels = hostname.Split('.')
            labels |> Array.forall (fun label ->
                not (String.IsNullOrEmpty(label)) &&
                label.Length <= 63 &&
                Regex.IsMatch(label, @"^[a-zA-Z0-9]([a-zA-Z0-9-]*[a-zA-Z0-9])?$")
            )

    /// Check if host:port string is valid.
    let isValidHostPort (hostPort: string) : bool =
        let lastColon = hostPort.LastIndexOf(':')
        if lastColon < 0 then false
        else
            let host = hostPort.Substring(0, lastColon)
            let portStr = hostPort.Substring(lastColon + 1)
            match Int32.TryParse(portStr) with
            | true, port when isValidPort port ->
                isValidIP host || isValidHostname host
            | _ -> false

    /// Check if string is a valid URL.
    let isValidUrl (url: string) : bool =
        try
            let uri = Uri(url)
            uri.Scheme = "http" || uri.Scheme = "https"
        with
        | _ -> false

    /// Parse a URL safely.
    let parseUrl (url: string) : Uri option =
        try
            let uri = Uri(url)
            Some uri
        with
        | _ -> None

    /// Check if URL host is a private IP (SSRF protection).
    let isPrivateUrl (url: string) : bool =
        match parseUrl url with
        | Some uri ->
            let host = uri.Host
            host = "localhost" || host = "127.0.0.1" || host = "::1" ||
            (match parseIPv4 host with
             | Some ip -> isPrivate ip || isLoopback ip || isReserved ip
             | None -> false)
        | None -> false
