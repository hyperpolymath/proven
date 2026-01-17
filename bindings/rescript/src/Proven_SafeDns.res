// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeDns - DNS record parsing and validation that cannot crash.
 *
 * Provides validation for domain names, DNS record types, and record data.
 */

/** Maximum length of a domain name (253 characters per RFC 1035) */
let maxDomainLength = 253

/** Maximum length of a single label (63 characters per RFC 1035) */
let maxLabelLength = 63

/** Error types for DNS operations */
type dnsError =
  | InvalidDomainName
  | InvalidLabel
  | LabelTooLong
  | DomainTooLong
  | InvalidRecordType
  | InvalidRecordData
  | InvalidTtl
  | InvalidIpAddress
  | InvalidMxPriority

/** DNS record types */
type recordType =
  | A
  | NS
  | CNAME
  | SOA
  | PTR
  | MX
  | TXT
  | AAAA
  | SRV
  | NAPTR
  | CAA

/** DNS record class */
type recordClass =
  | IN // Internet
  | CS // CSNET (obsolete)
  | CH // CHAOS
  | HS // Hesiod

/** Parsed domain name */
type domainName = {
  labels: array<string>,
  raw: string,
}

/** MX record data */
type mxRecord = {
  priority: int,
  exchange: string,
}

/** SRV record data */
type srvRecord = {
  priority: int,
  weight: int,
  port: int,
  target: string,
}

/** Generic DNS record */
type dnsRecord = {
  name: string,
  recordType: recordType,
  class: recordClass,
  ttl: int,
  data: string,
}

/** Get record type from string */
let recordTypeFromString = (str: string): option<recordType> => {
  switch Js.String2.toUpperCase(str) {
  | "A" => Some(A)
  | "NS" => Some(NS)
  | "CNAME" => Some(CNAME)
  | "SOA" => Some(SOA)
  | "PTR" => Some(PTR)
  | "MX" => Some(MX)
  | "TXT" => Some(TXT)
  | "AAAA" => Some(AAAA)
  | "SRV" => Some(SRV)
  | "NAPTR" => Some(NAPTR)
  | "CAA" => Some(CAA)
  | _ => None
  }
}

/** Get string representation of record type */
let recordTypeToString = (rt: recordType): string => {
  switch rt {
  | A => "A"
  | NS => "NS"
  | CNAME => "CNAME"
  | SOA => "SOA"
  | PTR => "PTR"
  | MX => "MX"
  | TXT => "TXT"
  | AAAA => "AAAA"
  | SRV => "SRV"
  | NAPTR => "NAPTR"
  | CAA => "CAA"
  }
}

/** Get numeric value of record type */
let recordTypeValue = (rt: recordType): int => {
  switch rt {
  | A => 1
  | NS => 2
  | CNAME => 5
  | SOA => 6
  | PTR => 12
  | MX => 15
  | TXT => 16
  | AAAA => 28
  | SRV => 33
  | NAPTR => 35
  | CAA => 257
  }
}

/** Get record class from string */
let recordClassFromString = (str: string): option<recordClass> => {
  switch Js.String2.toUpperCase(str) {
  | "IN" => Some(IN)
  | "CS" => Some(CS)
  | "CH" => Some(CH)
  | "HS" => Some(HS)
  | _ => None
  }
}

/** Get string representation of record class */
let recordClassToString = (rc: recordClass): string => {
  switch rc {
  | IN => "IN"
  | CS => "CS"
  | CH => "CH"
  | HS => "HS"
  }
}

/** Check if a character is alphanumeric */
let isAlnum = (char: string): bool => {
  let code = Js.String2.charCodeAt(char, 0)->Belt.Float.toInt
  (code >= 48 && code <= 57) || (code >= 65 && code <= 90) || (code >= 97 && code <= 122)
}

/** Validate a domain name label */
let isValidLabel = (label: string): bool => {
  let length = Js.String2.length(label)
  if length == 0 || length > maxLabelLength {
    false
  } else {
    let firstChar = Js.String2.charAt(label, 0)
    let lastChar = Js.String2.charAt(label, length - 1)

    // Must start and end with alphanumeric
    if !isAlnum(firstChar) || !isAlnum(lastChar) {
      false
    } else {
      let valid = ref(true)

      // Check all characters
      for i in 0 to length - 1 {
        if valid.contents {
          let char = Js.String2.charAt(label, i)
          if !isAlnum(char) && char != "-" {
            valid := false
          }
        }
      }

      // Cannot have consecutive hyphens at positions 2 and 3 (IDN restriction)
      // Exception for punycode (xn--)
      if valid.contents && length >= 4 {
        let char2 = Js.String2.charAt(label, 2)
        let char3 = Js.String2.charAt(label, 3)
        if char2 == "-" && char3 == "-" {
          if !Js.String2.startsWith(label, "xn--") {
            valid := false
          }
        }
      }

      valid.contents
    }
  }
}

/** Validate a full domain name */
let isValidDomainName = (domain: string): bool => {
  let length = Js.String2.length(domain)
  if length == 0 || length > maxDomainLength {
    false
  } else {
    // Remove trailing dot for validation
    let name = if Js.String2.charAt(domain, length - 1) == "." {
      Js.String2.slice(domain, ~from=0, ~to_=length - 1)
    } else {
      domain
    }

    if Js.String2.length(name) == 0 {
      false
    } else {
      let labels = Js.String2.split(name, ".")
      let labelCount = Belt.Array.length(labels)

      if labelCount < 1 {
        false
      } else {
        Belt.Array.every(labels, isValidLabel)
      }
    }
  }
}

/** Parse a domain name into labels */
let parseDomainName = (domain: string): result<domainName, dnsError> => {
  if !isValidDomainName(domain) {
    Error(InvalidDomainName)
  } else {
    let length = Js.String2.length(domain)
    let name = if length > 0 && Js.String2.charAt(domain, length - 1) == "." {
      Js.String2.slice(domain, ~from=0, ~to_=length - 1)
    } else {
      domain
    }

    let labels = Js.String2.split(name, ".")
    Ok({labels, raw: domain})
  }
}

/** Check if a domain name is a fully qualified domain name (ends with dot) */
let isFqdn = (domain: domainName): bool => {
  let length = Js.String2.length(domain.raw)
  length > 0 && Js.String2.charAt(domain.raw, length - 1) == "."
}

/** Get the number of labels in a domain name */
let labelCount = (domain: domainName): int => {
  Belt.Array.length(domain.labels)
}

/** Get the top-level domain */
let tld = (domain: domainName): option<string> => {
  let count = Belt.Array.length(domain.labels)
  if count == 0 {
    None
  } else {
    Belt.Array.get(domain.labels, count - 1)
  }
}

/** Get the second-level domain (e.g., "example" from "www.example.com") */
let sld = (domain: domainName): option<string> => {
  let count = Belt.Array.length(domain.labels)
  if count < 2 {
    None
  } else {
    Belt.Array.get(domain.labels, count - 2)
  }
}

/** Validate an IPv4 address for A record */
let isValidIPv4 = (address: string): bool => {
  let parts = Js.String2.split(address, ".")
  if Belt.Array.length(parts) != 4 {
    false
  } else {
    Belt.Array.every(parts, part => {
      switch Belt.Int.fromString(part) {
      | Some(n) => n >= 0 && n <= 255
      | None => false
      }
    })
  }
}

/** Validate an IPv6 address for AAAA record */
let isValidIPv6 = (address: string): bool => {
  let length = Js.String2.length(address)
  if length == 0 || length > 45 {
    false
  } else {
    // Count groups and check for ::
    let groups = ref(0)
    let doubleColon = ref(false)
    let i = ref(0)

    let valid = ref(true)

    while valid.contents && i.contents < length {
      // Check for double colon
      if i.contents + 1 < length {
        let char = Js.String2.charAt(address, i.contents)
        let nextChar = Js.String2.charAt(address, i.contents + 1)
        if char == ":" && nextChar == ":" {
          if doubleColon.contents {
            valid := false // Only one :: allowed
          } else {
            doubleColon := true
            i := i.contents + 2
          }
        } else if char == ":" {
          i := i.contents + 1
        } else {
          // Parse hex group
          let groupLen = ref(0)
          while i.contents < length && Js.String2.charAt(address, i.contents) != ":" {
            let c = Js.String2.charAt(address, i.contents)
            let code = Js.String2.charCodeAt(c, 0)->Belt.Float.toInt
            let isHex =
              (code >= 48 && code <= 57) ||
              (code >= 65 && code <= 70) ||
              (code >= 97 && code <= 102)
            if !isHex {
              valid := false
            }
            groupLen := groupLen.contents + 1
            if groupLen.contents > 4 {
              valid := false
            }
            i := i.contents + 1
          }
          if groupLen.contents == 0 {
            valid := false
          }
          groups := groups.contents + 1
        }
      } else {
        let char = Js.String2.charAt(address, i.contents)
        if char == ":" {
          i := i.contents + 1
        } else {
          // Single character group
          let code = Js.String2.charCodeAt(char, 0)->Belt.Float.toInt
          let isHex =
            (code >= 48 && code <= 57) ||
            (code >= 65 && code <= 70) ||
            (code >= 97 && code <= 102)
          if !isHex {
            valid := false
          }
          groups := groups.contents + 1
          i := i.contents + 1
        }
      }
    }

    // Must have exactly 8 groups, or fewer with ::
    if valid.contents {
      if doubleColon.contents {
        groups.contents <= 7
      } else {
        groups.contents == 8
      }
    } else {
      false
    }
  }
}

/** Parse an MX record value (priority + exchange) */
let parseMxRecord = (data: string): result<mxRecord, dnsError> => {
  switch Js.String2.indexOf(data, " ") {
  | -1 => Error(InvalidRecordData)
  | spacePos =>
    let priorityStr = Js.String2.slice(data, ~from=0, ~to_=spacePos)
    let exchange = Js.String2.trim(Js.String2.sliceToEnd(data, ~from=spacePos + 1))

    switch Belt.Int.fromString(priorityStr) {
    | None => Error(InvalidMxPriority)
    | Some(priority) =>
      if priority < 0 || priority > 65535 {
        Error(InvalidMxPriority)
      } else if !isValidDomainName(exchange) {
        Error(InvalidRecordData)
      } else {
        Ok({priority, exchange})
      }
    }
  }
}

/** Parse an SRV record value */
let parseSrvRecord = (data: string): result<srvRecord, dnsError> => {
  let parts = Js.String2.split(data, " ")
  if Belt.Array.length(parts) < 4 {
    Error(InvalidRecordData)
  } else {
    let priorityStr = Belt.Array.getUnsafe(parts, 0)
    let weightStr = Belt.Array.getUnsafe(parts, 1)
    let portStr = Belt.Array.getUnsafe(parts, 2)
    let target = Belt.Array.getUnsafe(parts, 3)

    switch (
      Belt.Int.fromString(priorityStr),
      Belt.Int.fromString(weightStr),
      Belt.Int.fromString(portStr),
    ) {
    | (Some(priority), Some(weight), Some(port)) =>
      if priority < 0 || priority > 65535 || weight < 0 || weight > 65535 || port < 0 || port >
        65535 {
        Error(InvalidRecordData)
      } else if Js.String2.length(target) > 0 && !isValidDomainName(target) {
        Error(InvalidRecordData)
      } else {
        Ok({priority, weight, port, target})
      }
    | _ => Error(InvalidRecordData)
    }
  }
}

/** Validate TXT record data */
let isValidTxtRecord = (data: string): bool => {
  let length = Js.String2.length(data)
  if length > 65535 {
    false
  } else {
    let inQuote = ref(false)
    let segmentLen = ref(0)
    let valid = ref(true)

    for i in 0 to length - 1 {
      if valid.contents {
        let char = Js.String2.charAt(data, i)
        if char == "\"" {
          inQuote := !inQuote.contents
          segmentLen := 0
        } else if inQuote.contents {
          segmentLen := segmentLen.contents + 1
          if segmentLen.contents > 255 {
            valid := false
          }
        } else if char != " " {
          // Outside quotes, only allow spaces between segments
          valid := false
        }
      }
    }

    valid.contents && !inQuote.contents // Must have balanced quotes
  }
}

/** Validate a TTL value */
let isValidTtl = (ttl: int): bool => {
  // TTL must be between 0 and 2147483647 (max signed 32-bit)
  ttl >= 0 && ttl <= 2147483647
}

/** Parse a TTL string (supports suffixes like 1h, 1d, 1w) */
let parseTtl = (str: string): result<int, dnsError> => {
  let length = Js.String2.length(str)
  if length == 0 {
    Error(InvalidTtl)
  } else {
    // Find where the numeric part ends
    let i = ref(0)
    while i.contents < length {
      let char = Js.String2.charAt(str, i.contents)
      let code = Js.String2.charCodeAt(char, 0)->Belt.Float.toInt
      if code >= 48 && code <= 57 {
        i := i.contents + 1
      } else {
        // Break the loop by setting to length
        i := length + 1
      }
    }

    let numericEnd = if i.contents > length {
      i.contents - 1
    } else {
      i.contents
    }

    if numericEnd == 0 {
      Error(InvalidTtl)
    } else {
      let numStr = Js.String2.slice(str, ~from=0, ~to_=numericEnd)
      switch Belt.Int.fromString(numStr) {
      | None => Error(InvalidTtl)
      | Some(value) =>
        // Parse optional suffix
        if numericEnd >= length {
          if isValidTtl(value) {
            Ok(value)
          } else {
            Error(InvalidTtl)
          }
        } else {
          let suffix = Js.String2.charAt(str, numericEnd)
          let multiplier = switch suffix {
          | "s" | "S" => Some(1) // seconds
          | "m" | "M" => Some(60) // minutes
          | "h" | "H" => Some(3600) // hours
          | "d" | "D" => Some(86400) // days
          | "w" | "W" => Some(604800) // weeks
          | _ => None
          }
          switch multiplier {
          | None => Error(InvalidTtl)
          | Some(m) =>
            let result = value * m
            if isValidTtl(result) {
              Ok(result)
            } else {
              Error(InvalidTtl)
            }
          }
        }
      }
    }
  }
}

/** Normalize a domain name (lowercase, remove trailing dot) */
let normalizeDomainName = (domain: string): result<string, dnsError> => {
  if !isValidDomainName(domain) {
    Error(InvalidDomainName)
  } else {
    let length = Js.String2.length(domain)
    let name = if length > 0 && Js.String2.charAt(domain, length - 1) == "." {
      Js.String2.slice(domain, ~from=0, ~to_=length - 1)
    } else {
      domain
    }
    Ok(Js.String2.toLowerCase(name))
  }
}

/** DNS error to string representation */
let dnsErrorToString = (error: dnsError): string => {
  switch error {
  | InvalidDomainName => "Invalid domain name"
  | InvalidLabel => "Invalid label"
  | LabelTooLong => "Label is too long"
  | DomainTooLong => "Domain name is too long"
  | InvalidRecordType => "Invalid record type"
  | InvalidRecordData => "Invalid record data"
  | InvalidTtl => "Invalid TTL value"
  | InvalidIpAddress => "Invalid IP address"
  | InvalidMxPriority => "Invalid MX priority"
  }
}
