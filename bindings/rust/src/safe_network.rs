// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
//! Safe network address operations.

use crate::core::{Error, Result};
use std::net::{IpAddr, Ipv4Addr, Ipv6Addr};

/// Safe network operations.
pub struct SafeNetwork;

/// IPv4 address classification.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Ipv4Class {
    /// Loopback (127.0.0.0/8)
    Loopback,
    /// Private (10.0.0.0/8, 172.16.0.0/12, 192.168.0.0/16)
    Private,
    /// Link-local (169.254.0.0/16)
    LinkLocal,
    /// Multicast (224.0.0.0/4)
    Multicast,
    /// Broadcast (255.255.255.255)
    Broadcast,
    /// Public routable address
    Public,
}

/// CIDR notation representation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Cidr {
    /// Network address
    pub address: IpAddr,
    /// Prefix length
    pub prefix_len: u8,
}

impl SafeNetwork {
    /// Parse an IPv4 address.
    pub fn parse_ipv4(s: &str) -> Result<Ipv4Addr> {
        s.parse::<Ipv4Addr>()
            .map_err(|e| Error::ParseError(e.to_string()))
    }

    /// Parse an IPv6 address.
    pub fn parse_ipv6(s: &str) -> Result<Ipv6Addr> {
        s.parse::<Ipv6Addr>()
            .map_err(|e| Error::ParseError(e.to_string()))
    }

    /// Parse any IP address.
    pub fn parse_ip(s: &str) -> Result<IpAddr> {
        s.parse::<IpAddr>()
            .map_err(|e| Error::ParseError(e.to_string()))
    }

    /// Classify an IPv4 address.
    pub fn classify_ipv4(addr: &Ipv4Addr) -> Ipv4Class {
        if addr.is_loopback() {
            Ipv4Class::Loopback
        } else if addr.is_private() {
            Ipv4Class::Private
        } else if addr.is_link_local() {
            Ipv4Class::LinkLocal
        } else if addr.is_multicast() {
            Ipv4Class::Multicast
        } else if addr.is_broadcast() {
            Ipv4Class::Broadcast
        } else {
            Ipv4Class::Public
        }
    }

    /// Parse CIDR notation.
    pub fn parse_cidr(s: &str) -> Result<Cidr> {
        let parts: Vec<&str> = s.split('/').collect();
        if parts.len() != 2 {
            return Err(Error::ParseError("Invalid CIDR notation".into()));
        }

        let address = Self::parse_ip(parts[0])?;
        let prefix_len = parts[1]
            .parse::<u8>()
            .map_err(|_| Error::ParseError("Invalid prefix length".into()))?;

        let max_prefix = match address {
            IpAddr::V4(_) => 32,
            IpAddr::V6(_) => 128,
        };

        if prefix_len > max_prefix {
            return Err(Error::ValidationError(format!(
                "Prefix length {} exceeds maximum {}",
                prefix_len, max_prefix
            )));
        }

        Ok(Cidr {
            address,
            prefix_len,
        })
    }

    /// Check if an IP is within a CIDR range.
    pub fn is_in_cidr(ip: &IpAddr, cidr: &Cidr) -> bool {
        match (ip, &cidr.address) {
            (IpAddr::V4(ip), IpAddr::V4(net)) => {
                let ip_bits = u32::from_be_bytes(ip.octets());
                let net_bits = u32::from_be_bytes(net.octets());
                let mask = if cidr.prefix_len == 0 {
                    0
                } else {
                    !0u32 << (32 - cidr.prefix_len)
                };
                (ip_bits & mask) == (net_bits & mask)
            }
            (IpAddr::V6(ip), IpAddr::V6(net)) => {
                let ip_bits = u128::from_be_bytes(ip.octets());
                let net_bits = u128::from_be_bytes(net.octets());
                let mask = if cidr.prefix_len == 0 {
                    0
                } else {
                    !0u128 << (128 - cidr.prefix_len)
                };
                (ip_bits & mask) == (net_bits & mask)
            }
            _ => false, // IPv4/IPv6 mismatch
        }
    }

    /// Validate a port number.
    pub fn validate_port(port: u16) -> bool {
        true // All u16 values are valid ports
    }

    /// Check if port is well-known (0-1023).
    pub fn is_well_known_port(port: u16) -> bool {
        port <= 1023
    }

    /// Check if port is registered (1024-49151).
    pub fn is_registered_port(port: u16) -> bool {
        (1024..=49151).contains(&port)
    }

    /// Check if port is dynamic/private (49152-65535).
    pub fn is_dynamic_port(port: u16) -> bool {
        port >= 49152
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_ipv4() {
        let addr = SafeNetwork::parse_ipv4("192.168.1.1").unwrap();
        assert_eq!(addr.octets(), [192, 168, 1, 1]);
    }

    #[test]
    fn test_classify_ipv4() {
        assert_eq!(
            SafeNetwork::classify_ipv4(&Ipv4Addr::new(127, 0, 0, 1)),
            Ipv4Class::Loopback
        );
        assert_eq!(
            SafeNetwork::classify_ipv4(&Ipv4Addr::new(192, 168, 1, 1)),
            Ipv4Class::Private
        );
        assert_eq!(
            SafeNetwork::classify_ipv4(&Ipv4Addr::new(8, 8, 8, 8)),
            Ipv4Class::Public
        );
    }

    #[test]
    fn test_cidr() {
        let cidr = SafeNetwork::parse_cidr("192.168.0.0/24").unwrap();
        let ip1 = IpAddr::V4(Ipv4Addr::new(192, 168, 0, 100));
        let ip2 = IpAddr::V4(Ipv4Addr::new(192, 168, 1, 1));

        assert!(SafeNetwork::is_in_cidr(&ip1, &cidr));
        assert!(!SafeNetwork::is_in_cidr(&ip2, &cidr));
    }
}
