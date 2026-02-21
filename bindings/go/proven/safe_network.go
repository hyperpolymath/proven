// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import (
	"fmt"
	"strconv"
	"strings"
)

// IPv4 represents an IPv4 address.
type IPv4 struct {
	A, B, C, D uint8
}

// String formats an IPv4 address as a string.
func (ip IPv4) String() string {
	return fmt.Sprintf("%d.%d.%d.%d", ip.A, ip.B, ip.C, ip.D)
}

// ParseIPv4 parses an IPv4 address string.
func ParseIPv4(address string) *IPv4 {
	parts := strings.Split(address, ".")
	if len(parts) != 4 {
		return nil
	}

	octets := make([]uint8, 4)
	for i, part := range parts {
		val, err := strconv.Atoi(part)
		if err != nil || val < 0 || val > 255 {
			return nil
		}
		octets[i] = uint8(val)
	}

	return &IPv4{
		A: octets[0],
		B: octets[1],
		C: octets[2],
		D: octets[3],
	}
}

// IsValidIPv4 checks if a string is a valid IPv4 address.
func IsValidIPv4(address string) bool {
	return ParseIPv4(address) != nil
}

// IsPrivate checks if an IPv4 address is in a private range.
func IsPrivate(address string) bool {
	ip := ParseIPv4(address)
	if ip == nil {
		return false
	}

	// 10.0.0.0/8
	if ip.A == 10 {
		return true
	}
	// 172.16.0.0/12
	if ip.A == 172 && ip.B >= 16 && ip.B <= 31 {
		return true
	}
	// 192.168.0.0/16
	if ip.A == 192 && ip.B == 168 {
		return true
	}

	return false
}

// IsLoopback checks if an IPv4 address is a loopback address (127.0.0.0/8).
func IsLoopback(address string) bool {
	ip := ParseIPv4(address)
	if ip == nil {
		return false
	}
	return ip.A == 127
}

// IsPublic checks if an IPv4 address is public (not private or loopback).
func IsPublic(address string) bool {
	return IsValidIPv4(address) && !IsPrivate(address) && !IsLoopback(address)
}

// FormatIPv4 formats an IPv4 address as a string.
func FormatIPv4(ip *IPv4) string {
	if ip == nil {
		return ""
	}
	return ip.String()
}
