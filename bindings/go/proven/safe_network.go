// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeNetwork provides IP address parsing and validation via the Proven FFI.
// All computation is performed in Idris 2 with formal verification.

package proven

// #include <stdint.h>
// #include <stdbool.h>
// #include <stdlib.h>
import "C"

// IPv4 represents a parsed IPv4 address.
type IPv4 struct {
	Octets [4]uint8
}

// ParseIPv4 parses an IPv4 address string into its four octets.
func ParseIPv4(address string) (*IPv4, error) {
	cs, length := cString(address)
	defer unsafeFree(cs)
	result := C.proven_network_parse_ipv4(cs, length)
	if int(result.status) != StatusOK {
		return nil, newError(int(result.status))
	}
	return &IPv4{
		Octets: [4]uint8{
			uint8(result.address.octets[0]),
			uint8(result.address.octets[1]),
			uint8(result.address.octets[2]),
			uint8(result.address.octets[3]),
		},
	}, nil
}

// IPv4IsPrivate checks whether an IPv4 address is in a private range (RFC 1918).
func IPv4IsPrivate(addr *IPv4) bool {
	cAddr := C.IPv4Address{}
	cAddr.octets[0] = C.uint8_t(addr.Octets[0])
	cAddr.octets[1] = C.uint8_t(addr.Octets[1])
	cAddr.octets[2] = C.uint8_t(addr.Octets[2])
	cAddr.octets[3] = C.uint8_t(addr.Octets[3])
	return bool(C.proven_network_ipv4_is_private(cAddr))
}

// IPv4IsLoopback checks whether an IPv4 address is a loopback address (127.0.0.0/8).
func IPv4IsLoopback(addr *IPv4) bool {
	cAddr := C.IPv4Address{}
	cAddr.octets[0] = C.uint8_t(addr.Octets[0])
	cAddr.octets[1] = C.uint8_t(addr.Octets[1])
	cAddr.octets[2] = C.uint8_t(addr.Octets[2])
	cAddr.octets[3] = C.uint8_t(addr.Octets[3])
	return bool(C.proven_network_ipv4_is_loopback(cAddr))
}
