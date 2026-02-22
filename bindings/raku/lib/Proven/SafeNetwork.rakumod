# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Proven::SafeNetwork - IP address parsing and classification.
#
# Thin wrapper over libproven's SafeNetwork module.  All parsing and
# classification is performed in formally verified Idris 2 code.  This module
# does NOT reimplement any network logic.

unit module Proven::SafeNetwork;

use NativeCall;
use Proven::LibProven;

# ============================================================================
# IPv4 parsing
# ============================================================================

#| Parse an IPv4 address string (e.g. "192.168.1.1").
#| Returns a hash with keys <o0 o1 o2 o3 str> on success, or Nil on error.
sub parse-ipv4(Str:D $addr --> Hash) is export {
    my ($buf, $len) = str-to-buf($addr);
    my IPv4Result $r = proven_network_parse_ipv4(nativecast(Pointer, $buf), $len);
    return Nil unless $r.status == 0;
    return {
        o0  => $r.o0,
        o1  => $r.o1,
        o2  => $r.o2,
        o3  => $r.o3,
        str => "{$r.o0}.{$r.o1}.{$r.o2}.{$r.o3}",
    };
}

#| Check if an IPv4 address (as dotted-quad string) is private (RFC 1918).
#| Parses the string, then calls the C classifier.  Returns Nil on parse error.
sub ipv4-is-private(Str:D $addr --> Bool) is export {
    my ($buf, $len) = str-to-buf($addr);
    my IPv4Result $r = proven_network_parse_ipv4(nativecast(Pointer, $buf), $len);
    return Nil unless $r.status == 0;
    my IPv4Address $a .= new(:o0($r.o0), :o1($r.o1), :o2($r.o2), :o3($r.o3));
    return proven_network_ipv4_is_private($a);
}

#| Check if an IPv4 address (as dotted-quad string) is loopback (127.0.0.0/8).
#| Parses the string, then calls the C classifier.  Returns Nil on parse error.
sub ipv4-is-loopback(Str:D $addr --> Bool) is export {
    my ($buf, $len) = str-to-buf($addr);
    my IPv4Result $r = proven_network_parse_ipv4(nativecast(Pointer, $buf), $len);
    return Nil unless $r.status == 0;
    my IPv4Address $a .= new(:o0($r.o0), :o1($r.o1), :o2($r.o2), :o3($r.o3));
    return proven_network_ipv4_is_loopback($a);
}
