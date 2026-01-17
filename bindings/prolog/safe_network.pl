% SPDX-License-Identifier: PMPL-1.0
% SPDX-FileCopyrightText: 2025 Hyperpolymath
%
% Proven SafeNetwork - Network validation for Prolog
% Provides IP address parsing, validation, and classification.

:- module(safe_network, [
    parse_ipv4/2,
    format_ipv4/2,
    is_loopback/1,
    is_private_ip/1,
    is_reserved_ip/1,
    is_public_ip/1,
    classify_ip/2,
    is_valid_port/1,
    is_privileged_port/1,
    is_valid_hostname/1,
    parse_ipv6/2,
    format_ipv6/2,
    ip_to_int/2,
    int_to_ip/2,
    cidr_contains/2,
    parse_cidr/2
]).

:- use_module(library(lists)).

%! parse_ipv4(+IpString, -Ip) is semidet.
%
%  Parse IPv4 address string.
%  Ip is ipv4(A, B, C, D) or fails.
%
%  @arg IpString IPv4 address string
%  @arg Ip ipv4/4 structure
parse_ipv4(IpString, Ip) :-
    atom_string(IpString, IpStr),
    split_string(IpStr, ".", "", Parts),
    Parts = [A, B, C, D],
    maplist(parse_octet, Parts, [Oa, Ob, Oc, Od]),
    !,
    Ip = ipv4(Oa, Ob, Oc, Od).

parse_octet(Str, Octet) :-
    number_string(Octet, Str),
    Octet >= 0,
    Octet =< 255.

%! format_ipv4(+Ip, -String) is det.
%
%  Format IPv4 address as dotted-decimal string.
%
%  @arg Ip ipv4/4 structure
%  @arg String Dotted-decimal string
format_ipv4(ipv4(A, B, C, D), String) :-
    format(atom(String), "~d.~d.~d.~d", [A, B, C, D]).

%! is_loopback(+Ip) is semidet.
%
%  Check if IP is a loopback address (127.x.x.x).
%
%  @arg Ip ipv4/4 structure
is_loopback(ipv4(127, _, _, _)).

%! is_private_ip(+Ip) is semidet.
%
%  Check if IP is a private address (RFC 1918).
%
%  @arg Ip ipv4/4 structure
is_private_ip(ipv4(10, _, _, _)) :- !.
is_private_ip(ipv4(172, B, _, _)) :- B >= 16, B =< 31, !.
is_private_ip(ipv4(192, 168, _, _)).

%! is_reserved_ip(+Ip) is semidet.
%
%  Check if IP is a reserved address.
%
%  @arg Ip ipv4/4 structure
is_reserved_ip(ipv4(0, _, _, _)) :- !.
is_reserved_ip(ipv4(100, B, _, _)) :- B >= 64, B =< 127, !.
is_reserved_ip(ipv4(169, 254, _, _)) :- !.
is_reserved_ip(ipv4(192, 0, 0, _)) :- !.
is_reserved_ip(ipv4(192, 0, 2, _)) :- !.
is_reserved_ip(ipv4(198, 51, 100, _)) :- !.
is_reserved_ip(ipv4(203, 0, 113, _)) :- !.
is_reserved_ip(ipv4(A, _, _, _)) :- A >= 224, A =< 239, !.
is_reserved_ip(ipv4(A, _, _, _)) :- A >= 240, !.
is_reserved_ip(ipv4(255, 255, 255, 255)).

%! is_public_ip(+Ip) is semidet.
%
%  Check if IP is a public (routable) address.
%
%  @arg Ip ipv4/4 structure
is_public_ip(Ip) :-
    Ip = ipv4(_, _, _, _),
    \+ is_loopback(Ip),
    \+ is_private_ip(Ip),
    \+ is_reserved_ip(Ip).

%! classify_ip(+Ip, -Class) is det.
%
%  Classify IP address.
%  Class is one of: loopback, private, reserved, public.
%
%  @arg Ip ipv4/4 structure
%  @arg Class Classification atom
classify_ip(Ip, loopback) :- is_loopback(Ip), !.
classify_ip(Ip, private) :- is_private_ip(Ip), !.
classify_ip(Ip, reserved) :- is_reserved_ip(Ip), !.
classify_ip(_, public).

%! is_valid_port(+Port) is semidet.
%
%  Check if port number is valid (1-65535).
%
%  @arg Port Port number
is_valid_port(Port) :-
    integer(Port),
    Port >= 1,
    Port =< 65535.

%! is_privileged_port(+Port) is semidet.
%
%  Check if port is privileged (1-1023).
%
%  @arg Port Port number
is_privileged_port(Port) :-
    is_valid_port(Port),
    Port =< 1023.

%! is_valid_hostname(+Hostname) is semidet.
%
%  Validate hostname according to RFC 1123.
%
%  @arg Hostname Hostname string
is_valid_hostname(Hostname) :-
    atom_string(Hostname, HostStr),
    string_length(HostStr, Len),
    Len > 0,
    Len =< 253,
    split_string(HostStr, ".", "", Labels),
    forall(member(Label, Labels), is_valid_label(Label)).

is_valid_label(Label) :-
    string_length(Label, Len),
    Len > 0,
    Len =< 63,
    string_chars(Label, Chars),
    Chars = [First|_],
    char_type(First, alnum),
    last(Chars, Last),
    char_type(Last, alnum),
    forall(member(C, Chars), is_hostname_char(C)).

is_hostname_char(C) :- char_type(C, alnum), !.
is_hostname_char('-').

%! parse_ipv6(+IpString, -Ip) is semidet.
%
%  Parse IPv6 address string.
%  Ip is ipv6(list of 8 16-bit values).
%
%  @arg IpString IPv6 address string
%  @arg Ip ipv6/1 structure with 8-element list
parse_ipv6(IpString, ipv6(Groups)) :-
    atom_string(IpString, IpStr),
    (   sub_string(IpStr, _, _, _, "::")
    ->  parse_ipv6_compressed(IpStr, Groups)
    ;   split_string(IpStr, ":", "", Parts),
        length(Parts, 8),
        maplist(parse_hex_group, Parts, Groups)
    ).

parse_ipv6_compressed(IpStr, Groups) :-
    split_string(IpStr, "::", "", [LeftStr, RightStr]),
    (   LeftStr = "" -> LeftGroups = [] ; split_string(LeftStr, ":", "", LeftParts), maplist(parse_hex_group, LeftParts, LeftGroups) ),
    (   RightStr = "" -> RightGroups = [] ; split_string(RightStr, ":", "", RightParts), maplist(parse_hex_group, RightParts, RightGroups) ),
    length(LeftGroups, LeftLen),
    length(RightGroups, RightLen),
    ZeroCount is 8 - LeftLen - RightLen,
    ZeroCount >= 0,
    length(Zeros, ZeroCount),
    maplist(=(0), Zeros),
    append(LeftGroups, Zeros, Temp),
    append(Temp, RightGroups, Groups),
    length(Groups, 8).

parse_hex_group(Str, Value) :-
    string_length(Str, Len),
    Len > 0,
    Len =< 4,
    string_lower(Str, LowerStr),
    atom_string(Atom, LowerStr),
    atom_concat('0x', Atom, HexAtom),
    term_to_atom(Value, HexAtom),
    Value >= 0,
    Value =< 65535.

%! format_ipv6(+Ip, -String) is det.
%
%  Format IPv6 address as string.
%
%  @arg Ip ipv6/1 structure
%  @arg String IPv6 string
format_ipv6(ipv6(Groups), String) :-
    length(Groups, 8),
    maplist(format_hex_group, Groups, GroupStrs),
    atomics_to_string(GroupStrs, ":", String).

format_hex_group(Value, Str) :-
    format(string(Str), "~16r", [Value]).

%! ip_to_int(+Ip, -Int) is det.
%
%  Convert IPv4 to 32-bit integer.
%
%  @arg Ip ipv4/4 structure
%  @arg Int 32-bit integer
ip_to_int(ipv4(A, B, C, D), Int) :-
    Int is (A << 24) \/ (B << 16) \/ (C << 8) \/ D.

%! int_to_ip(+Int, -Ip) is det.
%
%  Convert 32-bit integer to IPv4.
%
%  @arg Int 32-bit integer
%  @arg Ip ipv4/4 structure
int_to_ip(Int, ipv4(A, B, C, D)) :-
    A is (Int >> 24) /\ 255,
    B is (Int >> 16) /\ 255,
    C is (Int >> 8) /\ 255,
    D is Int /\ 255.

%! parse_cidr(+CidrString, -Cidr) is semidet.
%
%  Parse CIDR notation (e.g., "192.168.1.0/24").
%
%  @arg CidrString CIDR string
%  @arg Cidr cidr(Ip, PrefixLen) structure
parse_cidr(CidrString, cidr(Ip, PrefixLen)) :-
    atom_string(CidrString, CidrStr),
    split_string(CidrStr, "/", "", [IpStr, PrefixStr]),
    atom_string(IpAtom, IpStr),
    parse_ipv4(IpAtom, Ip),
    number_string(PrefixLen, PrefixStr),
    PrefixLen >= 0,
    PrefixLen =< 32.

%! cidr_contains(+Cidr, +Ip) is semidet.
%
%  Check if IP is within CIDR range.
%
%  @arg Cidr cidr/2 structure
%  @arg Ip ipv4/4 structure
cidr_contains(cidr(NetworkIp, PrefixLen), Ip) :-
    ip_to_int(NetworkIp, NetworkInt),
    ip_to_int(Ip, IpInt),
    Mask is (0xFFFFFFFF << (32 - PrefixLen)) /\ 0xFFFFFFFF,
    NetworkMasked is NetworkInt /\ Mask,
    IpMasked is IpInt /\ Mask,
    NetworkMasked =:= IpMasked.
