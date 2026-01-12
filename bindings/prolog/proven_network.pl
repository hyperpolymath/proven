%% SPDX-License-Identifier: PMPL-1.0
%% SPDX-FileCopyrightText: 2025 Hyperpolymath
%%
%% Proven SafeNetwork - Network validation for Prolog

:- module(proven_network, [
    parse_ipv4/2,
    format_ipv4/2,
    is_loopback/1,
    is_private_ip/1,
    is_reserved_ip/1,
    is_public_ip/1,
    classify_ip/2,
    is_valid_port/1,
    is_privileged_port/1
]).

:- use_module(library(lists)).

%% parse_ipv4(+IpString, -Ip)
%% Ip is ipv4(A, B, C, D) or invalid
parse_ipv4(IpString, Ip) :-
    atom_string(IpString, IpStr),
    split_string(IpStr, ".", "", Parts),
    Parts = [A, B, C, D],
    maplist(parse_octet, Parts, [Oa, Ob, Oc, Od]),
    !,
    Ip = ipv4(Oa, Ob, Oc, Od).
parse_ipv4(_, invalid).

parse_octet(Str, Octet) :-
    number_string(Octet, Str),
    Octet >= 0,
    Octet =< 255.

%% format_ipv4(+Ip, -String)
%% Format IPv4 address as dotted-decimal string
format_ipv4(ipv4(A, B, C, D), String) :-
    format(atom(String), "~d.~d.~d.~d", [A, B, C, D]).
format_ipv4(invalid, error).

%% is_loopback(+Ip)
%% Succeeds if Ip is a loopback address (127.x.x.x)
is_loopback(ipv4(127, _, _, _)).

%% is_private_ip(+Ip)
%% Succeeds if Ip is a private address
is_private_ip(ipv4(10, _, _, _)) :- !.
is_private_ip(ipv4(172, B, _, _)) :- B >= 16, B =< 31, !.
is_private_ip(ipv4(192, 168, _, _)).

%% is_reserved_ip(+Ip)
%% Succeeds if Ip is a reserved address
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

%% is_public_ip(+Ip)
%% Succeeds if Ip is a public address
is_public_ip(Ip) :-
    Ip \= invalid,
    \+ is_loopback(Ip),
    \+ is_private_ip(Ip),
    \+ is_reserved_ip(Ip).

%% classify_ip(+Ip, -Class)
%% Class is one of: invalid, loopback, private, reserved, public
classify_ip(invalid, invalid) :- !.
classify_ip(Ip, loopback) :- is_loopback(Ip), !.
classify_ip(Ip, private) :- is_private_ip(Ip), !.
classify_ip(Ip, reserved) :- is_reserved_ip(Ip), !.
classify_ip(_, public).

%% is_valid_port(+Port)
%% Succeeds if Port is a valid port number (1-65535)
is_valid_port(Port) :-
    integer(Port),
    Port >= 1,
    Port =< 65535.

%% is_privileged_port(+Port)
%% Succeeds if Port is a privileged port (1-1023)
is_privileged_port(Port) :-
    is_valid_port(Port),
    Port =< 1023.
