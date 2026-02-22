%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafeNetwork - FFI bindings to libproven network operations.
%% All IP parsing and classification is performed in verified Idris 2
%% code via libproven.

:- module(proven_network, [
    parse_ipv4/2,
    is_loopback/1,
    is_private_ip/1,
    is_public_ip/1,
    classify_ip/2,
    is_valid_port/1,
    is_privileged_port/1
]).

:- use_foreign_library(foreign(libproven)).

%% FFI declarations
:- foreign(proven_network_parse_ipv4_ffi, c,
           proven_network_parse_ipv4(+string, +integer, [-integer],
                                     [-integer], [-integer], [-integer], [-integer])).
:- foreign(proven_network_ipv4_is_private_ffi, c,
           proven_network_ipv4_is_private(+integer, +integer, +integer, +integer, [-integer])).
:- foreign(proven_network_ipv4_is_loopback_ffi, c,
           proven_network_ipv4_is_loopback(+integer, +integer, +integer, +integer, [-integer])).

%! parse_ipv4(+IpString, -Ip) is det.
%
%  Parse IPv4 address string via libproven.
%  Ip is ipv4(A, B, C, D) or invalid.
parse_ipv4(IpString, Ip) :-
    atom_string(IpString, IpStr),
    atom_length(IpString, Len),
    proven_network_parse_ipv4_ffi(IpStr, Len, Status, A, B, C, D),
    (   Status =:= 0
    ->  Ip = ipv4(A, B, C, D)
    ;   Ip = invalid
    ).

%! is_loopback(+Ip) is semidet.
%
%  Check if IPv4 address is loopback via libproven.
is_loopback(ipv4(A, B, C, D)) :-
    proven_network_ipv4_is_loopback_ffi(A, B, C, D, Result),
    Result =:= 1.

%! is_private_ip(+Ip) is semidet.
%
%  Check if IPv4 address is private via libproven.
is_private_ip(ipv4(A, B, C, D)) :-
    proven_network_ipv4_is_private_ffi(A, B, C, D, Result),
    Result =:= 1.

%! is_public_ip(+Ip) is semidet.
%
%  Check if IPv4 address is public (not loopback, private, or reserved).
is_public_ip(Ip) :-
    Ip \= invalid,
    \+ is_loopback(Ip),
    \+ is_private_ip(Ip).

%! classify_ip(+Ip, -Class) is det.
%
%  Classify an IPv4 address via libproven.
classify_ip(invalid, invalid) :- !.
classify_ip(Ip, loopback) :- is_loopback(Ip), !.
classify_ip(Ip, private) :- is_private_ip(Ip), !.
classify_ip(_, public).

%! is_valid_port(+Port) is semidet.
%
%  Check if Port is a valid port number (1-65535).
is_valid_port(Port) :-
    integer(Port),
    Port >= 1,
    Port =< 65535.

%! is_privileged_port(+Port) is semidet.
%
%  Check if Port is a privileged port (1-1023).
is_privileged_port(Port) :-
    is_valid_port(Port),
    Port =< 1023.
