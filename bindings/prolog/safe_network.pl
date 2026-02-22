%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafeNetwork - Extended FFI bindings to libproven network operations.
%% All network operations are performed in verified Idris 2 code via libproven.

:- module(safe_network, [
    parse_ipv4/2,
    is_loopback/1,
    is_private_ip/1,
    is_public_ip/1,
    classify_ip/2,
    is_valid_port/1,
    is_privileged_port/1
]).

:- use_module(proven_network, [
    parse_ipv4/2 as pn_parse_ipv4,
    is_loopback/1 as pn_is_loopback,
    is_private_ip/1 as pn_is_private_ip,
    is_public_ip/1 as pn_is_public_ip,
    classify_ip/2 as pn_classify_ip,
    is_valid_port/1 as pn_is_valid_port,
    is_privileged_port/1 as pn_is_privileged_port
]).

%! parse_ipv4(+IpString, -Ip) is det.
%
%  Parse IPv4 address string via libproven.
parse_ipv4(IpString, Ip) :-
    pn_parse_ipv4(IpString, Ip).

%! is_loopback(+Ip) is semidet.
%
%  Check if IPv4 address is loopback via libproven.
is_loopback(Ip) :-
    pn_is_loopback(Ip).

%! is_private_ip(+Ip) is semidet.
%
%  Check if IPv4 address is private via libproven.
is_private_ip(Ip) :-
    pn_is_private_ip(Ip).

%! is_public_ip(+Ip) is semidet.
%
%  Check if IPv4 address is public via libproven.
is_public_ip(Ip) :-
    pn_is_public_ip(Ip).

%! classify_ip(+Ip, -Class) is det.
%
%  Classify an IPv4 address via libproven.
classify_ip(Ip, Class) :-
    pn_classify_ip(Ip, Class).

%! is_valid_port(+Port) is semidet.
%
%  Check if port number is valid (1-65535).
%  Uses proven_network for validation.
is_valid_port(Port) :-
    pn_is_valid_port(Port).

%! is_privileged_port(+Port) is semidet.
%
%  Check if port is a privileged port (1-1023).
is_privileged_port(Port) :-
    pn_is_privileged_port(Port).
