%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafePath - Extended FFI bindings to libproven path operations.
%% All path operations are performed in verified Idris 2 code via libproven.

:- module(safe_path, [
    has_traversal/1,
    sanitize_filename/2,
    safe_path_join/3
]).

:- use_module(proven_path, [
    has_traversal/1 as pp_has_traversal,
    sanitize_filename/2 as pp_sanitize_filename,
    safe_path_join/3 as pp_safe_path_join
]).

%! has_traversal(+Path) is semidet.
%
%  Succeeds if Path contains directory traversal sequences.
%  Detection performed by libproven.
has_traversal(Path) :-
    pp_has_traversal(Path).

%! sanitize_filename(+Input, -Output) is det.
%
%  Sanitize a filename by removing dangerous characters via libproven.
sanitize_filename(Input, Output) :-
    pp_sanitize_filename(Input, Output).

%! safe_path_join(+Base, +Filename, -Result) is det.
%
%  Safely join base path and filename via libproven.
%  Result is ok(Path) or error(traversal_detected).
safe_path_join(Base, Filename, Result) :-
    pp_safe_path_join(Base, Filename, Result).
