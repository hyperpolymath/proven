%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafeJSON - FFI bindings to libproven JSON validation.
%% All JSON validation is performed in verified Idris 2 code via libproven.

:- module(safe_json, [
    is_valid_json/1,
    json_type/2
]).

:- use_foreign_library(foreign(libproven)).

%% FFI declarations for libproven JSON functions.
:- foreign(proven_json_is_valid_ffi, c,
           proven_json_is_valid(+string, +integer, [-integer], [-integer])).
:- foreign(proven_json_get_type_ffi, c,
           proven_json_get_type(+string, +integer, [-integer])).

%! is_valid_json(+JsonString) is semidet.
%
%  Check if string is valid JSON via libproven.
is_valid_json(JsonString) :-
    atom_string(JsonString, Str),
    atom_length(JsonString, Len),
    proven_json_is_valid_ffi(Str, Len, Status, Value),
    Status =:= 0,
    Value =:= 1.

%! json_type(+JsonString, -Type) is det.
%
%  Get JSON value type at root level via libproven.
%  Type is one of: null, boolean, number, string, array, object, invalid.
json_type(JsonString, Type) :-
    atom_string(JsonString, Str),
    atom_length(JsonString, Len),
    proven_json_get_type_ffi(Str, Len, TypeCode),
    type_code_atom(TypeCode, Type).

type_code_atom(0, null).
type_code_atom(1, boolean).
type_code_atom(2, number).
type_code_atom(3, string).
type_code_atom(4, array).
type_code_atom(5, object).
type_code_atom(-1, invalid).
type_code_atom(_, invalid).
