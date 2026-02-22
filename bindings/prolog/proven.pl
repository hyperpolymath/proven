%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven - FFI bindings to libproven for SWI-Prolog
%%
%% All computation is performed in verified Idris 2 code via the Zig FFI
%% bridge (libproven). These bindings are thin wrappers that marshal
%% data to/from the C ABI. Do NOT reimplement any logic in Prolog.

:- module(proven, []).

%% Load the shared library
:- use_foreign_library(foreign(libproven)).

%% Load all submodules (each wraps a portion of the libproven API)
:- use_module(proven_math).
:- use_module(proven_string).
:- use_module(proven_path).
:- use_module(proven_email).
:- use_module(proven_network).
:- use_module(proven_crypto).
:- use_module(safe_math).
:- use_module(safe_string).
:- use_module(safe_email).
:- use_module(safe_float).
:- use_module(safe_json).
:- use_module(safe_uuid).
:- use_module(safe_currency).
:- use_module(safe_phone).
:- use_module(safe_hex).
:- use_module(safe_color).
:- use_module(safe_crypto).
:- use_module(safe_datetime).
:- use_module(safe_url).
:- use_module(safe_version).
:- use_module(safe_network).
:- use_module(safe_path).

%% Re-export all predicates
:- reexport(proven_math).
:- reexport(proven_string).
:- reexport(proven_path).
:- reexport(proven_email).
:- reexport(proven_network).
:- reexport(proven_crypto).
:- reexport(safe_math).
:- reexport(safe_string).
:- reexport(safe_email).
:- reexport(safe_float).
:- reexport(safe_json).
:- reexport(safe_uuid).
:- reexport(safe_currency).
:- reexport(safe_phone).
:- reexport(safe_hex).
:- reexport(safe_color).
:- reexport(safe_crypto).
:- reexport(safe_datetime).
:- reexport(safe_url).
:- reexport(safe_version).
:- reexport(safe_network).
:- reexport(safe_path).

%% Lifecycle management
:- initialization(proven_init_runtime).

proven_init_runtime :-
    proven_init(Status),
    (   Status =:= 0
    ->  true
    ;   print_message(error, format('Failed to initialize libproven: ~w', [Status]))
    ).

%% FFI declarations for lifecycle
:- foreign(proven_init, c, proven_init([-integer])).
:- foreign(proven_deinit, c, proven_deinit).
