%% SPDX-License-Identifier: Apache-2.0
%% SPDX-FileCopyrightText: 2025 Hyperpolymath
%%
%% Proven - Safe, validated operations library for Prolog
%%
%% This is the main entry point that loads all modules.

:- module(proven, []).

%% Load all submodules
:- use_module(proven_math).
:- use_module(proven_string).
:- use_module(proven_path).
:- use_module(proven_email).
:- use_module(proven_network).
:- use_module(proven_crypto).
:- use_module(safe_uuid).
:- use_module(safe_currency).
:- use_module(safe_phone).
:- use_module(safe_hex).

%% Re-export all predicates
:- reexport(proven_math).
:- reexport(proven_string).
:- reexport(proven_path).
:- reexport(proven_email).
:- reexport(proven_network).
:- reexport(proven_crypto).
:- reexport(safe_uuid).
:- reexport(safe_currency).
:- reexport(safe_phone).
:- reexport(safe_hex).
