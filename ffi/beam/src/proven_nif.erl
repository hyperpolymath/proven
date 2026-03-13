%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
%%
%% Erlang NIF loader for proven_nif
%%
%% This module loads the Zig-compiled NIF shared library and provides
%% stub functions that get replaced by native implementations at load time.

-module(proven_nif).
-export([init/0]).

-on_load(init/0).

init() ->
    PrivDir = case code:priv_dir(proven) of
        {error, bad_name} ->
            %% Fallback: look relative to beam file location
            BeamDir = filename:dirname(code:which(?MODULE)),
            filename:join(BeamDir, "../priv");
        Dir -> Dir
    end,
    NifPath = filename:join(PrivDir, "libproven_nif"),
    erlang:load_nif(NifPath, 0).
