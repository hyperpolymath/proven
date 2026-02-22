% SPDX-License-Identifier: PMPL-1.0-or-later
% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%
% LibProven - Low-level library loader for libproven.
%
% This class manages the loadlibrary/unloadlibrary lifecycle and provides
% the foundational FFI calling interface.  All computation is performed in
% the Idris 2 core via the Zig FFI bridge; this file is a thin wrapper.
%
% Usage:
%   proven.LibProven.load();          % Load shared library
%   proven.LibProven.init();          % Initialize runtime
%   % ... use SafeMath, SafeString, etc. ...
%   proven.LibProven.deinit();        % Shut down runtime
%   proven.LibProven.unload();        % Unload shared library

classdef LibProven
    % LibProven  Low-level FFI loader for the Proven safety library.
    %
    % All functions are static; no instance is required.

    properties (Constant, Access = private)
        % Name of the shared library (without extension).
        LIB_NAME = 'proven';
        % Name of the C header file for loadlibrary declarations.
        HEADER   = 'proven_header.h';
    end

    methods (Static)

        function load()
            % LOAD  Load libproven shared library via loadlibrary.
            %
            %   proven.LibProven.load()
            %
            % The header file proven_header.h must be on the MATLAB path
            % or in the current directory.  The shared library (libproven.so
            % on Linux, libproven.dylib on macOS, proven.dll on Windows)
            % must be findable via the system library path.
            if ~libisloaded(proven.LibProven.LIB_NAME)
                headerPath = which(proven.LibProven.HEADER);
                if isempty(headerPath)
                    % Fall back to same directory as this file.
                    thisDir = fileparts(mfilename('fullpath'));
                    headerPath = fullfile(thisDir, '..', proven.LibProven.HEADER);
                end
                loadlibrary(proven.LibProven.LIB_NAME, headerPath);
            end
        end

        function unload()
            % UNLOAD  Unload libproven shared library.
            %
            %   proven.LibProven.unload()
            if libisloaded(proven.LibProven.LIB_NAME)
                unloadlibrary(proven.LibProven.LIB_NAME);
            end
        end

        function ensureLoaded()
            % ENSURELOADED  Load library if not already loaded.
            %
            %   proven.LibProven.ensureLoaded()
            if ~libisloaded(proven.LibProven.LIB_NAME)
                proven.LibProven.load();
            end
        end

        function status = init()
            % INIT  Initialize the Proven runtime.
            %
            %   status = proven.LibProven.init()
            %
            % Returns:
            %   status - int32, 0 on success, negative on error.
            proven.LibProven.ensureLoaded();
            status = calllib(proven.LibProven.LIB_NAME, 'proven_init');
        end

        function deinit()
            % DEINIT  Shut down the Proven runtime.
            %
            %   proven.LibProven.deinit()
            proven.LibProven.ensureLoaded();
            calllib(proven.LibProven.LIB_NAME, 'proven_deinit');
        end

        function tf = isInitialized()
            % ISINITIALIZED  Check whether Proven runtime is active.
            %
            %   tf = proven.LibProven.isInitialized()
            proven.LibProven.ensureLoaded();
            tf = logical(calllib(proven.LibProven.LIB_NAME, ...
                                 'proven_is_initialized'));
        end

        function v = abiVersion()
            % ABIVERSION  Return FFI ABI version number.
            %
            %   v = proven.LibProven.abiVersion()
            proven.LibProven.ensureLoaded();
            v = calllib(proven.LibProven.LIB_NAME, 'proven_ffi_abi_version');
        end

        function v = versionMajor()
            % VERSIONMAJOR  Return major version number.
            proven.LibProven.ensureLoaded();
            v = calllib(proven.LibProven.LIB_NAME, 'proven_version_major');
        end

        function v = versionMinor()
            % VERSIONMINOR  Return minor version number.
            proven.LibProven.ensureLoaded();
            v = calllib(proven.LibProven.LIB_NAME, 'proven_version_minor');
        end

        function v = versionPatch()
            % VERSIONPATCH  Return patch version number.
            proven.LibProven.ensureLoaded();
            v = calllib(proven.LibProven.LIB_NAME, 'proven_version_patch');
        end

        function n = moduleCount()
            % MODULECOUNT  Return total module count in libproven.
            proven.LibProven.ensureLoaded();
            n = calllib(proven.LibProven.LIB_NAME, 'proven_module_count');
        end

        function freeString(ptr)
            % FREESTRING  Free a string allocated by libproven.
            %
            %   proven.LibProven.freeString(ptr)
            %
            % Parameters:
            %   ptr - libpointer returned by a StringResult.
            if ~isempty(ptr) && isa(ptr, 'lib.pointer')
                calllib(proven.LibProven.LIB_NAME, 'proven_free_string', ptr);
            end
        end

        function name = libName()
            % LIBNAME  Return the library name string.
            name = proven.LibProven.LIB_NAME;
        end

    end
end
