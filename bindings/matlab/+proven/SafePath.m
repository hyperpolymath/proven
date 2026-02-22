% SPDX-License-Identifier: PMPL-1.0-or-later
% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%
% SafePath - Filesystem traversal prevention via libproven FFI.
%
% All computation is performed in the Idris 2 core via the Zig FFI bridge.
% This class is a thin wrapper; it does NOT reimplement any logic.

classdef SafePath
    % SafePath  Formally verified path safety operations.
    %
    % Methods return [] on error, or a char/logical value on success.
    %
    % Example:
    %   proven.LibProven.init();
    %   tf = proven.SafePath.hasTraversal('../../etc/passwd');
    %   % tf == true

    methods (Static)

        function result = hasTraversal(path)
            % HASTRAVERSAL  Check if path contains directory traversal.
            %
            %   tf = proven.SafePath.hasTraversal(path)
            %
            % Parameters:
            %   path - character vector.
            %
            % Returns logical true if ".." traversal detected, false if
            % safe, or [] on error.
            proven.LibProven.ensureLoaded();
            data = uint8(path);
            r = calllib(proven.LibProven.libName(), ...
                        'proven_path_has_traversal', data, numel(data));
            if r.status == 0
                result = logical(r.value);
            else
                result = [];
            end
        end

        function result = sanitizeFilename(filename)
            % SANITIZEFILENAME  Remove dangerous characters from filename.
            %
            %   safe = proven.SafePath.sanitizeFilename(filename)
            %
            % Returns sanitized char vector or [] on error.
            proven.LibProven.ensureLoaded();
            data = uint8(filename);
            r = calllib(proven.LibProven.libName(), ...
                        'proven_path_sanitize_filename', data, numel(data));
            if r.status == 0 && r.length > 0
                result = char(r.value(1:r.length));
                proven.LibProven.freeString(r.value);
            elseif r.status == 0
                result = '';
            else
                result = [];
            end
        end

    end
end
