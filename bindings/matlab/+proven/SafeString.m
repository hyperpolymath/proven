% SPDX-License-Identifier: PMPL-1.0-or-later
% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%
% SafeString - Safe string operations via libproven FFI.
%
% All computation is performed in the Idris 2 core via the Zig FFI bridge.
% This class is a thin wrapper; it does NOT reimplement any logic.

classdef SafeString
    % SafeString  Formally verified text operations that handle encoding safely.
    %
    % Methods return [] on error, or a char/logical value on success.
    %
    % Example:
    %   proven.LibProven.init();
    %   html = proven.SafeString.escapeHtml('<script>alert(1)</script>');
    %   % html == '&lt;script&gt;alert(1)&lt;/script&gt;'

    methods (Static)

        function result = isValidUtf8(str)
            % ISVALIDUTF8  Check if bytes are valid UTF-8.
            %
            %   tf = proven.SafeString.isValidUtf8(str)
            %
            % Parameters:
            %   str - character vector or uint8 array.
            %
            % Returns logical true/false or [] on error.
            proven.LibProven.ensureLoaded();
            data = uint8(str);
            r = calllib(proven.LibProven.libName(), ...
                        'proven_string_is_valid_utf8', data, numel(data));
            if r.status == 0
                result = logical(r.value);
            else
                result = [];
            end
        end

        function result = escapeSql(str)
            % ESCAPESQL  Escape string for SQL (single quotes).
            %
            %   escaped = proven.SafeString.escapeSql(str)
            %
            % Returns escaped char vector or [] on error.
            % Note: Prefer parameterized queries over string escaping.
            proven.LibProven.ensureLoaded();
            data = uint8(str);
            r = calllib(proven.LibProven.libName(), ...
                        'proven_string_escape_sql', data, numel(data));
            if r.status == 0 && r.length > 0
                result = char(r.value(1:r.length));
                proven.LibProven.freeString(r.value);
            elseif r.status == 0
                result = '';
            else
                result = [];
            end
        end

        function result = escapeHtml(str)
            % ESCAPEHTML  Escape string for HTML (prevents XSS).
            %
            %   escaped = proven.SafeString.escapeHtml(str)
            %
            % Returns escaped char vector or [] on error.
            proven.LibProven.ensureLoaded();
            data = uint8(str);
            r = calllib(proven.LibProven.libName(), ...
                        'proven_string_escape_html', data, numel(data));
            if r.status == 0 && r.length > 0
                result = char(r.value(1:r.length));
                proven.LibProven.freeString(r.value);
            elseif r.status == 0
                result = '';
            else
                result = [];
            end
        end

        function result = escapeJs(str)
            % ESCAPEJS  Escape string for JavaScript string literals.
            %
            %   escaped = proven.SafeString.escapeJs(str)
            %
            % Returns escaped char vector or [] on error.
            proven.LibProven.ensureLoaded();
            data = uint8(str);
            r = calllib(proven.LibProven.libName(), ...
                        'proven_string_escape_js', data, numel(data));
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
