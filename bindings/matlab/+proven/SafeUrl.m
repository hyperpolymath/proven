% SPDX-License-Identifier: PMPL-1.0-or-later
% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%
% SafeUrl - URL encoding/decoding via libproven FFI.
%
% All computation is performed in the Idris 2 core via the Zig FFI bridge.
% This class is a thin wrapper; it does NOT reimplement any logic.

classdef SafeUrl
    % SafeUrl  Formally verified URL operations.
    %
    % Example:
    %   proven.LibProven.init();
    %   encoded = proven.SafeUrl.encode('hello world');
    %   % encoded == 'hello%20world'

    methods (Static)

        function result = encode(str)
            % ENCODE  URL-encode a string (RFC 3986 percent encoding).
            %
            %   encoded = proven.SafeUrl.encode(str)
            %
            % Unreserved characters (A-Za-z0-9-._~) pass through; all
            % others become %XX.
            %
            % Returns encoded char vector or [] on error.
            proven.LibProven.ensureLoaded();
            data = uint8(str);
            r = calllib(proven.LibProven.libName(), ...
                        'proven_http_url_encode', data, numel(data));
            if r.status == 0 && r.length > 0
                result = char(r.value(1:r.length));
                proven.LibProven.freeString(r.value);
            elseif r.status == 0
                result = '';
            else
                result = [];
            end
        end

        function result = decode(str)
            % DECODE  URL-decode a percent-encoded string.
            %
            %   decoded = proven.SafeUrl.decode(encoded)
            %
            % Returns decoded char vector or [] on error.
            proven.LibProven.ensureLoaded();
            data = uint8(str);
            r = calllib(proven.LibProven.libName(), ...
                        'proven_http_url_decode', data, numel(data));
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
