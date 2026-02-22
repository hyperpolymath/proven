% SPDX-License-Identifier: PMPL-1.0-or-later
% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%
% SafeCrypto - Cryptographic primitives via libproven FFI.
%
% All computation is performed in the Idris 2 core via the Zig FFI bridge.
% This class is a thin wrapper; it does NOT reimplement any logic.

classdef SafeCrypto
    % SafeCrypto  Formally verified cryptographic operations.
    %
    % Example:
    %   proven.LibProven.init();
    %   tf = proven.SafeCrypto.constantTimeEq('secret', 'secret');
    %   % tf == true
    %   bytes = proven.SafeCrypto.randomBytes(32);
    %   % bytes is a 1x32 uint8 array

    methods (Static)

        function result = constantTimeEq(a, b)
            % CONSTANTTIMEEQ  Constant-time byte comparison (timing-attack safe).
            %
            %   tf = proven.SafeCrypto.constantTimeEq(a, b)
            %
            % Parameters:
            %   a - character vector or uint8 array.
            %   b - character vector or uint8 array.
            %
            % Returns logical true/false or [] on error.
            proven.LibProven.ensureLoaded();
            dataA = uint8(a);
            dataB = uint8(b);
            r = calllib(proven.LibProven.libName(), ...
                        'proven_crypto_constant_time_eq', ...
                        dataA, numel(dataA), dataB, numel(dataB));
            if r.status == 0
                result = logical(r.value);
            else
                result = [];
            end
        end

        function result = randomBytes(n)
            % RANDOMBYTES  Generate cryptographically secure random bytes.
            %
            %   bytes = proven.SafeCrypto.randomBytes(n)
            %
            % Parameters:
            %   n - number of bytes to generate (positive integer).
            %
            % Returns a 1xN uint8 array or [] on error.
            proven.LibProven.ensureLoaded();
            buf = zeros(1, n, 'uint8');
            status = calllib(proven.LibProven.libName(), ...
                             'proven_crypto_random_bytes', buf, uint64(n));
            if status == 0
                result = buf;
            else
                result = [];
            end
        end

        function result = hexEncode(data, uppercase)
            % HEXENCODE  Encode bytes to hexadecimal string.
            %
            %   hex = proven.SafeCrypto.hexEncode(data)
            %   hex = proven.SafeCrypto.hexEncode(data, true)
            %
            % Parameters:
            %   data      - uint8 array to encode.
            %   uppercase - logical, use uppercase hex digits (default false).
            %
            % Returns hex char vector or [] on error.
            if nargin < 2
                uppercase = false;
            end
            proven.LibProven.ensureLoaded();
            data = uint8(data);
            r = calllib(proven.LibProven.libName(), ...
                        'proven_hex_encode', ...
                        data, numel(data), int32(uppercase));
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
