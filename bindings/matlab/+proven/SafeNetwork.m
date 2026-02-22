% SPDX-License-Identifier: PMPL-1.0-or-later
% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%
% SafeNetwork - IP address parsing and classification via libproven FFI.
%
% All computation is performed in the Idris 2 core via the Zig FFI bridge.
% This class is a thin wrapper; it does NOT reimplement any logic.

classdef SafeNetwork
    % SafeNetwork  Formally verified network address operations.
    %
    % Example:
    %   proven.LibProven.init();
    %   tf = proven.SafeNetwork.validateIpv4('192.168.1.1');
    %   % tf == true

    methods (Static)

        function result = validateIpv4(ipStr)
            % VALIDATEIPV4  Check if string is a valid IPv4 address.
            %
            %   tf = proven.SafeNetwork.validateIpv4(ipStr)
            %
            % Parameters:
            %   ipStr - character vector (e.g. '192.168.1.1').
            %
            % Returns logical true/false or [] on error.
            proven.LibProven.ensureLoaded();
            data = uint8(ipStr);
            r = calllib(proven.LibProven.libName(), ...
                        'proven_network_parse_ipv4', data, numel(data));
            if r.status == 0
                result = true;
            elseif r.status < 0
                % Parse failure means invalid IPv4, not an internal error.
                result = false;
            else
                result = [];
            end
        end

        function result = isPrivate(ipStr)
            % ISPRIVATE  Check if IPv4 address is private (RFC 1918).
            %
            %   tf = proven.SafeNetwork.isPrivate(ipStr)
            %
            % Returns logical true if 10.x.x.x, 172.16-31.x.x,
            % or 192.168.x.x; false otherwise; [] on parse error.
            proven.LibProven.ensureLoaded();
            data = uint8(ipStr);
            % First parse the IPv4 to validate it.
            r = calllib(proven.LibProven.libName(), ...
                        'proven_network_parse_ipv4', data, numel(data));
            if r.status ~= 0
                result = [];
                return;
            end
            % Extract octets from packed int64 result.
            % The address is returned as a struct with octets packed
            % into the value field.
            packed = typecast(int64(r.value), 'uint8');
            o1 = packed(1); o2 = packed(2); o3 = packed(3); o4 = packed(4);
            rv = calllib(proven.LibProven.libName(), ...
                         'proven_network_ipv4_is_private', ...
                         o1, o2, o3, o4);
            result = logical(rv);
        end

        function result = isLoopback(ipStr)
            % ISLOOPBACK  Check if IPv4 address is loopback (127.0.0.0/8).
            %
            %   tf = proven.SafeNetwork.isLoopback(ipStr)
            %
            % Returns logical true/false or [] on parse error.
            proven.LibProven.ensureLoaded();
            data = uint8(ipStr);
            r = calllib(proven.LibProven.libName(), ...
                        'proven_network_parse_ipv4', data, numel(data));
            if r.status ~= 0
                result = [];
                return;
            end
            packed = typecast(int64(r.value), 'uint8');
            o1 = packed(1); o2 = packed(2); o3 = packed(3); o4 = packed(4);
            rv = calllib(proven.LibProven.libName(), ...
                         'proven_network_ipv4_is_loopback', ...
                         o1, o2, o3, o4);
            result = logical(rv);
        end

    end
end
