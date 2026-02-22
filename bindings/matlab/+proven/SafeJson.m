% SPDX-License-Identifier: PMPL-1.0-or-later
% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%
% SafeJson - JSON validation and type detection via libproven FFI.
%
% All computation is performed in the Idris 2 core via the Zig FFI bridge.
% This class is a thin wrapper; it does NOT reimplement any logic.

classdef SafeJson
    % SafeJson  Formally verified JSON operations.
    %
    % Example:
    %   proven.LibProven.init();
    %   tf = proven.SafeJson.isValid('{"key": "value"}');
    %   % tf == true
    %   t  = proven.SafeJson.getType('{"key": "value"}');
    %   % t  == 5 (PROVEN_JSON_OBJECT)

    properties (Constant)
        % JSON type constants matching ProvenJsonType enum.
        JSON_NULL    =  0;
        JSON_BOOL    =  1;
        JSON_NUMBER  =  2;
        JSON_STRING  =  3;
        JSON_ARRAY   =  4;
        JSON_OBJECT  =  5;
        JSON_INVALID = -1;
    end

    methods (Static)

        function result = isValid(jsonStr)
            % ISVALID  Check if string is valid JSON.
            %
            %   tf = proven.SafeJson.isValid(jsonStr)
            %
            % Returns logical true/false or [] on error.
            proven.LibProven.ensureLoaded();
            data = uint8(jsonStr);
            r = calllib(proven.LibProven.libName(), ...
                        'proven_json_is_valid', data, numel(data));
            if r.status == 0
                result = logical(r.value);
            else
                result = [];
            end
        end

        function result = getType(jsonStr)
            % GETTYPE  Get JSON value type at root level.
            %
            %   typeCode = proven.SafeJson.getType(jsonStr)
            %
            % Returns integer type code (see JSON_* constants) or [] on
            % error.  Returns JSON_INVALID (-1) for invalid JSON.
            proven.LibProven.ensureLoaded();
            data = uint8(jsonStr);
            result = calllib(proven.LibProven.libName(), ...
                             'proven_json_get_type', data, numel(data));
        end

        function name = typeName(typeCode)
            % TYPENAME  Convert JSON type code to human-readable name.
            %
            %   name = proven.SafeJson.typeName(typeCode)
            %
            % This is a local convenience; no FFI call is made.
            switch typeCode
                case proven.SafeJson.JSON_NULL
                    name = 'null';
                case proven.SafeJson.JSON_BOOL
                    name = 'boolean';
                case proven.SafeJson.JSON_NUMBER
                    name = 'number';
                case proven.SafeJson.JSON_STRING
                    name = 'string';
                case proven.SafeJson.JSON_ARRAY
                    name = 'array';
                case proven.SafeJson.JSON_OBJECT
                    name = 'object';
                otherwise
                    name = 'invalid';
            end
        end

    end
end
