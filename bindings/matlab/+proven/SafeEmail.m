% SPDX-License-Identifier: PMPL-1.0-or-later
% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%
% SafeEmail - Email validation via libproven FFI.
%
% All computation is performed in the Idris 2 core via the Zig FFI bridge.
% This class is a thin wrapper; it does NOT reimplement any logic.

classdef SafeEmail
    % SafeEmail  Formally verified email address validation.
    %
    % Example:
    %   proven.LibProven.init();
    %   tf = proven.SafeEmail.isValid('user@example.com');
    %   % tf == true

    methods (Static)

        function result = isValid(email)
            % ISVALID  Validate email address (RFC 5321 simplified).
            %
            %   tf = proven.SafeEmail.isValid(email)
            %
            % Parameters:
            %   email - character vector containing the email address.
            %
            % Returns logical true/false or [] on error.
            proven.LibProven.ensureLoaded();
            data = uint8(email);
            r = calllib(proven.LibProven.libName(), ...
                        'proven_email_is_valid', data, numel(data));
            if r.status == 0
                result = logical(r.value);
            else
                result = [];
            end
        end

    end
end
