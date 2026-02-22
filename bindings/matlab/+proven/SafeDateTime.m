% SPDX-License-Identifier: PMPL-1.0-or-later
% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%
% SafeDateTime - Date/time operations via libproven FFI.
%
% All computation is performed in the Idris 2 core via the Zig FFI bridge.
% This class is a thin wrapper; it does NOT reimplement any logic.

classdef SafeDateTime
    % SafeDateTime  Formally verified date/time operations.
    %
    % Example:
    %   proven.LibProven.init();
    %   tf = proven.SafeDateTime.isLeapYear(2024);
    %   % tf == true
    %   d  = proven.SafeDateTime.daysInMonth(2024, 2);
    %   % d  == 29

    methods (Static)

        function result = isLeapYear(year)
            % ISLEAPYEAR  Check if year is a leap year.
            %
            %   tf = proven.SafeDateTime.isLeapYear(year)
            %
            % Parameters:
            %   year - integer year.
            %
            % Returns logical true/false.
            proven.LibProven.ensureLoaded();
            rv = calllib(proven.LibProven.libName(), ...
                         'proven_datetime_is_leap_year', int32(year));
            result = logical(rv);
        end

        function result = daysInMonth(year, month)
            % DAYSINMONTH  Get number of days in a month.
            %
            %   d = proven.SafeDateTime.daysInMonth(year, month)
            %
            % Parameters:
            %   year  - integer year.
            %   month - integer month (1-12).
            %
            % Returns uint8 day count.  Returns 0 for invalid months.
            proven.LibProven.ensureLoaded();
            result = double(calllib(proven.LibProven.libName(), ...
                                    'proven_datetime_days_in_month', ...
                                    int32(year), uint8(month)));
        end

        function result = evalExpression(expr)
            % EVALEXPRESSION  Evaluate an arithmetic expression safely.
            %
            %   val = proven.SafeDateTime.evalExpression('2+3*4')
            %
            % This is a convenience wrapper around the SafeCalculator
            % which is included here because MATLAB users may combine
            % date arithmetic with expression evaluation.
            %
            % Returns double value or [] on parse/division error.
            proven.LibProven.ensureLoaded();
            data = uint8(expr);
            r = calllib(proven.LibProven.libName(), ...
                        'proven_calculator_eval', data, numel(data));
            if r.status == 0
                result = r.value;
            else
                result = [];
            end
        end

    end
end
