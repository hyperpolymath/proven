% SPDX-License-Identifier: PMPL-1.0-or-later
% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%
% SafeMath - Overflow-safe integer and float arithmetic via libproven FFI.
%
% All computation is performed in the Idris 2 core via the Zig FFI bridge.
% This class is a thin wrapper; it does NOT reimplement any logic.

classdef SafeMath
    % SafeMath  Formally verified arithmetic that cannot crash.
    %
    % Every method returns [] (empty) on error, or the numeric result on
    % success.  This is the idiomatic MATLAB/Octave way to signal failure
    % without exceptions.
    %
    % Example:
    %   proven.LibProven.init();
    %   result = proven.SafeMath.add(int64(2), int64(3));
    %   % result == 5
    %   result = proven.SafeMath.div(int64(10), int64(0));
    %   % result == []  (division by zero)

    methods (Static)

        function result = add(a, b)
            % ADD  Checked addition with overflow detection.
            %
            %   result = proven.SafeMath.add(int64(a), int64(b))
            %
            % Returns int64 value or [] on overflow.
            proven.LibProven.ensureLoaded();
            r = calllib(proven.LibProven.libName(), ...
                        'proven_math_add_checked', int64(a), int64(b));
            if r.status == 0
                result = r.value;
            else
                result = [];
            end
        end

        function result = sub(a, b)
            % SUB  Checked subtraction with underflow detection.
            %
            %   result = proven.SafeMath.sub(int64(a), int64(b))
            %
            % Returns int64 value or [] on underflow.
            proven.LibProven.ensureLoaded();
            r = calllib(proven.LibProven.libName(), ...
                        'proven_math_sub_checked', int64(a), int64(b));
            if r.status == 0
                result = r.value;
            else
                result = [];
            end
        end

        function result = mul(a, b)
            % MUL  Checked multiplication with overflow detection.
            %
            %   result = proven.SafeMath.mul(int64(a), int64(b))
            %
            % Returns int64 value or [] on overflow.
            proven.LibProven.ensureLoaded();
            r = calllib(proven.LibProven.libName(), ...
                        'proven_math_mul_checked', int64(a), int64(b));
            if r.status == 0
                result = r.value;
            else
                result = [];
            end
        end

        function result = div(a, b)
            % DIV  Safe integer division.
            %
            %   result = proven.SafeMath.div(int64(a), int64(b))
            %
            % Returns int64 quotient or [] on division by zero / overflow.
            proven.LibProven.ensureLoaded();
            r = calllib(proven.LibProven.libName(), ...
                        'proven_math_div', int64(a), int64(b));
            if r.status == 0
                result = r.value;
            else
                result = [];
            end
        end

        function result = mod(a, b)
            % MOD  Safe modulo operation.
            %
            %   result = proven.SafeMath.mod(int64(a), int64(b))
            %
            % Returns int64 remainder or [] on division by zero.
            proven.LibProven.ensureLoaded();
            r = calllib(proven.LibProven.libName(), ...
                        'proven_math_mod', int64(a), int64(b));
            if r.status == 0
                result = r.value;
            else
                result = [];
            end
        end

        function result = abs(n)
            % ABS  Safe absolute value.
            %
            %   result = proven.SafeMath.abs(int64(n))
            %
            % Returns int64 absolute value or [] for INT64_MIN.
            proven.LibProven.ensureLoaded();
            r = calllib(proven.LibProven.libName(), ...
                        'proven_math_abs_safe', int64(n));
            if r.status == 0
                result = r.value;
            else
                result = [];
            end
        end

        function result = clamp(lo, hi, value)
            % CLAMP  Clamp value to [lo, hi] range.
            %
            %   result = proven.SafeMath.clamp(int64(lo), int64(hi), int64(value))
            %
            % Always returns a valid int64 (no failure case).
            proven.LibProven.ensureLoaded();
            result = calllib(proven.LibProven.libName(), ...
                             'proven_math_clamp', ...
                             int64(lo), int64(hi), int64(value));
        end

        function result = pow(base, exp)
            % POW  Integer exponentiation with overflow checking.
            %
            %   result = proven.SafeMath.pow(int64(base), uint32(exp))
            %
            % Returns int64 value or [] on overflow.
            proven.LibProven.ensureLoaded();
            r = calllib(proven.LibProven.libName(), ...
                        'proven_math_pow_checked', ...
                        int64(base), uint32(exp));
            if r.status == 0
                result = r.value;
            else
                result = [];
            end
        end

        function result = negate(n)
            % NEGATE  Safe negation (delegates to sub(0, n)).
            %
            %   result = proven.SafeMath.negate(int64(n))
            %
            % Returns int64 value or [] on overflow (INT64_MIN).
            result = proven.SafeMath.sub(int64(0), int64(n));
        end

    end
end
