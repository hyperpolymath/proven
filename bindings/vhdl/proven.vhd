-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
--
-- Proven Safety Library for VHDL
--
-- Formally verified safety primitives for hardware description.
-- Provides safe arithmetic, bounded types, and validation for FPGA/ASIC designs.
--
-- Compatible with:
--   - Xilinx Vivado
--   - Intel Quartus
--   - ModelSim/QuestaSim
--   - GHDL
--
-- Version: 0.9.0

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

package proven is

    -- ========================================================================
    -- CONSTANTS
    -- ========================================================================

    constant VERSION_MAJOR : natural := 0;
    constant VERSION_MINOR : natural := 9;
    constant VERSION_PATCH : natural := 0;

    -- Safe integer bounds (32-bit)
    constant SAFE_INT_MAX : integer := 2147483647;
    constant SAFE_INT_MIN : integer := -2147483648;

    -- Port range
    constant PORT_MIN : natural := 1;
    constant PORT_MAX : natural := 65535;

    -- Percentage range
    constant PERCENT_MIN : natural := 0;
    constant PERCENT_MAX : natural := 100;

    -- ========================================================================
    -- RESULT TYPE
    -- ========================================================================

    -- Error codes
    type error_code_t is (
        ERR_NONE,
        ERR_OVERFLOW,
        ERR_UNDERFLOW,
        ERR_DIVISION_BY_ZERO,
        ERR_OUT_OF_BOUNDS,
        ERR_INVALID_PORT,
        ERR_INVALID_PERCENTAGE
    );

    -- Result record for integer operations
    type result_int_t is record
        ok    : boolean;
        value : integer;
        error : error_code_t;
    end record;

    -- Result record for unsigned operations
    type result_unsigned_t is record
        ok    : boolean;
        value : unsigned(31 downto 0);
        error : error_code_t;
    end record;

    -- Result record for signed operations
    type result_signed_t is record
        ok    : boolean;
        value : signed(31 downto 0);
        error : error_code_t;
    end record;

    -- ========================================================================
    -- SAFE MATH FUNCTIONS
    -- ========================================================================

    -- Safe addition with overflow detection
    function safe_add(a, b : integer) return result_int_t;
    function safe_add(a, b : unsigned) return result_unsigned_t;
    function safe_add(a, b : signed) return result_signed_t;

    -- Safe subtraction with underflow detection
    function safe_sub(a, b : integer) return result_int_t;
    function safe_sub(a, b : unsigned) return result_unsigned_t;
    function safe_sub(a, b : signed) return result_signed_t;

    -- Safe multiplication with overflow detection
    function safe_mul(a, b : integer) return result_int_t;
    function safe_mul(a, b : unsigned) return result_unsigned_t;
    function safe_mul(a, b : signed) return result_signed_t;

    -- Safe division with zero check
    function safe_div(a, b : integer) return result_int_t;
    function safe_div(a, b : unsigned) return result_unsigned_t;
    function safe_div(a, b : signed) return result_signed_t;

    -- Safe modulo with zero check
    function safe_mod(a, b : integer) return result_int_t;
    function safe_mod(a, b : unsigned) return result_unsigned_t;

    -- ========================================================================
    -- BOUNDED VALUE FUNCTIONS
    -- ========================================================================

    -- Clamp value to range
    function clamp(value, min_val, max_val : integer) return integer;
    function clamp(value, min_val, max_val : unsigned) return unsigned;
    function clamp(value, min_val, max_val : signed) return signed;

    -- Check if value is in range
    function in_range(value, min_val, max_val : integer) return boolean;
    function in_range(value, min_val, max_val : unsigned) return boolean;
    function in_range(value, min_val, max_val : signed) return boolean;

    -- Require value in range or return error
    function require_in_range(value, min_val, max_val : integer) return result_int_t;
    function require_in_range(value, min_val, max_val : unsigned) return result_unsigned_t;

    -- ========================================================================
    -- VALIDATION FUNCTIONS
    -- ========================================================================

    -- Validate port number
    function is_valid_port(port : natural) return boolean;
    function require_valid_port(port : natural) return result_int_t;

    -- Validate percentage
    function is_valid_percentage(value : natural) return boolean;
    function require_valid_percentage(value : natural) return result_int_t;

    -- ========================================================================
    -- PERCENTAGE CALCULATIONS
    -- ========================================================================

    -- Calculate percentage (integer approximation)
    function percent_of(amount : unsigned; pct : natural) return result_unsigned_t;

    -- Calculate basis points (100 bps = 1%)
    function bps_of(amount : unsigned; bps : natural) return result_unsigned_t;

    -- ========================================================================
    -- SATURATION ARITHMETIC
    -- ========================================================================

    -- Saturating addition (clamps to max instead of overflow)
    function sat_add(a, b : unsigned; max_val : unsigned) return unsigned;
    function sat_add(a, b : signed; min_val, max_val : signed) return signed;

    -- Saturating subtraction (clamps to min instead of underflow)
    function sat_sub(a, b : unsigned) return unsigned;
    function sat_sub(a, b : signed; min_val : signed) return signed;

    -- ========================================================================
    -- BIT MANIPULATION SAFETY
    -- ========================================================================

    -- Safe bit extraction with bounds checking
    function safe_bit(vec : std_logic_vector; idx : natural) return result_int_t;

    -- Safe range extraction with bounds checking
    function safe_range(vec : std_logic_vector; high, low : natural) return std_logic_vector;

    -- ========================================================================
    -- TIMING SAFETY
    -- ========================================================================

    -- Counter with overflow protection
    function safe_increment(value : unsigned; max_val : unsigned) return result_unsigned_t;
    function safe_decrement(value : unsigned) return result_unsigned_t;

end package proven;

package body proven is

    -- ========================================================================
    -- SAFE MATH IMPLEMENTATIONS
    -- ========================================================================

    function safe_add(a, b : integer) return result_int_t is
        variable result : result_int_t;
    begin
        -- Check for overflow
        if (b > 0 and a > SAFE_INT_MAX - b) or
           (b < 0 and a < SAFE_INT_MIN - b) then
            result.ok := false;
            result.value := 0;
            result.error := ERR_OVERFLOW;
        else
            result.ok := true;
            result.value := a + b;
            result.error := ERR_NONE;
        end if;
        return result;
    end function;

    function safe_add(a, b : unsigned) return result_unsigned_t is
        variable result : result_unsigned_t;
        variable sum : unsigned(a'length downto 0);
    begin
        sum := ('0' & a) + ('0' & b);
        if sum(sum'high) = '1' then
            result.ok := false;
            result.value := (others => '0');
            result.error := ERR_OVERFLOW;
        else
            result.ok := true;
            result.value := resize(sum(a'high downto 0), 32);
            result.error := ERR_NONE;
        end if;
        return result;
    end function;

    function safe_add(a, b : signed) return result_signed_t is
        variable result : result_signed_t;
        variable sum : signed(a'length downto 0);
    begin
        sum := resize(a, a'length + 1) + resize(b, b'length + 1);
        -- Check for overflow (positive + positive = negative or negative + negative = positive)
        if (a(a'high) = '0' and b(b'high) = '0' and sum(a'high) = '1') or
           (a(a'high) = '1' and b(b'high) = '1' and sum(a'high) = '0') then
            result.ok := false;
            result.value := (others => '0');
            result.error := ERR_OVERFLOW;
        else
            result.ok := true;
            result.value := resize(sum(a'high downto 0), 32);
            result.error := ERR_NONE;
        end if;
        return result;
    end function;

    function safe_sub(a, b : integer) return result_int_t is
        variable result : result_int_t;
    begin
        -- Check for underflow
        if (b > 0 and a < SAFE_INT_MIN + b) or
           (b < 0 and a > SAFE_INT_MAX + b) then
            result.ok := false;
            result.value := 0;
            result.error := ERR_UNDERFLOW;
        else
            result.ok := true;
            result.value := a - b;
            result.error := ERR_NONE;
        end if;
        return result;
    end function;

    function safe_sub(a, b : unsigned) return result_unsigned_t is
        variable result : result_unsigned_t;
    begin
        if b > a then
            result.ok := false;
            result.value := (others => '0');
            result.error := ERR_UNDERFLOW;
        else
            result.ok := true;
            result.value := resize(a - b, 32);
            result.error := ERR_NONE;
        end if;
        return result;
    end function;

    function safe_sub(a, b : signed) return result_signed_t is
        variable result : result_signed_t;
        variable diff : signed(a'length downto 0);
    begin
        diff := resize(a, a'length + 1) - resize(b, b'length + 1);
        -- Check for underflow
        if (a(a'high) = '0' and b(b'high) = '1' and diff(a'high) = '1') or
           (a(a'high) = '1' and b(b'high) = '0' and diff(a'high) = '0') then
            result.ok := false;
            result.value := (others => '0');
            result.error := ERR_UNDERFLOW;
        else
            result.ok := true;
            result.value := resize(diff(a'high downto 0), 32);
            result.error := ERR_NONE;
        end if;
        return result;
    end function;

    function safe_mul(a, b : integer) return result_int_t is
        variable result : result_int_t;
        variable prod : integer;
    begin
        -- Check for overflow (simplified check)
        if a /= 0 and b /= 0 then
            if (a > 0 and b > 0 and a > SAFE_INT_MAX / b) or
               (a < 0 and b < 0 and a < SAFE_INT_MAX / b) or
               (a > 0 and b < 0 and b < SAFE_INT_MIN / a) or
               (a < 0 and b > 0 and a < SAFE_INT_MIN / b) then
                result.ok := false;
                result.value := 0;
                result.error := ERR_OVERFLOW;
                return result;
            end if;
        end if;
        result.ok := true;
        result.value := a * b;
        result.error := ERR_NONE;
        return result;
    end function;

    function safe_mul(a, b : unsigned) return result_unsigned_t is
        variable result : result_unsigned_t;
        variable prod : unsigned((a'length + b'length - 1) downto 0);
    begin
        prod := a * b;
        -- Check if product fits in result width
        if prod(prod'high downto 32) /= 0 then
            result.ok := false;
            result.value := (others => '0');
            result.error := ERR_OVERFLOW;
        else
            result.ok := true;
            result.value := prod(31 downto 0);
            result.error := ERR_NONE;
        end if;
        return result;
    end function;

    function safe_mul(a, b : signed) return result_signed_t is
        variable result : result_signed_t;
        variable prod : signed((a'length + b'length - 1) downto 0);
    begin
        prod := a * b;
        -- Check if product fits in result width (accounting for sign extension)
        if prod(prod'high downto 31) /= (prod'high downto 31 => prod(31)) then
            result.ok := false;
            result.value := (others => '0');
            result.error := ERR_OVERFLOW;
        else
            result.ok := true;
            result.value := prod(31 downto 0);
            result.error := ERR_NONE;
        end if;
        return result;
    end function;

    function safe_div(a, b : integer) return result_int_t is
        variable result : result_int_t;
    begin
        if b = 0 then
            result.ok := false;
            result.value := 0;
            result.error := ERR_DIVISION_BY_ZERO;
        else
            result.ok := true;
            result.value := a / b;
            result.error := ERR_NONE;
        end if;
        return result;
    end function;

    function safe_div(a, b : unsigned) return result_unsigned_t is
        variable result : result_unsigned_t;
    begin
        if b = 0 then
            result.ok := false;
            result.value := (others => '0');
            result.error := ERR_DIVISION_BY_ZERO;
        else
            result.ok := true;
            result.value := resize(a / b, 32);
            result.error := ERR_NONE;
        end if;
        return result;
    end function;

    function safe_div(a, b : signed) return result_signed_t is
        variable result : result_signed_t;
    begin
        if b = 0 then
            result.ok := false;
            result.value := (others => '0');
            result.error := ERR_DIVISION_BY_ZERO;
        else
            result.ok := true;
            result.value := resize(a / b, 32);
            result.error := ERR_NONE;
        end if;
        return result;
    end function;

    function safe_mod(a, b : integer) return result_int_t is
        variable result : result_int_t;
    begin
        if b = 0 then
            result.ok := false;
            result.value := 0;
            result.error := ERR_DIVISION_BY_ZERO;
        else
            result.ok := true;
            result.value := a mod b;
            result.error := ERR_NONE;
        end if;
        return result;
    end function;

    function safe_mod(a, b : unsigned) return result_unsigned_t is
        variable result : result_unsigned_t;
    begin
        if b = 0 then
            result.ok := false;
            result.value := (others => '0');
            result.error := ERR_DIVISION_BY_ZERO;
        else
            result.ok := true;
            result.value := resize(a mod b, 32);
            result.error := ERR_NONE;
        end if;
        return result;
    end function;

    -- ========================================================================
    -- BOUNDED VALUE IMPLEMENTATIONS
    -- ========================================================================

    function clamp(value, min_val, max_val : integer) return integer is
    begin
        if value < min_val then
            return min_val;
        elsif value > max_val then
            return max_val;
        else
            return value;
        end if;
    end function;

    function clamp(value, min_val, max_val : unsigned) return unsigned is
    begin
        if value < min_val then
            return min_val;
        elsif value > max_val then
            return max_val;
        else
            return value;
        end if;
    end function;

    function clamp(value, min_val, max_val : signed) return signed is
    begin
        if value < min_val then
            return min_val;
        elsif value > max_val then
            return max_val;
        else
            return value;
        end if;
    end function;

    function in_range(value, min_val, max_val : integer) return boolean is
    begin
        return value >= min_val and value <= max_val;
    end function;

    function in_range(value, min_val, max_val : unsigned) return boolean is
    begin
        return value >= min_val and value <= max_val;
    end function;

    function in_range(value, min_val, max_val : signed) return boolean is
    begin
        return value >= min_val and value <= max_val;
    end function;

    function require_in_range(value, min_val, max_val : integer) return result_int_t is
        variable result : result_int_t;
    begin
        if in_range(value, min_val, max_val) then
            result.ok := true;
            result.value := value;
            result.error := ERR_NONE;
        else
            result.ok := false;
            result.value := 0;
            result.error := ERR_OUT_OF_BOUNDS;
        end if;
        return result;
    end function;

    function require_in_range(value, min_val, max_val : unsigned) return result_unsigned_t is
        variable result : result_unsigned_t;
    begin
        if in_range(value, min_val, max_val) then
            result.ok := true;
            result.value := resize(value, 32);
            result.error := ERR_NONE;
        else
            result.ok := false;
            result.value := (others => '0');
            result.error := ERR_OUT_OF_BOUNDS;
        end if;
        return result;
    end function;

    -- ========================================================================
    -- VALIDATION IMPLEMENTATIONS
    -- ========================================================================

    function is_valid_port(port : natural) return boolean is
    begin
        return port >= PORT_MIN and port <= PORT_MAX;
    end function;

    function require_valid_port(port : natural) return result_int_t is
        variable result : result_int_t;
    begin
        if is_valid_port(port) then
            result.ok := true;
            result.value := port;
            result.error := ERR_NONE;
        else
            result.ok := false;
            result.value := 0;
            result.error := ERR_INVALID_PORT;
        end if;
        return result;
    end function;

    function is_valid_percentage(value : natural) return boolean is
    begin
        return value >= PERCENT_MIN and value <= PERCENT_MAX;
    end function;

    function require_valid_percentage(value : natural) return result_int_t is
        variable result : result_int_t;
    begin
        if is_valid_percentage(value) then
            result.ok := true;
            result.value := value;
            result.error := ERR_NONE;
        else
            result.ok := false;
            result.value := 0;
            result.error := ERR_INVALID_PERCENTAGE;
        end if;
        return result;
    end function;

    -- ========================================================================
    -- PERCENTAGE IMPLEMENTATIONS
    -- ========================================================================

    function percent_of(amount : unsigned; pct : natural) return result_unsigned_t is
        variable result : result_unsigned_t;
        variable prod : unsigned(63 downto 0);
    begin
        if pct > 100 then
            result.ok := false;
            result.value := (others => '0');
            result.error := ERR_INVALID_PERCENTAGE;
        else
            prod := resize(amount, 64) * to_unsigned(pct, 32);
            result.ok := true;
            result.value := prod(63 downto 32) + resize(prod(31 downto 0) / 100, 32);
            result.error := ERR_NONE;
        end if;
        return result;
    end function;

    function bps_of(amount : unsigned; bps : natural) return result_unsigned_t is
        variable result : result_unsigned_t;
        variable prod : unsigned(63 downto 0);
    begin
        prod := resize(amount, 64) * to_unsigned(bps, 32);
        result.ok := true;
        result.value := resize(prod / 10000, 32);
        result.error := ERR_NONE;
        return result;
    end function;

    -- ========================================================================
    -- SATURATION ARITHMETIC IMPLEMENTATIONS
    -- ========================================================================

    function sat_add(a, b : unsigned; max_val : unsigned) return unsigned is
        variable sum : unsigned(a'length downto 0);
    begin
        sum := ('0' & a) + ('0' & b);
        if sum(sum'high) = '1' or sum(a'range) > max_val then
            return max_val;
        else
            return sum(a'range);
        end if;
    end function;

    function sat_add(a, b : signed; min_val, max_val : signed) return signed is
        variable sum : signed(a'length downto 0);
    begin
        sum := resize(a, a'length + 1) + resize(b, b'length + 1);
        if sum > resize(max_val, sum'length) then
            return max_val;
        elsif sum < resize(min_val, sum'length) then
            return min_val;
        else
            return sum(a'range);
        end if;
    end function;

    function sat_sub(a, b : unsigned) return unsigned is
    begin
        if b > a then
            return (a'range => '0');
        else
            return a - b;
        end if;
    end function;

    function sat_sub(a, b : signed; min_val : signed) return signed is
        variable diff : signed(a'length downto 0);
    begin
        diff := resize(a, a'length + 1) - resize(b, b'length + 1);
        if diff < resize(min_val, diff'length) then
            return min_val;
        else
            return diff(a'range);
        end if;
    end function;

    -- ========================================================================
    -- BIT MANIPULATION IMPLEMENTATIONS
    -- ========================================================================

    function safe_bit(vec : std_logic_vector; idx : natural) return result_int_t is
        variable result : result_int_t;
    begin
        if idx > vec'high or idx < vec'low then
            result.ok := false;
            result.value := 0;
            result.error := ERR_OUT_OF_BOUNDS;
        else
            result.ok := true;
            if vec(idx) = '1' then
                result.value := 1;
            else
                result.value := 0;
            end if;
            result.error := ERR_NONE;
        end if;
        return result;
    end function;

    function safe_range(vec : std_logic_vector; high, low : natural) return std_logic_vector is
    begin
        if high > vec'high or low < vec'low or high < low then
            return (0 downto 0 => '0');
        else
            return vec(high downto low);
        end if;
    end function;

    -- ========================================================================
    -- TIMING SAFETY IMPLEMENTATIONS
    -- ========================================================================

    function safe_increment(value : unsigned; max_val : unsigned) return result_unsigned_t is
        variable result : result_unsigned_t;
    begin
        if value >= max_val then
            result.ok := false;
            result.value := max_val;
            result.error := ERR_OVERFLOW;
        else
            result.ok := true;
            result.value := resize(value + 1, 32);
            result.error := ERR_NONE;
        end if;
        return result;
    end function;

    function safe_decrement(value : unsigned) return result_unsigned_t is
        variable result : result_unsigned_t;
    begin
        if value = 0 then
            result.ok := false;
            result.value := (others => '0');
            result.error := ERR_UNDERFLOW;
        else
            result.ok := true;
            result.value := resize(value - 1, 32);
            result.error := ERR_NONE;
        end if;
        return result;
    end function;

end package body proven;
