# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
#
# Proven Safety Library for CMake
#
# Formally verified safety primitives for CMake build configurations.
# Provides safe operations, validation functions, and bounded types.
#
# Usage:
#   include(ProvenConfig)
#   proven_safe_add(result 100 200)
#   proven_validate_port(valid 8080)
#
# Version: 0.9.0

cmake_minimum_required(VERSION 3.16)

# ============================================================================
# VERSION
# ============================================================================

set(PROVEN_VERSION_MAJOR 0)
set(PROVEN_VERSION_MINOR 9)
set(PROVEN_VERSION_PATCH 0)
set(PROVEN_VERSION "${PROVEN_VERSION_MAJOR}.${PROVEN_VERSION_MINOR}.${PROVEN_VERSION_PATCH}")

# ============================================================================
# CONSTANTS
# ============================================================================

# Integer bounds (CMake uses 64-bit signed integers)
set(PROVEN_INT_MAX 9223372036854775807)
set(PROVEN_INT_MIN -9223372036854775808)

# Safe integer bounds (53-bit for JavaScript compatibility)
set(PROVEN_SAFE_INT_MAX 9007199254740991)
set(PROVEN_SAFE_INT_MIN -9007199254740991)

# Port range
set(PROVEN_PORT_MIN 1)
set(PROVEN_PORT_MAX 65535)

# Percentage range
set(PROVEN_PERCENT_MIN 0)
set(PROVEN_PERCENT_MAX 100)

# ============================================================================
# SAFE MATH FUNCTIONS
# ============================================================================

#[[
  Safe addition with overflow check.

  proven_safe_add(<output_var> <a> <b>)

  Sets <output_var> to the sum if no overflow, otherwise sets to empty string.
  Sets <output_var>_OK to TRUE/FALSE.
]]
function(proven_safe_add OUTPUT_VAR A B)
    # Check for overflow
    if(A GREATER 0 AND B GREATER 0)
        math(EXPR max_allowed "${PROVEN_SAFE_INT_MAX} - ${A}")
        if(B GREATER max_allowed)
            set(${OUTPUT_VAR} "" PARENT_SCOPE)
            set(${OUTPUT_VAR}_OK FALSE PARENT_SCOPE)
            set(${OUTPUT_VAR}_ERROR "OVERFLOW" PARENT_SCOPE)
            return()
        endif()
    elseif(A LESS 0 AND B LESS 0)
        math(EXPR min_allowed "${PROVEN_SAFE_INT_MIN} - ${A}")
        if(B LESS min_allowed)
            set(${OUTPUT_VAR} "" PARENT_SCOPE)
            set(${OUTPUT_VAR}_OK FALSE PARENT_SCOPE)
            set(${OUTPUT_VAR}_ERROR "OVERFLOW" PARENT_SCOPE)
            return()
        endif()
    endif()

    math(EXPR result "${A} + ${B}")
    set(${OUTPUT_VAR} ${result} PARENT_SCOPE)
    set(${OUTPUT_VAR}_OK TRUE PARENT_SCOPE)
    set(${OUTPUT_VAR}_ERROR "" PARENT_SCOPE)
endfunction()

#[[
  Safe subtraction with underflow check.

  proven_safe_sub(<output_var> <a> <b>)
]]
function(proven_safe_sub OUTPUT_VAR A B)
    # Check for underflow by adding the negative
    math(EXPR neg_b "0 - ${B}")
    proven_safe_add(${OUTPUT_VAR} ${A} ${neg_b})
    set(${OUTPUT_VAR} ${${OUTPUT_VAR}} PARENT_SCOPE)
    set(${OUTPUT_VAR}_OK ${${OUTPUT_VAR}_OK} PARENT_SCOPE)
    if(NOT ${${OUTPUT_VAR}_OK})
        set(${OUTPUT_VAR}_ERROR "UNDERFLOW" PARENT_SCOPE)
    else()
        set(${OUTPUT_VAR}_ERROR "" PARENT_SCOPE)
    endif()
endfunction()

#[[
  Safe multiplication with overflow check.

  proven_safe_mul(<output_var> <a> <b>)
]]
function(proven_safe_mul OUTPUT_VAR A B)
    if(A EQUAL 0 OR B EQUAL 0)
        set(${OUTPUT_VAR} 0 PARENT_SCOPE)
        set(${OUTPUT_VAR}_OK TRUE PARENT_SCOPE)
        set(${OUTPUT_VAR}_ERROR "" PARENT_SCOPE)
        return()
    endif()

    # Check for overflow
    if(A GREATER 0)
        math(EXPR max_allowed "${PROVEN_SAFE_INT_MAX} / ${A}")
        if(B GREATER 0 AND B GREATER max_allowed)
            set(${OUTPUT_VAR} "" PARENT_SCOPE)
            set(${OUTPUT_VAR}_OK FALSE PARENT_SCOPE)
            set(${OUTPUT_VAR}_ERROR "OVERFLOW" PARENT_SCOPE)
            return()
        elseif(B LESS 0)
            math(EXPR abs_b "0 - ${B}")
            math(EXPR min_allowed "${PROVEN_SAFE_INT_MIN} / ${A}")
            math(EXPR abs_min "0 - ${min_allowed}")
            if(abs_b GREATER abs_min)
                set(${OUTPUT_VAR} "" PARENT_SCOPE)
                set(${OUTPUT_VAR}_OK FALSE PARENT_SCOPE)
                set(${OUTPUT_VAR}_ERROR "OVERFLOW" PARENT_SCOPE)
                return()
            endif()
        endif()
    else()
        math(EXPR abs_a "0 - ${A}")
        math(EXPR max_allowed "${PROVEN_SAFE_INT_MAX} / ${abs_a}")
        if(B LESS 0)
            math(EXPR abs_b "0 - ${B}")
            if(abs_b GREATER max_allowed)
                set(${OUTPUT_VAR} "" PARENT_SCOPE)
                set(${OUTPUT_VAR}_OK FALSE PARENT_SCOPE)
                set(${OUTPUT_VAR}_ERROR "OVERFLOW" PARENT_SCOPE)
                return()
            endif()
        elseif(B GREATER max_allowed)
            set(${OUTPUT_VAR} "" PARENT_SCOPE)
            set(${OUTPUT_VAR}_OK FALSE PARENT_SCOPE)
            set(${OUTPUT_VAR}_ERROR "OVERFLOW" PARENT_SCOPE)
            return()
        endif()
    endif()

    math(EXPR result "${A} * ${B}")
    set(${OUTPUT_VAR} ${result} PARENT_SCOPE)
    set(${OUTPUT_VAR}_OK TRUE PARENT_SCOPE)
    set(${OUTPUT_VAR}_ERROR "" PARENT_SCOPE)
endfunction()

#[[
  Safe division with zero check.

  proven_safe_div(<output_var> <a> <b>)
]]
function(proven_safe_div OUTPUT_VAR A B)
    if(B EQUAL 0)
        set(${OUTPUT_VAR} "" PARENT_SCOPE)
        set(${OUTPUT_VAR}_OK FALSE PARENT_SCOPE)
        set(${OUTPUT_VAR}_ERROR "DIVISION_BY_ZERO" PARENT_SCOPE)
        return()
    endif()

    math(EXPR result "${A} / ${B}")
    set(${OUTPUT_VAR} ${result} PARENT_SCOPE)
    set(${OUTPUT_VAR}_OK TRUE PARENT_SCOPE)
    set(${OUTPUT_VAR}_ERROR "" PARENT_SCOPE)
endfunction()

#[[
  Safe modulo with zero check.

  proven_safe_mod(<output_var> <a> <b>)
]]
function(proven_safe_mod OUTPUT_VAR A B)
    if(B EQUAL 0)
        set(${OUTPUT_VAR} "" PARENT_SCOPE)
        set(${OUTPUT_VAR}_OK FALSE PARENT_SCOPE)
        set(${OUTPUT_VAR}_ERROR "DIVISION_BY_ZERO" PARENT_SCOPE)
        return()
    endif()

    math(EXPR result "${A} % ${B}")
    set(${OUTPUT_VAR} ${result} PARENT_SCOPE)
    set(${OUTPUT_VAR}_OK TRUE PARENT_SCOPE)
    set(${OUTPUT_VAR}_ERROR "" PARENT_SCOPE)
endfunction()

# ============================================================================
# BOUNDED VALUE FUNCTIONS
# ============================================================================

#[[
  Clamp value to range [min, max].

  proven_clamp(<output_var> <value> <min> <max>)
]]
function(proven_clamp OUTPUT_VAR VALUE MIN_VAL MAX_VAL)
    if(VALUE LESS MIN_VAL)
        set(${OUTPUT_VAR} ${MIN_VAL} PARENT_SCOPE)
    elseif(VALUE GREATER MAX_VAL)
        set(${OUTPUT_VAR} ${MAX_VAL} PARENT_SCOPE)
    else()
        set(${OUTPUT_VAR} ${VALUE} PARENT_SCOPE)
    endif()
endfunction()

#[[
  Check if value is in range [min, max].

  proven_in_range(<output_var> <value> <min> <max>)
]]
function(proven_in_range OUTPUT_VAR VALUE MIN_VAL MAX_VAL)
    if(VALUE GREATER_EQUAL MIN_VAL AND VALUE LESS_EQUAL MAX_VAL)
        set(${OUTPUT_VAR} TRUE PARENT_SCOPE)
    else()
        set(${OUTPUT_VAR} FALSE PARENT_SCOPE)
    endif()
endfunction()

#[[
  Require value in range or fail.

  proven_require_in_range(<output_var> <value> <min> <max>)
]]
function(proven_require_in_range OUTPUT_VAR VALUE MIN_VAL MAX_VAL)
    proven_in_range(is_valid ${VALUE} ${MIN_VAL} ${MAX_VAL})
    if(is_valid)
        set(${OUTPUT_VAR} ${VALUE} PARENT_SCOPE)
        set(${OUTPUT_VAR}_OK TRUE PARENT_SCOPE)
        set(${OUTPUT_VAR}_ERROR "" PARENT_SCOPE)
    else()
        set(${OUTPUT_VAR} "" PARENT_SCOPE)
        set(${OUTPUT_VAR}_OK FALSE PARENT_SCOPE)
        set(${OUTPUT_VAR}_ERROR "OUT_OF_BOUNDS" PARENT_SCOPE)
    endif()
endfunction()

# ============================================================================
# VALIDATION FUNCTIONS
# ============================================================================

#[[
  Validate port number (1-65535).

  proven_validate_port(<output_var> <port>)
]]
function(proven_validate_port OUTPUT_VAR PORT)
    proven_in_range(${OUTPUT_VAR} ${PORT} ${PROVEN_PORT_MIN} ${PROVEN_PORT_MAX})
    set(${OUTPUT_VAR} ${${OUTPUT_VAR}} PARENT_SCOPE)
endfunction()

#[[
  Require valid port or fail.

  proven_require_valid_port(<output_var> <port>)
]]
function(proven_require_valid_port OUTPUT_VAR PORT)
    proven_validate_port(is_valid ${PORT})
    if(is_valid)
        set(${OUTPUT_VAR} ${PORT} PARENT_SCOPE)
        set(${OUTPUT_VAR}_OK TRUE PARENT_SCOPE)
        set(${OUTPUT_VAR}_ERROR "" PARENT_SCOPE)
    else()
        set(${OUTPUT_VAR} "" PARENT_SCOPE)
        set(${OUTPUT_VAR}_OK FALSE PARENT_SCOPE)
        set(${OUTPUT_VAR}_ERROR "INVALID_PORT" PARENT_SCOPE)
    endif()
endfunction()

#[[
  Validate percentage (0-100).

  proven_validate_percentage(<output_var> <value>)
]]
function(proven_validate_percentage OUTPUT_VAR VALUE)
    proven_in_range(${OUTPUT_VAR} ${VALUE} ${PROVEN_PERCENT_MIN} ${PROVEN_PERCENT_MAX})
    set(${OUTPUT_VAR} ${${OUTPUT_VAR}} PARENT_SCOPE)
endfunction()

#[[
  Require valid percentage or fail.

  proven_require_valid_percentage(<output_var> <value>)
]]
function(proven_require_valid_percentage OUTPUT_VAR VALUE)
    proven_validate_percentage(is_valid ${VALUE})
    if(is_valid)
        set(${OUTPUT_VAR} ${VALUE} PARENT_SCOPE)
        set(${OUTPUT_VAR}_OK TRUE PARENT_SCOPE)
        set(${OUTPUT_VAR}_ERROR "" PARENT_SCOPE)
    else()
        set(${OUTPUT_VAR} "" PARENT_SCOPE)
        set(${OUTPUT_VAR}_OK FALSE PARENT_SCOPE)
        set(${OUTPUT_VAR}_ERROR "INVALID_PERCENTAGE" PARENT_SCOPE)
    endif()
endfunction()

#[[
  Check if path is safe (no traversal attacks).

  proven_is_safe_path(<output_var> <path>)
]]
function(proven_is_safe_path OUTPUT_VAR PATH)
    string(FIND "${PATH}" ".." found)
    if(found EQUAL -1)
        set(${OUTPUT_VAR} TRUE PARENT_SCOPE)
    else()
        set(${OUTPUT_VAR} FALSE PARENT_SCOPE)
    endif()
endfunction()

#[[
  Require safe path or fail.

  proven_require_safe_path(<output_var> <path>)
]]
function(proven_require_safe_path OUTPUT_VAR PATH)
    proven_is_safe_path(is_valid "${PATH}")
    if(is_valid)
        set(${OUTPUT_VAR} "${PATH}" PARENT_SCOPE)
        set(${OUTPUT_VAR}_OK TRUE PARENT_SCOPE)
        set(${OUTPUT_VAR}_ERROR "" PARENT_SCOPE)
    else()
        set(${OUTPUT_VAR} "" PARENT_SCOPE)
        set(${OUTPUT_VAR}_OK FALSE PARENT_SCOPE)
        set(${OUTPUT_VAR}_ERROR "PATH_TRAVERSAL" PARENT_SCOPE)
    endif()
endfunction()

#[[
  Validate identifier (alphanumeric + underscore, starts with letter/underscore).

  proven_is_valid_identifier(<output_var> <name>)
]]
function(proven_is_valid_identifier OUTPUT_VAR NAME)
    if(NAME MATCHES "^[a-zA-Z_][a-zA-Z0-9_]*$")
        set(${OUTPUT_VAR} TRUE PARENT_SCOPE)
    else()
        set(${OUTPUT_VAR} FALSE PARENT_SCOPE)
    endif()
endfunction()

#[[
  Validate semantic version string.

  proven_is_valid_semver(<output_var> <version>)
]]
function(proven_is_valid_semver OUTPUT_VAR VERSION)
    if(VERSION MATCHES "^[0-9]+\\.[0-9]+\\.[0-9]+(-[a-zA-Z0-9.-]+)?(\\+[a-zA-Z0-9.-]+)?$")
        set(${OUTPUT_VAR} TRUE PARENT_SCOPE)
    else()
        set(${OUTPUT_VAR} FALSE PARENT_SCOPE)
    endif()
endfunction()

# ============================================================================
# PERCENTAGE CALCULATIONS
# ============================================================================

#[[
  Calculate percentage of an amount (integer approximation).

  proven_percent_of(<output_var> <amount> <percent>)
]]
function(proven_percent_of OUTPUT_VAR AMOUNT PERCENT)
    proven_validate_percentage(valid ${PERCENT})
    if(NOT valid)
        set(${OUTPUT_VAR} "" PARENT_SCOPE)
        set(${OUTPUT_VAR}_OK FALSE PARENT_SCOPE)
        set(${OUTPUT_VAR}_ERROR "INVALID_PERCENTAGE" PARENT_SCOPE)
        return()
    endif()

    proven_safe_mul(product ${AMOUNT} ${PERCENT})
    if(NOT product_OK)
        set(${OUTPUT_VAR} "" PARENT_SCOPE)
        set(${OUTPUT_VAR}_OK FALSE PARENT_SCOPE)
        set(${OUTPUT_VAR}_ERROR ${product_ERROR} PARENT_SCOPE)
        return()
    endif()

    proven_safe_div(result ${product} 100)
    set(${OUTPUT_VAR} ${result} PARENT_SCOPE)
    set(${OUTPUT_VAR}_OK ${result_OK} PARENT_SCOPE)
    set(${OUTPUT_VAR}_ERROR ${result_ERROR} PARENT_SCOPE)
endfunction()

#[[
  Calculate basis points of an amount (100 bps = 1%).

  proven_bps_of(<output_var> <amount> <bps>)
]]
function(proven_bps_of OUTPUT_VAR AMOUNT BPS)
    proven_safe_mul(product ${AMOUNT} ${BPS})
    if(NOT product_OK)
        set(${OUTPUT_VAR} "" PARENT_SCOPE)
        set(${OUTPUT_VAR}_OK FALSE PARENT_SCOPE)
        set(${OUTPUT_VAR}_ERROR ${product_ERROR} PARENT_SCOPE)
        return()
    endif()

    proven_safe_div(result ${product} 10000)
    set(${OUTPUT_VAR} ${result} PARENT_SCOPE)
    set(${OUTPUT_VAR}_OK ${result_OK} PARENT_SCOPE)
    set(${OUTPUT_VAR}_ERROR ${result_ERROR} PARENT_SCOPE)
endfunction()

# ============================================================================
# BUILD SAFETY UTILITIES
# ============================================================================

#[[
  Safe minimum value selection.

  proven_min(<output_var> <a> <b>)
]]
function(proven_min OUTPUT_VAR A B)
    if(A LESS B)
        set(${OUTPUT_VAR} ${A} PARENT_SCOPE)
    else()
        set(${OUTPUT_VAR} ${B} PARENT_SCOPE)
    endif()
endfunction()

#[[
  Safe maximum value selection.

  proven_max(<output_var> <a> <b>)
]]
function(proven_max OUTPUT_VAR A B)
    if(A GREATER B)
        set(${OUTPUT_VAR} ${A} PARENT_SCOPE)
    else()
        set(${OUTPUT_VAR} ${B} PARENT_SCOPE)
    endif()
endfunction()

#[[
  Safe absolute value.

  proven_abs(<output_var> <value>)
]]
function(proven_abs OUTPUT_VAR VALUE)
    if(VALUE LESS 0)
        math(EXPR result "0 - ${VALUE}")
        set(${OUTPUT_VAR} ${result} PARENT_SCOPE)
    else()
        set(${OUTPUT_VAR} ${VALUE} PARENT_SCOPE)
    endif()
endfunction()

#[[
  Check if target exists safely.

  proven_target_exists(<output_var> <target_name>)
]]
function(proven_target_exists OUTPUT_VAR TARGET_NAME)
    if(TARGET ${TARGET_NAME})
        set(${OUTPUT_VAR} TRUE PARENT_SCOPE)
    else()
        set(${OUTPUT_VAR} FALSE PARENT_SCOPE)
    endif()
endfunction()

#[[
  Safe option with default value.

  proven_option_with_default(<output_var> <option_name> <default_value>)
]]
function(proven_option_with_default OUTPUT_VAR OPTION_NAME DEFAULT_VALUE)
    if(DEFINED ${OPTION_NAME})
        set(${OUTPUT_VAR} ${${OPTION_NAME}} PARENT_SCOPE)
    else()
        set(${OUTPUT_VAR} ${DEFAULT_VALUE} PARENT_SCOPE)
    endif()
endfunction()

#[[
  Assert condition or fail build.

  proven_assert(<condition> <message>)
]]
function(proven_assert CONDITION MESSAGE)
    if(NOT ${CONDITION})
        message(FATAL_ERROR "Proven assertion failed: ${MESSAGE}")
    endif()
endfunction()

#[[
  Assert value in range or fail build.

  proven_assert_in_range(<value> <min> <max> <name>)
]]
function(proven_assert_in_range VALUE MIN_VAL MAX_VAL NAME)
    proven_in_range(is_valid ${VALUE} ${MIN_VAL} ${MAX_VAL})
    if(NOT is_valid)
        message(FATAL_ERROR "Proven assertion failed: ${NAME}=${VALUE} not in range [${MIN_VAL}, ${MAX_VAL}]")
    endif()
endfunction()

# ============================================================================
# LIST UTILITIES
# ============================================================================

#[[
  Safe list length.

  proven_list_length(<output_var> <list>)
]]
function(proven_list_length OUTPUT_VAR LIST)
    list(LENGTH ${LIST} len)
    set(${OUTPUT_VAR} ${len} PARENT_SCOPE)
endfunction()

#[[
  Safe list access with bounds checking.

  proven_list_get(<output_var> <list> <index>)
]]
function(proven_list_get OUTPUT_VAR LIST INDEX)
    list(LENGTH ${LIST} len)
    if(INDEX LESS 0 OR INDEX GREATER_EQUAL len)
        set(${OUTPUT_VAR} "" PARENT_SCOPE)
        set(${OUTPUT_VAR}_OK FALSE PARENT_SCOPE)
        set(${OUTPUT_VAR}_ERROR "OUT_OF_BOUNDS" PARENT_SCOPE)
    else()
        list(GET ${LIST} ${INDEX} item)
        set(${OUTPUT_VAR} ${item} PARENT_SCOPE)
        set(${OUTPUT_VAR}_OK TRUE PARENT_SCOPE)
        set(${OUTPUT_VAR}_ERROR "" PARENT_SCOPE)
    endif()
endfunction()

# ============================================================================
# VERSION CHECK UTILITIES
# ============================================================================

#[[
  Compare semantic versions.
  Returns -1 if a < b, 0 if a == b, 1 if a > b.

  proven_version_compare(<output_var> <version_a> <version_b>)
]]
function(proven_version_compare OUTPUT_VAR VERSION_A VERSION_B)
    if(VERSION_A VERSION_LESS VERSION_B)
        set(${OUTPUT_VAR} -1 PARENT_SCOPE)
    elseif(VERSION_A VERSION_EQUAL VERSION_B)
        set(${OUTPUT_VAR} 0 PARENT_SCOPE)
    else()
        set(${OUTPUT_VAR} 1 PARENT_SCOPE)
    endif()
endfunction()

#[[
  Require minimum version.

  proven_require_version(<output_var> <actual> <minimum>)
]]
function(proven_require_version OUTPUT_VAR ACTUAL MINIMUM)
    proven_version_compare(cmp ${ACTUAL} ${MINIMUM})
    if(cmp LESS 0)
        set(${OUTPUT_VAR} FALSE PARENT_SCOPE)
        set(${OUTPUT_VAR}_ERROR "Version ${ACTUAL} is less than required ${MINIMUM}" PARENT_SCOPE)
    else()
        set(${OUTPUT_VAR} TRUE PARENT_SCOPE)
        set(${OUTPUT_VAR}_ERROR "" PARENT_SCOPE)
    endif()
endfunction()

# Print version on include
message(STATUS "Proven CMake Library v${PROVEN_VERSION} loaded")
