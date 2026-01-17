// SPDX-License-Identifier: Apache-2.0
// Proven - Safety primitives demonstration for Hack Assembly
//
// ============================================================================
// EDUCATIONAL DEMONSTRATION - NOT A PRODUCTION SPECIFICATION
// ============================================================================
//
// This file demonstrates how proven safety concepts translate to the absolute
// lowest level of abstraction - a minimal assembly language with no hardware
// safety features whatsoever.
//
// PURPOSE:
// - Illustrate safety patterns for students learning computer architecture
// - Show what overflow detection, bounds checking, and safe memory access
//   look like when you have NOTHING but registers and RAM
// - Educational companion to the Nand2Tetris course
//
// LIMITATIONS:
// - Hack is a pedagogical architecture with no real-world hardware
// - No interrupts, no MMU, no privilege levels, no hardware bounds checking
// - 16-bit signed integers only (-32768 to 32767)
// - These patterns are ILLUSTRATIVE, not optimized or complete
//
// TARGET AUDIENCE: Students of "The Elements of Computing Systems" course
// who want to understand safety primitives at the bare metal level.
//
// ============================================================================
//
// Hack Architecture (for reference):
// - 16-bit word size
// - 32K RAM (addresses 0-32767)
// - 32K ROM (addresses 0-32767)
// - A and D registers only
// - Memory-mapped I/O: Screen (16384-24575), Keyboard (24576)

// ============================================================================
// MEMORY MAP CONSTANTS
// ============================================================================

// RAM regions
@0          // R0 - General purpose / SP
@1          // R1 - General purpose / LCL
@2          // R2 - General purpose / ARG
@3          // R3 - General purpose / THIS
@4          // R4 - General purpose / THAT
@5          // R5-R12 - General purpose (temp)
@13         // R13-R15 - General purpose (temp)
@16         // Static variables start (16-255)
@256        // Stack starts here

// Proven constants stored in fixed locations
@32767      // PROVEN_INT_MAX (max positive 16-bit signed)
@-32768     // PROVEN_INT_MIN (min negative 16-bit signed)

// Screen memory-mapped region
@16384      // SCREEN base address
@24575      // SCREEN end address

// Keyboard memory-mapped
@24576      // KBD

// ============================================================================
// PROVEN SAFE MEMORY ADDRESSES
// Use these constants for bounds checking
// ============================================================================

// Safe stack bounds
(PROVEN_STACK_MIN)
@256
D=A
@PROVEN_STACK_MIN_VAL
M=D

(PROVEN_STACK_MAX)
@2047
D=A
@PROVEN_STACK_MAX_VAL
M=D

// Safe heap bounds
(PROVEN_HEAP_MIN)
@2048
D=A
@PROVEN_HEAP_MIN_VAL
M=D

(PROVEN_HEAP_MAX)
@16383
D=A
@PROVEN_HEAP_MAX_VAL
M=D

// ============================================================================
// RESULT TYPE CONVENTION
// ============================================================================
// R0 = return value (result or error code)
// R1 = is_ok flag (0 = error, non-zero = ok)
// R2 = error code (if R1 = 0)

// Set OK result in R0
(PROVEN_SET_OK)
    @R1
    M=1         // is_ok = true
    @R2
    M=0         // error_code = 0
    @R14
    A=M
    0;JMP       // Return

// Set ERR result
// Call with error code in D
(PROVEN_SET_ERR)
    @R2
    M=D         // error_code = D
    @R1
    M=0         // is_ok = false
    @R0
    M=0         // result = 0
    @R14
    A=M
    0;JMP       // Return

// ============================================================================
// SAFE BOUNDED INTEGER OPERATIONS
// ============================================================================

// proven_clamp: Clamp D to range [R3, R4]
// Input: D = value, R3 = min, R4 = max
// Output: D = clamped value
(PROVEN_CLAMP)
    @R5
    M=D         // Store original value

    // Check if D < min
    @R3
    D=D-M       // D = value - min
    @PROVEN_CLAMP_USE_MIN
    D;JLT       // If value < min, use min

    // Check if value > max
    @R5
    D=M         // Restore value
    @R4
    D=D-M       // D = value - max
    @PROVEN_CLAMP_USE_MAX
    D;JGT       // If value > max, use max

    // Value is within range
    @R5
    D=M         // Return original value
    @R14
    A=M
    0;JMP

(PROVEN_CLAMP_USE_MIN)
    @R3
    D=M         // D = min
    @R14
    A=M
    0;JMP

(PROVEN_CLAMP_USE_MAX)
    @R4
    D=M         // D = max
    @R14
    A=M
    0;JMP

// ============================================================================
// SAFE ARITHMETIC WITH OVERFLOW DETECTION
// ============================================================================

// proven_safe_add: Add R0 + R1, detect overflow
// Input: R0 = a, R1 = b
// Output: R0 = result, R1 = is_ok, R2 = error_code
(PROVEN_SAFE_ADD)
    // Store return address
    @R14
    M=D

    // Load operands
    @R0
    D=M
    @R5
    M=D         // R5 = a

    @R1
    D=M
    @R6
    M=D         // R6 = b

    // Check for positive overflow: a > 0 && b > 0 && a > MAX - b
    @R5
    D=M
    @PROVEN_SAFE_ADD_CHECK_NEG
    D;JLE       // Skip if a <= 0

    @R6
    D=M
    @PROVEN_SAFE_ADD_CHECK_NEG
    D;JLE       // Skip if b <= 0

    // Both positive, check if a > 32767 - b
    @32767
    D=A
    @R6
    D=D-M       // D = MAX - b
    @R5
    D=D-M       // D = MAX - b - a
    @PROVEN_SAFE_ADD_OVERFLOW
    D;JLT       // Overflow if MAX - b - a < 0

(PROVEN_SAFE_ADD_CHECK_NEG)
    // Check for negative overflow: a < 0 && b < 0 && a < MIN - b
    @R5
    D=M
    @PROVEN_SAFE_ADD_OK
    D;JGE       // Skip if a >= 0

    @R6
    D=M
    @PROVEN_SAFE_ADD_OK
    D;JGE       // Skip if b >= 0

    // Both negative, check if a < -32768 - b
    @32767
    D=A
    D=!D        // D = -32768 (two's complement trick: ~32767 = -32768)
    @R6
    D=D-M       // D = MIN - b
    @R5
    D=M-D       // D = a - (MIN - b)
    @PROVEN_SAFE_ADD_UNDERFLOW
    D;JLT       // Underflow if a < MIN - b

(PROVEN_SAFE_ADD_OK)
    // No overflow, compute result
    @R5
    D=M
    @R6
    D=D+M       // D = a + b
    @R0
    M=D         // Store result

    @R1
    M=1         // is_ok = true
    @R2
    M=0         // error_code = 0

    @R14
    A=M
    0;JMP

(PROVEN_SAFE_ADD_OVERFLOW)
    @1          // Error code 1 = overflow
    D=A
    @R2
    M=D
    @R1
    M=0         // is_ok = false
    @32767
    D=A
    @R0
    M=D         // Saturate to max
    @R14
    A=M
    0;JMP

(PROVEN_SAFE_ADD_UNDERFLOW)
    @2          // Error code 2 = underflow
    D=A
    @R2
    M=D
    @R1
    M=0         // is_ok = false
    @32767
    D=A
    D=!D        // D = -32768
    @R0
    M=D         // Saturate to min
    @R14
    A=M
    0;JMP

// ============================================================================
// SAFE MULTIPLICATION (16-bit, detects overflow)
// ============================================================================

// proven_safe_mul: Multiply R0 * R1
// Uses shift-and-add algorithm with overflow checking
// Input: R0 = a, R1 = b
// Output: R0 = result, R1 = is_ok, R2 = error_code
(PROVEN_SAFE_MUL)
    @R14
    M=D         // Store return address

    @R0
    D=M
    @R5
    M=D         // R5 = a (multiplicand)

    @R1
    D=M
    @R6
    M=D         // R6 = b (multiplier)

    @R7
    M=0         // R7 = result accumulator

    // Handle negative numbers by tracking sign
    @R8
    M=0         // R8 = sign flag (0 = positive, 1 = negative)

    // If a < 0, negate and set sign
    @R5
    D=M
    @PROVEN_MUL_CHECK_B
    D;JGE
    @R5
    M=-M
    @R8
    M=M+1

(PROVEN_MUL_CHECK_B)
    // If b < 0, negate and toggle sign
    @R6
    D=M
    @PROVEN_MUL_LOOP
    D;JGE
    @R6
    M=-M
    @R8
    M=M+1

(PROVEN_MUL_LOOP)
    // While b > 0
    @R6
    D=M
    @PROVEN_MUL_DONE
    D;JLE

    // If b & 1 (odd), add a to result
    @R6
    D=M
    @1
    D=D&A       // D = b & 1
    @PROVEN_MUL_SKIP_ADD
    D;JEQ

    // result += a
    @R5
    D=M
    @R7
    M=D+M

    // Check for overflow
    @R7
    D=M
    @PROVEN_MUL_OVERFLOW
    D;JLT       // If result went negative, overflow occurred

(PROVEN_MUL_SKIP_ADD)
    // a <<= 1 (double a)
    @R5
    D=M
    @R5
    M=D+M

    // b >>= 1 (halve b) - use division by 2
    @R6
    D=M
    @2
    D=D-A
    D=D+1       // Approximate: D = (b-1)/2 rounded
    // Actually for shift: need proper divide by 2
    @R6
    D=M
    @R9
    M=0         // Counter
(PROVEN_MUL_DIV2)
    @R6
    D=M
    @PROVEN_MUL_DIV2_DONE
    D;JLE
    @R6
    M=M-1
    M=M-1       // Subtract 2
    @R9
    M=M+1       // Increment counter
    @PROVEN_MUL_DIV2
    0;JMP
(PROVEN_MUL_DIV2_DONE)
    @R9
    D=M
    @R6
    M=D         // b = b / 2

    @PROVEN_MUL_LOOP
    0;JMP

(PROVEN_MUL_DONE)
    // Apply sign
    @R8
    D=M
    @1
    D=D&A       // Check if sign flag is odd (result should be negative)
    @PROVEN_MUL_RETURN_POS
    D;JEQ

    // Negate result
    @R7
    M=-M

(PROVEN_MUL_RETURN_POS)
    @R7
    D=M
    @R0
    M=D         // Return result
    @R1
    M=1         // is_ok = true
    @R2
    M=0         // error_code = 0
    @R14
    A=M
    0;JMP

(PROVEN_MUL_OVERFLOW)
    @3          // Error code 3 = multiplication overflow
    D=A
    @R2
    M=D
    @R1
    M=0         // is_ok = false
    @R14
    A=M
    0;JMP

// ============================================================================
// SAFE DIVISION (with divide-by-zero protection)
// ============================================================================

// proven_safe_div: Divide R0 / R1
// Input: R0 = dividend, R1 = divisor
// Output: R0 = quotient, R1 = is_ok, R2 = error_code, R3 = remainder
(PROVEN_SAFE_DIV)
    @R14
    M=D         // Store return address

    // Check for divide by zero
    @R1
    D=M
    @PROVEN_DIV_BY_ZERO
    D;JEQ

    // Proceed with division...
    @R0
    D=M
    @R5
    M=D         // R5 = dividend

    @R1
    D=M
    @R6
    M=D         // R6 = divisor

    @R7
    M=0         // R7 = quotient

    // Handle signs
    @R8
    M=0         // Sign flag

    @R5
    D=M
    @PROVEN_DIV_CHECK_DIVISOR
    D;JGE
    @R5
    M=-M
    @R8
    M=M+1

(PROVEN_DIV_CHECK_DIVISOR)
    @R6
    D=M
    @PROVEN_DIV_LOOP
    D;JGE
    @R6
    M=-M
    @R8
    M=M+1

(PROVEN_DIV_LOOP)
    // While dividend >= divisor
    @R6
    D=M
    @R5
    D=M-D       // D = dividend - divisor
    @PROVEN_DIV_DONE
    D;JLT       // Exit if dividend < divisor

    // dividend -= divisor
    @R6
    D=M
    @R5
    M=M-D

    // quotient++
    @R7
    M=M+1

    @PROVEN_DIV_LOOP
    0;JMP

(PROVEN_DIV_DONE)
    // R3 = remainder (always positive)
    @R5
    D=M
    @R3
    M=D

    // Apply sign to quotient
    @R8
    D=M
    @1
    D=D&A
    @PROVEN_DIV_RETURN
    D;JEQ
    @R7
    M=-M

(PROVEN_DIV_RETURN)
    @R7
    D=M
    @R0
    M=D         // Return quotient
    @R1
    M=1         // is_ok = true
    @R2
    M=0         // error_code = 0
    @R14
    A=M
    0;JMP

(PROVEN_DIV_BY_ZERO)
    @4          // Error code 4 = divide by zero
    D=A
    @R2
    M=D
    @R1
    M=0         // is_ok = false
    @R0
    M=0         // Return 0 as safe default
    @R14
    A=M
    0;JMP

// ============================================================================
// SAFE MEMORY ACCESS
// ============================================================================

// proven_safe_load: Load from address in R0 with bounds checking
// Input: R0 = address
// Output: R0 = value, R1 = is_ok, R2 = error_code
(PROVEN_SAFE_LOAD)
    @R14
    M=D

    // Check if address is in valid RAM range (0-16383)
    @R0
    D=M
    @PROVEN_LOAD_INVALID
    D;JLT       // Address < 0 is invalid

    @16383
    D=A
    @R0
    D=D-M       // D = 16383 - address
    @PROVEN_LOAD_INVALID
    D;JLT       // Address > 16383 is invalid (for regular RAM)

    // Valid address, load value
    @R0
    A=M         // A = address
    D=M         // D = value at address
    @R0
    M=D         // Return value

    @R1
    M=1         // is_ok = true
    @R2
    M=0
    @R14
    A=M
    0;JMP

(PROVEN_LOAD_INVALID)
    @5          // Error code 5 = invalid memory address
    D=A
    @R2
    M=D
    @R1
    M=0         // is_ok = false
    @R0
    M=0         // Return 0 as safe default
    @R14
    A=M
    0;JMP

// proven_safe_store: Store D to address in R0 with bounds checking
// Input: R0 = address, D = value to store
// Output: R1 = is_ok, R2 = error_code
(PROVEN_SAFE_STORE)
    @R5
    M=D         // Store value temporarily

    // Check if address is in valid writable range (0-16383, not ROM)
    @R0
    D=M
    @PROVEN_STORE_INVALID
    D;JLT

    @16383
    D=A
    @R0
    D=D-M
    @PROVEN_STORE_INVALID
    D;JLT

    // Valid address, store value
    @R5
    D=M         // Retrieve value
    @R0
    A=M         // A = address
    M=D         // Store value

    @R1
    M=1         // is_ok = true
    @R2
    M=0
    @R14
    A=M
    0;JMP

(PROVEN_STORE_INVALID)
    @6          // Error code 6 = invalid store address
    D=A
    @R2
    M=D
    @R1
    M=0         // is_ok = false
    @R14
    A=M
    0;JMP

// ============================================================================
// SAFE SCREEN ACCESS
// ============================================================================

// proven_plot_pixel: Safely set a pixel at (x, y)
// Input: R0 = x (0-511), R1 = y (0-255)
// Output: R2 = is_ok, R3 = error_code
(PROVEN_PLOT_PIXEL)
    // Bounds check X
    @R0
    D=M
    @PROVEN_PIXEL_INVALID
    D;JLT       // x < 0

    @511
    D=A
    @R0
    D=D-M
    @PROVEN_PIXEL_INVALID
    D;JLT       // x > 511

    // Bounds check Y
    @R1
    D=M
    @PROVEN_PIXEL_INVALID
    D;JLT       // y < 0

    @255
    D=A
    @R1
    D=D-M
    @PROVEN_PIXEL_INVALID
    D;JLT       // y > 255

    // Calculate screen address: 16384 + (y * 32) + (x / 16)
    // Word address = 16384 + y*32 + x/16
    // Bit position = x % 16

    @R1
    D=M
    @R5
    M=D         // R5 = y

    // Multiply y by 32 (shift left 5)
    @R5
    D=M
    D=D+M
    D=D+M
    D=D+M
    D=D+M       // D = y * 16
    D=D+M       // D = y * 32
    @R6
    M=D         // R6 = y * 32

    // Calculate x / 16 (shift right 4)
    @R0
    D=M
    @R7
    M=0         // R7 = x / 16
(PROVEN_PIXEL_DIV16)
    @16
    D=D-A
    @PROVEN_PIXEL_DIV16_DONE
    D;JLT
    @R7
    M=M+1
    @PROVEN_PIXEL_DIV16
    0;JMP
(PROVEN_PIXEL_DIV16_DONE)
    // Now R7 = x / 16

    // Calculate bit position: x % 16
    @R0
    D=M
    @15
    D=D&A       // D = x & 15 = x % 16
    @R8
    M=D         // R8 = bit position

    // Screen address = 16384 + R6 + R7
    @16384
    D=A
    @R6
    D=D+M
    @R7
    D=D+M
    @R9
    M=D         // R9 = screen word address

    // Create bit mask (1 << bit_position)
    @1
    D=A
    @R10
    M=D         // R10 = bit mask
(PROVEN_PIXEL_SHIFT_MASK)
    @R8
    D=M
    @PROVEN_PIXEL_SET_BIT
    D;JEQ
    @R10
    D=M
    @R10
    M=D+M       // Shift left by 1
    @R8
    M=M-1
    @PROVEN_PIXEL_SHIFT_MASK
    0;JMP

(PROVEN_PIXEL_SET_BIT)
    // OR the bit into the screen word
    @R9
    A=M
    D=M         // D = current screen word
    @R10
    D=D|M       // D = word | mask
    @R9
    A=M
    M=D         // Store back

    @R2
    M=1         // is_ok = true
    @R3
    M=0
    @R14
    A=M
    0;JMP

(PROVEN_PIXEL_INVALID)
    @7          // Error code 7 = pixel out of bounds
    D=A
    @R3
    M=D
    @R2
    M=0         // is_ok = false
    @R14
    A=M
    0;JMP

// ============================================================================
// SAFE KEYBOARD INPUT
// ============================================================================

// proven_get_key: Safely read keyboard input
// Output: R0 = key code (0 if no key pressed)
(PROVEN_GET_KEY)
    @24576      // KBD address
    D=M
    @R0
    M=D
    @R14
    A=M
    0;JMP

// proven_wait_key: Wait for key press with timeout
// Input: R0 = max iterations (0 = infinite wait)
// Output: R0 = key code, R1 = is_ok (0 if timeout)
(PROVEN_WAIT_KEY)
    @R0
    D=M
    @R5
    M=D         // R5 = timeout counter

(PROVEN_WAIT_KEY_LOOP)
    @24576
    D=M
    @PROVEN_WAIT_KEY_GOT
    D;JNE       // Key pressed

    // Check timeout
    @R5
    D=M
    @PROVEN_WAIT_KEY_LOOP
    D;JEQ       // Infinite wait if 0

    @R5
    M=M-1
    D=M
    @PROVEN_WAIT_KEY_TIMEOUT
    D;JEQ       // Timeout reached

    @PROVEN_WAIT_KEY_LOOP
    0;JMP

(PROVEN_WAIT_KEY_GOT)
    @R0
    M=D
    @R1
    M=1         // is_ok = true
    @R14
    A=M
    0;JMP

(PROVEN_WAIT_KEY_TIMEOUT)
    @R0
    M=0
    @R1
    M=0         // is_ok = false (timeout)
    @R14
    A=M
    0;JMP

// ============================================================================
// BOUNDED VALUE TYPES
// ============================================================================

// proven_percentage: Create percentage value (0-100) clamped
// Input: D = value
// Output: D = clamped percentage
(PROVEN_PERCENTAGE)
    @PROVEN_PCT_CLAMP_MIN
    D;JLT

    @100
    D=D-A
    @PROVEN_PCT_CLAMP_MAX
    D;JGT

    // Restore and return (was in range)
    @100
    D=D+A
    @R14
    A=M
    0;JMP

(PROVEN_PCT_CLAMP_MIN)
    D=0
    @R14
    A=M
    0;JMP

(PROVEN_PCT_CLAMP_MAX)
    @100
    D=A
    @R14
    A=M
    0;JMP

// proven_port: Validate port number (1-65535, but Hack only has 16-bit signed)
// In Hack's 16-bit signed world, valid ports are 1-32767
// Input: D = port
// Output: D = valid port (0 if invalid), R1 = is_ok
(PROVEN_PORT)
    @PROVEN_PORT_INVALID
    D;JLE       // port <= 0 is invalid

    // In 16-bit signed, max is 32767 which is fine for ports
    @R1
    M=1         // is_ok = true
    @R14
    A=M
    0;JMP

(PROVEN_PORT_INVALID)
    D=0
    @R1
    M=0         // is_ok = false
    @R14
    A=M
    0;JMP

// ============================================================================
// END OF PROVEN LIBRARY
// ============================================================================
(PROVEN_END)
    @PROVEN_END
    0;JMP       // Infinite loop (standard Hack program termination)
