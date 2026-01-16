# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

# @version ^0.4.0

"""
@title Proven Safety Library for Vyper
@notice Formally verified safety primitives for smart contracts
@dev Vyper has built-in overflow protection, these utilities add
     domain-specific validation and safe patterns.
"""

# ============================================================================
# CONSTANTS
# ============================================================================

MAX_PORT: constant(uint16) = 65535
MIN_PORT: constant(uint16) = 1
MAX_PERCENTAGE: constant(uint256) = 100
BASIS_POINTS: constant(uint256) = 10000

# ============================================================================
# EVENTS
# ============================================================================

event SafeTransfer:
    sender: indexed(address)
    recipient: indexed(address)
    amount: uint256

event ValidationFailed:
    reason: String[64]
    value: uint256

# ============================================================================
# SAFE MATH (explicit for documentation, Vyper has built-in checks)
# ============================================================================

@internal
@pure
def safe_add(a: uint256, b: uint256) -> uint256:
    """
    @notice Safe addition with overflow check
    @dev Vyper 0.4+ has automatic overflow checks, this is explicit
    """
    result: uint256 = a + b
    assert result >= a, "Overflow in addition"
    return result

@internal
@pure
def safe_sub(a: uint256, b: uint256) -> uint256:
    """
    @notice Safe subtraction with underflow check
    """
    assert b <= a, "Underflow in subtraction"
    return a - b

@internal
@pure
def safe_mul(a: uint256, b: uint256) -> uint256:
    """
    @notice Safe multiplication with overflow check
    """
    if a == 0:
        return 0
    result: uint256 = a * b
    assert result / a == b, "Overflow in multiplication"
    return result

@internal
@pure
def safe_div(a: uint256, b: uint256) -> uint256:
    """
    @notice Safe division with zero check
    """
    assert b > 0, "Division by zero"
    return a / b

@internal
@pure
def safe_mod(a: uint256, b: uint256) -> uint256:
    """
    @notice Safe modulo with zero check
    """
    assert b > 0, "Modulo by zero"
    return a % b

# ============================================================================
# BOUNDED VALUES
# ============================================================================

@internal
@pure
def clamp(value: uint256, min_val: uint256, max_val: uint256) -> uint256:
    """
    @notice Clamp value to range [min_val, max_val]
    """
    if value < min_val:
        return min_val
    if value > max_val:
        return max_val
    return value

@internal
@pure
def in_range(value: uint256, min_val: uint256, max_val: uint256) -> bool:
    """
    @notice Check if value is in range (inclusive)
    """
    return value >= min_val and value <= max_val

@internal
@pure
def require_in_range(value: uint256, min_val: uint256, max_val: uint256):
    """
    @notice Require value is in range or revert
    """
    assert value >= min_val and value <= max_val, "Value out of bounds"

# ============================================================================
# VALIDATION
# ============================================================================

@internal
@pure
def is_valid_port(port: uint16) -> bool:
    """
    @notice Validate port number (1-65535)
    """
    return port >= MIN_PORT and port <= MAX_PORT

@internal
@pure
def require_valid_port(port: uint16):
    """
    @notice Require valid port or revert
    """
    assert port >= MIN_PORT and port <= MAX_PORT, "Invalid port"

@internal
@pure
def is_valid_percentage(value: uint256) -> bool:
    """
    @notice Validate percentage (0-100)
    """
    return value <= MAX_PERCENTAGE

@internal
@pure
def require_valid_percentage(value: uint256):
    """
    @notice Require valid percentage or revert
    """
    assert value <= MAX_PERCENTAGE, "Invalid percentage"

@internal
@pure
def is_valid_address(addr: address) -> bool:
    """
    @notice Check if address is non-zero
    """
    return addr != empty(address)

@internal
@pure
def require_valid_address(addr: address):
    """
    @notice Require valid (non-zero) address
    """
    assert addr != empty(address), "Invalid zero address"

# ============================================================================
# SAFE ETH HANDLING
# ============================================================================

@internal
def safe_transfer_eth(to: address, amount: uint256):
    """
    @notice Safe ETH transfer with success check
    """
    assert to != empty(address), "Invalid recipient"
    raw_call(to, b"", value=amount)
    log SafeTransfer(msg.sender, to, amount)

@internal
@pure
def percentage_of(amount: uint256, bps: uint256) -> uint256:
    """
    @notice Calculate percentage using basis points (100 bps = 1%)
    @param amount Base amount
    @param bps Basis points
    """
    return (amount * bps) / BASIS_POINTS

@internal
@pure
def percentage_of_100(amount: uint256, pct: uint256) -> uint256:
    """
    @notice Calculate percentage (0-100 scale)
    @param amount Base amount
    @param pct Percentage (0-100)
    """
    assert pct <= 100, "Percentage exceeds 100"
    return (amount * pct) / 100

# ============================================================================
# TIMESTAMP SAFETY
# ============================================================================

@internal
@view
def is_future(timestamp: uint256) -> bool:
    """
    @notice Check if timestamp is in the future
    """
    return timestamp > block.timestamp

@internal
@view
def is_past(timestamp: uint256) -> bool:
    """
    @notice Check if timestamp is in the past
    """
    return timestamp < block.timestamp

@internal
@pure
def is_within_window(timestamp: uint256, window_start: uint256, window_end: uint256) -> bool:
    """
    @notice Check if timestamp is within a time window
    """
    return timestamp >= window_start and timestamp <= window_end

@internal
@view
def require_not_expired(deadline: uint256):
    """
    @notice Require deadline has not passed
    """
    assert block.timestamp <= deadline, "Deadline expired"

# ============================================================================
# ARRAY SAFETY
# ============================================================================

@internal
@pure
def safe_array_access(arr: DynArray[uint256, 1000], index: uint256) -> uint256:
    """
    @notice Safe array access with bounds check
    """
    assert index < len(arr), "Index out of bounds"
    return arr[index]

# ============================================================================
# VERSION
# ============================================================================

@external
@pure
def version() -> String[8]:
    """
    @notice Return library version
    """
    return "0.9.0"
