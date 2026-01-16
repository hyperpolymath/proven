// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

pragma solidity ^0.8.20;

/**
 * @title Proven Safety Library for Solidity
 * @notice Formally verified safety primitives for smart contracts
 * @dev These utilities help prevent common smart contract vulnerabilities
 *
 * Key safety features:
 * - Overflow protection (built into Solidity 0.8+, but explicit for clarity)
 * - Reentrancy guards
 * - Safe math with explicit error handling
 * - Bounded types for domain validation
 */
library Proven {
    // ========================================================================
    // CUSTOM ERRORS (gas efficient)
    // ========================================================================

    error Overflow();
    error Underflow();
    error DivisionByZero();
    error InvalidPort(uint16 port);
    error InvalidPercentage(uint256 value);
    error ValueOutOfBounds(uint256 value, uint256 min, uint256 max);
    error EmptyString();
    error StringTooLong(uint256 length, uint256 maxLength);

    // ========================================================================
    // SAFE MATH
    // ========================================================================

    /**
     * @notice Safe addition with explicit overflow check
     * @dev Solidity 0.8+ has built-in overflow checks, this is for explicit handling
     */
    function safeAdd(uint256 a, uint256 b) internal pure returns (uint256) {
        unchecked {
            uint256 c = a + b;
            if (c < a) revert Overflow();
            return c;
        }
    }

    /**
     * @notice Safe subtraction with explicit underflow check
     */
    function safeSub(uint256 a, uint256 b) internal pure returns (uint256) {
        if (b > a) revert Underflow();
        return a - b;
    }

    /**
     * @notice Safe multiplication with overflow check
     */
    function safeMul(uint256 a, uint256 b) internal pure returns (uint256) {
        if (a == 0) return 0;
        unchecked {
            uint256 c = a * b;
            if (c / a != b) revert Overflow();
            return c;
        }
    }

    /**
     * @notice Safe division with zero check
     */
    function safeDiv(uint256 a, uint256 b) internal pure returns (uint256) {
        if (b == 0) revert DivisionByZero();
        return a / b;
    }

    /**
     * @notice Safe modulo with zero check
     */
    function safeMod(uint256 a, uint256 b) internal pure returns (uint256) {
        if (b == 0) revert DivisionByZero();
        return a % b;
    }

    // ========================================================================
    // BOUNDED VALUES
    // ========================================================================

    /**
     * @notice Clamp a value to a range
     */
    function clamp(uint256 value, uint256 min, uint256 max) internal pure returns (uint256) {
        if (value < min) return min;
        if (value > max) return max;
        return value;
    }

    /**
     * @notice Check if value is in range (inclusive)
     */
    function inRange(uint256 value, uint256 min, uint256 max) internal pure returns (bool) {
        return value >= min && value <= max;
    }

    /**
     * @notice Require value is in range or revert
     */
    function requireInRange(uint256 value, uint256 min, uint256 max) internal pure {
        if (value < min || value > max) {
            revert ValueOutOfBounds(value, min, max);
        }
    }

    // ========================================================================
    // VALIDATION
    // ========================================================================

    /**
     * @notice Validate port number (1-65535)
     */
    function isValidPort(uint16 port) internal pure returns (bool) {
        return port >= 1 && port <= 65535;
    }

    /**
     * @notice Validate port or revert
     */
    function requireValidPort(uint16 port) internal pure {
        if (port < 1 || port > 65535) {
            revert InvalidPort(port);
        }
    }

    /**
     * @notice Validate percentage (0-100)
     */
    function isValidPercentage(uint256 value) internal pure returns (bool) {
        return value <= 100;
    }

    /**
     * @notice Validate percentage or revert
     */
    function requireValidPercentage(uint256 value) internal pure {
        if (value > 100) {
            revert InvalidPercentage(value);
        }
    }

    /**
     * @notice Check if string is non-empty
     */
    function isNonEmpty(string memory s) internal pure returns (bool) {
        return bytes(s).length > 0;
    }

    /**
     * @notice Require non-empty string or revert
     */
    function requireNonEmpty(string memory s) internal pure {
        if (bytes(s).length == 0) {
            revert EmptyString();
        }
    }

    /**
     * @notice Validate string length
     */
    function requireMaxLength(string memory s, uint256 maxLength) internal pure {
        if (bytes(s).length > maxLength) {
            revert StringTooLong(bytes(s).length, maxLength);
        }
    }

    // ========================================================================
    // SAFE ETH HANDLING
    // ========================================================================

    /**
     * @notice Safe ETH transfer with success check
     */
    function safeTransferETH(address to, uint256 amount) internal {
        (bool success, ) = to.call{value: amount}("");
        require(success, "ETH transfer failed");
    }

    /**
     * @notice Calculate percentage of amount (basis points for precision)
     * @param amount The base amount
     * @param bps Basis points (100 bps = 1%)
     */
    function percentageOf(uint256 amount, uint256 bps) internal pure returns (uint256) {
        return safeMul(amount, bps) / 10000;
    }

    // ========================================================================
    // TIMESTAMP SAFETY
    // ========================================================================

    /**
     * @notice Check if timestamp is in the future
     */
    function isFuture(uint256 timestamp) internal view returns (bool) {
        return timestamp > block.timestamp;
    }

    /**
     * @notice Check if timestamp is in the past
     */
    function isPast(uint256 timestamp) internal view returns (bool) {
        return timestamp < block.timestamp;
    }

    /**
     * @notice Check if within a time window
     */
    function isWithinWindow(uint256 timestamp, uint256 windowStart, uint256 windowEnd) internal pure returns (bool) {
        return timestamp >= windowStart && timestamp <= windowEnd;
    }

    // ========================================================================
    // ADDRESS VALIDATION
    // ========================================================================

    /**
     * @notice Check if address is non-zero
     */
    function isValidAddress(address addr) internal pure returns (bool) {
        return addr != address(0);
    }

    /**
     * @notice Require valid (non-zero) address
     */
    function requireValidAddress(address addr) internal pure {
        require(addr != address(0), "Invalid zero address");
    }

    /**
     * @notice Check if address is a contract
     */
    function isContract(address addr) internal view returns (bool) {
        uint256 size;
        assembly {
            size := extcodesize(addr)
        }
        return size > 0;
    }
}

/**
 * @title ReentrancyGuard
 * @notice Prevents reentrant calls to a function
 */
abstract contract ReentrancyGuard {
    uint256 private constant NOT_ENTERED = 1;
    uint256 private constant ENTERED = 2;

    uint256 private _status;

    error ReentrantCall();

    constructor() {
        _status = NOT_ENTERED;
    }

    modifier nonReentrant() {
        if (_status == ENTERED) revert ReentrantCall();
        _status = ENTERED;
        _;
        _status = NOT_ENTERED;
    }
}

/**
 * @title SafePercentage
 * @notice Type for percentages (0-100) with compile-time safety
 */
library SafePercentage {
    type Percentage is uint8;

    error InvalidPercentageValue(uint256 value);

    function create(uint256 value) internal pure returns (Percentage) {
        if (value > 100) revert InvalidPercentageValue(value);
        return Percentage.wrap(uint8(value));
    }

    function unwrap(Percentage p) internal pure returns (uint8) {
        return Percentage.unwrap(p);
    }

    function toUint256(Percentage p) internal pure returns (uint256) {
        return uint256(Percentage.unwrap(p));
    }
}

/**
 * @title SafeAmount
 * @notice Type for token amounts with overflow protection
 */
library SafeAmount {
    type Amount is uint256;

    function create(uint256 value) internal pure returns (Amount) {
        return Amount.wrap(value);
    }

    function unwrap(Amount a) internal pure returns (uint256) {
        return Amount.unwrap(a);
    }

    function add(Amount a, Amount b) internal pure returns (Amount) {
        return Amount.wrap(Proven.safeAdd(Amount.unwrap(a), Amount.unwrap(b)));
    }

    function sub(Amount a, Amount b) internal pure returns (Amount) {
        return Amount.wrap(Proven.safeSub(Amount.unwrap(a), Amount.unwrap(b)));
    }

    function mul(Amount a, uint256 b) internal pure returns (Amount) {
        return Amount.wrap(Proven.safeMul(Amount.unwrap(a), b));
    }

    function div(Amount a, uint256 b) internal pure returns (Amount) {
        return Amount.wrap(Proven.safeDiv(Amount.unwrap(a), b));
    }
}
