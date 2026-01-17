// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafePassword - Password validation without security holes.
 *
 * Provides password strength checking and policy enforcement.
 * @module
 */

import { ok, err } from './result.js';

/**
 * Password strength levels.
 * @readonly
 * @enum {string}
 */
export const PasswordStrength = Object.freeze({
  VERY_WEAK: 'very_weak',
  WEAK: 'weak',
  FAIR: 'fair',
  STRONG: 'strong',
  VERY_STRONG: 'very_strong',
});

/**
 * Password validation result.
 * @typedef {Object} PasswordValidation
 * @property {boolean} valid - Whether password meets policy
 * @property {string} strength - Strength level
 * @property {number} entropy - Estimated entropy in bits
 * @property {string[]} issues - List of issues found
 */

/**
 * Password policy configuration.
 * @typedef {Object} PasswordPolicy
 * @property {number} minLength - Minimum length
 * @property {number} maxLength - Maximum length
 * @property {boolean} requireUppercase - Require uppercase letter
 * @property {boolean} requireLowercase - Require lowercase letter
 * @property {boolean} requireDigit - Require digit
 * @property {boolean} requireSpecial - Require special character
 * @property {number} minEntropy - Minimum entropy in bits
 */

/** @type {PasswordPolicy} */
const DEFAULT_POLICY = {
  minLength: 8,
  maxLength: 128,
  requireUppercase: true,
  requireLowercase: true,
  requireDigit: true,
  requireSpecial: false,
  minEntropy: 25,
};

/**
 * Common passwords to reject (subset).
 * @type {Set<string>}
 */
const COMMON_PASSWORDS = new Set([
  'password',
  '123456',
  '12345678',
  'qwerty',
  'abc123',
  'password1',
  'admin',
  'letmein',
  'welcome',
  'monkey',
  'dragon',
  'master',
  '111111',
  'baseball',
  'iloveyou',
  'trustno1',
  'sunshine',
  'princess',
]);

/**
 * Safe password operations.
 */
export class SafePassword {
  /**
   * Validate a password against a policy.
   *
   * @param {string} password - Password to validate
   * @param {Partial<PasswordPolicy>} [policy] - Policy to use
   * @returns {PasswordValidation}
   *
   * @example
   * SafePassword.validate("MyP@ssw0rd!")
   */
  static validate(password, policy = {}) {
    const effectivePolicy = { ...DEFAULT_POLICY, ...policy };
    /** @type {string[]} */
    const issues = [];

    if (typeof password !== 'string') {
      return {
        valid: false,
        strength: PasswordStrength.VERY_WEAK,
        entropy: 0,
        issues: ['Password must be a string'],
      };
    }

    // Length checks
    if (password.length < effectivePolicy.minLength) {
      issues.push(`Password must be at least ${effectivePolicy.minLength} characters`);
    }
    if (password.length > effectivePolicy.maxLength) {
      issues.push(`Password must be at most ${effectivePolicy.maxLength} characters`);
    }

    // Character class checks
    const hasUppercase = /[A-Z]/.test(password);
    const hasLowercase = /[a-z]/.test(password);
    const hasDigit = /\d/.test(password);
    const hasSpecial = /[!@#$%^&*()_+\-=\[\]{};':"\\|,.<>\/?]/.test(password);

    if (effectivePolicy.requireUppercase && !hasUppercase) {
      issues.push('Password must contain an uppercase letter');
    }
    if (effectivePolicy.requireLowercase && !hasLowercase) {
      issues.push('Password must contain a lowercase letter');
    }
    if (effectivePolicy.requireDigit && !hasDigit) {
      issues.push('Password must contain a digit');
    }
    if (effectivePolicy.requireSpecial && !hasSpecial) {
      issues.push('Password must contain a special character');
    }

    // Common password check
    if (COMMON_PASSWORDS.has(password.toLowerCase())) {
      issues.push('Password is too common');
    }

    // Calculate entropy
    const entropy = SafePassword.calculateEntropy(password);
    if (entropy < effectivePolicy.minEntropy) {
      issues.push(`Password entropy too low (${entropy.toFixed(1)} bits, need ${effectivePolicy.minEntropy})`);
    }

    // Determine strength
    const strength = SafePassword.getStrength(entropy);

    return {
      valid: issues.length === 0,
      strength,
      entropy,
      issues,
    };
  }

  /**
   * Calculate password entropy in bits.
   *
   * @param {string} password - Password to analyze
   * @returns {number}
   */
  static calculateEntropy(password) {
    if (!password || password.length === 0) {
      return 0;
    }

    let poolSize = 0;

    if (/[a-z]/.test(password)) poolSize += 26;
    if (/[A-Z]/.test(password)) poolSize += 26;
    if (/\d/.test(password)) poolSize += 10;
    if (/[!@#$%^&*()_+\-=\[\]{};':"\\|,.<>\/?]/.test(password)) poolSize += 32;
    if (/[^\w!@#$%^&*()_+\-=\[\]{};':"\\|,.<>\/?]/.test(password)) poolSize += 128;

    if (poolSize === 0) poolSize = 1;

    return password.length * Math.log2(poolSize);
  }

  /**
   * Get strength level from entropy.
   *
   * @param {number} entropy - Entropy in bits
   * @returns {string}
   */
  static getStrength(entropy) {
    if (entropy < 20) return PasswordStrength.VERY_WEAK;
    if (entropy < 35) return PasswordStrength.WEAK;
    if (entropy < 50) return PasswordStrength.FAIR;
    if (entropy < 70) return PasswordStrength.STRONG;
    return PasswordStrength.VERY_STRONG;
  }

  /**
   * Check if password is a common password.
   *
   * @param {string} password - Password to check
   * @returns {boolean}
   */
  static isCommon(password) {
    return COMMON_PASSWORDS.has(password.toLowerCase());
  }

  /**
   * Generate a secure random password.
   *
   * @param {number} length - Password length
   * @param {Object} options - Generation options
   * @param {boolean} [options.uppercase=true] - Include uppercase
   * @param {boolean} [options.lowercase=true] - Include lowercase
   * @param {boolean} [options.digits=true] - Include digits
   * @param {boolean} [options.special=true] - Include special chars
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   */
  static generate(length, options = {}) {
    const { uppercase = true, lowercase = true, digits = true, special = true } = options;

    if (length < 4) {
      return err('Password length must be at least 4');
    }

    if (length > 256) {
      return err('Password length must be at most 256');
    }

    let charset = '';
    if (lowercase) charset += 'abcdefghijklmnopqrstuvwxyz';
    if (uppercase) charset += 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
    if (digits) charset += '0123456789';
    if (special) charset += '!@#$%^&*()_+-=[]{}|;:,.<>?';

    if (charset.length === 0) {
      return err('At least one character class must be enabled');
    }

    // Use crypto.getRandomValues for secure random
    const randomValues = new Uint32Array(length);
    crypto.getRandomValues(randomValues);

    let password = '';
    for (let charIndex = 0; charIndex < length; charIndex++) {
      password += charset[randomValues[charIndex] % charset.length];
    }

    return ok(password);
  }

  /**
   * Check for sequential characters.
   *
   * @param {string} password - Password to check
   * @param {number} minSequence - Minimum sequence length to detect
   * @returns {boolean}
   */
  static hasSequential(password, minSequence = 3) {
    const lower = password.toLowerCase();
    for (let startIndex = 0; startIndex <= lower.length - minSequence; startIndex++) {
      let isSequence = true;
      for (let offset = 1; offset < minSequence; offset++) {
        if (lower.charCodeAt(startIndex + offset) !== lower.charCodeAt(startIndex) + offset) {
          isSequence = false;
          break;
        }
      }
      if (isSequence) return true;
    }
    return false;
  }

  /**
   * Check for repeated characters.
   *
   * @param {string} password - Password to check
   * @param {number} minRepeat - Minimum repeat count to detect
   * @returns {boolean}
   */
  static hasRepeated(password, minRepeat = 3) {
    const lower = password.toLowerCase();
    for (let startIndex = 0; startIndex <= lower.length - minRepeat; startIndex++) {
      const char = lower[startIndex];
      let repeatCount = 1;
      for (let offset = 1; offset < minRepeat && startIndex + offset < lower.length; offset++) {
        if (lower[startIndex + offset] === char) {
          repeatCount++;
        } else {
          break;
        }
      }
      if (repeatCount >= minRepeat) return true;
    }
    return false;
  }
}
