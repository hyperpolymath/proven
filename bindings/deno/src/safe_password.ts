// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * Safe password handling and validation.
 */

import { err, ok, type Result } from './result.ts';

export enum PasswordStrength {
  VeryWeak = 'very-weak',
  Weak = 'weak',
  Fair = 'fair',
  Strong = 'strong',
  VeryStrong = 'very-strong',
}

export interface PasswordPolicy {
  minLength: number;
  maxLength: number;
  requireUppercase: boolean;
  requireLowercase: boolean;
  requireDigit: boolean;
  requireSpecial: boolean;
}

/**
 * Safe password operations.
 */
export class SafePassword {
  /** NIST SP 800-63B compliant policy. */
  static nistPolicy(): PasswordPolicy {
    return {
      minLength: 8,
      maxLength: 64,
      requireUppercase: false,
      requireLowercase: false,
      requireDigit: false,
      requireSpecial: false,
    };
  }

  /** PCI-DSS compliant policy. */
  static pciDssPolicy(): PasswordPolicy {
    return {
      minLength: 7,
      maxLength: 128,
      requireUppercase: true,
      requireLowercase: true,
      requireDigit: true,
      requireSpecial: false,
    };
  }

  /** HIPAA compliant policy. */
  static hipaaPolicy(): PasswordPolicy {
    return {
      minLength: 8,
      maxLength: 128,
      requireUppercase: true,
      requireLowercase: true,
      requireDigit: true,
      requireSpecial: true,
    };
  }

  /** Validate password against a policy. */
  static validate(password: string, policy: PasswordPolicy): Result<void> {
    if (password.length < policy.minLength) {
      return err(`Password must be at least ${policy.minLength} characters`);
    }
    if (password.length > policy.maxLength) {
      return err(`Password must be at most ${policy.maxLength} characters`);
    }
    if (policy.requireUppercase && !/[A-Z]/.test(password)) {
      return err('Password must contain uppercase letter');
    }
    if (policy.requireLowercase && !/[a-z]/.test(password)) {
      return err('Password must contain lowercase letter');
    }
    if (policy.requireDigit && !/[0-9]/.test(password)) {
      return err('Password must contain digit');
    }
    if (policy.requireSpecial && !/[^a-zA-Z0-9]/.test(password)) {
      return err('Password must contain special character');
    }
    return ok(undefined);
  }

  /** Check password strength. */
  static strength(password: string): PasswordStrength {
    const len = password.length;
    const hasUpper = /[A-Z]/.test(password);
    const hasLower = /[a-z]/.test(password);
    const hasDigit = /[0-9]/.test(password);
    const hasSpecial = /[^a-zA-Z0-9]/.test(password);

    const variety = [hasUpper, hasLower, hasDigit, hasSpecial].filter(Boolean).length;

    if (len <= 5) return PasswordStrength.VeryWeak;
    if (len <= 7) return variety <= 1 ? PasswordStrength.VeryWeak : PasswordStrength.Weak;
    if (len <= 11) return variety <= 2 ? PasswordStrength.Weak : PasswordStrength.Fair;
    if (len <= 15) return variety <= 2 ? PasswordStrength.Fair : PasswordStrength.Strong;
    return variety >= 4 ? PasswordStrength.VeryStrong : PasswordStrength.Strong;
  }

  /** Check for common password patterns. */
  static hasCommonPattern(password: string): boolean {
    const common = [
      'password',
      '123456',
      'qwerty',
      'admin',
      'letmein',
      'welcome',
      'monkey',
      'dragon',
      'master',
      'login',
    ];
    const lower = password.toLowerCase();
    return common.some((p) => lower.includes(p));
  }
}
