// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

export enum PasswordStrength {
  VeryWeak = 0,
  Weak = 1,
  Fair = 2,
  Strong = 3,
  VeryStrong = 4,
}

export interface PasswordAnalysis {
  length: number;
  hasLowercase: boolean;
  hasUppercase: boolean;
  hasDigits: boolean;
  hasSpecial: boolean;
  hasCommonPatterns: boolean;
  entropy: number;
  strength: PasswordStrength;
  score: number;
}

export interface PasswordPolicy {
  minLength?: number;
  maxLength?: number;
  requireLowercase?: boolean;
  requireUppercase?: boolean;
  requireDigits?: boolean;
  requireSpecial?: boolean;
  forbidCommonPasswords?: boolean;
  minEntropy?: number;
  minStrength?: PasswordStrength;
}

const COMMON_PASSWORDS: ReadonlySet<string> = new Set([
  'password', '123456', '12345678', 'qwerty', 'abc123', 'monkey', '1234567',
  'letmein', 'trustno1', 'dragon', 'baseball', 'iloveyou', 'master', 'sunshine',
  'ashley', 'football', 'shadow', 'passw0rd', 'admin', 'welcome', 'login',
]);

const COMMON_PATTERNS: RegExp[] = [
  /^(.)\1+$/,                    // Repeated character
  /^(012|123|234|345|456|567|678|789)+/, // Sequential numbers
  /^(abc|bcd|cde|def|efg|fgh|ghi|hij|ijk|jkl|klm|lmn|mno|nop|opq|pqr|qrs|rst|stu|tuv|uvw|vwx|wxy|xyz)+/i, // Sequential letters
  /^(qwerty|asdfgh|zxcvbn)/i,   // Keyboard patterns
];

/**
 * Calculate password entropy.
 */
function calculateEntropy(password: string): number {
  let charsetSize = 0;

  if (/[a-z]/.test(password)) charsetSize += 26;
  if (/[A-Z]/.test(password)) charsetSize += 26;
  if (/[0-9]/.test(password)) charsetSize += 10;
  if (/[^a-zA-Z0-9]/.test(password)) charsetSize += 32;

  if (charsetSize === 0) return 0;

  return password.length * Math.log2(charsetSize);
}

/**
 * Analyze password strength and characteristics.
 */
export function analyzePassword(password: string): PasswordAnalysis {
  const hasLowercase = /[a-z]/.test(password);
  const hasUppercase = /[A-Z]/.test(password);
  const hasDigits = /[0-9]/.test(password);
  const hasSpecial = /[^a-zA-Z0-9]/.test(password);

  const hasCommonPatterns = COMMON_PATTERNS.some((p) => p.test(password)) ||
    COMMON_PASSWORDS.has(password.toLowerCase());

  const entropy = calculateEntropy(password);

  // Calculate score (0-100)
  let score = 0;

  // Length contribution (up to 30 points)
  score += Math.min(password.length * 2, 30);

  // Character variety (up to 40 points)
  if (hasLowercase) score += 10;
  if (hasUppercase) score += 10;
  if (hasDigits) score += 10;
  if (hasSpecial) score += 10;

  // Entropy bonus (up to 30 points)
  score += Math.min(entropy / 2, 30);

  // Penalties
  if (hasCommonPatterns) score = Math.max(0, score - 30);
  if (password.length < 8) score = Math.max(0, score - 20);

  score = Math.min(100, Math.max(0, score));

  // Determine strength
  let strength: PasswordStrength;
  if (score < 20) strength = PasswordStrength.VeryWeak;
  else if (score < 40) strength = PasswordStrength.Weak;
  else if (score < 60) strength = PasswordStrength.Fair;
  else if (score < 80) strength = PasswordStrength.Strong;
  else strength = PasswordStrength.VeryStrong;

  return {
    length: password.length,
    hasLowercase,
    hasUppercase,
    hasDigits,
    hasSpecial,
    hasCommonPatterns,
    entropy,
    strength,
    score,
  };
}

/**
 * Validate password against a policy.
 */
export function validatePassword(password: string, policy: PasswordPolicy): string[] {
  const errors: string[] = [];
  const analysis = analyzePassword(password);

  if (policy.minLength !== undefined && password.length < policy.minLength) {
    errors.push(`Password must be at least ${policy.minLength} characters`);
  }

  if (policy.maxLength !== undefined && password.length > policy.maxLength) {
    errors.push(`Password must be at most ${policy.maxLength} characters`);
  }

  if (policy.requireLowercase && !analysis.hasLowercase) {
    errors.push('Password must contain a lowercase letter');
  }

  if (policy.requireUppercase && !analysis.hasUppercase) {
    errors.push('Password must contain an uppercase letter');
  }

  if (policy.requireDigits && !analysis.hasDigits) {
    errors.push('Password must contain a digit');
  }

  if (policy.requireSpecial && !analysis.hasSpecial) {
    errors.push('Password must contain a special character');
  }

  if (policy.forbidCommonPasswords && COMMON_PASSWORDS.has(password.toLowerCase())) {
    errors.push('Password is too common');
  }

  if (policy.minEntropy !== undefined && analysis.entropy < policy.minEntropy) {
    errors.push(`Password entropy must be at least ${policy.minEntropy}`);
  }

  if (policy.minStrength !== undefined && analysis.strength < policy.minStrength) {
    errors.push('Password is not strong enough');
  }

  return errors;
}

/**
 * Check if password meets policy.
 */
export function meetsPolicy(password: string, policy: PasswordPolicy): boolean {
  return validatePassword(password, policy).length === 0;
}

/**
 * Generate a random password.
 */
export function generatePassword(length: number = 16, options: {
  includeLowercase?: boolean;
  includeUppercase?: boolean;
  includeDigits?: boolean;
  includeSpecial?: boolean;
} = {}): string {
  const {
    includeLowercase = true,
    includeUppercase = true,
    includeDigits = true,
    includeSpecial = true,
  } = options;

  let charset = '';
  if (includeLowercase) charset += 'abcdefghijklmnopqrstuvwxyz';
  if (includeUppercase) charset += 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  if (includeDigits) charset += '0123456789';
  if (includeSpecial) charset += '!@#$%^&*()_+-=[]{}|;:,.<>?';

  if (charset.length === 0) {
    charset = 'abcdefghijklmnopqrstuvwxyz';
  }

  const array = new Uint32Array(length);
  crypto.getRandomValues(array);

  let password = '';
  for (let i = 0; i < length; i++) {
    password += charset[array[i] % charset.length];
  }

  return password;
}

export const DEFAULT_POLICY: PasswordPolicy = {
  minLength: 8,
  requireLowercase: true,
  requireUppercase: true,
  requireDigits: true,
  requireSpecial: false,
  forbidCommonPasswords: true,
  minStrength: PasswordStrength.Fair,
};

export const STRONG_POLICY: PasswordPolicy = {
  minLength: 12,
  requireLowercase: true,
  requireUppercase: true,
  requireDigits: true,
  requireSpecial: true,
  forbidCommonPasswords: true,
  minEntropy: 50,
  minStrength: PasswordStrength.Strong,
};

export const SafePassword = {
  PasswordStrength,
  analyze: analyzePassword,
  validate: validatePassword,
  meetsPolicy,
  generate: generatePassword,
  DEFAULT_POLICY,
  STRONG_POLICY,
};
