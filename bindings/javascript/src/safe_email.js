// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafeEmail - Email validation that cannot crash.
 *
 * Provides safe email parsing and validation without regex catastrophic backtracking.
 * @module
 */

import { ok, err } from './result.js';

/**
 * Parsed email address components.
 * @typedef {Object} ParsedEmail
 * @property {string} local - Local part (before @)
 * @property {string} domain - Domain part (after @)
 * @property {string} normalized - Normalized email address
 */

/**
 * Safe email operations.
 */
export class SafeEmail {
  /**
   * Validate and parse an email address.
   *
   * @param {string} email - Email address to validate
   * @returns {{ ok: true, value: ParsedEmail } | { ok: false, error: string }}
   *
   * @example
   * SafeEmail.parse("user@example.com")
   */
  static parse(email) {
    if (typeof email !== 'string') {
      return err('Email must be a string');
    }

    const trimmed = email.trim();

    if (trimmed.length === 0) {
      return err('Email cannot be empty');
    }

    if (trimmed.length > 254) {
      return err('Email too long');
    }

    const atIndex = trimmed.lastIndexOf('@');
    if (atIndex === -1) {
      return err('Email must contain @');
    }

    const local = trimmed.slice(0, atIndex);
    const domain = trimmed.slice(atIndex + 1);

    // Validate local part
    if (local.length === 0) {
      return err('Local part cannot be empty');
    }

    if (local.length > 64) {
      return err('Local part too long');
    }

    if (local.startsWith('.') || local.endsWith('.')) {
      return err('Local part cannot start or end with dot');
    }

    if (local.includes('..')) {
      return err('Local part cannot have consecutive dots');
    }

    // Validate domain
    if (domain.length === 0) {
      return err('Domain cannot be empty');
    }

    if (domain.length > 253) {
      return err('Domain too long');
    }

    if (domain.startsWith('.') || domain.endsWith('.')) {
      return err('Domain cannot start or end with dot');
    }

    if (domain.startsWith('-') || domain.endsWith('-')) {
      return err('Domain cannot start or end with hyphen');
    }

    if (!domain.includes('.')) {
      return err('Domain must have at least one dot');
    }

    // Check for valid domain characters
    const domainParts = domain.split('.');
    for (const part of domainParts) {
      if (part.length === 0) {
        return err('Domain cannot have empty labels');
      }
      if (part.length > 63) {
        return err('Domain label too long');
      }
      if (!/^[a-zA-Z0-9]([a-zA-Z0-9-]*[a-zA-Z0-9])?$/.test(part) && part.length > 1) {
        // Allow single character labels
        if (!/^[a-zA-Z0-9]$/.test(part)) {
          return err('Invalid domain label');
        }
      }
    }

    return ok({
      local,
      domain: domain.toLowerCase(),
      normalized: `${local}@${domain.toLowerCase()}`,
    });
  }

  /**
   * Check if string is a valid email.
   *
   * @param {string} email - Email to validate
   * @returns {boolean}
   */
  static isValid(email) {
    return SafeEmail.parse(email).ok;
  }

  /**
   * Normalize an email address.
   *
   * @param {string} email - Email to normalize
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   */
  static normalize(email) {
    const result = SafeEmail.parse(email);
    if (!result.ok) {
      return result;
    }
    return ok(result.value.normalized);
  }

  /**
   * Get the domain from an email address.
   *
   * @param {string} email - Email address
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   */
  static getDomain(email) {
    const result = SafeEmail.parse(email);
    if (!result.ok) {
      return result;
    }
    return ok(result.value.domain);
  }

  /**
   * Get the local part from an email address.
   *
   * @param {string} email - Email address
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   */
  static getLocalPart(email) {
    const result = SafeEmail.parse(email);
    if (!result.ok) {
      return result;
    }
    return ok(result.value.local);
  }

  /**
   * Check if email has a plus-addressed tag.
   *
   * @param {string} email - Email address
   * @returns {boolean}
   */
  static hasTag(email) {
    const result = SafeEmail.parse(email);
    if (!result.ok) {
      return false;
    }
    return result.value.local.includes('+');
  }

  /**
   * Get the base email (without plus-address tag).
   *
   * @param {string} email - Email address
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   */
  static getBaseEmail(email) {
    const result = SafeEmail.parse(email);
    if (!result.ok) {
      return result;
    }
    const local = result.value.local.split('+')[0];
    return ok(`${local}@${result.value.domain}`);
  }

  /**
   * Check if two emails are equivalent (ignoring plus-addressing and case).
   *
   * @param {string} email1 - First email
   * @param {string} email2 - Second email
   * @returns {boolean}
   */
  static areEquivalent(email1, email2) {
    const base1 = SafeEmail.getBaseEmail(email1);
    const base2 = SafeEmail.getBaseEmail(email2);
    if (!base1.ok || !base2.ok) {
      return false;
    }
    return base1.value.toLowerCase() === base2.value.toLowerCase();
  }
}
