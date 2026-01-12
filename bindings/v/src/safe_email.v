// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

module proven

import strings

// Result type for email parsing.
pub struct EmailResult {
pub:
	local_part string
	domain     string
	error      string
	ok         bool
}

// Create a successful EmailResult.
pub fn email_ok(local_part string, domain string) EmailResult {
	return EmailResult{
		local_part: local_part
		domain: domain
		error: ''
		ok: true
	}
}

// Create an error EmailResult.
pub fn email_error(message string) EmailResult {
	return EmailResult{
		local_part: ''
		domain: ''
		error: message
		ok: false
	}
}

// Common disposable email domains.
const disposable_domains = ['mailinator.com', 'guerrillamail.com', 'tempmail.com', 'throwaway.email',
	'10minutemail.com', 'fakeinbox.com', 'trashmail.com', 'maildrop.cc', 'yopmail.com',
	'sharklasers.com']

// Check if email format is valid.
pub fn is_valid_email(email string) bool {
	if email.len == 0 || email.len > 254 {
		return false
	}

	at_index := email.index('@') or { return false }

	if at_index == 0 || at_index == email.len - 1 {
		return false
	}

	local_part := email[..at_index]
	domain := email[at_index + 1..]

	if local_part.len > 64 || domain.len > 253 {
		return false
	}

	// Basic format validation
	if !is_valid_local_part(local_part) {
		return false
	}
	if !is_valid_domain(domain) {
		return false
	}

	return true
}

// Validate local part of email.
fn is_valid_local_part(local_part string) bool {
	if local_part.len == 0 {
		return false
	}

	allowed := "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.!#\$%&'*+/=?^_`{|}~-"
	for c in local_part {
		if !allowed.contains(c.ascii_str()) {
			return false
		}
	}
	return true
}

// Validate domain part of email.
fn is_valid_domain(domain string) bool {
	if domain.len == 0 || !domain.contains('.') {
		return false
	}

	labels := domain.split('.')
	for label in labels {
		if label.len == 0 || label.len > 63 {
			return false
		}
		// Must start and end with alphanumeric
		if !is_alphanumeric(label[0]) {
			return false
		}
		if !is_alphanumeric(label[label.len - 1]) {
			return false
		}
		// Middle characters can include hyphens
		for c in label {
			if !is_alphanumeric(c) && c != `-` {
				return false
			}
		}
	}
	return true
}

// Check if character is alphanumeric.
fn is_alphanumeric(c u8) bool {
	return (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || (c >= `0` && c <= `9`)
}

// Parse and validate email address.
pub fn parse_email(email string) EmailResult {
	if email.len == 0 {
		return email_error('Email is empty')
	}
	if email.len > 254 {
		return email_error('Email too long')
	}

	at_index := email.index('@') or { return email_error('Missing @ symbol') }

	local_part := email[..at_index]
	domain := email[at_index + 1..]

	if local_part.len == 0 {
		return email_error('Local part is empty')
	}
	if local_part.len > 64 {
		return email_error('Local part too long')
	}
	if domain.len == 0 {
		return email_error('Domain is empty')
	}
	if domain.len > 253 {
		return email_error('Domain too long')
	}

	if !is_valid_email(email) {
		return email_error('Invalid email format')
	}

	return email_ok(local_part, domain)
}

// Check if domain is a disposable email provider.
pub fn is_disposable_email(email string) bool {
	at_index := email.index('@') or { return false }
	domain := email[at_index + 1..].to_lower()
	return domain in proven.disposable_domains
}

// Normalize email address.
pub fn normalize_email(email string) ?string {
	result := parse_email(email)
	if !result.ok {
		return none
	}
	return '${result.local_part}@${result.domain.to_lower()}'
}

// Get domain from email.
pub fn get_email_domain(email string) ?string {
	at_index := email.index('@') or { return none }
	return email[at_index + 1..]
}

// Get local part from email.
pub fn get_local_part(email string) ?string {
	at_index := email.index('@') or { return none }
	return email[..at_index]
}

// Mask email for display.
pub fn mask_email(email string) ?string {
	result := parse_email(email)
	if !result.ok {
		return none
	}

	local := result.local_part
	domain := result.domain

	if local.len <= 1 {
		return '${local}***@${domain}'
	}
	return '${local[0..1]}***@${domain}'
}

// Check if two emails are equal (case-insensitive domain).
pub fn emails_equal(email1 string, email2 string) bool {
	norm1 := normalize_email(email1) or { return false }
	norm2 := normalize_email(email2) or { return false }
	return norm1 == norm2
}
