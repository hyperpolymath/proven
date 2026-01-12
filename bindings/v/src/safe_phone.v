// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

module proven

import strings

// Country calling codes (ITU-T E.164).
pub enum CountryCode {
	us      // +1 USA, Canada
	ru      // +7 Russia
	eg      // +20 Egypt
	za      // +27 South Africa
	nl      // +31 Netherlands
	be      // +32 Belgium
	fr      // +33 France
	es      // +34 Spain
	it      // +39 Italy
	ch      // +41 Switzerland
	at      // +43 Austria
	uk      // +44 United Kingdom
	dk      // +45 Denmark
	se      // +46 Sweden
	no      // +47 Norway
	de      // +49 Germany
	mx      // +52 Mexico
	br      // +55 Brazil
	au      // +61 Australia
	nz      // +64 New Zealand
	sg      // +65 Singapore
	jp      // +81 Japan
	kr      // +82 South Korea
	cn      // +86 China
	in_     // +91 India (in is a keyword)
	unknown // Unknown/unrecognized
}

// Get numeric value of country code.
pub fn (cc CountryCode) value() u16 {
	return match cc {
		.us { 1 }
		.ru { 7 }
		.eg { 20 }
		.za { 27 }
		.nl { 31 }
		.be { 32 }
		.fr { 33 }
		.es { 34 }
		.it { 39 }
		.ch { 41 }
		.at { 43 }
		.uk { 44 }
		.dk { 45 }
		.se { 46 }
		.no { 47 }
		.de { 49 }
		.mx { 52 }
		.br { 55 }
		.au { 61 }
		.nz { 64 }
		.sg { 65 }
		.jp { 81 }
		.kr { 82 }
		.cn { 86 }
		.in_ { 91 }
		.unknown { 0 }
	}
}

// Get ISO 3166-1 alpha-2 country code.
pub fn (cc CountryCode) iso_code() string {
	return match cc {
		.us { 'US' }
		.ru { 'RU' }
		.eg { 'EG' }
		.za { 'ZA' }
		.nl { 'NL' }
		.be { 'BE' }
		.fr { 'FR' }
		.es { 'ES' }
		.it { 'IT' }
		.ch { 'CH' }
		.at { 'AT' }
		.uk { 'GB' }
		.dk { 'DK' }
		.se { 'SE' }
		.no { 'NO' }
		.de { 'DE' }
		.mx { 'MX' }
		.br { 'BR' }
		.au { 'AU' }
		.nz { 'NZ' }
		.sg { 'SG' }
		.jp { 'JP' }
		.kr { 'KR' }
		.cn { 'CN' }
		.in_ { 'IN' }
		.unknown { '' }
	}
}

// Get country name.
pub fn (cc CountryCode) country_name() string {
	return match cc {
		.us { 'United States' }
		.ru { 'Russia' }
		.eg { 'Egypt' }
		.za { 'South Africa' }
		.nl { 'Netherlands' }
		.be { 'Belgium' }
		.fr { 'France' }
		.es { 'Spain' }
		.it { 'Italy' }
		.ch { 'Switzerland' }
		.at { 'Austria' }
		.uk { 'United Kingdom' }
		.dk { 'Denmark' }
		.se { 'Sweden' }
		.no { 'Norway' }
		.de { 'Germany' }
		.mx { 'Mexico' }
		.br { 'Brazil' }
		.au { 'Australia' }
		.nz { 'New Zealand' }
		.sg { 'Singapore' }
		.jp { 'Japan' }
		.kr { 'South Korea' }
		.cn { 'China' }
		.in_ { 'India' }
		.unknown { 'Unknown' }
	}
}

// Parse country code from numeric value.
pub fn country_code_from_value(value u16) CountryCode {
	return match value {
		1 { .us }
		7 { .ru }
		20 { .eg }
		27 { .za }
		31 { .nl }
		32 { .be }
		33 { .fr }
		34 { .es }
		39 { .it }
		41 { .ch }
		43 { .at }
		44 { .uk }
		45 { .dk }
		46 { .se }
		47 { .no }
		49 { .de }
		52 { .mx }
		55 { .br }
		61 { .au }
		64 { .nz }
		65 { .sg }
		81 { .jp }
		82 { .kr }
		86 { .cn }
		91 { .in_ }
		else { .unknown }
	}
}

// Parse country code from ISO 3166-1 alpha-2 code.
pub fn country_code_from_iso(code string) CountryCode {
	upper := code.to_upper()
	return match upper {
		'US', 'CA' { .us }  // US and Canada share +1
		'RU' { .ru }
		'EG' { .eg }
		'ZA' { .za }
		'NL' { .nl }
		'BE' { .be }
		'FR' { .fr }
		'ES' { .es }
		'IT' { .it }
		'CH' { .ch }
		'AT' { .at }
		'GB', 'UK' { .uk }
		'DK' { .dk }
		'SE' { .se }
		'NO' { .no }
		'DE' { .de }
		'MX' { .mx }
		'BR' { .br }
		'AU' { .au }
		'NZ' { .nz }
		'SG' { .sg }
		'JP' { .jp }
		'KR' { .kr }
		'CN' { .cn }
		'IN' { .in_ }
		else { .unknown }
	}
}

// Validated phone number.
pub struct PhoneNumber {
pub:
	country_code    CountryCode // Country calling code
	national_number string      // National significant number
}

// Create a phone number.
pub fn phone_number_new(country_code CountryCode, national_number string) PhoneNumber {
	return PhoneNumber{
		country_code: country_code
		national_number: national_number
	}
}

// Format in E.164 format (+[country][national]).
pub fn (phone PhoneNumber) to_e164() string {
	return '+${phone.country_code.value()}${phone.national_number}'
}

// Format in international format with spaces.
pub fn (phone PhoneNumber) to_international() string {
	cc := phone.country_code.value()
	nat := phone.national_number
	len := nat.len

	if len <= 4 {
		return '+${cc} ${nat}'
	} else if len <= 7 {
		return '+${cc} ${nat[..3]} ${nat[3..]}'
	} else if len <= 10 {
		return '+${cc} ${nat[..3]} ${nat[3..6]} ${nat[6..]}'
	}
	return '+${cc} ${nat}'
}

// Format in national format (no country code).
pub fn (phone PhoneNumber) to_national() string {
	nat := phone.national_number
	len := nat.len

	if len <= 4 {
		return nat
	} else if len <= 7 {
		return '${nat[..3]}-${nat[3..]}'
	} else if len <= 10 {
		return '(${nat[..3]}) ${nat[3..6]}-${nat[6..]}'
	}
	return nat
}

// Get total digit count (including country code).
pub fn (phone PhoneNumber) digit_count() int {
	cc_value := phone.country_code.value()
	cc_digits := if cc_value >= 100 {
		3
	} else if cc_value >= 10 {
		2
	} else {
		1
	}
	return cc_digits + phone.national_number.len
}

// String representation (E.164 format).
pub fn (phone PhoneNumber) str() string {
	return phone.to_e164()
}

// Check equality.
pub fn (phone PhoneNumber) equals(other PhoneNumber) bool {
	return phone.country_code == other.country_code && phone.national_number == other.national_number
}

// Parse phone number from string.
pub fn parse_phone(input string) ?PhoneNumber {
	trimmed := input.trim_space()
	if trimmed.len == 0 {
		return none
	}

	// Extract digits only
	mut digits := strings.new_builder(trimmed.len)
	for c in trimmed {
		if c >= `0` && c <= `9` {
			digits.write_u8(c)
		}
	}
	digits_str := digits.str()

	if digits_str.len < 7 {
		return none // Phone number too short
	}
	if digits_str.len > 15 {
		return none // Phone number too long (E.164 limit)
	}

	// Try to parse country code (2-digit, 1-digit order)
	// Note: We try common patterns first
	cc, national_start := parse_country_code_from_digits(digits_str) or { return none }

	if digits_str.len - national_start < 4 {
		return none // National number too short
	}

	return PhoneNumber{
		country_code: cc
		national_number: digits_str[national_start..]
	}
}

// Helper: parse country code from beginning of digit string.
fn parse_country_code_from_digits(digits string) ?(CountryCode, int) {
	// Try 2-digit codes first, then 1-digit
	for len in [2, 1] {
		if digits.len >= len {
			value_str := digits[..len]
			value := value_str.u16()
			cc := country_code_from_value(value)
			if cc != .unknown {
				return cc, len
			}
		}
	}
	return none
}

// Check if string is a valid phone number.
pub fn is_valid_phone(input string) bool {
	_ := parse_phone(input) or { return false }
	return true
}

// Normalize phone number to E.164 format.
pub fn normalize_phone(input string) ?string {
	phone := parse_phone(input) or { return none }
	return phone.to_e164()
}

// Extract digits from phone string.
pub fn extract_phone_digits(input string) string {
	mut builder := strings.new_builder(input.len)
	for c in input {
		if c >= `0` && c <= `9` {
			builder.write_u8(c)
		}
	}
	return builder.str()
}

// Mask phone number for display (show last 4 digits).
pub fn mask_phone(input string) ?string {
	phone := parse_phone(input) or { return none }
	nat := phone.national_number

	if nat.len <= 4 {
		return '+${phone.country_code.value()} ****'
	}

	visible := nat[nat.len - 4..]
	masked_len := nat.len - 4
	mut masked := strings.new_builder(masked_len + 10)
	masked.write_string('+${phone.country_code.value()} ')
	for _ in 0 .. masked_len {
		masked.write_u8(`*`)
	}
	masked.write_string(visible)
	return masked.str()
}

// Compare two phone numbers.
pub fn phones_equal(phone1 string, phone2 string) bool {
	p1 := parse_phone(phone1) or { return false }
	p2 := parse_phone(phone2) or { return false }
	return p1.equals(p2)
}
