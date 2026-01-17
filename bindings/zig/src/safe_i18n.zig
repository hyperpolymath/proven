// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe internationalization utilities for locale and language tag validation.
//! Provides BCP 47 language tag parsing and validation that cannot crash.

const std = @import("std");

/// Error types for i18n operations.
pub const I18nError = error{
    InvalidLanguageTag,
    InvalidLocale,
    InvalidLanguageCode,
    InvalidCountryCode,
    InvalidScriptCode,
    InvalidVariant,
    OutOfMemory,
};

/// ISO 639-1 two-letter language codes (subset of common languages).
pub const LanguageCode = enum {
    aa, ab, af, am, ar, as_, az,
    ba, be, bg, bh, bi, bn, bo, br, bs,
    ca, co, cs, cy,
    da, de, dz,
    el, en, eo, es, et, eu,
    fa, fi, fj, fo, fr, fy,
    ga, gd, gl, gn, gu, gv,
    ha, he, hi, ho, hr, ht, hu, hy,
    ia, id, ie, ig, ii, ik, @"in", is, it, iu,
    ja, jv,
    ka, kg, ki, kk, kl, km, kn, ko, kr, ks, ku, kv, kw, ky,
    la, lb, lg, li, ln, lo, lt, lu, lv,
    mg, mh, mi, mk, ml, mn, mr, ms, mt, my,
    na, nb, nd, ne, ng, nl, nn, no, nr, nv, ny,
    oc, oj, om, or_, os,
    pa, pi, pl, ps, pt,
    qu,
    rm, rn, ro, ru, rw,
    sa, sc, sd, se, sg, si, sk, sl, sm, sn, so, sq, sr, ss, st, su, sv, sw,
    ta, te, tg, th, ti, tk, tl, tn, to, tr, ts, tt, tw, ty,
    ug, uk, ur, uz,
    ve, vi, vo,
    wa, wo,
    xh,
    yi, yo,
    za, zh, zu,

    /// Get the language name in English.
    pub fn name(self: LanguageCode) []const u8 {
        return switch (self) {
            .en => "English",
            .es => "Spanish",
            .fr => "French",
            .de => "German",
            .it => "Italian",
            .pt => "Portuguese",
            .ru => "Russian",
            .zh => "Chinese",
            .ja => "Japanese",
            .ko => "Korean",
            .ar => "Arabic",
            .hi => "Hindi",
            .bn => "Bengali",
            .nl => "Dutch",
            .pl => "Polish",
            .tr => "Turkish",
            .vi => "Vietnamese",
            .th => "Thai",
            .sv => "Swedish",
            .no => "Norwegian",
            .da => "Danish",
            .fi => "Finnish",
            .cs => "Czech",
            .el => "Greek",
            .he => "Hebrew",
            .hu => "Hungarian",
            .id => "Indonesian",
            .ms => "Malay",
            .ro => "Romanian",
            .uk => "Ukrainian",
            else => @tagName(self),
        };
    }

    /// Get the two-letter code as string.
    pub fn toString(self: LanguageCode) []const u8 {
        return @tagName(self);
    }

    /// Check if this language uses right-to-left script by default.
    pub fn isRtl(self: LanguageCode) bool {
        return switch (self) {
            .ar, .he, .fa, .ur, .yi => true,
            else => false,
        };
    }
};

/// ISO 3166-1 alpha-2 country codes (subset of common countries).
pub const CountryCode = enum {
    AD, AE, AF, AG, AI, AL, AM, AO, AQ, AR, AS, AT, AU, AW, AX, AZ,
    BA, BB, BD, BE, BF, BG, BH, BI, BJ, BL, BM, BN, BO, BQ, BR, BS, BT, BV, BW, BY, BZ,
    CA, CC, CD, CF, CG, CH, CI, CK, CL, CM, CN, CO, CR, CU, CV, CW, CX, CY, CZ,
    DE, DJ, DK, DM, DO, DZ,
    EC, EE, EG, EH, ER, ES, ET,
    FI, FJ, FK, FM, FO, FR,
    GA, GB, GD, GE, GF, GG, GH, GI, GL, GM, GN, GP, GQ, GR, GS, GT, GU, GW, GY,
    HK, HM, HN, HR, HT, HU,
    ID, IE, IL, IM, IN, IO, IQ, IR, IS, IT,
    JE, JM, JO, JP,
    KE, KG, KH, KI, KM, KN, KP, KR, KW, KY, KZ,
    LA, LB, LC, LI, LK, LR, LS, LT, LU, LV, LY,
    MA, MC, MD, ME, MF, MG, MH, MK, ML, MM, MN, MO, MP, MQ, MR, MS, MT, MU, MV, MW, MX, MY, MZ,
    NA, NC, NE, NF, NG, NI, NL, NO, NP, NR, NU, NZ,
    OM,
    PA, PE, PF, PG, PH, PK, PL, PM, PN, PR, PS, PT, PW, PY,
    QA,
    RE, RO, RS, RU, RW,
    SA, SB, SC, SD, SE, SG, SH, SI, SJ, SK, SL, SM, SN, SO, SR, SS, ST, SV, SX, SY, SZ,
    TC, TD, TF, TG, TH, TJ, TK, TL, TM, TN, TO, TR, TT, TV, TW, TZ,
    UA, UG, UM, US, UY, UZ,
    VA, VC, VE, VG, VI, VN, VU,
    WF, WS,
    YE, YT,
    ZA, ZM, ZW,

    /// Get the country name in English.
    pub fn name(self: CountryCode) []const u8 {
        return switch (self) {
            .US => "United States",
            .GB => "United Kingdom",
            .CA => "Canada",
            .AU => "Australia",
            .DE => "Germany",
            .FR => "France",
            .ES => "Spain",
            .IT => "Italy",
            .JP => "Japan",
            .CN => "China",
            .KR => "South Korea",
            .IN => "India",
            .BR => "Brazil",
            .MX => "Mexico",
            .RU => "Russia",
            .NL => "Netherlands",
            .SE => "Sweden",
            .NO => "Norway",
            .DK => "Denmark",
            .FI => "Finland",
            .PL => "Poland",
            .TR => "Turkey",
            .SA => "Saudi Arabia",
            .AE => "United Arab Emirates",
            .IL => "Israel",
            .SG => "Singapore",
            .HK => "Hong Kong",
            .TW => "Taiwan",
            .NZ => "New Zealand",
            .ZA => "South Africa",
            else => @tagName(self),
        };
    }

    /// Get the two-letter code as string.
    pub fn toString(self: CountryCode) []const u8 {
        return @tagName(self);
    }
};

/// ISO 15924 script codes (subset of common scripts).
pub const ScriptCode = enum {
    Arab, // Arabic
    Armn, // Armenian
    Beng, // Bengali
    Cyrl, // Cyrillic
    Deva, // Devanagari
    Geor, // Georgian
    Grek, // Greek
    Gujr, // Gujarati
    Guru, // Gurmukhi
    Hang, // Hangul
    Hani, // Han (Chinese)
    Hans, // Simplified Han
    Hant, // Traditional Han
    Hebr, // Hebrew
    Jpan, // Japanese
    Kana, // Katakana
    Khmr, // Khmer
    Knda, // Kannada
    Kore, // Korean
    Laoo, // Lao
    Latn, // Latin
    Mlym, // Malayalam
    Mymr, // Myanmar
    Orya, // Oriya
    Sinh, // Sinhala
    Taml, // Tamil
    Telu, // Telugu
    Thai, // Thai
    Tibt, // Tibetan

    /// Get the script name in English.
    pub fn name(self: ScriptCode) []const u8 {
        return switch (self) {
            .Latn => "Latin",
            .Cyrl => "Cyrillic",
            .Arab => "Arabic",
            .Hebr => "Hebrew",
            .Grek => "Greek",
            .Hans => "Simplified Chinese",
            .Hant => "Traditional Chinese",
            .Jpan => "Japanese",
            .Kore => "Korean",
            .Deva => "Devanagari",
            .Thai => "Thai",
            else => @tagName(self),
        };
    }

    /// Check if this script is right-to-left.
    pub fn isRtl(self: ScriptCode) bool {
        return switch (self) {
            .Arab, .Hebr => true,
            else => false,
        };
    }
};

/// A parsed BCP 47 language tag.
pub const LanguageTag = struct {
    language: []const u8,
    script: ?[]const u8 = null,
    region: ?[]const u8 = null,
    variants: ?[]const []const u8 = null,
    extensions: ?[]const u8 = null,
    private_use: ?[]const u8 = null,

    /// Format the language tag as a string.
    pub fn format(self: LanguageTag, buffer: []u8) []u8 {
        var pos: usize = 0;

        // Language
        @memcpy(buffer[pos..][0..self.language.len], self.language);
        pos += self.language.len;

        // Script
        if (self.script) |s| {
            buffer[pos] = '-';
            pos += 1;
            @memcpy(buffer[pos..][0..s.len], s);
            pos += s.len;
        }

        // Region
        if (self.region) |r| {
            buffer[pos] = '-';
            pos += 1;
            @memcpy(buffer[pos..][0..r.len], r);
            pos += r.len;
        }

        return buffer[0..pos];
    }

    /// Check if this tag represents a right-to-left language.
    pub fn isRtl(self: LanguageTag) bool {
        // Check script first
        if (self.script) |s| {
            if (parseScriptCode(s)) |script| {
                return script.isRtl();
            }
        }
        // Fall back to language
        if (parseLanguageCode(self.language)) |lang| {
            return lang.isRtl();
        }
        return false;
    }

    /// Get a canonical form (lowercase language, titlecase script, uppercase region).
    pub fn canonicalize(self: LanguageTag, buffer: []u8) []u8 {
        var pos: usize = 0;

        // Language - lowercase
        for (self.language) |c| {
            buffer[pos] = std.ascii.toLower(c);
            pos += 1;
        }

        // Script - titlecase
        if (self.script) |s| {
            buffer[pos] = '-';
            pos += 1;
            if (s.len > 0) {
                buffer[pos] = std.ascii.toUpper(s[0]);
                pos += 1;
                for (s[1..]) |c| {
                    buffer[pos] = std.ascii.toLower(c);
                    pos += 1;
                }
            }
        }

        // Region - uppercase
        if (self.region) |r| {
            buffer[pos] = '-';
            pos += 1;
            for (r) |c| {
                buffer[pos] = std.ascii.toUpper(c);
                pos += 1;
            }
        }

        return buffer[0..pos];
    }
};

/// A locale combining language, country, and optional variant.
pub const Locale = struct {
    language: LanguageCode,
    country: ?CountryCode = null,
    script: ?ScriptCode = null,
    variant: ?[]const u8 = null,

    /// Format as BCP 47 language tag.
    pub fn toLanguageTag(self: Locale, buffer: []u8) []u8 {
        var pos: usize = 0;

        const lang = self.language.toString();
        @memcpy(buffer[pos..][0..lang.len], lang);
        pos += lang.len;

        if (self.script) |s| {
            buffer[pos] = '-';
            pos += 1;
            const script_str = @tagName(s);
            @memcpy(buffer[pos..][0..script_str.len], script_str);
            pos += script_str.len;
        }

        if (self.country) |c| {
            buffer[pos] = '-';
            pos += 1;
            const country_str = c.toString();
            @memcpy(buffer[pos..][0..country_str.len], country_str);
            pos += country_str.len;
        }

        return buffer[0..pos];
    }

    /// Format as POSIX locale string (e.g., "en_US").
    pub fn toPosix(self: Locale, buffer: []u8) []u8 {
        var pos: usize = 0;

        const lang = self.language.toString();
        @memcpy(buffer[pos..][0..lang.len], lang);
        pos += lang.len;

        if (self.country) |c| {
            buffer[pos] = '_';
            pos += 1;
            const country_str = c.toString();
            @memcpy(buffer[pos..][0..country_str.len], country_str);
            pos += country_str.len;
        }

        return buffer[0..pos];
    }

    /// Check if this locale uses right-to-left script.
    pub fn isRtl(self: Locale) bool {
        if (self.script) |s| {
            return s.isRtl();
        }
        return self.language.isRtl();
    }
};

/// Common locale constants.
pub const Locales = struct {
    pub const en_US = Locale{ .language = .en, .country = .US };
    pub const en_GB = Locale{ .language = .en, .country = .GB };
    pub const en_AU = Locale{ .language = .en, .country = .AU };
    pub const en_CA = Locale{ .language = .en, .country = .CA };
    pub const es_ES = Locale{ .language = .es, .country = .ES };
    pub const es_MX = Locale{ .language = .es, .country = .MX };
    pub const fr_FR = Locale{ .language = .fr, .country = .FR };
    pub const fr_CA = Locale{ .language = .fr, .country = .CA };
    pub const de_DE = Locale{ .language = .de, .country = .DE };
    pub const de_AT = Locale{ .language = .de, .country = .AT };
    pub const de_CH = Locale{ .language = .de, .country = .CH };
    pub const it_IT = Locale{ .language = .it, .country = .IT };
    pub const pt_BR = Locale{ .language = .pt, .country = .BR };
    pub const pt_PT = Locale{ .language = .pt, .country = .PT };
    pub const ru_RU = Locale{ .language = .ru, .country = .RU };
    pub const zh_CN = Locale{ .language = .zh, .country = .CN };
    pub const zh_TW = Locale{ .language = .zh, .country = .TW };
    pub const ja_JP = Locale{ .language = .ja, .country = .JP };
    pub const ko_KR = Locale{ .language = .ko, .country = .KR };
    pub const ar_SA = Locale{ .language = .ar, .country = .SA };
    pub const he_IL = Locale{ .language = .he, .country = .IL };
    pub const hi_IN = Locale{ .language = .hi, .country = .IN };
    pub const nl_NL = Locale{ .language = .nl, .country = .NL };
    pub const sv_SE = Locale{ .language = .sv, .country = .SE };
    pub const no_NO = Locale{ .language = .no, .country = .NO };
    pub const da_DK = Locale{ .language = .da, .country = .DK };
    pub const fi_FI = Locale{ .language = .fi, .country = .FI };
    pub const pl_PL = Locale{ .language = .pl, .country = .PL };
    pub const tr_TR = Locale{ .language = .tr, .country = .TR };
};

/// Parse a two-letter language code.
pub fn parseLanguageCode(str: []const u8) ?LanguageCode {
    if (str.len != 2) return null;
    var lower: [2]u8 = undefined;
    lower[0] = std.ascii.toLower(str[0]);
    lower[1] = std.ascii.toLower(str[1]);

    const fields = std.meta.fields(LanguageCode);
    inline for (fields) |field| {
        if (std.mem.eql(u8, &lower, field.name)) {
            return @field(LanguageCode, field.name);
        }
    }
    return null;
}

/// Parse a two-letter country code.
pub fn parseCountryCode(str: []const u8) ?CountryCode {
    if (str.len != 2) return null;
    var upper: [2]u8 = undefined;
    upper[0] = std.ascii.toUpper(str[0]);
    upper[1] = std.ascii.toUpper(str[1]);

    const fields = std.meta.fields(CountryCode);
    inline for (fields) |field| {
        if (std.mem.eql(u8, &upper, field.name)) {
            return @field(CountryCode, field.name);
        }
    }
    return null;
}

/// Parse a four-letter script code.
pub fn parseScriptCode(str: []const u8) ?ScriptCode {
    if (str.len != 4) return null;

    const fields = std.meta.fields(ScriptCode);
    inline for (fields) |field| {
        if (std.ascii.eqlIgnoreCase(str, field.name)) {
            return @field(ScriptCode, field.name);
        }
    }
    return null;
}

/// Parse a BCP 47 language tag.
pub fn parseLanguageTag(tag: []const u8) I18nError!LanguageTag {
    if (tag.len == 0) return error.InvalidLanguageTag;

    var it = std.mem.splitScalar(u8, tag, '-');

    // First subtag is always the language
    const language = it.next() orelse return error.InvalidLanguageTag;
    if (language.len < 2 or language.len > 3) return error.InvalidLanguageCode;

    // Validate language subtag (must be alphabetic)
    for (language) |c| {
        if (!std.ascii.isAlphabetic(c)) return error.InvalidLanguageCode;
    }

    var result = LanguageTag{ .language = language };

    // Parse remaining subtags
    while (it.next()) |subtag| {
        if (subtag.len == 0) continue;

        // Script (4 letters, alphabetic)
        if (subtag.len == 4 and result.script == null) {
            var is_script = true;
            for (subtag) |c| {
                if (!std.ascii.isAlphabetic(c)) {
                    is_script = false;
                    break;
                }
            }
            if (is_script) {
                result.script = subtag;
                continue;
            }
        }

        // Region (2 letters or 3 digits)
        if (result.region == null) {
            if (subtag.len == 2) {
                var is_alpha = true;
                for (subtag) |c| {
                    if (!std.ascii.isAlphabetic(c)) {
                        is_alpha = false;
                        break;
                    }
                }
                if (is_alpha) {
                    result.region = subtag;
                    continue;
                }
            } else if (subtag.len == 3) {
                var is_digit = true;
                for (subtag) |c| {
                    if (!std.ascii.isDigit(c)) {
                        is_digit = false;
                        break;
                    }
                }
                if (is_digit) {
                    result.region = subtag;
                    continue;
                }
            }
        }

        // Private use
        if (subtag.len == 1 and subtag[0] == 'x') {
            // Rest is private use
            break;
        }
    }

    return result;
}

/// Parse a POSIX locale string (e.g., "en_US", "en_US.UTF-8").
pub fn parseLocale(str: []const u8) I18nError!Locale {
    if (str.len == 0) return error.InvalidLocale;

    // Strip encoding suffix if present
    var locale_str = str;
    if (std.mem.indexOfScalar(u8, str, '.')) |dot_pos| {
        locale_str = str[0..dot_pos];
    }

    // Split by underscore
    var it = std.mem.splitScalar(u8, locale_str, '_');

    // Language
    const lang_str = it.next() orelse return error.InvalidLocale;
    const language = parseLanguageCode(lang_str) orelse return error.InvalidLanguageCode;

    var result = Locale{ .language = language };

    // Country (optional)
    if (it.next()) |country_str| {
        result.country = parseCountryCode(country_str);
    }

    return result;
}

/// Check if a string is a valid BCP 47 language tag.
pub fn isValidLanguageTag(tag: []const u8) bool {
    _ = parseLanguageTag(tag) catch return false;
    return true;
}

/// Check if a string is a valid POSIX locale.
pub fn isValidLocale(str: []const u8) bool {
    _ = parseLocale(str) catch return false;
    return true;
}

/// Check if a string is a valid two-letter language code.
pub fn isValidLanguageCode(str: []const u8) bool {
    return parseLanguageCode(str) != null;
}

/// Check if a string is a valid two-letter country code.
pub fn isValidCountryCode(str: []const u8) bool {
    return parseCountryCode(str) != null;
}

test "parseLanguageCode" {
    try std.testing.expect(parseLanguageCode("en") == .en);
    try std.testing.expect(parseLanguageCode("EN") == .en);
    try std.testing.expect(parseLanguageCode("xx") == null);
    try std.testing.expect(parseLanguageCode("english") == null);
}

test "parseCountryCode" {
    try std.testing.expect(parseCountryCode("US") == .US);
    try std.testing.expect(parseCountryCode("us") == .US);
    try std.testing.expect(parseCountryCode("XX") == null);
}

test "parseLanguageTag" {
    const tag1 = try parseLanguageTag("en-US");
    try std.testing.expectEqualStrings("en", tag1.language);
    try std.testing.expectEqualStrings("US", tag1.region.?);

    const tag2 = try parseLanguageTag("zh-Hans-CN");
    try std.testing.expectEqualStrings("zh", tag2.language);
    try std.testing.expectEqualStrings("Hans", tag2.script.?);
    try std.testing.expectEqualStrings("CN", tag2.region.?);
}

test "parseLocale" {
    const locale = try parseLocale("en_US");
    try std.testing.expect(locale.language == .en);
    try std.testing.expect(locale.country == .US);

    const locale2 = try parseLocale("en_US.UTF-8");
    try std.testing.expect(locale2.language == .en);
    try std.testing.expect(locale2.country == .US);
}

test "Locale formatting" {
    var buffer: [32]u8 = undefined;

    const locale = Locales.en_US;
    const bcp47 = locale.toLanguageTag(&buffer);
    try std.testing.expectEqualStrings("en-US", bcp47);

    const posix = locale.toPosix(&buffer);
    try std.testing.expectEqualStrings("en_US", posix);
}

test "RTL detection" {
    try std.testing.expect(LanguageCode.ar.isRtl());
    try std.testing.expect(LanguageCode.he.isRtl());
    try std.testing.expect(!LanguageCode.en.isRtl());

    try std.testing.expect(Locales.ar_SA.isRtl());
    try std.testing.expect(Locales.he_IL.isRtl());
    try std.testing.expect(!Locales.en_US.isRtl());
}

test "LanguageTag canonicalize" {
    const tag = LanguageTag{
        .language = "EN",
        .script = "latn",
        .region = "us",
    };
    var buffer: [32]u8 = undefined;
    const canonical = tag.canonicalize(&buffer);
    try std.testing.expectEqualStrings("en-Latn-US", canonical);
}
