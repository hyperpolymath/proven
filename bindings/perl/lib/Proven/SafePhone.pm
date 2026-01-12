# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

package Proven::SafePhone;
use strict;
use warnings;
use Exporter 'import';

our $VERSION = '0.3.0';
our @EXPORT_OK = qw(
    parse format is_valid normalize get_country_code
    get_national_number is_mobile is_landline
    phone_number countries_list country_info
);

=head1 NAME

Proven::SafePhone - Safe phone number parsing and validation

=head1 SYNOPSIS

    use Proven::SafePhone qw(parse format is_valid);

    my $phone = parse('+1 (555) 123-4567', 'US');
    if (defined $phone) {
        print format($phone, 'e164');    # +15551234567
        print format($phone, 'national'); # (555) 123-4567
    }

=head1 DESCRIPTION

Provides safe phone number operations without exceptions.
Returns undef for invalid input.

=cut

# Country calling codes and metadata
# Format: code => { calling_code, name, example, mobile_prefix, landline_prefix, min_length, max_length }
my %COUNTRIES = (
    US => { calling_code => '1',  name => 'United States',  example => '2025551234',   min_length => 10, max_length => 10 },
    CA => { calling_code => '1',  name => 'Canada',         example => '4165551234',   min_length => 10, max_length => 10 },
    GB => { calling_code => '44', name => 'United Kingdom', example => '7911123456',   min_length => 10, max_length => 10 },
    DE => { calling_code => '49', name => 'Germany',        example => '15123456789',  min_length => 10, max_length => 11 },
    FR => { calling_code => '33', name => 'France',         example => '612345678',    min_length => 9,  max_length => 9 },
    IT => { calling_code => '39', name => 'Italy',          example => '3123456789',   min_length => 9,  max_length => 11 },
    ES => { calling_code => '34', name => 'Spain',          example => '612345678',    min_length => 9,  max_length => 9 },
    JP => { calling_code => '81', name => 'Japan',          example => '9012345678',   min_length => 10, max_length => 10 },
    CN => { calling_code => '86', name => 'China',          example => '13123456789',  min_length => 11, max_length => 11 },
    IN => { calling_code => '91', name => 'India',          example => '9876543210',   min_length => 10, max_length => 10 },
    AU => { calling_code => '61', name => 'Australia',      example => '412345678',    min_length => 9,  max_length => 9 },
    BR => { calling_code => '55', name => 'Brazil',         example => '11912345678',  min_length => 10, max_length => 11 },
    RU => { calling_code => '7',  name => 'Russia',         example => '9123456789',   min_length => 10, max_length => 10 },
    MX => { calling_code => '52', name => 'Mexico',         example => '5512345678',   min_length => 10, max_length => 10 },
    KR => { calling_code => '82', name => 'South Korea',    example => '1012345678',   min_length => 9,  max_length => 10 },
    NL => { calling_code => '31', name => 'Netherlands',    example => '612345678',    min_length => 9,  max_length => 9 },
    BE => { calling_code => '32', name => 'Belgium',        example => '470123456',    min_length => 9,  max_length => 9 },
    CH => { calling_code => '41', name => 'Switzerland',    example => '781234567',    min_length => 9,  max_length => 9 },
    AT => { calling_code => '43', name => 'Austria',        example => '6641234567',   min_length => 10, max_length => 13 },
    SE => { calling_code => '46', name => 'Sweden',         example => '701234567',    min_length => 9,  max_length => 9 },
    NO => { calling_code => '47', name => 'Norway',         example => '41234567',     min_length => 8,  max_length => 8 },
    DK => { calling_code => '45', name => 'Denmark',        example => '20123456',     min_length => 8,  max_length => 8 },
    FI => { calling_code => '358', name => 'Finland',       example => '401234567',    min_length => 9,  max_length => 10 },
    PL => { calling_code => '48', name => 'Poland',         example => '512345678',    min_length => 9,  max_length => 9 },
    PT => { calling_code => '351', name => 'Portugal',      example => '912345678',    min_length => 9,  max_length => 9 },
    GR => { calling_code => '30', name => 'Greece',         example => '6912345678',   min_length => 10, max_length => 10 },
    TR => { calling_code => '90', name => 'Turkey',         example => '5321234567',   min_length => 10, max_length => 10 },
    ZA => { calling_code => '27', name => 'South Africa',   example => '821234567',    min_length => 9,  max_length => 9 },
    AE => { calling_code => '971', name => 'UAE',           example => '501234567',    min_length => 9,  max_length => 9 },
    SA => { calling_code => '966', name => 'Saudi Arabia',  example => '512345678',    min_length => 9,  max_length => 9 },
    IL => { calling_code => '972', name => 'Israel',        example => '501234567',    min_length => 9,  max_length => 9 },
    EG => { calling_code => '20', name => 'Egypt',          example => '1001234567',   min_length => 10, max_length => 10 },
    NG => { calling_code => '234', name => 'Nigeria',       example => '8012345678',   min_length => 10, max_length => 10 },
    KE => { calling_code => '254', name => 'Kenya',         example => '712345678',    min_length => 9,  max_length => 9 },
    PH => { calling_code => '63', name => 'Philippines',    example => '9171234567',   min_length => 10, max_length => 10 },
    VN => { calling_code => '84', name => 'Vietnam',        example => '912345678',    min_length => 9,  max_length => 10 },
    TH => { calling_code => '66', name => 'Thailand',       example => '812345678',    min_length => 9,  max_length => 9 },
    ID => { calling_code => '62', name => 'Indonesia',      example => '812345678',    min_length => 9,  max_length => 12 },
    MY => { calling_code => '60', name => 'Malaysia',       example => '123456789',    min_length => 9,  max_length => 10 },
    SG => { calling_code => '65', name => 'Singapore',      example => '81234567',     min_length => 8,  max_length => 8 },
    HK => { calling_code => '852', name => 'Hong Kong',     example => '51234567',     min_length => 8,  max_length => 8 },
    TW => { calling_code => '886', name => 'Taiwan',        example => '912345678',    min_length => 9,  max_length => 9 },
    NZ => { calling_code => '64', name => 'New Zealand',    example => '211234567',    min_length => 9,  max_length => 10 },
    AR => { calling_code => '54', name => 'Argentina',      example => '91123456789',  min_length => 10, max_length => 11 },
    CL => { calling_code => '56', name => 'Chile',          example => '912345678',    min_length => 9,  max_length => 9 },
    CO => { calling_code => '57', name => 'Colombia',       example => '3101234567',   min_length => 10, max_length => 10 },
    IE => { calling_code => '353', name => 'Ireland',       example => '851234567',    min_length => 9,  max_length => 9 },
);

# Reverse lookup: calling code -> country code(s)
my %CALLING_CODE_TO_COUNTRY;
for my $country (keys %COUNTRIES) {
    my $cc = $COUNTRIES{$country}{calling_code};
    push @{$CALLING_CODE_TO_COUNTRY{$cc}}, $country;
}

# PhoneNumber class for OO interface
package Proven::SafePhone::PhoneNumber;
use strict;
use warnings;

=head2 Proven::SafePhone::PhoneNumber->new($country_code, $national_number)

Create a new PhoneNumber object.

=cut

sub new {
    my ($class, $country_code, $national_number) = @_;
    return undef unless defined $country_code && defined $national_number;
    return undef unless $national_number =~ /^\d+$/;

    $country_code = uc($country_code);
    return undef unless exists $COUNTRIES{$country_code};

    return bless {
        country         => $country_code,
        national_number => $national_number,
    }, $class;
}

=head2 $phone->country()

Get the ISO 3166-1 alpha-2 country code.

=cut

sub country {
    my ($self) = @_;
    return $self->{country};
}

=head2 $phone->national_number()

Get the national number (without country code).

=cut

sub national_number {
    my ($self) = @_;
    return $self->{national_number};
}

=head2 $phone->calling_code()

Get the international calling code.

=cut

sub calling_code {
    my ($self) = @_;
    return $COUNTRIES{$self->{country}}{calling_code};
}

=head2 $phone->to_e164()

Format as E.164 (+CCNNNNNNN).

=cut

sub to_e164 {
    my ($self) = @_;
    return '+' . $self->calling_code . $self->{national_number};
}

=head2 $phone->to_national()

Format in national format.

=cut

sub to_national {
    my ($self) = @_;
    return Proven::SafePhone::format($self, 'national');
}

=head2 $phone->to_international()

Format in international format.

=cut

sub to_international {
    my ($self) = @_;
    return Proven::SafePhone::format($self, 'international');
}

=head2 $phone->equals($other)

Check equality with another PhoneNumber.

=cut

sub equals {
    my ($self, $other) = @_;
    return 0 unless ref($other) && $other->isa('Proven::SafePhone::PhoneNumber');
    return $self->{country} eq $other->{country}
        && $self->{national_number} eq $other->{national_number};
}

package Proven::SafePhone;

=head2 parse($number, $default_country)

Parse a phone number string.
If number starts with +, country code is extracted.
Otherwise, default_country is used.
Returns undef if invalid.

=cut

sub parse {
    my ($number, $default_country) = @_;
    return undef unless defined $number;

    # Normalize: remove all non-digit characters except leading +
    my $has_plus = ($number =~ /^\s*\+/);
    my $digits = $number;
    $digits =~ s/[^\d]//g;

    return undef unless length($digits) >= 7;
    return undef unless length($digits) <= 15;

    my ($country_code, $national_number);

    if ($has_plus) {
        # Try to match calling codes (longest match first)
        for my $len (reverse 1..4) {
            my $cc = substr($digits, 0, $len);
            if (exists $CALLING_CODE_TO_COUNTRY{$cc}) {
                my $countries = $CALLING_CODE_TO_COUNTRY{$cc};
                # Use first matching country or specified default
                if (defined $default_country && grep { $_ eq uc($default_country) } @$countries) {
                    $country_code = uc($default_country);
                } else {
                    $country_code = $countries->[0];
                }
                $national_number = substr($digits, $len);
                last;
            }
        }
        return undef unless defined $country_code;
    } else {
        return undef unless defined $default_country;
        $country_code = uc($default_country);
        return undef unless exists $COUNTRIES{$country_code};

        # Remove leading 0 (trunk prefix) if present
        if ($digits =~ /^0/) {
            $digits = substr($digits, 1);
        }
        $national_number = $digits;
    }

    # Validate length for country
    my $info = $COUNTRIES{$country_code};
    my $len = length($national_number);
    return undef if $len < $info->{min_length};
    return undef if $len > $info->{max_length};

    return Proven::SafePhone::PhoneNumber->new($country_code, $national_number);
}

=head2 format($phone, $format_type)

Format a PhoneNumber object.
Format types: 'e164', 'national', 'international', 'rfc3966'
Default is 'e164'.

=cut

sub format {
    my ($phone, $format_type) = @_;
    return undef unless ref($phone) && $phone->isa('Proven::SafePhone::PhoneNumber');
    $format_type //= 'e164';

    my $national = $phone->national_number;
    my $calling_code = $phone->calling_code;

    if ($format_type eq 'e164') {
        return '+' . $calling_code . $national;
    } elsif ($format_type eq 'national') {
        # Format with local conventions (simplified)
        if (length($national) == 10) {
            # Format as (XXX) XXX-XXXX for 10-digit numbers
            return sprintf('(%s) %s-%s',
                substr($national, 0, 3),
                substr($national, 3, 3),
                substr($national, 6));
        } elsif (length($national) == 9) {
            # Format as XXX XXX XXX
            return sprintf('%s %s %s',
                substr($national, 0, 3),
                substr($national, 3, 3),
                substr($national, 6));
        } else {
            # Return as-is with spaces every 3 digits
            my $formatted = '';
            for my $i (0 .. length($national) - 1) {
                $formatted .= ' ' if $i > 0 && $i % 3 == 0;
                $formatted .= substr($national, $i, 1);
            }
            return $formatted;
        }
    } elsif ($format_type eq 'international') {
        return '+' . $calling_code . ' ' . _group_digits($national);
    } elsif ($format_type eq 'rfc3966') {
        return 'tel:+' . $calling_code . '-' . $national;
    }

    return undef;
}

sub _group_digits {
    my ($digits) = @_;
    my $len = length($digits);
    if ($len <= 4) {
        return $digits;
    } elsif ($len <= 7) {
        return substr($digits, 0, 3) . ' ' . substr($digits, 3);
    } else {
        return substr($digits, 0, 3) . ' ' .
               substr($digits, 3, 3) . ' ' .
               substr($digits, 6);
    }
}

=head2 is_valid($number, $country)

Check if a phone number string is valid.

=cut

sub is_valid {
    my ($number, $country) = @_;
    return defined parse($number, $country);
}

=head2 normalize($number, $country)

Normalize a phone number to E.164 format.
Returns undef if invalid.

=cut

sub normalize {
    my ($number, $country) = @_;
    my $phone = parse($number, $country);
    return $phone ? Proven::SafePhone::format($phone, 'e164') : undef;
}

=head2 get_country_code($phone)

Get the ISO country code from a PhoneNumber object.

=cut

sub get_country_code {
    my ($phone) = @_;
    return undef unless ref($phone) && $phone->isa('Proven::SafePhone::PhoneNumber');
    return $phone->country;
}

=head2 get_national_number($phone)

Get the national number from a PhoneNumber object.

=cut

sub get_national_number {
    my ($phone) = @_;
    return undef unless ref($phone) && $phone->isa('Proven::SafePhone::PhoneNumber');
    return $phone->national_number;
}

=head2 is_mobile($phone)

Check if a phone number is likely a mobile number.
Note: This is a heuristic based on common patterns.

=cut

sub is_mobile {
    my ($phone) = @_;
    return 0 unless ref($phone) && $phone->isa('Proven::SafePhone::PhoneNumber');

    my $national = $phone->national_number;
    my $country = $phone->country;

    # Common mobile prefixes by country (simplified)
    my %mobile_prefixes = (
        US => [qr/^[2-9]/],          # Most US numbers could be mobile
        GB => [qr/^7/],               # UK mobile starts with 7
        DE => [qr/^1[5-7]/],          # Germany mobile 15x, 16x, 17x
        FR => [qr/^6/, qr/^7/],       # France 06, 07
        AU => [qr/^4/],               # Australia 04
        JP => [qr/^[89]0/],           # Japan 080, 090
        CN => [qr/^1[3-9]/],          # China 13x-19x
        IN => [qr/^[6-9]/],           # India 6-9
    );

    if (exists $mobile_prefixes{$country}) {
        for my $pattern (@{$mobile_prefixes{$country}}) {
            return 1 if $national =~ $pattern;
        }
        return 0;
    }

    # Default: assume could be mobile if we don't have data
    return 1;
}

=head2 is_landline($phone)

Check if a phone number is likely a landline.
Note: This is a heuristic; returns opposite of is_mobile for known patterns.

=cut

sub is_landline {
    my ($phone) = @_;
    return 0 unless ref($phone) && $phone->isa('Proven::SafePhone::PhoneNumber');
    return !is_mobile($phone);
}

=head2 phone_number($country_code, $national_number)

Convenience function to create a PhoneNumber object.

=cut

sub phone_number {
    my ($country_code, $national_number) = @_;
    return Proven::SafePhone::PhoneNumber->new($country_code, $national_number);
}

=head2 countries_list()

Return a list of supported country codes.

=cut

sub countries_list {
    return sort keys %COUNTRIES;
}

=head2 country_info($country_code)

Get country information.
Returns { calling_code, name, example, min_length, max_length } or undef.

=cut

sub country_info {
    my ($code) = @_;
    return undef unless defined $code;
    $code = uc($code);
    return $COUNTRIES{$code};
}

1;

__END__

=head1 AUTHOR

Hyperpolymath

=head1 LICENSE

AGPL-3.0-or-later

=cut
