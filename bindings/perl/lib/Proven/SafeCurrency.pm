# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

package Proven::SafeCurrency;
use strict;
use warnings;
use Exporter 'import';

our $VERSION = '0.4.0';
our @EXPORT_OK = qw(
    is_valid_code get_currency_info get_decimals get_symbol
    parse_amount format_amount money add subtract multiply
    divide compare currencies_list
);

=head1 NAME

Proven::SafeCurrency - Safe currency and monetary value operations

=head1 SYNOPSIS

    use Proven::SafeCurrency qw(is_valid_code money format_amount);

    if (is_valid_code('USD')) {
        my $amount = money(1999, 'USD');  # $19.99 in cents
        print format_amount($amount);     # "19.99"
    }

=head1 DESCRIPTION

Provides safe currency operations without floating-point errors.
All monetary amounts are stored as integers in the smallest unit
(cents, pence, etc.) to prevent rounding errors.

=cut

# ISO 4217 currency codes with their properties
# Format: code => { name, symbol, decimals, country }
my %CURRENCIES = (
    USD => { name => 'US Dollar',             symbol => '$',   decimals => 2, country => 'United States' },
    EUR => { name => 'Euro',                  symbol => "\x{20AC}", decimals => 2, country => 'European Union' },
    GBP => { name => 'British Pound',         symbol => "\x{00A3}", decimals => 2, country => 'United Kingdom' },
    JPY => { name => 'Japanese Yen',          symbol => "\x{00A5}", decimals => 0, country => 'Japan' },
    CHF => { name => 'Swiss Franc',           symbol => 'CHF', decimals => 2, country => 'Switzerland' },
    CAD => { name => 'Canadian Dollar',       symbol => 'C$',  decimals => 2, country => 'Canada' },
    AUD => { name => 'Australian Dollar',     symbol => 'A$',  decimals => 2, country => 'Australia' },
    NZD => { name => 'New Zealand Dollar',    symbol => 'NZ$', decimals => 2, country => 'New Zealand' },
    CNY => { name => 'Chinese Yuan',          symbol => "\x{00A5}", decimals => 2, country => 'China' },
    HKD => { name => 'Hong Kong Dollar',      symbol => 'HK$', decimals => 2, country => 'Hong Kong' },
    SGD => { name => 'Singapore Dollar',      symbol => 'S$',  decimals => 2, country => 'Singapore' },
    SEK => { name => 'Swedish Krona',         symbol => 'kr',  decimals => 2, country => 'Sweden' },
    NOK => { name => 'Norwegian Krone',       symbol => 'kr',  decimals => 2, country => 'Norway' },
    DKK => { name => 'Danish Krone',          symbol => 'kr',  decimals => 2, country => 'Denmark' },
    INR => { name => 'Indian Rupee',          symbol => "\x{20B9}", decimals => 2, country => 'India' },
    KRW => { name => 'South Korean Won',      symbol => "\x{20A9}", decimals => 0, country => 'South Korea' },
    MXN => { name => 'Mexican Peso',          symbol => 'MX$', decimals => 2, country => 'Mexico' },
    BRL => { name => 'Brazilian Real',        symbol => 'R$',  decimals => 2, country => 'Brazil' },
    ZAR => { name => 'South African Rand',    symbol => 'R',   decimals => 2, country => 'South Africa' },
    RUB => { name => 'Russian Ruble',         symbol => "\x{20BD}", decimals => 2, country => 'Russia' },
    PLN => { name => 'Polish Zloty',          symbol => "z\x{0142}", decimals => 2, country => 'Poland' },
    THB => { name => 'Thai Baht',             symbol => "\x{0E3F}", decimals => 2, country => 'Thailand' },
    IDR => { name => 'Indonesian Rupiah',     symbol => 'Rp',  decimals => 2, country => 'Indonesia' },
    MYR => { name => 'Malaysian Ringgit',     symbol => 'RM',  decimals => 2, country => 'Malaysia' },
    PHP => { name => 'Philippine Peso',       symbol => "\x{20B1}", decimals => 2, country => 'Philippines' },
    VND => { name => 'Vietnamese Dong',       symbol => "\x{20AB}", decimals => 0, country => 'Vietnam' },
    TRY => { name => 'Turkish Lira',          symbol => "\x{20BA}", decimals => 2, country => 'Turkey' },
    ILS => { name => 'Israeli Shekel',        symbol => "\x{20AA}", decimals => 2, country => 'Israel' },
    AED => { name => 'UAE Dirham',            symbol => 'AED', decimals => 2, country => 'United Arab Emirates' },
    SAR => { name => 'Saudi Riyal',           symbol => 'SAR', decimals => 2, country => 'Saudi Arabia' },
    TWD => { name => 'Taiwan Dollar',         symbol => 'NT$', decimals => 2, country => 'Taiwan' },
    CZK => { name => 'Czech Koruna',          symbol => "K\x{010D}", decimals => 2, country => 'Czech Republic' },
    HUF => { name => 'Hungarian Forint',      symbol => 'Ft',  decimals => 2, country => 'Hungary' },
    CLP => { name => 'Chilean Peso',          symbol => 'CLP', decimals => 0, country => 'Chile' },
    COP => { name => 'Colombian Peso',        symbol => 'COP', decimals => 2, country => 'Colombia' },
    PEN => { name => 'Peruvian Sol',          symbol => 'S/',  decimals => 2, country => 'Peru' },
    ARS => { name => 'Argentine Peso',        symbol => 'ARS', decimals => 2, country => 'Argentina' },
    EGP => { name => 'Egyptian Pound',        symbol => 'E\x{00A3}', decimals => 2, country => 'Egypt' },
    NGN => { name => 'Nigerian Naira',        symbol => "\x{20A6}", decimals => 2, country => 'Nigeria' },
    KWD => { name => 'Kuwaiti Dinar',         symbol => 'KWD', decimals => 3, country => 'Kuwait' },
    BHD => { name => 'Bahraini Dinar',        symbol => 'BHD', decimals => 3, country => 'Bahrain' },
    OMR => { name => 'Omani Rial',            symbol => 'OMR', decimals => 3, country => 'Oman' },
);

# Money class for OO interface
package Proven::SafeCurrency::Money;
use strict;
use warnings;

=head2 Proven::SafeCurrency::Money->new($amount_minor, $currency_code)

Create a new Money object.
Amount is in minor units (cents, pence, etc.).

=cut

sub new {
    my ($class, $amount_minor, $currency_code) = @_;
    return undef unless defined $amount_minor;
    return undef unless defined $currency_code;
    return undef unless Proven::SafeCurrency::is_valid_code($currency_code);
    return undef unless $amount_minor =~ /^-?\d+$/;

    return bless {
        amount   => int($amount_minor),
        currency => uc($currency_code),
    }, $class;
}

=head2 $money->amount()

Get the amount in minor units.

=cut

sub amount {
    my ($self) = @_;
    return $self->{amount};
}

=head2 $money->currency()

Get the currency code.

=cut

sub currency {
    my ($self) = @_;
    return $self->{currency};
}

=head2 $money->to_decimal()

Convert to decimal string representation.

=cut

sub to_decimal {
    my ($self) = @_;
    return Proven::SafeCurrency::format_amount($self);
}

=head2 $money->to_string()

Format as string with currency symbol.

=cut

sub to_string {
    my ($self) = @_;
    my $info = Proven::SafeCurrency::get_currency_info($self->{currency});
    my $formatted = Proven::SafeCurrency::format_amount($self);
    return $info->{symbol} . $formatted;
}

=head2 $money->is_negative()

Check if the amount is negative.

=cut

sub is_negative {
    my ($self) = @_;
    return $self->{amount} < 0;
}

=head2 $money->is_zero()

Check if the amount is zero.

=cut

sub is_zero {
    my ($self) = @_;
    return $self->{amount} == 0;
}

=head2 $money->abs()

Return a new Money with the absolute value.

=cut

sub abs {
    my ($self) = @_;
    return Proven::SafeCurrency::Money->new(
        CORE::abs($self->{amount}),
        $self->{currency}
    );
}

=head2 $money->negate()

Return a new Money with negated value.

=cut

sub negate {
    my ($self) = @_;
    return Proven::SafeCurrency::Money->new(
        -$self->{amount},
        $self->{currency}
    );
}

=head2 $money->equals($other)

Check equality with another Money object.

=cut

sub equals {
    my ($self, $other) = @_;
    return 0 unless ref($other) && $other->isa('Proven::SafeCurrency::Money');
    return $self->{currency} eq $other->{currency}
        && $self->{amount} == $other->{amount};
}

package Proven::SafeCurrency;

=head2 is_valid_code($code)

Check if a currency code is valid ISO 4217.

=cut

sub is_valid_code {
    my ($code) = @_;
    return 0 unless defined $code;
    return exists $CURRENCIES{uc($code)};
}

=head2 get_currency_info($code)

Get currency information hashref.
Returns { name, symbol, decimals, country } or undef.

=cut

sub get_currency_info {
    my ($code) = @_;
    return undef unless defined $code;
    return $CURRENCIES{uc($code)};
}

=head2 get_decimals($code)

Get the number of decimal places for a currency.

=cut

sub get_decimals {
    my ($code) = @_;
    my $info = get_currency_info($code);
    return $info ? $info->{decimals} : undef;
}

=head2 get_symbol($code)

Get the currency symbol.

=cut

sub get_symbol {
    my ($code) = @_;
    my $info = get_currency_info($code);
    return $info ? $info->{symbol} : undef;
}

=head2 parse_amount($string, $currency_code)

Parse a decimal string into a Money object.
Examples: "19.99" with USD -> Money(1999, 'USD')

=cut

sub parse_amount {
    my ($string, $currency_code) = @_;
    return undef unless defined $string;
    return undef unless is_valid_code($currency_code);

    my $decimals = get_decimals($currency_code);

    # Handle negative sign
    my $negative = 0;
    if ($string =~ /^-/) {
        $negative = 1;
        $string = substr($string, 1);
    }

    # Remove currency symbols and whitespace
    $string =~ s/[^\d.]//g;

    # Parse decimal number
    my ($integer_part, $decimal_part) = split /\./, $string, 2;
    $integer_part //= '0';
    $decimal_part //= '';

    # Validate parts
    return undef unless $integer_part =~ /^\d+$/;
    return undef unless $decimal_part =~ /^\d*$/;

    # Pad or truncate decimal part to currency precision
    if (length($decimal_part) < $decimals) {
        $decimal_part .= '0' x ($decimals - length($decimal_part));
    } elsif (length($decimal_part) > $decimals) {
        $decimal_part = substr($decimal_part, 0, $decimals);
    }

    # Calculate minor units
    my $minor_units;
    if ($decimals == 0) {
        $minor_units = int($integer_part);
    } else {
        $minor_units = int($integer_part) * (10 ** $decimals) + int($decimal_part || 0);
    }

    $minor_units = -$minor_units if $negative;

    return Proven::SafeCurrency::Money->new($minor_units, $currency_code);
}

=head2 format_amount($money)

Format a Money object as a decimal string.

=cut

sub format_amount {
    my ($money) = @_;
    return undef unless ref($money) && $money->isa('Proven::SafeCurrency::Money');

    my $decimals = get_decimals($money->currency);
    my $amount = $money->amount;

    my $negative = $amount < 0 ? 1 : 0;
    $amount = CORE::abs($amount);

    if ($decimals == 0) {
        return ($negative ? '-' : '') . $amount;
    }

    my $divisor = 10 ** $decimals;
    my $integer = int($amount / $divisor);
    my $decimal = $amount % $divisor;

    my $formatted = sprintf("%d.%0${decimals}d", $integer, $decimal);
    return ($negative ? '-' : '') . $formatted;
}

=head2 money($amount_minor, $currency_code)

Convenience function to create a Money object.

=cut

sub money {
    my ($amount_minor, $currency_code) = @_;
    return Proven::SafeCurrency::Money->new($amount_minor, $currency_code);
}

=head2 add($money_a, $money_b)

Add two Money objects of the same currency.
Returns undef if currencies don't match.

=cut

sub add {
    my ($money_a, $money_b) = @_;
    return undef unless ref($money_a) && $money_a->isa('Proven::SafeCurrency::Money');
    return undef unless ref($money_b) && $money_b->isa('Proven::SafeCurrency::Money');
    return undef unless $money_a->currency eq $money_b->currency;

    return Proven::SafeCurrency::Money->new(
        $money_a->amount + $money_b->amount,
        $money_a->currency
    );
}

=head2 subtract($money_a, $money_b)

Subtract two Money objects of the same currency.
Returns undef if currencies don't match.

=cut

sub subtract {
    my ($money_a, $money_b) = @_;
    return undef unless ref($money_a) && $money_a->isa('Proven::SafeCurrency::Money');
    return undef unless ref($money_b) && $money_b->isa('Proven::SafeCurrency::Money');
    return undef unless $money_a->currency eq $money_b->currency;

    return Proven::SafeCurrency::Money->new(
        $money_a->amount - $money_b->amount,
        $money_a->currency
    );
}

=head2 multiply($money, $factor)

Multiply a Money object by an integer factor.

=cut

sub multiply {
    my ($money, $factor) = @_;
    return undef unless ref($money) && $money->isa('Proven::SafeCurrency::Money');
    return undef unless defined $factor && $factor =~ /^-?\d+$/;

    return Proven::SafeCurrency::Money->new(
        $money->amount * int($factor),
        $money->currency
    );
}

=head2 divide($money, $divisor)

Divide a Money object by an integer divisor.
Uses floor division. Returns undef if divisor is zero.

=cut

sub divide {
    my ($money, $divisor) = @_;
    return undef unless ref($money) && $money->isa('Proven::SafeCurrency::Money');
    return undef unless defined $divisor && $divisor =~ /^-?\d+$/;
    return undef if $divisor == 0;

    my $result = int($money->amount / $divisor);
    return Proven::SafeCurrency::Money->new($result, $money->currency);
}

=head2 compare($money_a, $money_b)

Compare two Money objects.
Returns -1, 0, or 1 like <=> operator.
Returns undef if currencies don't match.

=cut

sub compare {
    my ($money_a, $money_b) = @_;
    return undef unless ref($money_a) && $money_a->isa('Proven::SafeCurrency::Money');
    return undef unless ref($money_b) && $money_b->isa('Proven::SafeCurrency::Money');
    return undef unless $money_a->currency eq $money_b->currency;

    return $money_a->amount <=> $money_b->amount;
}

=head2 currencies_list()

Return a list of all supported currency codes.

=cut

sub currencies_list {
    return sort keys %CURRENCIES;
}

1;

__END__

=head1 AUTHOR

Hyperpolymath

=head1 LICENSE

PMPL-1.0

=cut
