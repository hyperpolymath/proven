#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

# safe_currency.sh - Safe currency operations for Bash
# Source this file: source /path/to/safe_currency.sh

# Currency codes associative array: CODE -> "name,symbol,decimal_places"
declare -gA CURRENCY_DATA=(
    # Major currencies
    ["USD"]="US Dollar,$,2"
    ["EUR"]="Euro,\u20AC,2"
    ["GBP"]="British Pound,\u00A3,2"
    ["JPY"]="Japanese Yen,\u00A5,0"
    ["CHF"]="Swiss Franc,CHF,2"
    ["CAD"]="Canadian Dollar,CA$,2"
    ["AUD"]="Australian Dollar,A$,2"
    ["NZD"]="New Zealand Dollar,NZ$,2"
    ["CNY"]="Chinese Yuan,\u00A5,2"
    ["HKD"]="Hong Kong Dollar,HK$,2"
    ["SGD"]="Singapore Dollar,S$,2"
    ["INR"]="Indian Rupee,\u20B9,2"
    ["KRW"]="South Korean Won,\u20A9,0"
    ["MXN"]="Mexican Peso,MX$,2"
    ["BRL"]="Brazilian Real,R$,2"
    ["RUB"]="Russian Ruble,\u20BD,2"
    ["ZAR"]="South African Rand,R,2"
    ["SEK"]="Swedish Krona,kr,2"
    ["NOK"]="Norwegian Krone,kr,2"
    ["DKK"]="Danish Krone,kr,2"
    ["PLN"]="Polish Zloty,z\u0142,2"
    ["THB"]="Thai Baht,\u0E3F,2"
    ["IDR"]="Indonesian Rupiah,Rp,2"
    ["MYR"]="Malaysian Ringgit,RM,2"
    ["PHP"]="Philippine Peso,\u20B1,2"
    ["VND"]="Vietnamese Dong,\u20AB,0"
    ["TWD"]="Taiwan Dollar,NT$,2"
    ["TRY"]="Turkish Lira,\u20BA,2"
    ["AED"]="UAE Dirham,\u062F.\u0625,2"
    ["SAR"]="Saudi Riyal,\uFDFC,2"
    ["ILS"]="Israeli Shekel,\u20AA,2"
    ["EGP"]="Egyptian Pound,E\u00A3,2"
    ["NGN"]="Nigerian Naira,\u20A6,2"
    ["KES"]="Kenyan Shilling,KSh,2"
    ["PKR"]="Pakistani Rupee,\u20A8,2"
    ["BDT"]="Bangladeshi Taka,\u09F3,2"
    ["CLP"]="Chilean Peso,CLP$,0"
    ["COP"]="Colombian Peso,COL$,2"
    ["PEN"]="Peruvian Sol,S/,2"
    ["ARS"]="Argentine Peso,ARS$,2"
    # Cryptocurrencies (common)
    ["BTC"]="Bitcoin,\u20BF,8"
    ["ETH"]="Ethereum,\u039E,18"
    ["XRP"]="Ripple,XRP,6"
    ["LTC"]="Litecoin,\u0141,8"
    # Precious metals
    ["XAU"]="Gold (troy oz),XAU,4"
    ["XAG"]="Silver (troy oz),XAG,4"
    ["XPT"]="Platinum (troy oz),XPT,4"
    ["XPD"]="Palladium (troy oz),XPD,4"
)

# Check if a currency code is valid
# Usage: currency_is_valid "USD" && echo "valid"
# Returns: 0 if valid, 1 if invalid
currency_is_valid() {
    local code="${1^^}"  # Convert to uppercase

    [[ -n "${CURRENCY_DATA[$code]}" ]]
}

# Get currency name
# Usage: name=$(currency_name "USD")
# Returns: Currency name or empty on invalid
currency_name() {
    local code="${1^^}"
    local data="${CURRENCY_DATA[$code]}"

    [[ -z "$data" ]] && return 1

    printf '%s' "${data%%,*}"
}

# Get currency symbol
# Usage: symbol=$(currency_symbol "USD")
# Returns: Currency symbol or empty on invalid
currency_symbol() {
    local code="${1^^}"
    local data="${CURRENCY_DATA[$code]}"

    [[ -z "$data" ]] && return 1

    local without_name="${data#*,}"
    printf '%b' "${without_name%%,*}"
}

# Get decimal places for a currency
# Usage: decimals=$(currency_decimals "USD")
# Returns: Number of decimal places or empty on invalid
currency_decimals() {
    local code="${1^^}"
    local data="${CURRENCY_DATA[$code]}"

    [[ -z "$data" ]] && return 1

    printf '%s' "${data##*,}"
}

# Parse a money string to cents/minor units
# Usage: cents=$(money_parse "123.45" "USD")
# Returns: Amount in minor units (cents), or error
money_parse() {
    local amount="$1"
    local code="${2:-USD}"
    code="${code^^}"

    local decimals
    decimals=$(currency_decimals "$code") || {
        echo "ERROR: Unknown currency: $code" >&2
        return 1
    }

    # Remove currency symbols and whitespace
    local cleaned="${amount}"
    cleaned="${cleaned//[^0-9.-]/}"

    # Validate numeric format
    if ! [[ "$cleaned" =~ ^-?[0-9]+\.?[0-9]*$ ]]; then
        echo "ERROR: Invalid amount format: $amount" >&2
        return 1
    fi

    # Handle zero decimal currencies
    if [[ "$decimals" -eq 0 ]]; then
        # Remove any decimal portion for zero-decimal currencies
        local integer_part="${cleaned%%.*}"
        printf '%s' "$integer_part"
        return 0
    fi

    # Split into integer and decimal parts
    local integer_part="${cleaned%%.*}"
    local decimal_part=""

    if [[ "$cleaned" == *"."* ]]; then
        decimal_part="${cleaned#*.}"
    fi

    # Pad or truncate decimal part to correct length
    while [[ "${#decimal_part}" -lt "$decimals" ]]; do
        decimal_part="${decimal_part}0"
    done

    if [[ "${#decimal_part}" -gt "$decimals" ]]; then
        decimal_part="${decimal_part:0:$decimals}"
    fi

    # Remove leading zeros from integer part (except for "0")
    integer_part="${integer_part#"${integer_part%%[!0]*}"}"
    [[ -z "$integer_part" ]] && integer_part="0"

    # Combine and return
    local minor_units="${integer_part}${decimal_part}"
    # Remove leading zeros
    minor_units="${minor_units#"${minor_units%%[!0]*}"}"
    [[ -z "$minor_units" ]] && minor_units="0"

    printf '%s' "$minor_units"
    return 0
}

# Format minor units to a display string
# Usage: display=$(money_format 12345 "USD")
# Returns: Formatted string like "123.45"
money_format() {
    local minor_units="$1"
    local code="${2:-USD}"
    code="${code^^}"

    local decimals
    decimals=$(currency_decimals "$code") || {
        echo "ERROR: Unknown currency: $code" >&2
        return 1
    }

    # Handle negative amounts
    local negative=""
    if [[ "${minor_units:0:1}" == "-" ]]; then
        negative="-"
        minor_units="${minor_units:1}"
    fi

    # Handle zero decimal currencies
    if [[ "$decimals" -eq 0 ]]; then
        printf '%s%s' "$negative" "$minor_units"
        return 0
    fi

    # Pad with leading zeros if necessary
    while [[ "${#minor_units}" -le "$decimals" ]]; do
        minor_units="0${minor_units}"
    done

    local integer_part="${minor_units:0:-$decimals}"
    local decimal_part="${minor_units: -$decimals}"

    printf '%s%s.%s' "$negative" "$integer_part" "$decimal_part"
    return 0
}

# Format with currency symbol
# Usage: display=$(money_display 12345 "USD")
# Returns: Formatted string like "$123.45"
money_display() {
    local minor_units="$1"
    local code="${2:-USD}"
    code="${code^^}"

    local symbol
    symbol=$(currency_symbol "$code") || return 1

    local formatted
    formatted=$(money_format "$minor_units" "$code") || return 1

    # Handle negative: symbol before or after number varies by locale
    # Default to symbol before number
    if [[ "${formatted:0:1}" == "-" ]]; then
        printf '%s%s%s' "-" "$symbol" "${formatted:1}"
    else
        printf '%s%s' "$symbol" "$formatted"
    fi
}

# Add two money values (in minor units)
# Usage: result=$(money_add 100 50)
# Returns: Sum in minor units
money_add() {
    local a="$1"
    local b="$2"

    printf '%s' "$((a + b))"
}

# Subtract two money values (in minor units)
# Usage: result=$(money_subtract 100 50)
# Returns: Difference in minor units
money_subtract() {
    local a="$1"
    local b="$2"

    printf '%s' "$((a - b))"
}

# Multiply money by a factor (careful with rounding)
# Usage: result=$(money_multiply 100 1.5)
# Returns: Product in minor units (rounded)
money_multiply() {
    local minor_units="$1"
    local factor="$2"

    # Use awk for floating point math
    local result
    result=$(awk -v a="$minor_units" -v b="$factor" 'BEGIN { printf "%.0f", a * b }')

    printf '%s' "$result"
}

# Divide money with banker's rounding
# Usage: result=$(money_divide 100 3)
# Returns: Quotient in minor units (rounded)
money_divide() {
    local minor_units="$1"
    local divisor="$2"

    if [[ "$divisor" == "0" ]]; then
        echo "ERROR: Division by zero" >&2
        return 1
    fi

    local result
    result=$(awk -v a="$minor_units" -v b="$divisor" 'BEGIN { printf "%.0f", a / b }')

    printf '%s' "$result"
}

# Compare two money values
# Usage: money_compare 100 50
# Returns: -1 if a < b, 0 if a == b, 1 if a > b
money_compare() {
    local a="$1"
    local b="$2"

    if [[ "$a" -lt "$b" ]]; then
        printf '%s' "-1"
    elif [[ "$a" -gt "$b" ]]; then
        printf '%s' "1"
    else
        printf '%s' "0"
    fi
}

# Check if money value is zero
# Usage: money_is_zero 0 && echo "zero"
# Returns: 0 if zero, 1 otherwise
money_is_zero() {
    local amount="$1"

    [[ "$amount" -eq 0 ]]
}

# Check if money value is negative
# Usage: money_is_negative -100 && echo "negative"
# Returns: 0 if negative, 1 otherwise
money_is_negative() {
    local amount="$1"

    [[ "$amount" -lt 0 ]]
}

# Convert between currencies (requires exchange rate)
# Usage: result=$(money_convert 10000 "USD" "EUR" 0.85)
# Returns: Amount in target currency minor units
money_convert() {
    local minor_units="$1"
    local from_code="${2^^}"
    local to_code="${3^^}"
    local rate="$4"

    # Validate currencies
    currency_is_valid "$from_code" || {
        echo "ERROR: Unknown source currency: $from_code" >&2
        return 1
    }
    currency_is_valid "$to_code" || {
        echo "ERROR: Unknown target currency: $to_code" >&2
        return 1
    }

    local from_decimals to_decimals
    from_decimals=$(currency_decimals "$from_code")
    to_decimals=$(currency_decimals "$to_code")

    # Convert to base units, apply rate, convert to target minor units
    local result
    result=$(awk -v amount="$minor_units" \
                 -v from_dec="$from_decimals" \
                 -v to_dec="$to_decimals" \
                 -v rate="$rate" \
                 'BEGIN {
                     base = amount / (10 ^ from_dec)
                     converted = base * rate
                     minor = converted * (10 ^ to_dec)
                     printf "%.0f", minor
                 }')

    printf '%s' "$result"
}

# List all supported currency codes
# Usage: codes=$(currency_list)
# Returns: Space-separated list of currency codes
currency_list() {
    printf '%s\n' "${!CURRENCY_DATA[@]}" | sort | tr '\n' ' ' | sed 's/ $//'
}

# Validate a money amount string
# Usage: money_is_valid "$amount" "USD" && echo "valid"
# Returns: 0 if valid, 1 if invalid
money_is_valid() {
    local amount="$1"
    local code="${2:-USD}"

    money_parse "$amount" "$code" >/dev/null 2>&1
}
