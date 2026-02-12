#!/usr/bin/env bash
# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

# safe_phone.sh - Safe phone number operations for Bash
# Source this file: source /path/to/safe_phone.sh

# Country calling codes: CODE -> "country_code,country_name,phone_length_range"
# phone_length_range is "min-max" for national number (excluding country code)
declare -gA PHONE_COUNTRY_CODES=(
    # North America (NANP)
    ["US"]="1,United States,10-10"
    ["CA"]="1,Canada,10-10"
    ["MX"]="52,Mexico,10-10"
    # Europe
    ["GB"]="44,United Kingdom,10-10"
    ["DE"]="49,Germany,10-11"
    ["FR"]="33,France,9-9"
    ["IT"]="39,Italy,9-10"
    ["ES"]="34,Spain,9-9"
    ["NL"]="31,Netherlands,9-9"
    ["BE"]="32,Belgium,8-9"
    ["AT"]="43,Austria,10-13"
    ["CH"]="41,Switzerland,9-9"
    ["PL"]="48,Poland,9-9"
    ["SE"]="46,Sweden,7-13"
    ["NO"]="47,Norway,8-8"
    ["DK"]="45,Denmark,8-8"
    ["FI"]="358,Finland,6-11"
    ["PT"]="351,Portugal,9-9"
    ["IE"]="353,Ireland,7-9"
    ["GR"]="30,Greece,10-10"
    ["CZ"]="420,Czech Republic,9-9"
    ["RO"]="40,Romania,9-9"
    ["HU"]="36,Hungary,8-9"
    ["UA"]="380,Ukraine,9-9"
    ["RU"]="7,Russia,10-10"
    # Asia Pacific
    ["CN"]="86,China,11-11"
    ["JP"]="81,Japan,10-10"
    ["KR"]="82,South Korea,9-10"
    ["IN"]="91,India,10-10"
    ["ID"]="62,Indonesia,9-12"
    ["TH"]="66,Thailand,9-9"
    ["VN"]="84,Vietnam,9-10"
    ["MY"]="60,Malaysia,9-10"
    ["PH"]="63,Philippines,10-10"
    ["SG"]="65,Singapore,8-8"
    ["HK"]="852,Hong Kong,8-8"
    ["TW"]="886,Taiwan,9-9"
    ["AU"]="61,Australia,9-9"
    ["NZ"]="64,New Zealand,8-10"
    ["PK"]="92,Pakistan,10-10"
    ["BD"]="880,Bangladesh,10-10"
    # Middle East
    ["AE"]="971,United Arab Emirates,9-9"
    ["SA"]="966,Saudi Arabia,9-9"
    ["IL"]="972,Israel,9-9"
    ["TR"]="90,Turkey,10-10"
    ["EG"]="20,Egypt,10-10"
    # Africa
    ["ZA"]="27,South Africa,9-9"
    ["NG"]="234,Nigeria,10-10"
    ["KE"]="254,Kenya,9-9"
    ["GH"]="233,Ghana,9-9"
    ["ET"]="251,Ethiopia,9-9"
    ["TZ"]="255,Tanzania,9-9"
    # South America
    ["BR"]="55,Brazil,10-11"
    ["AR"]="54,Argentina,10-10"
    ["CO"]="57,Colombia,10-10"
    ["CL"]="56,Chile,9-9"
    ["PE"]="51,Peru,9-9"
    ["VE"]="58,Venezuela,10-10"
)

# Check if a country code is valid
# Usage: phone_country_is_valid "US" && echo "valid"
# Returns: 0 if valid, 1 if invalid
phone_country_is_valid() {
    local country="${1^^}"

    [[ -n "${PHONE_COUNTRY_CODES[$country]}" ]]
}

# Get calling code for a country
# Usage: code=$(phone_calling_code "US")
# Returns: Calling code (e.g., "1") or empty on invalid
phone_calling_code() {
    local country="${1^^}"
    local data="${PHONE_COUNTRY_CODES[$country]}"

    [[ -z "$data" ]] && return 1

    printf '%s' "${data%%,*}"
}

# Get country name
# Usage: name=$(phone_country_name "US")
# Returns: Country name or empty on invalid
phone_country_name() {
    local country="${1^^}"
    local data="${PHONE_COUNTRY_CODES[$country]}"

    [[ -z "$data" ]] && return 1

    local without_code="${data#*,}"
    printf '%s' "${without_code%%,*}"
}

# Parse a phone number and extract components
# Usage: phone_parse "+1 (555) 123-4567" "US"
# Output: country_code national_number formatted_e164
# Returns: 0 on success, 1 on invalid
phone_parse() {
    local input="$1"
    local default_country="${2:-US}"
    default_country="${default_country^^}"

    # Remove all non-digit characters except leading +
    local has_plus=""
    if [[ "${input:0:1}" == "+" ]]; then
        has_plus="1"
    fi

    local digits="${input//[^0-9]/}"

    # If empty, return error
    if [[ -z "$digits" ]]; then
        echo "ERROR: No digits found in phone number" >&2
        return 1
    fi

    local country_code=""
    local national_number=""
    local detected_country=""

    if [[ -n "$has_plus" ]]; then
        # International format - try to detect country code
        # Try 1, 2, 3 digit country codes
        for len in 1 2 3; do
            local try_code="${digits:0:$len}"
            local country
            for country in "${!PHONE_COUNTRY_CODES[@]}"; do
                local cc
                cc=$(phone_calling_code "$country")
                if [[ "$cc" == "$try_code" ]]; then
                    country_code="$try_code"
                    national_number="${digits:$len}"
                    detected_country="$country"
                    break 2
                fi
            done
        done

        if [[ -z "$country_code" ]]; then
            echo "ERROR: Unknown country code in: $input" >&2
            return 1
        fi
    else
        # National format - use default country
        if ! phone_country_is_valid "$default_country"; then
            echo "ERROR: Unknown default country: $default_country" >&2
            return 1
        fi

        country_code=$(phone_calling_code "$default_country")
        national_number="$digits"
        detected_country="$default_country"

        # Remove leading 0 if present (common in many countries)
        if [[ "${national_number:0:1}" == "0" ]]; then
            national_number="${national_number:1}"
        fi

        # Handle US/CA leading 1
        if [[ "$country_code" == "1" && "${#national_number}" == "11" && "${national_number:0:1}" == "1" ]]; then
            national_number="${national_number:1}"
        fi
    fi

    # Validate national number length
    local data="${PHONE_COUNTRY_CODES[$detected_country]}"
    local length_range="${data##*,}"
    local min_len="${length_range%-*}"
    local max_len="${length_range#*-}"

    if [[ "${#national_number}" -lt "$min_len" || "${#national_number}" -gt "$max_len" ]]; then
        echo "ERROR: Invalid phone number length for $detected_country (expected $min_len-$max_len digits, got ${#national_number})" >&2
        return 1
    fi

    local e164="+${country_code}${national_number}"

    printf '%s %s %s %s\n' "$country_code" "$national_number" "$e164" "$detected_country"
    return 0
}

# Format a phone number in E.164 format
# Usage: e164=$(phone_format_e164 "+1 555 123 4567")
# Returns: E.164 formatted number (e.g., "+15551234567")
phone_format_e164() {
    local input="$1"
    local default_country="${2:-US}"

    local parsed
    parsed=$(phone_parse "$input" "$default_country") || return 1

    local components
    read -r -a components <<< "$parsed"

    printf '%s' "${components[2]}"
}

# Format a phone number for display
# Usage: display=$(phone_format_display "+15551234567" "US")
# Returns: Formatted number (e.g., "(555) 123-4567" for US)
phone_format_display() {
    local input="$1"
    local default_country="${2:-US}"

    local parsed
    parsed=$(phone_parse "$input" "$default_country") || return 1

    local components
    read -r -a components <<< "$parsed"

    local country_code="${components[0]}"
    local national="${components[1]}"
    local detected_country="${components[3]}"

    # Format based on country
    case "$detected_country" in
        US|CA)
            # NANP format: (XXX) XXX-XXXX
            if [[ "${#national}" == "10" ]]; then
                printf '(%s) %s-%s' "${national:0:3}" "${national:3:3}" "${national:6:4}"
            else
                printf '+%s %s' "$country_code" "$national"
            fi
            ;;
        GB)
            # UK format: XXXX XXX XXXX or similar
            if [[ "${#national}" == "10" ]]; then
                printf '+%s %s %s %s' "$country_code" "${national:0:4}" "${national:4:3}" "${national:7:3}"
            else
                printf '+%s %s' "$country_code" "$national"
            fi
            ;;
        DE)
            # Germany format varies by area code
            printf '+%s %s' "$country_code" "$national"
            ;;
        FR)
            # France format: X XX XX XX XX
            if [[ "${#national}" == "9" ]]; then
                printf '+%s %s %s %s %s %s' "$country_code" \
                    "${national:0:1}" "${national:1:2}" "${national:3:2}" \
                    "${national:5:2}" "${national:7:2}"
            else
                printf '+%s %s' "$country_code" "$national"
            fi
            ;;
        *)
            # Default: +CC NNNNNNNNNN
            printf '+%s %s' "$country_code" "$national"
            ;;
    esac
}

# Format for international dialing (from a specific country)
# Usage: dial=$(phone_format_dial "+15551234567" "GB")
# Returns: Number formatted for dialing from the specified country
phone_format_dial() {
    local input="$1"
    local from_country="${2:-US}"
    from_country="${from_country^^}"

    local parsed
    parsed=$(phone_parse "$input" "US") || return 1

    local components
    read -r -a components <<< "$parsed"

    local country_code="${components[0]}"
    local national="${components[1]}"
    local detected_country="${components[3]}"

    # Check if calling within same country
    local from_cc
    from_cc=$(phone_calling_code "$from_country")

    if [[ "$country_code" == "$from_cc" ]]; then
        # National call - return national format
        case "$from_country" in
            US|CA)
                printf '1%s' "$national"
                ;;
            GB|DE|FR|IT|ES)
                printf '0%s' "$national"
                ;;
            *)
                printf '%s' "$national"
                ;;
        esac
    else
        # International call - add IDD prefix
        local idd_prefix
        case "$from_country" in
            US|CA)
                idd_prefix="011"
                ;;
            GB|DE|FR|IT|ES|NL|BE|AT|CH)
                idd_prefix="00"
                ;;
            AU)
                idd_prefix="0011"
                ;;
            JP)
                idd_prefix="010"
                ;;
            *)
                idd_prefix="00"
                ;;
        esac

        printf '%s%s%s' "$idd_prefix" "$country_code" "$national"
    fi
}

# Validate a phone number
# Usage: phone_is_valid "+1 555 123 4567" "US" && echo "valid"
# Returns: 0 if valid, 1 if invalid
phone_is_valid() {
    local input="$1"
    local default_country="${2:-US}"

    phone_parse "$input" "$default_country" >/dev/null 2>&1
}

# Extract just the digits from a phone number
# Usage: digits=$(phone_digits "+1 (555) 123-4567")
# Returns: All digits (e.g., "15551234567")
phone_digits() {
    local input="$1"

    printf '%s' "${input//[^0-9]/}"
}

# Mask a phone number for display (privacy)
# Usage: masked=$(phone_mask "+15551234567")
# Returns: Masked number (e.g., "+1 *** *** 4567")
phone_mask() {
    local input="$1"
    local visible_digits="${2:-4}"
    local default_country="${3:-US}"

    local parsed
    parsed=$(phone_parse "$input" "$default_country") || return 1

    local components
    read -r -a components <<< "$parsed"

    local country_code="${components[0]}"
    local national="${components[1]}"

    local national_len="${#national}"

    if [[ "$national_len" -le "$visible_digits" ]]; then
        printf '+%s %s' "$country_code" "$national"
        return
    fi

    local hidden_len=$((national_len - visible_digits))
    local hidden=""
    local i

    for ((i = 0; i < hidden_len; i++)); do
        hidden+="*"
    done

    printf '+%s %s%s' "$country_code" "$hidden" "${national: -$visible_digits}"
}

# Get the country for a phone number
# Usage: country=$(phone_country "+15551234567")
# Returns: Country code (e.g., "US")
phone_country() {
    local input="$1"
    local default_country="${2:-US}"

    local parsed
    parsed=$(phone_parse "$input" "$default_country") || return 1

    local components
    read -r -a components <<< "$parsed"

    printf '%s' "${components[3]}"
}

# Check if a phone number is from a specific country
# Usage: phone_is_country "+15551234567" "US" && echo "yes"
# Returns: 0 if from the country, 1 otherwise
phone_is_country() {
    local input="$1"
    local expected_country="${2^^}"

    local actual_country
    actual_country=$(phone_country "$input" "$expected_country") || return 1

    [[ "$actual_country" == "$expected_country" ]]
}

# List all supported country codes
# Usage: countries=$(phone_country_list)
# Returns: Space-separated list of country codes
phone_country_list() {
    printf '%s\n' "${!PHONE_COUNTRY_CODES[@]}" | sort | tr '\n' ' ' | sed 's/ $//'
}

# Compare two phone numbers for equality
# Usage: phone_equals "+1 555 1234567" "5551234567" "US" && echo "equal"
# Returns: 0 if equal, 1 if different
phone_equals() {
    local phone1="$1"
    local phone2="$2"
    local default_country="${3:-US}"

    local e164_1 e164_2

    e164_1=$(phone_format_e164 "$phone1" "$default_country") || return 1
    e164_2=$(phone_format_e164 "$phone2" "$default_country") || return 1

    [[ "$e164_1" == "$e164_2" ]]
}
