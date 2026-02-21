#!/bin/sh
# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

# safe_datetime.sh - Safe datetime parsing and validation for POSIX shells
# Source this file: . /path/to/safe_datetime.sh

# Parsed date/time components
DATE_YEAR=""
DATE_MONTH=""
DATE_DAY=""
TIME_HOUR=""
TIME_MINUTE=""
TIME_SECOND=""
TZ_OFFSET=""

# Check if a year is a leap year
# Usage: datetime_is_leap_year 2024 && echo "leap year"
# Returns: 0 if leap year, 1 otherwise
datetime_is_leap_year() {
    year="$1"
    if [ $((year % 400)) -eq 0 ]; then
        return 0
    elif [ $((year % 100)) -eq 0 ]; then
        return 1
    elif [ $((year % 4)) -eq 0 ]; then
        return 0
    else
        return 1
    fi
}

# Get days in a month
# Usage: days=$(datetime_days_in_month 2024 2)
# Returns: Number of days
datetime_days_in_month() {
    year="$1"
    month="$2"

    case "$month" in
        1|3|5|7|8|10|12) printf '31' ;;
        4|6|9|11) printf '30' ;;
        2)
            if datetime_is_leap_year "$year"; then
                printf '29'
            else
                printf '28'
            fi
            ;;
        *) printf '0' ;;
    esac
}

# Parse ISO 8601 date (YYYY-MM-DD)
# Usage: datetime_parse_date "2024-01-15"
# Sets: DATE_YEAR, DATE_MONTH, DATE_DAY
# Returns: 0 on success, 1 on error
datetime_parse_date() {
    input="$1"
    DATE_YEAR=""
    DATE_MONTH=""
    DATE_DAY=""
    PROVEN_ERROR=""

    # Check format
    case "$input" in
        [0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9])
            ;;
        *)
            PROVEN_ERROR="invalid_date_format"
            return 1
            ;;
    esac

    # Extract parts
    DATE_YEAR="${input%%-*}"
    rest="${input#*-}"
    DATE_MONTH="${rest%%-*}"
    DATE_DAY="${rest#*-}"

    # Remove leading zeros for arithmetic
    month_num=$((10#$DATE_MONTH))
    day_num=$((10#$DATE_DAY))
    year_num=$((10#$DATE_YEAR))

    # Validate month
    if [ "$month_num" -lt 1 ] || [ "$month_num" -gt 12 ]; then
        PROVEN_ERROR="invalid_month"
        return 1
    fi

    # Validate day
    max_days=$(datetime_days_in_month "$year_num" "$month_num")
    if [ "$day_num" -lt 1 ] || [ "$day_num" -gt "$max_days" ]; then
        PROVEN_ERROR="invalid_day"
        return 1
    fi

    return 0
}

# Parse ISO 8601 time (HH:MM:SS or HH:MM)
# Usage: datetime_parse_time "14:30:00"
# Sets: TIME_HOUR, TIME_MINUTE, TIME_SECOND
# Returns: 0 on success, 1 on error
datetime_parse_time() {
    input="$1"
    TIME_HOUR=""
    TIME_MINUTE=""
    TIME_SECOND=""
    PROVEN_ERROR=""

    # Check format
    case "$input" in
        [0-9][0-9]:[0-9][0-9]:[0-9][0-9]*)
            TIME_HOUR="${input%%:*}"
            rest="${input#*:}"
            TIME_MINUTE="${rest%%:*}"
            rest="${rest#*:}"
            TIME_SECOND="${rest%%[.Z+-]*}"
            ;;
        [0-9][0-9]:[0-9][0-9])
            TIME_HOUR="${input%%:*}"
            TIME_MINUTE="${input#*:}"
            TIME_SECOND="00"
            ;;
        *)
            PROVEN_ERROR="invalid_time_format"
            return 1
            ;;
    esac

    # Validate hour
    hour_num=$((10#$TIME_HOUR))
    if [ "$hour_num" -lt 0 ] || [ "$hour_num" -gt 23 ]; then
        PROVEN_ERROR="invalid_hour"
        return 1
    fi

    # Validate minute
    min_num=$((10#$TIME_MINUTE))
    if [ "$min_num" -lt 0 ] || [ "$min_num" -gt 59 ]; then
        PROVEN_ERROR="invalid_minute"
        return 1
    fi

    # Validate second
    sec_num=$((10#$TIME_SECOND))
    if [ "$sec_num" -lt 0 ] || [ "$sec_num" -gt 59 ]; then
        PROVEN_ERROR="invalid_second"
        return 1
    fi

    return 0
}

# Parse ISO 8601 datetime (YYYY-MM-DDTHH:MM:SS)
# Usage: datetime_parse "2024-01-15T14:30:00Z"
# Sets: DATE_YEAR, DATE_MONTH, DATE_DAY, TIME_HOUR, TIME_MINUTE, TIME_SECOND, TZ_OFFSET
# Returns: 0 on success, 1 on error
datetime_parse() {
    input="$1"
    TZ_OFFSET=""

    # Split at T or space
    case "$input" in
        *T*)
            date_part="${input%%T*}"
            time_part="${input#*T}"
            ;;
        *" "*)
            date_part="${input%% *}"
            time_part="${input#* }"
            ;;
        *)
            # Date only
            datetime_parse_date "$input"
            return $?
            ;;
    esac

    # Extract timezone if present
    case "$time_part" in
        *Z)
            TZ_OFFSET="+00:00"
            time_part="${time_part%Z}"
            ;;
        *+[0-9][0-9]:[0-9][0-9])
            TZ_OFFSET="+${time_part#*+}"
            time_part="${time_part%+*}"
            ;;
        *-[0-9][0-9]:[0-9][0-9])
            offset_part="${time_part##*-}"
            case "$offset_part" in
                [0-9][0-9]:[0-9][0-9])
                    TZ_OFFSET="-${offset_part}"
                    time_part="${time_part%-*}"
                    ;;
            esac
            ;;
    esac

    datetime_parse_date "$date_part" || return 1
    datetime_parse_time "$time_part" || return 1

    return 0
}

# Validate a date
# Usage: datetime_is_valid_date "2024-01-15" && echo "valid"
# Returns: 0 if valid, 1 otherwise
datetime_is_valid_date() {
    datetime_parse_date "$1"
}

# Validate a time
# Usage: datetime_is_valid_time "14:30:00" && echo "valid"
# Returns: 0 if valid, 1 otherwise
datetime_is_valid_time() {
    datetime_parse_time "$1"
}

# Validate a datetime
# Usage: datetime_is_valid "2024-01-15T14:30:00Z" && echo "valid"
# Returns: 0 if valid, 1 otherwise
datetime_is_valid() {
    datetime_parse "$1"
}

# Format date as ISO 8601
# Usage: formatted=$(datetime_format_date 2024 1 15)
# Returns: YYYY-MM-DD
datetime_format_date() {
    year="$1"
    month="$2"
    day="$3"

    printf '%04d-%02d-%02d' "$year" "$month" "$day"
}

# Format time as ISO 8601
# Usage: formatted=$(datetime_format_time 14 30 0)
# Returns: HH:MM:SS
datetime_format_time() {
    hour="$1"
    minute="$2"
    second="${3:-0}"

    printf '%02d:%02d:%02d' "$hour" "$minute" "$second"
}

# Format datetime as ISO 8601
# Usage: formatted=$(datetime_format 2024 1 15 14 30 0 "+00:00")
# Returns: YYYY-MM-DDTHH:MM:SS+00:00
datetime_format() {
    year="$1"
    month="$2"
    day="$3"
    hour="$4"
    minute="$5"
    second="${6:-0}"
    tz="${7:-}"

    result=$(datetime_format_date "$year" "$month" "$day")
    result="${result}T$(datetime_format_time "$hour" "$minute" "$second")"

    if [ -n "$tz" ]; then
        result="${result}${tz}"
    fi

    printf '%s' "$result"
}

# Get current date as ISO 8601
# Usage: today=$(datetime_today)
# Returns: YYYY-MM-DD
datetime_today() {
    date '+%Y-%m-%d'
}

# Get current time as ISO 8601
# Usage: now=$(datetime_now)
# Returns: YYYY-MM-DDTHH:MM:SS
datetime_now() {
    date '+%Y-%m-%dT%H:%M:%S'
}

# Get current datetime in UTC
# Usage: utc=$(datetime_now_utc)
# Returns: YYYY-MM-DDTHH:MM:SSZ
datetime_now_utc() {
    date -u '+%Y-%m-%dT%H:%M:%SZ'
}

# Get Unix timestamp
# Usage: ts=$(datetime_timestamp)
# Returns: Unix timestamp in seconds
datetime_timestamp() {
    date '+%s'
}

# Convert Unix timestamp to ISO 8601
# Usage: iso=$(datetime_from_timestamp 1705334400)
# Returns: YYYY-MM-DDTHH:MM:SS
datetime_from_timestamp() {
    timestamp="$1"
    date -d "@$timestamp" '+%Y-%m-%dT%H:%M:%S' 2>/dev/null || \
        date -r "$timestamp" '+%Y-%m-%dT%H:%M:%S' 2>/dev/null
}

# Calculate day of week (0=Sunday, 6=Saturday)
# Usage: dow=$(datetime_day_of_week 2024 1 15)
# Returns: 0-6
datetime_day_of_week() {
    year="$1"
    month="$2"
    day="$3"

    # Zeller's formula (adjusted for 0-indexed Sunday)
    if [ "$month" -lt 3 ]; then
        month=$((month + 12))
        year=$((year - 1))
    fi

    q="$day"
    m="$month"
    k=$((year % 100))
    j=$((year / 100))

    h=$(( (q + (13 * (m + 1)) / 5 + k + k / 4 + j / 4 - 2 * j) % 7 ))
    dow=$(( (h + 6) % 7 ))

    printf '%d' "$dow"
}

# Get day of week name
# Usage: name=$(datetime_day_name 2024 1 15)
# Returns: Monday, Tuesday, etc.
datetime_day_name() {
    dow=$(datetime_day_of_week "$1" "$2" "$3")
    case "$dow" in
        0) printf 'Sunday' ;;
        1) printf 'Monday' ;;
        2) printf 'Tuesday' ;;
        3) printf 'Wednesday' ;;
        4) printf 'Thursday' ;;
        5) printf 'Friday' ;;
        6) printf 'Saturday' ;;
    esac
}

# Get month name
# Usage: name=$(datetime_month_name 1)
# Returns: January, February, etc.
datetime_month_name() {
    month="$1"
    case "$month" in
        1) printf 'January' ;;
        2) printf 'February' ;;
        3) printf 'March' ;;
        4) printf 'April' ;;
        5) printf 'May' ;;
        6) printf 'June' ;;
        7) printf 'July' ;;
        8) printf 'August' ;;
        9) printf 'September' ;;
        10) printf 'October' ;;
        11) printf 'November' ;;
        12) printf 'December' ;;
    esac
}

# Compare two dates
# Usage: result=$(datetime_compare_dates "2024-01-15" "2024-01-16")
# Returns: -1, 0, or 1
datetime_compare_dates() {
    if ! datetime_parse_date "$1"; then
        return 1
    fi
    y1="$DATE_YEAR"
    m1="$DATE_MONTH"
    d1="$DATE_DAY"

    if ! datetime_parse_date "$2"; then
        return 1
    fi
    y2="$DATE_YEAR"
    m2="$DATE_MONTH"
    d2="$DATE_DAY"

    # Compare year
    if [ "$y1" -lt "$y2" ]; then
        printf '%d' "-1"
    elif [ "$y1" -gt "$y2" ]; then
        printf '%d' "1"
    # Compare month
    elif [ "$m1" -lt "$m2" ]; then
        printf '%d' "-1"
    elif [ "$m1" -gt "$m2" ]; then
        printf '%d' "1"
    # Compare day
    elif [ "$d1" -lt "$d2" ]; then
        printf '%d' "-1"
    elif [ "$d1" -gt "$d2" ]; then
        printf '%d' "1"
    else
        printf '%d' "0"
    fi
}

# Add days to a date
# Usage: newdate=$(datetime_add_days "2024-01-15" 30)
# Returns: YYYY-MM-DD
datetime_add_days() {
    startdate="$1"
    days="$2"

    # Use date command if available
    if date -d "$startdate + $days days" '+%Y-%m-%d' 2>/dev/null; then
        return 0
    fi

    # macOS/BSD fallback
    if date -j -v "+${days}d" -f "%Y-%m-%d" "$startdate" '+%Y-%m-%d' 2>/dev/null; then
        return 0
    fi

    return 1
}
