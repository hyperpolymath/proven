# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
#
# SafePhone - Phone number validation following E.164.
#

package provide proven::phone 0.1.0
package require Tcl 8.6

namespace eval ::proven::phone {
    namespace export parse isValid toE164 toInternational toNational
    namespace export getCountryCode getNationalNumber getDigitCount
    namespace export parseCountryCode countryCodeValue countryCodeName
    namespace ensemble create

    # Country calling codes: code -> {name}
    variable COUNTRY_CODES
    array set COUNTRY_CODES {
        1   {US "United States / Canada"}
        7   {RU "Russia"}
        20  {EG "Egypt"}
        27  {ZA "South Africa"}
        30  {GR "Greece"}
        31  {NL "Netherlands"}
        32  {BE "Belgium"}
        33  {FR "France"}
        34  {ES "Spain"}
        36  {HU "Hungary"}
        39  {IT "Italy"}
        40  {RO "Romania"}
        41  {CH "Switzerland"}
        43  {AT "Austria"}
        44  {UK "United Kingdom"}
        45  {DK "Denmark"}
        46  {SE "Sweden"}
        47  {NO "Norway"}
        48  {PL "Poland"}
        49  {DE "Germany"}
        51  {PE "Peru"}
        52  {MX "Mexico"}
        53  {CU "Cuba"}
        54  {AR "Argentina"}
        55  {BR "Brazil"}
        56  {CL "Chile"}
        57  {CO "Colombia"}
        58  {VE "Venezuela"}
        60  {MY "Malaysia"}
        61  {AU "Australia"}
        62  {ID "Indonesia"}
        63  {PH "Philippines"}
        64  {NZ "New Zealand"}
        65  {SG "Singapore"}
        66  {TH "Thailand"}
        81  {JP "Japan"}
        82  {KR "South Korea"}
        84  {VN "Vietnam"}
        86  {CN "China"}
        90  {TR "Turkey"}
        91  {IN "India"}
        92  {PK "Pakistan"}
        93  {AF "Afghanistan"}
        94  {LK "Sri Lanka"}
        95  {MM "Myanmar"}
        98  {IR "Iran"}
        212 {MA "Morocco"}
        213 {DZ "Algeria"}
        216 {TN "Tunisia"}
        220 {GM "Gambia"}
        234 {NG "Nigeria"}
        249 {SD "Sudan"}
        254 {KE "Kenya"}
        255 {TZ "Tanzania"}
        256 {UG "Uganda"}
        260 {ZM "Zambia"}
        263 {ZW "Zimbabwe"}
        351 {PT "Portugal"}
        352 {LU "Luxembourg"}
        353 {IE "Ireland"}
        354 {IS "Iceland"}
        358 {FI "Finland"}
        370 {LT "Lithuania"}
        371 {LV "Latvia"}
        372 {EE "Estonia"}
        380 {UA "Ukraine"}
        381 {RS "Serbia"}
        385 {HR "Croatia"}
        386 {SI "Slovenia"}
        420 {CZ "Czech Republic"}
        421 {SK "Slovakia"}
        852 {HK "Hong Kong"}
        853 {MO "Macau"}
        855 {KH "Cambodia"}
        856 {LA "Laos"}
        880 {BD "Bangladesh"}
        886 {TW "Taiwan"}
        960 {MV "Maldives"}
        961 {LB "Lebanon"}
        962 {JO "Jordan"}
        963 {SY "Syria"}
        964 {IQ "Iraq"}
        965 {KW "Kuwait"}
        966 {SA "Saudi Arabia"}
        967 {YE "Yemen"}
        968 {OM "Oman"}
        971 {AE "United Arab Emirates"}
        972 {IL "Israel"}
        973 {BH "Bahrain"}
        974 {QA "Qatar"}
        975 {BT "Bhutan"}
        976 {MN "Mongolia"}
        977 {NP "Nepal"}
    }

    # Extract digits only from input
    proc _extractDigits {input} {
        regsub -all {[^0-9]} $input {} digits
        return $digits
    }

    # Try to parse country code from beginning of digit string
    # Returns {countryCode startIndex} or {0 0} on failure
    proc _tryParseCountryCode {digits} {
        variable COUNTRY_CODES

        # Try 3-digit codes first, then 2, then 1
        foreach codeLen {3 2 1} {
            if {[string length $digits] >= $codeLen} {
                set potentialCode [string range $digits 0 [expr {$codeLen - 1}]]
                # Remove leading zeros for lookup
                set codeValue [scan $potentialCode %d]
                if {$codeValue ne "" && [info exists COUNTRY_CODES($codeValue)]} {
                    return [list $codeValue $codeLen]
                }
            }
        }

        return [list 0 0]
    }

    # Parse phone number from string
    # Returns dict with {phone ok error}
    # Phone dict contains: {country_code national_number}
    proc parse {input} {
        set trimmedInput [string trim $input]

        if {$trimmedInput eq ""} {
            return [dict create phone {} ok 0 error "Phone number is empty"]
        }

        # Extract digits only
        set digits [_extractDigits $trimmedInput]

        if {[string length $digits] < 7} {
            return [dict create phone {} ok 0 \
                error "Phone number too short: minimum 7 digits"]
        }

        if {[string length $digits] > 15} {
            return [dict create phone {} ok 0 \
                error "Phone number too long: maximum 15 digits (E.164)"]
        }

        # Try to parse country code
        lassign [_tryParseCountryCode $digits] countryCode nationalStart

        if {$countryCode == 0} {
            return [dict create phone {} ok 0 \
                error "Unknown country code"]
        }

        # Extract national number
        set nationalNumber [string range $digits $nationalStart end]

        if {[string length $nationalNumber] < 4} {
            return [dict create phone {} ok 0 \
                error "National number too short"]
        }

        set phoneDict [dict create \
            country_code $countryCode \
            national_number $nationalNumber]

        return [dict create phone $phoneDict ok 1 error ""]
    }

    # Check if valid phone number
    proc isValid {input} {
        set result [parse $input]
        return [dict get $result ok]
    }

    # Format phone in E.164 format (+CCNNN...)
    proc toE164 {phoneValue} {
        set countryCode [dict get $phoneValue country_code]
        set nationalNumber [dict get $phoneValue national_number]
        return "+${countryCode}${nationalNumber}"
    }

    # Format phone in international format (+CC NNN NNN NNNN)
    proc toInternational {phoneValue} {
        set countryCode [dict get $phoneValue country_code]
        set nationalNumber [dict get $phoneValue national_number]
        set nationalLength [string length $nationalNumber]

        if {$nationalLength <= 4} {
            return "+$countryCode $nationalNumber"
        } elseif {$nationalLength <= 7} {
            set part1 [string range $nationalNumber 0 2]
            set part2 [string range $nationalNumber 3 end]
            return "+$countryCode $part1 $part2"
        } elseif {$nationalLength <= 10} {
            set part1 [string range $nationalNumber 0 2]
            set part2 [string range $nationalNumber 3 5]
            set part3 [string range $nationalNumber 6 end]
            return "+$countryCode $part1 $part2 $part3"
        } else {
            return "+$countryCode $nationalNumber"
        }
    }

    # Format phone in national format (no country code)
    proc toNational {phoneValue} {
        set nationalNumber [dict get $phoneValue national_number]
        set nationalLength [string length $nationalNumber]

        if {$nationalLength <= 4} {
            return $nationalNumber
        } elseif {$nationalLength <= 7} {
            set part1 [string range $nationalNumber 0 2]
            set part2 [string range $nationalNumber 3 end]
            return "$part1-$part2"
        } elseif {$nationalLength == 10} {
            # US-style: (NNN) NNN-NNNN
            set area [string range $nationalNumber 0 2]
            set exchange [string range $nationalNumber 3 5]
            set subscriber [string range $nationalNumber 6 end]
            return "($area) $exchange-$subscriber"
        } else {
            return $nationalNumber
        }
    }

    # Get country code from phone
    proc getCountryCode {phoneValue} {
        return [dict get $phoneValue country_code]
    }

    # Get national number from phone
    proc getNationalNumber {phoneValue} {
        return [dict get $phoneValue national_number]
    }

    # Get total digit count (country code + national number)
    proc getDigitCount {phoneValue} {
        set countryCode [dict get $phoneValue country_code]
        set nationalNumber [dict get $phoneValue national_number]

        # Count digits in country code
        set countryCodeDigits [string length $countryCode]

        return [expr {$countryCodeDigits + [string length $nationalNumber]}]
    }

    # Parse just a country code from string
    # Returns dict with {code ok error}
    proc parseCountryCode {codeString} {
        variable COUNTRY_CODES

        set trimmedInput [string trim $codeString]
        set digits [_extractDigits $trimmedInput]

        if {$digits eq ""} {
            return [dict create code 0 ok 0 error "No digits in country code"]
        }

        set codeValue [scan $digits %d]
        if {$codeValue eq "" || ![info exists COUNTRY_CODES($codeValue)]} {
            return [dict create code 0 ok 0 \
                error "Unknown country code: $codeString"]
        }

        return [dict create code $codeValue ok 1 error ""]
    }

    # Get numeric value of country code
    proc countryCodeValue {countryCode} {
        return $countryCode
    }

    # Get country name for country code
    proc countryCodeName {countryCode} {
        variable COUNTRY_CODES

        if {[info exists COUNTRY_CODES($countryCode)]} {
            return [lindex $COUNTRY_CODES($countryCode) 1]
        }
        return "Unknown"
    }
}
