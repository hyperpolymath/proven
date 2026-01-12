# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath
#
# SafeCurrency - Type-safe monetary values following ISO 4217.
#

package provide proven::currency 0.1.0
package require Tcl 8.6

namespace eval ::proven::currency {
    namespace export parseCode isValidCode getDecimals getSymbol getName
    namespace export moneyFromMajor moneyFromMinor moneyZero
    namespace export moneyAdd moneySub moneyMul moneyDiv
    namespace export moneyMajor moneyMinor moneyCurrency
    namespace export moneyIsZero moneyIsPositive moneyIsNegative moneyAbs
    namespace export moneyFormat
    namespace ensemble create

    # Currency data: code -> {decimals symbol name}
    variable CURRENCIES
    array set CURRENCIES {
        USD {2 "$" "US Dollar"}
        EUR {2 "\u20AC" "Euro"}
        GBP {2 "\u00A3" "British Pound"}
        JPY {0 "\u00A5" "Japanese Yen"}
        CHF {2 "Fr" "Swiss Franc"}
        CAD {2 "$" "Canadian Dollar"}
        AUD {2 "$" "Australian Dollar"}
        NZD {2 "$" "New Zealand Dollar"}
        CNY {2 "\u00A5" "Chinese Yuan"}
        INR {2 "\u20B9" "Indian Rupee"}
        BRL {2 "R$" "Brazilian Real"}
        MXN {2 "$" "Mexican Peso"}
        KRW {0 "\u20A9" "South Korean Won"}
        SGD {2 "$" "Singapore Dollar"}
        HKD {2 "$" "Hong Kong Dollar"}
        SEK {2 "kr" "Swedish Krona"}
        NOK {2 "kr" "Norwegian Krone"}
        DKK {2 "kr" "Danish Krone"}
        PLN {2 "z\u0142" "Polish Zloty"}
        RUB {2 "\u20BD" "Russian Ruble"}
        ZAR {2 "R" "South African Rand"}
        TRY {2 "\u20BA" "Turkish Lira"}
        THB {2 "\u0E3F" "Thai Baht"}
        MYR {2 "RM" "Malaysian Ringgit"}
        IDR {2 "Rp" "Indonesian Rupiah"}
        PHP {2 "\u20B1" "Philippine Peso"}
        VND {0 "\u20AB" "Vietnamese Dong"}
        AED {2 "AED" "UAE Dirham"}
        SAR {2 "SAR" "Saudi Riyal"}
        ILS {2 "\u20AA" "Israeli Shekel"}
        CZK {2 "K\u010D" "Czech Koruna"}
        HUF {2 "Ft" "Hungarian Forint"}
        RON {2 "lei" "Romanian Leu"}
        BGN {2 "лв" "Bulgarian Lev"}
        HRK {2 "kn" "Croatian Kuna"}
        ISK {0 "kr" "Icelandic Krona"}
        CLP {0 "$" "Chilean Peso"}
        COP {2 "$" "Colombian Peso"}
        PEN {2 "S/" "Peruvian Sol"}
        ARS {2 "$" "Argentine Peso"}
        BTC {8 "\u20BF" "Bitcoin"}
        ETH {8 "\u039E" "Ethereum"}
    }

    # Parse currency code from string
    # Returns dict with {code ok error}
    proc parseCode {codeString} {
        variable CURRENCIES

        set upperCode [string toupper [string trim $codeString]]

        if {$upperCode eq ""} {
            return [dict create code "" ok 0 error "Currency code is empty"]
        }

        if {![info exists CURRENCIES($upperCode)]} {
            return [dict create code "" ok 0 \
                error "Unknown currency code: $codeString"]
        }

        return [dict create code $upperCode ok 1 error ""]
    }

    # Check if valid currency code
    proc isValidCode {codeString} {
        set result [parseCode $codeString]
        return [dict get $result ok]
    }

    # Get decimal places for currency
    proc getDecimals {currencyCode} {
        variable CURRENCIES

        set upperCode [string toupper $currencyCode]
        if {![info exists CURRENCIES($upperCode)]} {
            return 2
        }

        return [lindex $CURRENCIES($upperCode) 0]
    }

    # Get symbol for currency
    proc getSymbol {currencyCode} {
        variable CURRENCIES

        set upperCode [string toupper $currencyCode]
        if {![info exists CURRENCIES($upperCode)]} {
            return ""
        }

        return [lindex $CURRENCIES($upperCode) 1]
    }

    # Get name for currency
    proc getName {currencyCode} {
        variable CURRENCIES

        set upperCode [string toupper $currencyCode]
        if {![info exists CURRENCIES($upperCode)]} {
            return "Currency"
        }

        return [lindex $CURRENCIES($upperCode) 2]
    }

    # Create money dict from major units (dollars, euros, etc.)
    # Returns dict with {money ok error}
    # Money dict contains: {minor_units currency}
    proc moneyFromMajor {amount currencyCode} {
        set parseResult [parseCode $currencyCode]
        if {![dict get $parseResult ok]} {
            return [dict create money {} ok 0 \
                error [dict get $parseResult error]]
        }

        set code [dict get $parseResult code]
        set decimals [getDecimals $code]
        set multiplier [expr {10 ** $decimals}]

        # Convert to minor units
        set minorUnits [expr {wide($amount) * $multiplier}]

        set moneyDict [dict create minor_units $minorUnits currency $code]
        return [dict create money $moneyDict ok 1 error ""]
    }

    # Create money dict from minor units (cents, satoshis, etc.)
    # Returns dict with {money ok error}
    proc moneyFromMinor {amount currencyCode} {
        set parseResult [parseCode $currencyCode]
        if {![dict get $parseResult ok]} {
            return [dict create money {} ok 0 \
                error [dict get $parseResult error]]
        }

        set code [dict get $parseResult code]
        set moneyDict [dict create minor_units [expr {wide($amount)}] currency $code]
        return [dict create money $moneyDict ok 1 error ""]
    }

    # Create zero money
    proc moneyZero {currencyCode} {
        return [moneyFromMinor 0 $currencyCode]
    }

    # Add two money values
    # Returns dict with {money ok error}
    proc moneyAdd {moneyA moneyB} {
        set currencyA [dict get $moneyA currency]
        set currencyB [dict get $moneyB currency]

        if {$currencyA ne $currencyB} {
            return [dict create money {} ok 0 \
                error "Currency mismatch: $currencyA vs $currencyB"]
        }

        set minorA [dict get $moneyA minor_units]
        set minorB [dict get $moneyB minor_units]
        set result [expr {$minorA + $minorB}]

        set moneyDict [dict create minor_units $result currency $currencyA]
        return [dict create money $moneyDict ok 1 error ""]
    }

    # Subtract two money values
    # Returns dict with {money ok error}
    proc moneySub {moneyA moneyB} {
        set currencyA [dict get $moneyA currency]
        set currencyB [dict get $moneyB currency]

        if {$currencyA ne $currencyB} {
            return [dict create money {} ok 0 \
                error "Currency mismatch: $currencyA vs $currencyB"]
        }

        set minorA [dict get $moneyA minor_units]
        set minorB [dict get $moneyB minor_units]
        set result [expr {$minorA - $minorB}]

        set moneyDict [dict create minor_units $result currency $currencyA]
        return [dict create money $moneyDict ok 1 error ""]
    }

    # Multiply money by scalar
    # Returns dict with {money ok error}
    proc moneyMul {moneyValue scalar} {
        set minorUnits [dict get $moneyValue minor_units]
        set currency [dict get $moneyValue currency]
        set result [expr {$minorUnits * $scalar}]

        set moneyDict [dict create minor_units $result currency $currency]
        return [dict create money $moneyDict ok 1 error ""]
    }

    # Divide money by scalar
    # Returns dict with {money ok error}
    proc moneyDiv {moneyValue scalar} {
        if {$scalar == 0} {
            return [dict create money {} ok 0 error "Division by zero"]
        }

        set minorUnits [dict get $moneyValue minor_units]
        set currency [dict get $moneyValue currency]
        set result [expr {$minorUnits / $scalar}]

        set moneyDict [dict create minor_units $result currency $currency]
        return [dict create money $moneyDict ok 1 error ""]
    }

    # Get major units (truncated)
    proc moneyMajor {moneyValue} {
        set minorUnits [dict get $moneyValue minor_units]
        set currency [dict get $moneyValue currency]
        set decimals [getDecimals $currency]
        set divisor [expr {10 ** $decimals}]
        return [expr {$minorUnits / $divisor}]
    }

    # Get minor units
    proc moneyMinor {moneyValue} {
        return [dict get $moneyValue minor_units]
    }

    # Get currency code
    proc moneyCurrency {moneyValue} {
        return [dict get $moneyValue currency]
    }

    # Check if zero
    proc moneyIsZero {moneyValue} {
        return [expr {[dict get $moneyValue minor_units] == 0}]
    }

    # Check if positive
    proc moneyIsPositive {moneyValue} {
        return [expr {[dict get $moneyValue minor_units] > 0}]
    }

    # Check if negative
    proc moneyIsNegative {moneyValue} {
        return [expr {[dict get $moneyValue minor_units] < 0}]
    }

    # Absolute value
    # Returns dict with {money ok error}
    proc moneyAbs {moneyValue} {
        set minorUnits [dict get $moneyValue minor_units]
        set currency [dict get $moneyValue currency]
        set result [expr {abs($minorUnits)}]

        set moneyDict [dict create minor_units $result currency $currency]
        return [dict create money $moneyDict ok 1 error ""]
    }

    # Format money as string
    proc moneyFormat {moneyValue} {
        set minorUnits [dict get $moneyValue minor_units]
        set currency [dict get $moneyValue currency]
        set decimals [getDecimals $currency]
        set symbol [getSymbol $currency]
        set divisor [expr {10 ** $decimals}]

        set absMinor [expr {abs($minorUnits)}]
        set majorPart [expr {$absMinor / $divisor}]
        set minorPart [expr {$absMinor % $divisor}]
        set signPrefix [expr {$minorUnits < 0 ? "-" : ""}]

        if {$decimals == 0} {
            return "${signPrefix}${symbol}${majorPart}"
        } else {
            set formatSpec "%0${decimals}d"
            set minorStr [format $formatSpec $minorPart]
            return "${signPrefix}${symbol}${majorPart}.${minorStr}"
        }
    }
}
