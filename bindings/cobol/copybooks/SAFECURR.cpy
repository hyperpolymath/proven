      * SPDX-License-Identifier: AGPL-3.0-or-later
      * SPDX-FileCopyrightText: 2025 Hyperpolymath
      *
      * Proven SafeCurrency - Currency data structures for COBOL
      *

      * ISO 4217 Currency code (3-letter)
       01 CURRENCY-CODE               PIC X(3).
          88 CURR-USD                 VALUE "USD".
          88 CURR-EUR                 VALUE "EUR".
          88 CURR-GBP                 VALUE "GBP".
          88 CURR-JPY                 VALUE "JPY".
          88 CURR-CHF                 VALUE "CHF".
          88 CURR-CAD                 VALUE "CAD".
          88 CURR-AUD                 VALUE "AUD".
          88 CURR-NZD                 VALUE "NZD".
          88 CURR-CNY                 VALUE "CNY".
          88 CURR-HKD                 VALUE "HKD".
          88 CURR-SGD                 VALUE "SGD".
          88 CURR-SEK                 VALUE "SEK".
          88 CURR-NOK                 VALUE "NOK".
          88 CURR-DKK                 VALUE "DKK".
          88 CURR-KRW                 VALUE "KRW".
          88 CURR-INR                 VALUE "INR".
          88 CURR-BRL                 VALUE "BRL".
          88 CURR-MXN                 VALUE "MXN".
          88 CURR-ZAR                 VALUE "ZAR".
          88 CURR-RUB                 VALUE "RUB".
          88 CURR-PLN                 VALUE "PLN".
          88 CURR-THB                 VALUE "THB".
          88 CURR-IDR                 VALUE "IDR".
          88 CURR-MYR                 VALUE "MYR".
          88 CURR-PHP                 VALUE "PHP".
          88 CURR-TRY                 VALUE "TRY".
          88 CURR-AED                 VALUE "AED".
          88 CURR-SAR                 VALUE "SAR".
          88 CURR-ILS                 VALUE "ILS".
          88 CURR-CZK                 VALUE "CZK".
          88 CURR-HUF                 VALUE "HUF".
          88 CURR-BTC                 VALUE "BTC".
          88 CURR-ETH                 VALUE "ETH".
          88 CURR-XAU                 VALUE "XAU".
          88 CURR-XAG                 VALUE "XAG".

      * Money record with currency and amount
      * Amount stored as minor units (cents, pence, etc.)
       01 MONEY-RECORD.
          05 MONEY-CURRENCY           PIC X(3).
          05 MONEY-AMOUNT             PIC S9(15)V9(4) COMP-3.
          05 MONEY-MINOR-UNITS        PIC S9(18) COMP-3.
          05 MONEY-DECIMAL-PLACES     PIC 9 VALUE 2.

      * Currency information table entry
       01 CURRENCY-INFO.
          05 CURR-INFO-CODE           PIC X(3).
          05 CURR-INFO-NUMERIC        PIC 9(3).
          05 CURR-INFO-DECIMALS       PIC 9 VALUE 2.
          05 CURR-INFO-NAME           PIC X(30).
          05 CURR-INFO-SYMBOL         PIC X(5).

      * Arithmetic operation result
       01 MONEY-RESULT.
          05 MONEY-RESULT-STATUS      PIC 9 VALUE 0.
             88 MONEY-OP-OK           VALUE 1.
             88 MONEY-OP-OVERFLOW     VALUE 0.
             88 MONEY-OP-UNDERFLOW    VALUE 2.
             88 MONEY-CURRENCY-MISMATCH VALUE 3.
          05 MONEY-RESULT-VALUE       PIC S9(15)V9(4) COMP-3.
          05 MONEY-RESULT-MINOR       PIC S9(18) COMP-3.
          05 MONEY-RESULT-ERROR       PIC X(50).

      * Exchange rate record
       01 EXCHANGE-RATE-RECORD.
          05 EXCH-FROM-CURRENCY       PIC X(3).
          05 EXCH-TO-CURRENCY         PIC X(3).
          05 EXCH-RATE                PIC 9(6)V9(8).
          05 EXCH-TIMESTAMP           PIC 9(14).
          05 EXCH-VALID               PIC 9 VALUE 0.
             88 EXCH-RATE-VALID       VALUE 1.
             88 EXCH-RATE-INVALID     VALUE 0.

      * Rounding modes
       01 MONEY-ROUNDING-MODE         PIC 9 VALUE 0.
          88 ROUND-HALF-UP            VALUE 0.
          88 ROUND-HALF-DOWN          VALUE 1.
          88 ROUND-HALF-EVEN          VALUE 2.
          88 ROUND-CEILING            VALUE 3.
          88 ROUND-FLOOR              VALUE 4.
          88 ROUND-TRUNCATE           VALUE 5.

      * Formatting options
       01 MONEY-FORMAT-OPTIONS.
          05 FORMAT-SHOW-SYMBOL       PIC 9 VALUE 1.
          05 FORMAT-SYMBOL-POSITION   PIC 9 VALUE 0.
             88 FORMAT-SYMBOL-BEFORE  VALUE 0.
             88 FORMAT-SYMBOL-AFTER   VALUE 1.
          05 FORMAT-THOUSANDS-SEP     PIC X VALUE ",".
          05 FORMAT-DECIMAL-SEP       PIC X VALUE ".".
          05 FORMAT-NEGATIVE-FORMAT   PIC 9 VALUE 0.
             88 FORMAT-NEG-MINUS      VALUE 0.
             88 FORMAT-NEG-PARENS     VALUE 1.
             88 FORMAT-NEG-CR         VALUE 2.

