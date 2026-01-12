      * SPDX-License-Identifier: AGPL-3.0-or-later
      * SPDX-FileCopyrightText: 2025 Hyperpolymath
      *
      * Proven SafePhone - Phone number data structures for COBOL
      *

      * Country calling codes (ITU-T E.164)
       01 COUNTRY-CODE                PIC 9(4).
          88 CC-US-CA                 VALUE 1.
          88 CC-RUSSIA                VALUE 7.
          88 CC-EGYPT                 VALUE 20.
          88 CC-SOUTH-AFRICA          VALUE 27.
          88 CC-GREECE                VALUE 30.
          88 CC-NETHERLANDS           VALUE 31.
          88 CC-BELGIUM               VALUE 32.
          88 CC-FRANCE                VALUE 33.
          88 CC-SPAIN                 VALUE 34.
          88 CC-HUNGARY               VALUE 36.
          88 CC-ITALY                 VALUE 39.
          88 CC-ROMANIA               VALUE 40.
          88 CC-SWITZERLAND           VALUE 41.
          88 CC-AUSTRIA               VALUE 43.
          88 CC-UK                    VALUE 44.
          88 CC-DENMARK               VALUE 45.
          88 CC-SWEDEN                VALUE 46.
          88 CC-NORWAY                VALUE 47.
          88 CC-POLAND                VALUE 48.
          88 CC-GERMANY               VALUE 49.
          88 CC-PERU                  VALUE 51.
          88 CC-MEXICO                VALUE 52.
          88 CC-CUBA                  VALUE 53.
          88 CC-ARGENTINA             VALUE 54.
          88 CC-BRAZIL                VALUE 55.
          88 CC-CHILE                 VALUE 56.
          88 CC-COLOMBIA              VALUE 57.
          88 CC-VENEZUELA             VALUE 58.
          88 CC-MALAYSIA              VALUE 60.
          88 CC-AUSTRALIA             VALUE 61.
          88 CC-INDONESIA             VALUE 62.
          88 CC-PHILIPPINES           VALUE 63.
          88 CC-NEW-ZEALAND           VALUE 64.
          88 CC-SINGAPORE             VALUE 65.
          88 CC-THAILAND              VALUE 66.
          88 CC-JAPAN                 VALUE 81.
          88 CC-SOUTH-KOREA           VALUE 82.
          88 CC-VIETNAM               VALUE 84.
          88 CC-CHINA                 VALUE 86.
          88 CC-TURKEY                VALUE 90.
          88 CC-INDIA                 VALUE 91.
          88 CC-PAKISTAN              VALUE 92.
          88 CC-AFGHANISTAN           VALUE 93.
          88 CC-SRI-LANKA             VALUE 94.
          88 CC-IRAN                  VALUE 98.
          88 CC-MOROCCO               VALUE 212.
          88 CC-ALGERIA               VALUE 213.
          88 CC-TUNISIA               VALUE 216.
          88 CC-LIBYA                 VALUE 218.
          88 CC-NIGERIA               VALUE 234.
          88 CC-GHANA                 VALUE 233.
          88 CC-KENYA                 VALUE 254.
          88 CC-TANZANIA              VALUE 255.
          88 CC-UAE                   VALUE 971.
          88 CC-ISRAEL                VALUE 972.
          88 CC-SAUDI-ARABIA          VALUE 966.
          88 CC-QATAR                 VALUE 974.
          88 CC-KUWAIT                VALUE 965.

      * Phone number record (E.164 format)
       01 PHONE-NUMBER-RECORD.
          05 PHONE-COUNTRY-CODE       PIC 9(4).
          05 PHONE-NATIONAL-NUMBER    PIC X(15).
          05 PHONE-EXTENSION          PIC X(10).
          05 PHONE-RAW-INPUT          PIC X(30).
          05 PHONE-E164-FORMAT        PIC X(20).
          05 PHONE-NATIONAL-FORMAT    PIC X(25).
          05 PHONE-INTERNATIONAL-FMT  PIC X(25).

      * Phone number type classification
       01 PHONE-NUMBER-TYPE           PIC 9 VALUE 0.
          88 PHONE-TYPE-UNKNOWN       VALUE 0.
          88 PHONE-TYPE-FIXED-LINE    VALUE 1.
          88 PHONE-TYPE-MOBILE        VALUE 2.
          88 PHONE-TYPE-FIXED-OR-MOB  VALUE 3.
          88 PHONE-TYPE-TOLL-FREE     VALUE 4.
          88 PHONE-TYPE-PREMIUM-RATE  VALUE 5.
          88 PHONE-TYPE-SHARED-COST   VALUE 6.
          88 PHONE-TYPE-VOIP          VALUE 7.
          88 PHONE-TYPE-PERSONAL      VALUE 8.
          88 PHONE-TYPE-PAGER         VALUE 9.

      * Phone parsing result
       01 PHONE-PARSE-RESULT.
          05 PHONE-PARSE-STATUS       PIC 9 VALUE 0.
             88 PHONE-PARSE-OK        VALUE 1.
             88 PHONE-PARSE-FAILED    VALUE 0.
          05 PHONE-PARSE-ERROR        PIC X(50).
          05 PHONE-VALIDATION-STATUS  PIC 9 VALUE 0.
             88 PHONE-VALID           VALUE 1.
             88 PHONE-INVALID         VALUE 0.
             88 PHONE-POSSIBLE        VALUE 2.

      * Country information for phone formatting
       01 PHONE-COUNTRY-INFO.
          05 PHONE-COUNTRY-ISO        PIC X(2).
          05 PHONE-COUNTRY-NAME       PIC X(40).
          05 PHONE-NATIONAL-PREFIX    PIC X(3).
          05 PHONE-INTL-PREFIX        PIC X(5).
          05 PHONE-MOBILE-PREFIX      PIC X(5).
          05 PHONE-TYPICAL-LENGTH     PIC 9(2).

      * Formatting options
       01 PHONE-FORMAT-OPTIONS.
          05 PHONE-FMT-TYPE           PIC 9 VALUE 0.
             88 PHONE-FMT-E164        VALUE 0.
             88 PHONE-FMT-INTL        VALUE 1.
             88 PHONE-FMT-NATIONAL    VALUE 2.
             88 PHONE-FMT-RFC3966     VALUE 3.
          05 PHONE-FMT-SEPARATOR      PIC X VALUE " ".
          05 PHONE-FMT-INCLUDE-EXT    PIC 9 VALUE 1.

