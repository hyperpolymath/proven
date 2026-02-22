\ SPDX-License-Identifier: PMPL-1.0-or-later
\ Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
\
\ Proven SafeDatetime - FFI bindings to libproven datetime operations.
\ All datetime operations are performed in verified Idris 2 code via libproven.

c-library proven_datetime
s" proven" add-lib

c-function proven-dt-parse      proven_datetime_parse          a n -- a
c-function proven-dt-format     proven_datetime_format_iso8601 a -- a
c-function proven-dt-leap       proven_datetime_is_leap_year   n -- n
c-function proven-dt-days       proven_datetime_days_in_month  n n -- n

end-c-library

\ Parse ISO 8601 datetime string via libproven
\ ( c-addr len -- status year month day hour minute second )
: parse-iso8601 ( c-addr len -- status year month day hour min sec )
    proven-dt-parse ;

\ Check if year is a leap year via libproven
\ ( year -- flag )
: leap-year? ( year -- flag )
    proven-dt-leap 0<> ;

\ Get days in month via libproven
\ ( year month -- days )
: days-in-month ( year month -- days )
    proven-dt-days ;
