\ SPDX-License-Identifier: PMPL-1.0-or-later
\ SPDX-FileCopyrightText: 2025 Hyperpolymath

\ SAFE-VERSION - Semantic versioning (SemVer) for Forth
\ Stack-based version parsing and comparison

\ ============================================================
\ Version Storage
\ ============================================================

variable ver-major
variable ver-minor
variable ver-patch
variable ver-prerelease-type  \ 0=none, 1=alpha, 2=beta, 3=rc
variable ver-prerelease-num

\ Pre-release types
0 constant VER-RELEASE
1 constant VER-ALPHA
2 constant VER-BETA
3 constant VER-RC

\ Output buffer
64 constant VER-OUT-MAX
create ver-out-buffer VER-OUT-MAX allot
variable ver-out-pos

: ver-out-reset ( -- ) 0 ver-out-pos ! ;
: ver-out-emit ( c -- ) ver-out-buffer ver-out-pos @ + c! 1 ver-out-pos +! ;
: ver-out-result ( -- addr len ) ver-out-buffer ver-out-pos @ ;

\ ============================================================
\ Parsing Helpers
\ ============================================================

\ Parse number from string
\ ( addr len -- value addr' len' flag )
: ver-parse-num ( addr len -- value addr' len' flag )
    dup 0= if 0 -rot false exit then
    0 -rot                      \ value addr len
    begin
        dup 0> while
        over c@
        dup [char] 0 >= over [char] 9 <= and if
            [char] 0 -
            rot 10 * + -rot
            1- swap 1+ swap
        else
            drop
            rot -rot true exit
        then
    repeat
    rot -rot true
;

\ Skip character if present
\ ( addr len c -- addr' len' flag )
: ver-skip-char ( addr len c -- addr' len' flag )
    -rot 2dup                   \ c addr len addr len
    0> if
        over c@ 4 pick = if
            1- swap 1+ swap
            rot drop true
        else
            rot drop false
        then
    else
        rot drop false
    then
;

\ ============================================================
\ Version Parsing
\ ============================================================

\ Parse semantic version string "MAJOR.MINOR.PATCH[-prerelease]"
\ ( addr len -- flag )
: ver-parse ( addr len -- flag )
    0 ver-major !
    0 ver-minor !
    0 ver-patch !
    VER-RELEASE ver-prerelease-type !
    0 ver-prerelease-num !

    \ Parse major version
    ver-parse-num 0= if 2drop false exit then
    ver-major !

    \ Expect dot
    [char] . ver-skip-char 0= if 2drop false exit then

    \ Parse minor version
    ver-parse-num 0= if 2drop false exit then
    ver-minor !

    \ Expect dot
    [char] . ver-skip-char 0= if 2drop false exit then

    \ Parse patch version
    ver-parse-num 0= if 2drop false exit then
    ver-patch !

    \ Check for pre-release
    dup 0= if 2drop true exit then
    over c@ [char] - = if
        1- swap 1+ swap
        \ Parse pre-release type
        2dup 5 min s" alpha" compare 0= if
            VER-ALPHA ver-prerelease-type !
            5 min - swap 5 + swap
        else 2dup 4 min s" beta" compare 0= if
            VER-BETA ver-prerelease-type !
            4 min - swap 4 + swap
        else 2dup 2 min s" rc" compare 0= if
            VER-RC ver-prerelease-type !
            2 min - swap 2 + swap
        else
            2drop false exit
        then then then
        \ Parse pre-release number if present
        dup 0> if
            over c@ [char] . = if
                1- swap 1+ swap
                ver-parse-num drop
                ver-prerelease-num !
            then
        then
    then
    2drop true
;

\ ============================================================
\ Version Components
\ ============================================================

\ Set version components
\ ( major minor patch -- )
: ver-set ( major minor patch -- )
    ver-patch ! ver-minor ! ver-major !
    VER-RELEASE ver-prerelease-type !
    0 ver-prerelease-num !
;

\ Set version with pre-release
\ ( major minor patch type num -- )
: ver-set-prerelease ( major minor patch type num -- )
    ver-prerelease-num !
    ver-prerelease-type !
    ver-patch ! ver-minor ! ver-major !
;

\ Get version components
\ ( -- major minor patch )
: ver-get ( -- major minor patch )
    ver-major @ ver-minor @ ver-patch @
;

\ Get pre-release info
\ ( -- type num )
: ver-get-prerelease ( -- type num )
    ver-prerelease-type @ ver-prerelease-num @
;

\ ============================================================
\ Version Comparison
\ ============================================================

\ Compare two versions (returns -1, 0, or 1)
\ Uses stored version vs provided version
\ ( major minor patch type num -- cmp )
: ver-compare ( major minor patch type num -- cmp )
    \ Compare major
    ver-major @ 4 pick
    2dup < if 2drop 4 ndrop -1 exit then
    2dup > if 2drop 4 ndrop 1 exit then
    2drop

    \ Compare minor
    ver-minor @ 3 pick
    2dup < if 2drop 3 ndrop -1 exit then
    2dup > if 2drop 3 ndrop 1 exit then
    2drop

    \ Compare patch
    ver-patch @ 2 pick
    2dup < if 2drop 2 ndrop -1 exit then
    2dup > if 2drop 2 ndrop 1 exit then
    2drop

    \ Compare pre-release type (release > rc > beta > alpha)
    ver-prerelease-type @ over
    over VER-RELEASE = over VER-RELEASE <> and if
        2drop 2drop 1 exit      \ stored is release, other isn't
    then
    over VER-RELEASE <> over VER-RELEASE = and if
        2drop 2drop -1 exit     \ other is release, stored isn't
    then
    2dup < if 2drop 2drop -1 exit then
    2dup > if 2drop 2drop 1 exit then
    2drop

    \ Compare pre-release number
    ver-prerelease-num @ swap
    2dup < if 2drop -1 exit then
    2dup > if 2drop 1 exit then
    2drop 0
;

\ Check if stored version is less than given version
\ ( major minor patch -- flag )
: ver< ( major minor patch -- flag )
    VER-RELEASE 0 ver-compare -1 =
;

\ Check if stored version is greater than given version
\ ( major minor patch -- flag )
: ver> ( major minor patch -- flag )
    VER-RELEASE 0 ver-compare 1 =
;

\ Check if stored version equals given version
\ ( major minor patch -- flag )
: ver= ( major minor patch -- flag )
    VER-RELEASE 0 ver-compare 0=
;

\ ============================================================
\ Version Bumping
\ ============================================================

\ Bump major version
: ver-bump-major ( -- )
    ver-major @ 1+ ver-major !
    0 ver-minor !
    0 ver-patch !
    VER-RELEASE ver-prerelease-type !
    0 ver-prerelease-num !
;

\ Bump minor version
: ver-bump-minor ( -- )
    ver-minor @ 1+ ver-minor !
    0 ver-patch !
    VER-RELEASE ver-prerelease-type !
    0 ver-prerelease-num !
;

\ Bump patch version
: ver-bump-patch ( -- )
    ver-patch @ 1+ ver-patch !
    VER-RELEASE ver-prerelease-type !
    0 ver-prerelease-num !
;

\ ============================================================
\ Version Formatting
\ ============================================================

\ Emit number to version buffer
\ ( n -- )
: ver-emit-num ( n -- )
    dup 0= if
        drop [char] 0 ver-out-emit
        exit
    then
    0 swap                      \ count n
    begin dup 0> while
        10 /mod swap
        [char] 0 + swap
        rot 1+ -rot
    repeat
    drop
    0 ?do ver-out-emit loop
;

\ Format version to string
\ ( -- addr len )
: ver-format ( -- addr len )
    ver-out-reset
    ver-major @ ver-emit-num
    [char] . ver-out-emit
    ver-minor @ ver-emit-num
    [char] . ver-out-emit
    ver-patch @ ver-emit-num
    ver-prerelease-type @ VER-RELEASE <> if
        [char] - ver-out-emit
        ver-prerelease-type @
        case
            VER-ALPHA of s" alpha" bounds ?do i c@ ver-out-emit loop endof
            VER-BETA of s" beta" bounds ?do i c@ ver-out-emit loop endof
            VER-RC of s" rc" bounds ?do i c@ ver-out-emit loop endof
        endcase
        ver-prerelease-num @ dup 0> if
            [char] . ver-out-emit
            ver-emit-num
        else
            drop
        then
    then
    ver-out-result
;

\ ============================================================
\ Compatibility Checking
\ ============================================================

\ Check if version is compatible (same major, >= minor)
\ ( major minor patch -- flag )
: ver-compatible? ( major minor patch -- flag )
    drop                        \ ignore patch
    ver-major @ over <> if 2drop false exit then
    drop
    ver-minor @ <=
;

\ Check if major version matches
\ ( major -- flag )
: ver-major= ( major -- flag )
    ver-major @ =
;

\ ============================================================
\ Module Info
\ ============================================================

: safe-version-version ( -- ) ." SAFE-VERSION for Forth v0.4.0" cr ;

: safe-version-help ( -- )
    cr
    ." SAFE-VERSION - Semantic Versioning" cr
    ." ===================================" cr
    cr
    ." Parsing:" cr
    ."   ver-parse ( addr len -- flag ) \ Parse 'X.Y.Z[-pre.N]'" cr
    cr
    ." Components:" cr
    ."   ver-set   ( major minor patch -- )" cr
    ."   ver-get   ( -- major minor patch )" cr
    ."   ver-set-prerelease ( maj min pat type num -- )" cr
    ."   ver-get-prerelease ( -- type num )" cr
    cr
    ." Pre-release types: VER-RELEASE VER-ALPHA VER-BETA VER-RC" cr
    cr
    ." Comparison:" cr
    ."   ver-compare ( maj min pat type num -- cmp )" cr
    ."   ver< ver> ver= ( major minor patch -- flag )" cr
    cr
    ." Bumping:" cr
    ."   ver-bump-major ver-bump-minor ver-bump-patch" cr
    cr
    ." Formatting:" cr
    ."   ver-format ( -- addr len )" cr
    cr
    ." Compatibility:" cr
    ."   ver-compatible? ( major minor patch -- flag )" cr
    ."   ver-major=      ( major -- flag )" cr
    cr
;
