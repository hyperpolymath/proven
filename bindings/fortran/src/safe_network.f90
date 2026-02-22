! SPDX-License-Identifier: PMPL-1.0-or-later
! Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
!
! Proven SafeNetwork - IP address parsing and classification for Fortran
!
! Thin wrapper around libproven's verified SafeNetwork module.
! All IP parsing and classification logic is executed in the Idris 2 core
! via the Zig FFI bridge; this module only marshals types between Fortran
! and C.
!
! Link with: -lproven

module safe_network
    use, intrinsic :: iso_c_binding
    use proven_ffi
    implicit none
    private

    !> IPv4 address type (Fortran-facing)
    type, public :: IPv4Address
        integer(c_int8_t) :: octets(4) = [0_c_int8_t, 0_c_int8_t, &
                                           0_c_int8_t, 0_c_int8_t]
        logical            :: valid     = .false.
        integer(c_int32_t) :: status    = PROVEN_ERR_NOT_IMPLEMENTED
    end type IPv4Address

    public :: parse_ipv4, is_loopback, is_private_ip

contains

    ! =========================================================================
    ! Parse IPv4 address string  ->  proven_network_parse_ipv4
    ! =========================================================================
    function parse_ipv4(address) result(ip)
        character(len=*), intent(in) :: address
        type(IPv4Address) :: ip
        character(len=len_trim(address), kind=c_char), target :: c_buf
        type(c_proven_ipv4_result) :: c_res
        integer :: trimmed_len

        trimmed_len = len_trim(address)
        c_buf = address(1:trimmed_len)

        c_res = proven_network_parse_ipv4(c_loc(c_buf), int(trimmed_len, c_size_t))

        ip%status = c_res%status
        ip%valid  = (c_res%status == PROVEN_OK)

        if (ip%valid) then
            ip%octets = c_res%address%octets
        end if
    end function parse_ipv4

    ! =========================================================================
    ! Check if IPv4 is loopback  ->  proven_network_ipv4_is_loopback
    ! =========================================================================
    function is_loopback(ip) result(is_loop)
        type(IPv4Address), intent(in) :: ip
        logical :: is_loop
        type(c_proven_ipv4_address) :: c_addr

        c_addr%octets = ip%octets
        is_loop = logical(proven_network_ipv4_is_loopback(c_addr))
    end function is_loopback

    ! =========================================================================
    ! Check if IPv4 is private (RFC 1918)  ->  proven_network_ipv4_is_private
    ! =========================================================================
    function is_private_ip(ip) result(is_priv)
        type(IPv4Address), intent(in) :: ip
        logical :: is_priv
        type(c_proven_ipv4_address) :: c_addr

        c_addr%octets = ip%octets
        is_priv = logical(proven_network_ipv4_is_private(c_addr))
    end function is_private_ip

end module safe_network
