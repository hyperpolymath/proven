! SPDX-License-Identifier: PMPL-1.0
! SPDX-FileCopyrightText: 2025 Hyperpolymath
!
! Proven SafeNetwork - IP validation for Fortran
!

module safe_network
    implicit none
    private

    !> IP classification enum
    integer, parameter, public :: IP_CLASS_INVALID  = 0
    integer, parameter, public :: IP_CLASS_LOOPBACK = 1
    integer, parameter, public :: IP_CLASS_PRIVATE  = 2
    integer, parameter, public :: IP_CLASS_RESERVED = 3
    integer, parameter, public :: IP_CLASS_PUBLIC   = 4

    !> IPv4 address type
    type, public :: IPv4Address
        integer :: octets(4) = [0, 0, 0, 0]
        logical :: valid = .false.
    end type IPv4Address

    public :: parse_ipv4, format_ipv4, is_loopback, is_private_ip, is_reserved_ip
    public :: is_public_ip, classify_ip, is_valid_port, is_privileged_port

contains

    !> Parse IPv4 address string
    function parse_ipv4(address) result(ip)
        character(len=*), intent(in) :: address
        type(IPv4Address) :: ip
        integer :: i, pos, start, octet_idx, value, addr_len
        character(len=1) :: c
        character(len=3) :: octet_str

        ip%valid = .false.
        addr_len = len_trim(address)

        if (addr_len == 0 .or. addr_len > 15) return

        octet_idx = 1
        start = 1
        pos = 1

        do while (pos <= addr_len + 1)
            if (pos > addr_len) then
                c = '.'  ! Treat end as delimiter
            else
                c = address(pos:pos)
            end if

            if (c == '.' .or. pos > addr_len) then
                if (pos == start) return  ! Empty octet

                ! Extract octet string
                octet_str = ''
                if (pos - start > 3) return  ! Too many digits
                octet_str(1:pos-start) = address(start:pos-1)

                ! Check for leading zeros
                if (pos - start > 1 .and. address(start:start) == '0') return

                ! Parse value
                read(octet_str, *, err=999) value
                if (value < 0 .or. value > 255) return

                if (octet_idx > 4) return  ! Too many octets
                ip%octets(octet_idx) = value
                octet_idx = octet_idx + 1
                start = pos + 1
            else if (c < '0' .or. c > '9') then
                return  ! Invalid character
            end if

            pos = pos + 1
        end do

        if (octet_idx /= 5) return  ! Wrong number of octets

        ip%valid = .true.
        return

999     continue  ! Parse error
        return
    end function parse_ipv4

    !> Format IPv4 address as string
    function format_ipv4(ip) result(str)
        type(IPv4Address), intent(in) :: ip
        character(len=15) :: str

        write(str, '(I0,".",I0,".",I0,".",I0)') &
            ip%octets(1), ip%octets(2), ip%octets(3), ip%octets(4)
        str = adjustl(str)
    end function format_ipv4

    !> Check if IP is loopback (127.x.x.x)
    pure function is_loopback(ip) result(is_loop)
        type(IPv4Address), intent(in) :: ip
        logical :: is_loop

        is_loop = ip%octets(1) == 127
    end function is_loopback

    !> Check if IP is private (RFC 1918)
    pure function is_private_ip(ip) result(is_private)
        type(IPv4Address), intent(in) :: ip
        logical :: is_private

        is_private = .false.

        ! 10.0.0.0/8
        if (ip%octets(1) == 10) then
            is_private = .true.
            return
        end if

        ! 172.16.0.0/12
        if (ip%octets(1) == 172 .and. &
            ip%octets(2) >= 16 .and. ip%octets(2) <= 31) then
            is_private = .true.
            return
        end if

        ! 192.168.0.0/16
        if (ip%octets(1) == 192 .and. ip%octets(2) == 168) then
            is_private = .true.
            return
        end if
    end function is_private_ip

    !> Check if IP is reserved
    pure function is_reserved_ip(ip) result(is_reserved)
        type(IPv4Address), intent(in) :: ip
        logical :: is_reserved

        is_reserved = .false.

        ! 0.0.0.0/8
        if (ip%octets(1) == 0) then
            is_reserved = .true.
            return
        end if

        ! 100.64.0.0/10 (CGNAT)
        if (ip%octets(1) == 100 .and. &
            ip%octets(2) >= 64 .and. ip%octets(2) <= 127) then
            is_reserved = .true.
            return
        end if

        ! 169.254.0.0/16 (link-local)
        if (ip%octets(1) == 169 .and. ip%octets(2) == 254) then
            is_reserved = .true.
            return
        end if

        ! 192.0.0.0/24, 192.0.2.0/24
        if (ip%octets(1) == 192 .and. ip%octets(2) == 0) then
            if (ip%octets(3) == 0 .or. ip%octets(3) == 2) then
                is_reserved = .true.
                return
            end if
        end if

        ! 198.51.100.0/24
        if (ip%octets(1) == 198 .and. ip%octets(2) == 51 .and. &
            ip%octets(3) == 100) then
            is_reserved = .true.
            return
        end if

        ! 203.0.113.0/24
        if (ip%octets(1) == 203 .and. ip%octets(2) == 0 .and. &
            ip%octets(3) == 113) then
            is_reserved = .true.
            return
        end if

        ! 224.0.0.0/4 (multicast)
        if (ip%octets(1) >= 224 .and. ip%octets(1) <= 239) then
            is_reserved = .true.
            return
        end if

        ! 240.0.0.0/4 (reserved/broadcast)
        if (ip%octets(1) >= 240) then
            is_reserved = .true.
            return
        end if
    end function is_reserved_ip

    !> Check if IP is public
    pure function is_public_ip(ip) result(is_public)
        type(IPv4Address), intent(in) :: ip
        logical :: is_public

        is_public = .not. is_loopback(ip) .and. &
                    .not. is_private_ip(ip) .and. &
                    .not. is_reserved_ip(ip)
    end function is_public_ip

    !> Classify an IP address
    pure function classify_ip(ip) result(classification)
        type(IPv4Address), intent(in) :: ip
        integer :: classification

        if (.not. ip%valid) then
            classification = IP_CLASS_INVALID
        else if (is_loopback(ip)) then
            classification = IP_CLASS_LOOPBACK
        else if (is_private_ip(ip)) then
            classification = IP_CLASS_PRIVATE
        else if (is_reserved_ip(ip)) then
            classification = IP_CLASS_RESERVED
        else
            classification = IP_CLASS_PUBLIC
        end if
    end function classify_ip

    !> Check if port is valid (1-65535)
    pure function is_valid_port(port) result(is_valid)
        integer, intent(in) :: port
        logical :: is_valid

        is_valid = port >= 1 .and. port <= 65535
    end function is_valid_port

    !> Check if port is privileged (<1024)
    pure function is_privileged_port(port) result(is_privileged)
        integer, intent(in) :: port
        logical :: is_privileged

        is_privileged = port >= 1 .and. port < 1024
    end function is_privileged_port

end module safe_network
