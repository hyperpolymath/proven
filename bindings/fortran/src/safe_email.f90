! SPDX-License-Identifier: PMPL-1.0-or-later
! SPDX-FileCopyrightText: 2025 Hyperpolymath
!
! Proven SafeEmail - Email validation for Fortran
!

module safe_email
    implicit none
    private

    integer, parameter :: MAX_EMAIL_LEN = 254
    integer, parameter :: MAX_LOCAL_LEN = 64
    integer, parameter :: MAX_DOMAIN_LEN = 255

    !> Email result type
    type, public :: EmailResult
        character(len=64) :: local_part = ''
        character(len=255) :: domain = ''
        character(len=256) :: error = ''
        logical :: ok = .false.
    end type EmailResult

    !> List of known disposable email domains
    character(len=20), dimension(8), parameter :: DISPOSABLE_DOMAINS = [ &
        'mailinator.com      ', &
        'guerrillamail.com   ', &
        '10minutemail.com    ', &
        'tempmail.com        ', &
        'throwaway.email     ', &
        'fakeinbox.com       ', &
        'trashmail.com       ', &
        'maildrop.cc         ' &
    ]

    public :: is_valid_email, parse_email, is_disposable_email, normalize_email

contains

    !> Basic email format validation
    function is_valid_email(email) result(is_valid)
        character(len=*), intent(in) :: email
        logical :: is_valid
        integer :: at_pos, at_count, i, email_len
        character(len=1) :: c

        is_valid = .false.
        email_len = len_trim(email)

        ! Check length bounds
        if (email_len == 0 .or. email_len > MAX_EMAIL_LEN) return

        ! Find @ symbol and count occurrences
        at_count = 0
        at_pos = 0
        do i = 1, email_len
            c = email(i:i)
            if (c == '@') then
                at_count = at_count + 1
                at_pos = i
            end if
        end do

        ! Must have exactly one @
        if (at_count /= 1) return

        ! @ cannot be first or last
        if (at_pos == 1 .or. at_pos == email_len) return

        ! Check local part length
        if (at_pos - 1 > MAX_LOCAL_LEN) return

        ! Check domain length
        if (email_len - at_pos > MAX_DOMAIN_LEN) return

        ! Domain must have at least one dot
        if (index(email(at_pos+1:email_len), '.') == 0) return

        is_valid = .true.
    end function is_valid_email

    !> Parse email into local part and domain
    function parse_email(email) result(res)
        character(len=*), intent(in) :: email
        type(EmailResult) :: res
        integer :: at_pos, email_len

        email_len = len_trim(email)

        ! Check length
        if (email_len == 0) then
            res%error = 'Email is empty'
            return
        end if

        if (email_len > MAX_EMAIL_LEN) then
            res%error = 'Email exceeds maximum length'
            return
        end if

        ! Find @ symbol
        at_pos = index(email, '@')
        if (at_pos == 0) then
            res%error = 'Missing @ symbol'
            return
        end if

        ! Check local part
        if (at_pos == 1) then
            res%error = 'Local part is empty'
            return
        end if
        if (at_pos - 1 > MAX_LOCAL_LEN) then
            res%error = 'Local part exceeds maximum length'
            return
        end if

        ! Check domain
        if (at_pos == email_len) then
            res%error = 'Domain is empty'
            return
        end if

        ! Extract parts
        res%local_part = email(1:at_pos-1)
        res%domain = email(at_pos+1:email_len)

        ! Domain must have a dot
        if (index(res%domain, '.') == 0) then
            res%error = 'Domain must contain a dot'
            return
        end if

        res%ok = .true.
    end function parse_email

    !> Check if email uses a disposable domain
    function is_disposable_email(domain) result(is_disposable)
        character(len=*), intent(in) :: domain
        logical :: is_disposable
        character(len=:), allocatable :: lower_domain
        integer :: i

        is_disposable = .false.
        lower_domain = to_lower_string(trim(adjustl(domain)))

        do i = 1, size(DISPOSABLE_DOMAINS)
            if (lower_domain == trim(adjustl(DISPOSABLE_DOMAINS(i)))) then
                is_disposable = .true.
                return
            end if
        end do
    end function is_disposable_email

    !> Normalize email (lowercase domain)
    function normalize_email(email) result(normalized)
        character(len=*), intent(in) :: email
        character(len=:), allocatable :: normalized
        integer :: at_pos, email_len
        character(len=:), allocatable :: local_part, domain

        email_len = len_trim(email)
        at_pos = index(email, '@')

        if (at_pos == 0 .or. at_pos == 1 .or. at_pos == email_len) then
            normalized = email
            return
        end if

        local_part = email(1:at_pos-1)
        domain = to_lower_string(email(at_pos+1:email_len))

        normalized = trim(local_part) // '@' // trim(domain)
    end function normalize_email

    !> Convert string to lowercase
    function to_lower_string(input) result(output)
        character(len=*), intent(in) :: input
        character(len=:), allocatable :: output
        integer :: i
        character(len=1) :: c

        allocate(character(len=len(input)) :: output)
        output = input

        do i = 1, len(input)
            c = input(i:i)
            if (c >= 'A' .and. c <= 'Z') then
                output(i:i) = achar(iachar(c) + 32)
            end if
        end do
    end function to_lower_string

end module safe_email
