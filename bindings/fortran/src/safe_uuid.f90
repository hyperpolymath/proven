! SPDX-License-Identifier: PMPL-1.0-or-later
! Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
!
! Proven SafeUUID - UUID generation and validation for Fortran
!
! Thin wrapper around libproven's verified SafeUUID module.
! All UUID logic (generation, parsing, formatting) is executed in the
! Idris 2 core via the Zig FFI bridge; this module only marshals types
! between Fortran and C.
!
! Link with: -lproven

module safe_uuid
    use, intrinsic :: iso_c_binding
    use proven_ffi
    implicit none
    private

    !> UUID type (Fortran-facing)
    type, public :: uuid_type
        integer(c_int8_t) :: bytes(16) = 0_c_int8_t
        logical            :: valid     = .false.
    end type uuid_type

    !> UUID parse/generate result type
    type, public :: UuidResult
        type(uuid_type)    :: uuid
        logical            :: ok     = .false.
        integer(c_int32_t) :: status = PROVEN_ERR_NOT_IMPLEMENTED
    end type UuidResult

    public :: generate_uuid_v4, parse_uuid, format_uuid
    public :: uuid_is_nil, uuid_get_version

contains

    ! =========================================================================
    ! Generate UUID v4  ->  proven_uuid_v4
    ! =========================================================================
    function generate_uuid_v4() result(res)
        type(UuidResult) :: res
        type(c_proven_uuid_result) :: c_res

        c_res = proven_uuid_v4()

        res%status    = c_res%status
        res%ok        = (c_res%status == PROVEN_OK)
        res%uuid%bytes = c_res%uuid%bytes
        res%uuid%valid = res%ok
    end function generate_uuid_v4

    ! =========================================================================
    ! Parse UUID from string  ->  proven_uuid_parse
    ! =========================================================================
    function parse_uuid(uuid_string) result(res)
        character(len=*), intent(in) :: uuid_string
        type(UuidResult) :: res
        character(len=len_trim(uuid_string), kind=c_char), target :: c_buf
        type(c_proven_uuid_result) :: c_res
        integer :: trimmed_len

        trimmed_len = len_trim(uuid_string)
        c_buf = uuid_string(1:trimmed_len)

        c_res = proven_uuid_parse(c_loc(c_buf), int(trimmed_len, c_size_t))

        res%status    = c_res%status
        res%ok        = (c_res%status == PROVEN_OK)
        res%uuid%bytes = c_res%uuid%bytes
        res%uuid%valid = res%ok
    end function parse_uuid

    ! =========================================================================
    ! Format UUID as string  ->  proven_uuid_to_string
    ! =========================================================================
    function format_uuid(uuid_instance) result(uuid_string)
        type(uuid_type), intent(in) :: uuid_instance
        character(len=36) :: uuid_string
        type(c_proven_uuid) :: c_uuid
        type(c_proven_string_result) :: c_res
        character(len=1, kind=c_char), pointer :: f_ptr(:)

        uuid_string = ''
        c_uuid%bytes = uuid_instance%bytes

        c_res = proven_uuid_to_string(c_uuid)

        if (c_res%status == PROVEN_OK .and. c_associated(c_res%value)) then
            call c_f_pointer(c_res%value, f_ptr, [c_res%length])
            block
                integer :: i, copy_len
                copy_len = min(int(c_res%length), 36)
                do i = 1, copy_len
                    uuid_string(i:i) = f_ptr(i)
                end do
            end block
            call proven_free_string(c_res%value)
        end if
    end function format_uuid

    ! =========================================================================
    ! Check if UUID is nil  ->  proven_uuid_is_nil
    ! =========================================================================
    function uuid_is_nil(uuid_instance) result(is_nil)
        type(uuid_type), intent(in) :: uuid_instance
        logical :: is_nil
        type(c_proven_uuid) :: c_uuid

        c_uuid%bytes = uuid_instance%bytes
        is_nil = logical(proven_uuid_is_nil(c_uuid))
    end function uuid_is_nil

    ! =========================================================================
    ! Get UUID version  ->  proven_uuid_version
    ! =========================================================================
    function uuid_get_version(uuid_instance) result(ver)
        type(uuid_type), intent(in) :: uuid_instance
        integer :: ver
        type(c_proven_uuid) :: c_uuid

        c_uuid%bytes = uuid_instance%bytes
        ver = int(proven_uuid_version(c_uuid))
    end function uuid_get_version

end module safe_uuid
