! SPDX-License-Identifier: PMPL-1.0
! SPDX-FileCopyrightText: 2025 Hyperpolymath
!
! Proven SafeDatetime - Date and time validation for Fortran
!

module safe_datetime
    use, intrinsic :: iso_fortran_env, only: int64, int32
    implicit none
    private

    !> Days in each month (non-leap year)
    integer, dimension(12), parameter :: DAYS_IN_MONTH = &
        [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

    !> Unix epoch (1970-01-01)
    integer, parameter :: EPOCH_YEAR = 1970

    !> Datetime type
    type, public :: datetime_type
        integer :: year = 0
        integer :: month = 0
        integer :: day = 0
        integer :: hour = 0
        integer :: minute = 0
        integer :: second = 0
        integer :: millisecond = 0
        integer :: timezone_offset = 0  ! Minutes from UTC
        logical :: valid = .false.
    contains
        procedure :: to_iso8601 => datetime_to_iso8601
        procedure :: to_unix_timestamp => datetime_to_unix
        procedure :: day_of_week => datetime_day_of_week
        procedure :: day_of_year => datetime_day_of_year
        procedure :: is_weekend => datetime_is_weekend
    end type datetime_type

    !> Datetime result type
    type, public :: DatetimeResult
        type(datetime_type) :: datetime
        character(len=256) :: error = ''
        logical :: ok = .false.
    end type DatetimeResult

    !> Duration type
    type, public :: duration_type
        integer(int64) :: total_seconds = 0_int64
        integer :: days = 0
        integer :: hours = 0
        integer :: minutes = 0
        integer :: seconds = 0
        logical :: valid = .false.
    end type duration_type

    public :: parse_datetime, parse_date, parse_time, parse_iso8601
    public :: is_valid_date, is_valid_time, is_leap_year
    public :: days_in_month_func, datetime_from_unix, datetime_now
    public :: datetime_add_days, datetime_diff, datetime_compare

contains

    !> Parse ISO 8601 datetime string (YYYY-MM-DDTHH:MM:SS[.mmm][Z|+HH:MM])
    function parse_iso8601(datetime_string) result(res)
        character(len=*), intent(in) :: datetime_string
        type(DatetimeResult) :: res
        integer :: str_len, t_pos, z_pos, plus_pos, minus_pos, dot_pos
        integer :: io_stat, tz_hours, tz_minutes
        character(len=32) :: date_part, time_part, tz_part

        str_len = len_trim(datetime_string)

        if (str_len < 10) then
            res%error = 'Datetime string too short'
            return
        end if

        ! Find T separator
        t_pos = index(datetime_string, 'T')
        if (t_pos == 0) t_pos = index(datetime_string, ' ')

        if (t_pos == 0) then
            ! Date only
            date_part = datetime_string
            time_part = ''
        else
            date_part = datetime_string(1:t_pos-1)
            time_part = datetime_string(t_pos+1:str_len)
        end if

        ! Parse date (YYYY-MM-DD)
        if (len_trim(date_part) /= 10) then
            res%error = 'Invalid date format (expected YYYY-MM-DD)'
            return
        end if

        read(date_part(1:4), *, iostat=io_stat) res%datetime%year
        if (io_stat /= 0) then
            res%error = 'Invalid year'
            return
        end if

        read(date_part(6:7), *, iostat=io_stat) res%datetime%month
        if (io_stat /= 0) then
            res%error = 'Invalid month'
            return
        end if

        read(date_part(9:10), *, iostat=io_stat) res%datetime%day
        if (io_stat /= 0) then
            res%error = 'Invalid day'
            return
        end if

        ! Validate date
        if (.not. is_valid_date(res%datetime%year, res%datetime%month, res%datetime%day)) then
            res%error = 'Invalid date values'
            return
        end if

        ! Parse time if present
        if (len_trim(time_part) > 0) then
            ! Find timezone indicator
            z_pos = index(time_part, 'Z')
            plus_pos = index(time_part, '+')
            minus_pos = 0
            if (len_trim(time_part) > 8) then
                minus_pos = index(time_part(9:), '-')
                if (minus_pos > 0) minus_pos = minus_pos + 8
            end if

            ! Extract timezone
            if (z_pos > 0) then
                tz_part = 'Z'
                time_part = time_part(1:z_pos-1)
                res%datetime%timezone_offset = 0
            else if (plus_pos > 0) then
                tz_part = time_part(plus_pos:)
                time_part = time_part(1:plus_pos-1)
                call parse_timezone(tz_part, res%datetime%timezone_offset, io_stat)
                if (io_stat /= 0) then
                    res%error = 'Invalid timezone'
                    return
                end if
            else if (minus_pos > 0) then
                tz_part = time_part(minus_pos:)
                time_part = time_part(1:minus_pos-1)
                call parse_timezone(tz_part, res%datetime%timezone_offset, io_stat)
                if (io_stat /= 0) then
                    res%error = 'Invalid timezone'
                    return
                end if
            end if

            ! Find milliseconds
            dot_pos = index(time_part, '.')
            if (dot_pos > 0) then
                read(time_part(dot_pos+1:), *, iostat=io_stat) res%datetime%millisecond
                time_part = time_part(1:dot_pos-1)
            end if

            ! Parse HH:MM:SS
            if (len_trim(time_part) >= 8) then
                read(time_part(1:2), *, iostat=io_stat) res%datetime%hour
                read(time_part(4:5), *, iostat=io_stat) res%datetime%minute
                read(time_part(7:8), *, iostat=io_stat) res%datetime%second
            else if (len_trim(time_part) >= 5) then
                read(time_part(1:2), *, iostat=io_stat) res%datetime%hour
                read(time_part(4:5), *, iostat=io_stat) res%datetime%minute
            end if

            ! Validate time
            if (.not. is_valid_time(res%datetime%hour, res%datetime%minute, &
                                     res%datetime%second)) then
                res%error = 'Invalid time values'
                return
            end if
        end if

        res%datetime%valid = .true.
        res%ok = .true.
    end function parse_iso8601

    !> Parse timezone string (+HH:MM or -HH:MM)
    subroutine parse_timezone(tz_string, offset_minutes, io_stat)
        character(len=*), intent(in) :: tz_string
        integer, intent(out) :: offset_minutes
        integer, intent(out) :: io_stat
        integer :: hours, minutes, sign

        io_stat = 0
        offset_minutes = 0

        if (len_trim(tz_string) == 0 .or. tz_string(1:1) == 'Z') return

        if (tz_string(1:1) == '+') then
            sign = 1
        else if (tz_string(1:1) == '-') then
            sign = -1
        else
            io_stat = 1
            return
        end if

        if (len_trim(tz_string) >= 6) then
            read(tz_string(2:3), *, iostat=io_stat) hours
            if (io_stat /= 0) return
            read(tz_string(5:6), *, iostat=io_stat) minutes
            if (io_stat /= 0) return
        else if (len_trim(tz_string) >= 3) then
            read(tz_string(2:3), *, iostat=io_stat) hours
            if (io_stat /= 0) return
            minutes = 0
        else
            io_stat = 1
            return
        end if

        offset_minutes = sign * (hours * 60 + minutes)
    end subroutine parse_timezone

    !> Parse datetime (wrapper)
    function parse_datetime(datetime_string) result(res)
        character(len=*), intent(in) :: datetime_string
        type(DatetimeResult) :: res

        res = parse_iso8601(datetime_string)
    end function parse_datetime

    !> Parse date only (YYYY-MM-DD)
    function parse_date(date_string) result(res)
        character(len=*), intent(in) :: date_string
        type(DatetimeResult) :: res

        res = parse_iso8601(date_string)
    end function parse_date

    !> Parse time only (HH:MM:SS)
    function parse_time(time_string) result(res)
        character(len=*), intent(in) :: time_string
        type(DatetimeResult) :: res
        integer :: io_stat

        if (len_trim(time_string) < 5) then
            res%error = 'Time string too short'
            return
        end if

        read(time_string(1:2), *, iostat=io_stat) res%datetime%hour
        if (io_stat /= 0) then
            res%error = 'Invalid hour'
            return
        end if

        read(time_string(4:5), *, iostat=io_stat) res%datetime%minute
        if (io_stat /= 0) then
            res%error = 'Invalid minute'
            return
        end if

        if (len_trim(time_string) >= 8) then
            read(time_string(7:8), *, iostat=io_stat) res%datetime%second
        end if

        if (.not. is_valid_time(res%datetime%hour, res%datetime%minute, res%datetime%second)) then
            res%error = 'Invalid time values'
            return
        end if

        res%datetime%year = 1970
        res%datetime%month = 1
        res%datetime%day = 1
        res%datetime%valid = .true.
        res%ok = .true.
    end function parse_time

    !> Check if date is valid
    pure function is_valid_date(year, month, day) result(is_valid)
        integer, intent(in) :: year, month, day
        logical :: is_valid
        integer :: max_days

        is_valid = .false.

        if (year < 1 .or. year > 9999) return
        if (month < 1 .or. month > 12) return
        if (day < 1) return

        max_days = days_in_month_func(year, month)
        if (day > max_days) return

        is_valid = .true.
    end function is_valid_date

    !> Check if time is valid
    pure function is_valid_time(hour, minute, second) result(is_valid)
        integer, intent(in) :: hour, minute, second
        logical :: is_valid

        is_valid = hour >= 0 .and. hour <= 23 .and. &
                   minute >= 0 .and. minute <= 59 .and. &
                   second >= 0 .and. second <= 59
    end function is_valid_time

    !> Check if year is a leap year
    pure function is_leap_year(year) result(is_leap)
        integer, intent(in) :: year
        logical :: is_leap

        is_leap = (mod(year, 4) == 0 .and. mod(year, 100) /= 0) .or. &
                  (mod(year, 400) == 0)
    end function is_leap_year

    !> Get number of days in a month
    pure function days_in_month_func(year, month) result(days)
        integer, intent(in) :: year, month
        integer :: days

        if (month < 1 .or. month > 12) then
            days = 0
            return
        end if

        days = DAYS_IN_MONTH(month)
        if (month == 2 .and. is_leap_year(year)) then
            days = 29
        end if
    end function days_in_month_func

    !> Format datetime as ISO 8601
    function datetime_to_iso8601(self) result(iso_string)
        class(datetime_type), intent(in) :: self
        character(len=32) :: iso_string
        character(len=6) :: tz_string
        integer :: abs_offset, tz_hours, tz_minutes

        if (.not. self%valid) then
            iso_string = ''
            return
        end if

        ! Format timezone
        if (self%timezone_offset == 0) then
            tz_string = 'Z'
        else
            abs_offset = abs(self%timezone_offset)
            tz_hours = abs_offset / 60
            tz_minutes = mod(abs_offset, 60)
            if (self%timezone_offset > 0) then
                write(tz_string, '(A,I2.2,A,I2.2)') '+', tz_hours, ':', tz_minutes
            else
                write(tz_string, '(A,I2.2,A,I2.2)') '-', tz_hours, ':', tz_minutes
            end if
        end if

        write(iso_string, '(I4.4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A)') &
            self%year, '-', self%month, '-', self%day, 'T', &
            self%hour, ':', self%minute, ':', self%second, trim(tz_string)
    end function datetime_to_iso8601

    !> Convert datetime to Unix timestamp
    pure function datetime_to_unix(self) result(timestamp)
        class(datetime_type), intent(in) :: self
        integer(int64) :: timestamp
        integer :: y, m, days

        if (.not. self%valid) then
            timestamp = 0_int64
            return
        end if

        ! Calculate days since epoch
        days = 0

        ! Add days for years
        do y = EPOCH_YEAR, self%year - 1
            if (is_leap_year(y)) then
                days = days + 366
            else
                days = days + 365
            end if
        end do

        ! Handle years before epoch
        do y = self%year, EPOCH_YEAR - 1
            if (is_leap_year(y)) then
                days = days - 366
            else
                days = days - 365
            end if
        end do

        ! Add days for months in current year
        do m = 1, self%month - 1
            days = days + days_in_month_func(self%year, m)
        end do

        ! Add remaining days
        days = days + self%day - 1

        ! Convert to seconds
        timestamp = int(days, int64) * 86400_int64 + &
                    int(self%hour, int64) * 3600_int64 + &
                    int(self%minute, int64) * 60_int64 + &
                    int(self%second, int64)

        ! Adjust for timezone
        timestamp = timestamp - int(self%timezone_offset, int64) * 60_int64
    end function datetime_to_unix

    !> Create datetime from Unix timestamp
    function datetime_from_unix(timestamp) result(dt)
        integer(int64), intent(in) :: timestamp
        type(datetime_type) :: dt
        integer(int64) :: remaining
        integer :: year, month, day_of_month, days_in_year

        remaining = timestamp

        ! Extract time components
        dt%second = int(mod(remaining, 60_int64))
        remaining = remaining / 60_int64
        dt%minute = int(mod(remaining, 60_int64))
        remaining = remaining / 60_int64
        dt%hour = int(mod(remaining, 24_int64))
        remaining = remaining / 24_int64

        ! remaining is now days since epoch
        year = EPOCH_YEAR

        do while (.true.)
            if (is_leap_year(year)) then
                days_in_year = 366
            else
                days_in_year = 365
            end if

            if (remaining < days_in_year) exit
            remaining = remaining - days_in_year
            year = year + 1
        end do

        dt%year = year

        ! Find month
        do month = 1, 12
            day_of_month = days_in_month_func(year, month)
            if (remaining < day_of_month) exit
            remaining = remaining - day_of_month
        end do

        dt%month = month
        dt%day = int(remaining) + 1
        dt%timezone_offset = 0
        dt%valid = .true.
    end function datetime_from_unix

    !> Get current datetime (uses system clock)
    function datetime_now() result(dt)
        type(datetime_type) :: dt
        integer :: values(8)

        call date_and_time(values=values)

        dt%year = values(1)
        dt%month = values(2)
        dt%day = values(3)
        dt%timezone_offset = values(4)
        dt%hour = values(5)
        dt%minute = values(6)
        dt%second = values(7)
        dt%millisecond = values(8)
        dt%valid = .true.
    end function datetime_now

    !> Get day of week (0=Sunday, 6=Saturday)
    pure function datetime_day_of_week(self) result(dow)
        class(datetime_type), intent(in) :: self
        integer :: dow
        integer :: y, m, d, century, year_of_century

        if (.not. self%valid) then
            dow = -1
            return
        end if

        ! Zeller's congruence
        y = self%year
        m = self%month
        d = self%day

        if (m < 3) then
            m = m + 12
            y = y - 1
        end if

        century = y / 100
        year_of_century = mod(y, 100)

        dow = mod(d + (13 * (m + 1)) / 5 + year_of_century + &
                  year_of_century / 4 + century / 4 - 2 * century, 7)

        ! Adjust to 0=Sunday
        dow = mod(dow + 6, 7)
    end function datetime_day_of_week

    !> Get day of year (1-366)
    pure function datetime_day_of_year(self) result(doy)
        class(datetime_type), intent(in) :: self
        integer :: doy
        integer :: m

        if (.not. self%valid) then
            doy = 0
            return
        end if

        doy = self%day
        do m = 1, self%month - 1
            doy = doy + days_in_month_func(self%year, m)
        end do
    end function datetime_day_of_year

    !> Check if datetime falls on weekend
    pure function datetime_is_weekend(self) result(is_weekend)
        class(datetime_type), intent(in) :: self
        logical :: is_weekend
        integer :: dow

        dow = self%day_of_week()
        is_weekend = (dow == 0 .or. dow == 6)
    end function datetime_is_weekend

    !> Add days to datetime
    function datetime_add_days(dt, days) result(new_dt)
        type(datetime_type), intent(in) :: dt
        integer, intent(in) :: days
        type(datetime_type) :: new_dt
        integer(int64) :: timestamp

        timestamp = dt%to_unix_timestamp()
        timestamp = timestamp + int(days, int64) * 86400_int64
        new_dt = datetime_from_unix(timestamp)
        new_dt%timezone_offset = dt%timezone_offset
    end function datetime_add_days

    !> Calculate difference between two datetimes
    function datetime_diff(dt1, dt2) result(duration)
        type(datetime_type), intent(in) :: dt1, dt2
        type(duration_type) :: duration
        integer(int64) :: ts1, ts2, diff_seconds

        ts1 = dt1%to_unix_timestamp()
        ts2 = dt2%to_unix_timestamp()

        diff_seconds = abs(ts1 - ts2)

        duration%total_seconds = diff_seconds
        duration%days = int(diff_seconds / 86400_int64)
        diff_seconds = mod(diff_seconds, 86400_int64)
        duration%hours = int(diff_seconds / 3600_int64)
        diff_seconds = mod(diff_seconds, 3600_int64)
        duration%minutes = int(diff_seconds / 60_int64)
        duration%seconds = int(mod(diff_seconds, 60_int64))
        duration%valid = .true.
    end function datetime_diff

    !> Compare two datetimes
    !> Returns: -1 if dt1 < dt2, 0 if equal, 1 if dt1 > dt2
    pure function datetime_compare(dt1, dt2) result(cmp)
        type(datetime_type), intent(in) :: dt1, dt2
        integer :: cmp
        integer(int64) :: ts1, ts2

        ts1 = dt1%to_unix_timestamp()
        ts2 = dt2%to_unix_timestamp()

        if (ts1 < ts2) then
            cmp = -1
        else if (ts1 > ts2) then
            cmp = 1
        else
            cmp = 0
        end if
    end function datetime_compare

end module safe_datetime
