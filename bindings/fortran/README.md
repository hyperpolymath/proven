# Proven for Fortran

Safe, validated operations library for Fortran applications.

## Installation

Copy the `src/` directory to your project and compile the modules:

```bash
# With gfortran
gfortran -c src/safe_math.f90
gfortran -c src/safe_string.f90
gfortran -c src/safe_path.f90
gfortran -c src/safe_email.f90
gfortran -c src/safe_network.f90
gfortran -c src/safe_crypto.f90
gfortran -c src/proven.f90

# Link with your program
gfortran -o myprogram myprogram.f90 *.o
```

Or with fpm (Fortran Package Manager):

```toml
# fpm.toml
[dependencies]
proven = { path = "path/to/proven/bindings/fortran" }
```

## Usage

```fortran
program example
    use proven
    implicit none

    type(SafeResult) :: result
    type(IPv4Address) :: ip
    type(EmailResult) :: email_res
    character(len=:), allocatable :: escaped

    ! Safe math with overflow checking
    result = safe_add(1000000000_int64, 2000000000_int64)
    if (result%ok) then
        print *, "Sum:", result%value
    else
        print *, "Overflow!"
    end if

    ! XSS prevention
    escaped = escape_html("<script>alert('xss')</script>")
    print *, "Escaped:", escaped

    ! Path safety
    if (has_traversal("../../../etc/passwd")) then
        print *, "Dangerous path!"
    end if

    ! Email validation
    email_res = parse_email("user@example.com")
    if (email_res%ok) then
        print *, "Local:", trim(email_res%local_part)
        print *, "Domain:", trim(email_res%domain)
    else
        print *, "Invalid:", trim(email_res%error)
    end if

    ! IP classification
    ip = parse_ipv4("192.168.1.1")
    if (ip%valid) then
        print *, "Private:", is_private_ip(ip)
        print *, "Classification:", classify_ip(ip)
    end if

end program example
```

## Modules

### safe_math

Overflow-checked arithmetic with result types:

```fortran
use safe_math

type(SafeResult) :: result

! All operations return SafeResult with value and ok fields
result = safe_add(a, b)     ! Safe addition
result = safe_sub(a, b)     ! Safe subtraction
result = safe_mul(a, b)     ! Safe multiplication
result = safe_div(a, b)     ! Safe division (checks zero)
result = safe_mod(a, b)     ! Safe modulo

result = safe_abs(a)        ! Safe absolute value
result = safe_negate(a)     ! Safe negation

! Check result
if (result%ok) then
    print *, result%value
else
    print *, "Overflow!"
end if

! Range operations (pure functions)
integer(int64) :: clamped
clamped = clamp(value, min_val, max_val)

logical :: in_bounds
in_bounds = in_range(value, min_val, max_val)
```

### safe_string

XSS prevention and string sanitization:

```fortran
use safe_string

character(len=:), allocatable :: output

output = escape_html("<script>")     ! "&lt;script&gt;"
output = escape_sql("O'Brien")       ! "O''Brien"
output = sanitize_default(input)     ! Alphanumeric + _-
output = slugify("Hello World!")     ! "hello-world"

! Character checks (pure)
logical :: res
res = is_alpha('A')      ! .true.
res = is_digit('5')      ! .true.
res = is_alpha_num('z')  ! .true.

! Case conversion
character(len=1) :: lower
lower = to_lower('A')    ! 'a'
```

### safe_path

Directory traversal protection:

```fortran
use safe_path

type(PathResult) :: path_res

! Check for traversal
if (has_traversal("../../../etc/passwd")) then
    print *, "Path traversal detected!"
end if

! Sanitize filename
character(len=:), allocatable :: safe_name
safe_name = sanitize_filename("../bad<name>.txt")

! Safe path joining
path_res = safe_path_join("/base", "file.txt")
if (path_res%ok) then
    print *, "Path:", trim(path_res%path)
else
    print *, "Error:", trim(path_res%error)
end if
```

### safe_email

Email validation:

```fortran
use safe_email

type(EmailResult) :: res

! Simple validation
if (is_valid_email("user@example.com")) then
    print *, "Valid email"
end if

! Parse with result checking
res = parse_email("user@example.com")
if (res%ok) then
    print *, "Local:", trim(res%local_part)
    print *, "Domain:", trim(res%domain)
else
    print *, "Error:", trim(res%error)
end if

! Check for disposable emails
if (is_disposable_email("mailinator.com")) then
    print *, "Disposable domain"
end if

! Normalize (lowercase domain)
character(len=:), allocatable :: normalized
normalized = normalize_email("User@EXAMPLE.COM")
! "User@example.com"
```

### safe_network

IP address validation and classification:

```fortran
use safe_network

type(IPv4Address) :: ip
integer :: classification

! Parse IPv4
ip = parse_ipv4("192.168.1.1")
if (ip%valid) then
    print *, "Octets:", ip%octets

    ! Classification checks (pure functions)
    print *, "Loopback:", is_loopback(ip)
    print *, "Private:", is_private_ip(ip)
    print *, "Reserved:", is_reserved_ip(ip)
    print *, "Public:", is_public_ip(ip)

    ! Get classification constant
    classification = classify_ip(ip)
    select case (classification)
        case (IP_CLASS_LOOPBACK)
            print *, "Loopback"
        case (IP_CLASS_PRIVATE)
            print *, "Private network"
        case (IP_CLASS_RESERVED)
            print *, "Reserved"
        case (IP_CLASS_PUBLIC)
            print *, "Public IP"
    end select
end if

! Format as string
character(len=15) :: ip_str
ip_str = format_ipv4(ip)

! Port validation (pure functions)
if (is_valid_port(8080)) print *, "Valid port"
if (is_privileged_port(80)) print *, "Privileged port"
```

### safe_crypto

Cryptographic operations:

```fortran
use safe_crypto

! Constant-time comparison (timing attack prevention)
if (constant_time_equals(actual, expected)) then
    print *, "Match"
end if

! Simple hash (demo only - NOT cryptographically secure)
character(len=16) :: hash
hash = simple_hash("data")

! Generate random token
character(len=:), allocatable :: token
token = generate_token(32)

! Random integer in range
integer :: val
val = random_int(1, 100)

! Secure memory wipe
character(len=100) :: sensitive_data
! ... use data ...
call secure_wipe(sensitive_data, 100)
```

**Note:** The crypto operations are simplified for demonstration. For production applications, integrate with external cryptographic libraries like OpenSSL.

## Data Types

### SafeResult

```fortran
type :: SafeResult
    integer(int64) :: value = 0   ! Result value
    logical :: ok = .false.        ! .true. if operation succeeded
end type SafeResult
```

### PathResult

```fortran
type :: PathResult
    character(len=1024) :: path = ''   ! Result path
    character(len=256) :: error = ''   ! Error message
    logical :: ok = .false.             ! .true. if operation succeeded
end type PathResult
```

### EmailResult

```fortran
type :: EmailResult
    character(len=64) :: local_part = ''   ! Local part before @
    character(len=255) :: domain = ''       ! Domain part after @
    character(len=256) :: error = ''        ! Error message
    logical :: ok = .false.                  ! .true. if valid
end type EmailResult
```

### IPv4Address

```fortran
type :: IPv4Address
    integer :: octets(4) = [0, 0, 0, 0]  ! Four octets
    logical :: valid = .false.             ! .true. if valid IP
end type IPv4Address
```

### IP Classification Constants

```fortran
integer, parameter :: IP_CLASS_INVALID  = 0
integer, parameter :: IP_CLASS_LOOPBACK = 1
integer, parameter :: IP_CLASS_PRIVATE  = 2
integer, parameter :: IP_CLASS_RESERVED = 3
integer, parameter :: IP_CLASS_PUBLIC   = 4
```

## Examples

### Scientific Calculation with Overflow Protection

```fortran
program safe_factorial
    use safe_math
    implicit none

    integer(int64) :: n
    type(SafeResult) :: factorial

    n = 20
    factorial = compute_factorial(n)

    if (factorial%ok) then
        print '(A,I0,A,I0)', 'Factorial of ', n, ' = ', factorial%value
    else
        print '(A,I0,A)', 'Factorial of ', n, ' overflowed!'
    end if

contains

    function compute_factorial(n) result(res)
        integer(int64), intent(in) :: n
        type(SafeResult) :: res
        integer(int64) :: i

        res%value = 1_int64
        res%ok = .true.

        do i = 2, n
            res = safe_mul(res%value, i)
            if (.not. res%ok) return
        end do
    end function compute_factorial

end program safe_factorial
```

### Input Validation for Data Processing

```fortran
program data_processor
    use safe_email
    use safe_path
    implicit none

    character(len=256) :: user_email, output_file
    type(EmailResult) :: email_res
    type(PathResult) :: path_res

    ! Validate email from user input
    user_email = "researcher@university.edu"
    email_res = parse_email(user_email)

    if (.not. email_res%ok) then
        print *, "Invalid email:", trim(email_res%error)
        stop 1
    end if

    ! Validate output file path
    output_file = "results.dat"
    if (has_traversal(output_file)) then
        print *, "Invalid output path!"
        stop 1
    end if

    path_res = safe_path_join("/data/output", output_file)
    if (path_res%ok) then
        print *, "Writing to:", trim(path_res%path)
    end if

end program data_processor
```

## Compatibility

This library requires:
- Fortran 2003 or later (for derived types, allocatable strings)
- Fortran 2008 features used where available

Compatible compilers:
- gfortran 4.8+
- Intel Fortran (ifort/ifx)
- NAG Fortran
- Cray Fortran
- IBM XL Fortran
- NVIDIA nvfortran

## License

PMPL-1.0
