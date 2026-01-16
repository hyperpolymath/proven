# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

<#
.SYNOPSIS
    Proven Safety Library for PowerShell
.DESCRIPTION
    Formally verified safety primitives for PowerShell scripts.
    Provides safe math operations, input validation, and bounded types.
.NOTES
    Version: 0.9.0
#>

# ============================================================================
# SAFE MATH
# ============================================================================

function Add-Safely {
    <#
    .SYNOPSIS
        Safe addition with overflow check
    .PARAMETER A
        First operand
    .PARAMETER B
        Second operand
    .OUTPUTS
        [hashtable] Result with Success and Value or Error
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)]
        [long]$A,
        [Parameter(Mandatory)]
        [long]$B
    )

    try {
        $result = [long]::MaxValue
        if ($B -gt 0 -and $A -gt ([long]::MaxValue - $B)) {
            return @{ Success = $false; Error = "Overflow in addition" }
        }
        if ($B -lt 0 -and $A -lt ([long]::MinValue - $B)) {
            return @{ Success = $false; Error = "Underflow in addition" }
        }
        return @{ Success = $true; Value = $A + $B }
    }
    catch {
        return @{ Success = $false; Error = $_.Exception.Message }
    }
}

function Subtract-Safely {
    <#
    .SYNOPSIS
        Safe subtraction with underflow check
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)]
        [long]$A,
        [Parameter(Mandatory)]
        [long]$B
    )

    try {
        if ($B -lt 0 -and $A -gt ([long]::MaxValue + $B)) {
            return @{ Success = $false; Error = "Overflow in subtraction" }
        }
        if ($B -gt 0 -and $A -lt ([long]::MinValue + $B)) {
            return @{ Success = $false; Error = "Underflow in subtraction" }
        }
        return @{ Success = $true; Value = $A - $B }
    }
    catch {
        return @{ Success = $false; Error = $_.Exception.Message }
    }
}

function Multiply-Safely {
    <#
    .SYNOPSIS
        Safe multiplication with overflow check
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)]
        [long]$A,
        [Parameter(Mandatory)]
        [long]$B
    )

    try {
        if ($A -eq 0 -or $B -eq 0) {
            return @{ Success = $true; Value = 0 }
        }

        $result = $A * $B
        if ($result / $A -ne $B) {
            return @{ Success = $false; Error = "Overflow in multiplication" }
        }
        return @{ Success = $true; Value = $result }
    }
    catch {
        return @{ Success = $false; Error = $_.Exception.Message }
    }
}

function Divide-Safely {
    <#
    .SYNOPSIS
        Safe division with zero check
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)]
        [long]$A,
        [Parameter(Mandatory)]
        [long]$B
    )

    if ($B -eq 0) {
        return @{ Success = $false; Error = "Division by zero" }
    }

    # Handle special case of MIN_VALUE / -1
    if ($A -eq [long]::MinValue -and $B -eq -1) {
        return @{ Success = $false; Error = "Overflow in division" }
    }

    return @{ Success = $true; Value = [Math]::Truncate($A / $B) }
}

function Get-Modulo-Safely {
    <#
    .SYNOPSIS
        Safe modulo with zero check
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)]
        [long]$A,
        [Parameter(Mandatory)]
        [long]$B
    )

    if ($B -eq 0) {
        return @{ Success = $false; Error = "Modulo by zero" }
    }

    return @{ Success = $true; Value = $A % $B }
}

# ============================================================================
# BOUNDED VALUES
# ============================================================================

function Get-Clamped {
    <#
    .SYNOPSIS
        Clamp a value to a range
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)]
        [long]$Value,
        [Parameter(Mandatory)]
        [long]$Min,
        [Parameter(Mandatory)]
        [long]$Max
    )

    if ($Value -lt $Min) { return $Min }
    if ($Value -gt $Max) { return $Max }
    return $Value
}

function Test-InRange {
    <#
    .SYNOPSIS
        Check if value is in range (inclusive)
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)]
        [long]$Value,
        [Parameter(Mandatory)]
        [long]$Min,
        [Parameter(Mandatory)]
        [long]$Max
    )

    return ($Value -ge $Min) -and ($Value -le $Max)
}

function Assert-InRange {
    <#
    .SYNOPSIS
        Require value is in range or throw
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)]
        [long]$Value,
        [Parameter(Mandatory)]
        [long]$Min,
        [Parameter(Mandatory)]
        [long]$Max
    )

    if (-not (Test-InRange -Value $Value -Min $Min -Max $Max)) {
        throw "Value $Value out of bounds [$Min, $Max]"
    }
}

# ============================================================================
# VALIDATION
# ============================================================================

function Test-ValidPort {
    <#
    .SYNOPSIS
        Validate port number (1-65535)
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)]
        [int]$Port
    )

    return ($Port -ge 1) -and ($Port -le 65535)
}

function Assert-ValidPort {
    <#
    .SYNOPSIS
        Require valid port or throw
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)]
        [int]$Port
    )

    if (-not (Test-ValidPort -Port $Port)) {
        throw "Invalid port: $Port (must be 1-65535)"
    }
}

function Test-ValidPercentage {
    <#
    .SYNOPSIS
        Validate percentage (0-100)
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)]
        [double]$Value
    )

    return ($Value -ge 0) -and ($Value -le 100)
}

function Assert-ValidPercentage {
    <#
    .SYNOPSIS
        Require valid percentage or throw
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)]
        [double]$Value
    )

    if (-not (Test-ValidPercentage -Value $Value)) {
        throw "Invalid percentage: $Value (must be 0-100)"
    }
}

function Test-ValidEmail {
    <#
    .SYNOPSIS
        Basic email format validation
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)]
        [string]$Email
    )

    $pattern = '^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$'
    return $Email -match $pattern
}

function Test-ValidIPv4 {
    <#
    .SYNOPSIS
        Validate IPv4 address format
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)]
        [string]$Address
    )

    try {
        $ip = [System.Net.IPAddress]::Parse($Address)
        return $ip.AddressFamily -eq [System.Net.Sockets.AddressFamily]::InterNetwork
    }
    catch {
        return $false
    }
}

function Test-NonEmptyString {
    <#
    .SYNOPSIS
        Check if string is non-empty
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)]
        [AllowEmptyString()]
        [string]$Value
    )

    return -not [string]::IsNullOrWhiteSpace($Value)
}

function Assert-NonEmptyString {
    <#
    .SYNOPSIS
        Require non-empty string or throw
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)]
        [AllowEmptyString()]
        [string]$Value,
        [string]$ParameterName = "Value"
    )

    if ([string]::IsNullOrWhiteSpace($Value)) {
        throw "$ParameterName cannot be empty"
    }
}

# ============================================================================
# SAFE FILE OPERATIONS
# ============================================================================

function Test-SafePath {
    <#
    .SYNOPSIS
        Check if path is safe (no traversal attacks)
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)]
        [string]$Path
    )

    # Check for path traversal attempts
    if ($Path -match '\.\.') {
        return $false
    }

    # Check for absolute paths when not expected
    if ([System.IO.Path]::IsPathRooted($Path)) {
        return $false
    }

    # Check for null bytes
    if ($Path.Contains("`0")) {
        return $false
    }

    return $true
}

function Get-SafeFilePath {
    <#
    .SYNOPSIS
        Sanitize and resolve a file path safely
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)]
        [string]$BasePath,
        [Parameter(Mandatory)]
        [string]$RelativePath
    )

    if (-not (Test-SafePath -Path $RelativePath)) {
        return @{ Success = $false; Error = "Unsafe path detected" }
    }

    $fullPath = Join-Path -Path $BasePath -ChildPath $RelativePath
    $resolvedPath = [System.IO.Path]::GetFullPath($fullPath)

    # Ensure resolved path is still under base path
    $resolvedBase = [System.IO.Path]::GetFullPath($BasePath)
    if (-not $resolvedPath.StartsWith($resolvedBase)) {
        return @{ Success = $false; Error = "Path escapes base directory" }
    }

    return @{ Success = $true; Value = $resolvedPath }
}

# ============================================================================
# PERCENTAGE CALCULATIONS
# ============================================================================

function Get-PercentageOf {
    <#
    .SYNOPSIS
        Calculate percentage of amount (basis points for precision)
    .PARAMETER Amount
        Base amount
    .PARAMETER BasisPoints
        Basis points (100 bps = 1%)
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)]
        [long]$Amount,
        [Parameter(Mandatory)]
        [int]$BasisPoints
    )

    $mulResult = Multiply-Safely -A $Amount -B $BasisPoints
    if (-not $mulResult.Success) {
        return $mulResult
    }

    return Divide-Safely -A $mulResult.Value -B 10000
}

# ============================================================================
# VERSION
# ============================================================================

function Get-ProvenVersion {
    <#
    .SYNOPSIS
        Get library version
    #>
    return "0.9.0"
}

# ============================================================================
# EXPORTS
# ============================================================================

Export-ModuleMember -Function @(
    'Add-Safely',
    'Subtract-Safely',
    'Multiply-Safely',
    'Divide-Safely',
    'Get-Modulo-Safely',
    'Get-Clamped',
    'Test-InRange',
    'Assert-InRange',
    'Test-ValidPort',
    'Assert-ValidPort',
    'Test-ValidPercentage',
    'Assert-ValidPercentage',
    'Test-ValidEmail',
    'Test-ValidIPv4',
    'Test-NonEmptyString',
    'Assert-NonEmptyString',
    'Test-SafePath',
    'Get-SafeFilePath',
    'Get-PercentageOf',
    'Get-ProvenVersion'
)
