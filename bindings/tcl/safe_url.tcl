# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath
#
# SafeUrl - Safe URL parsing and manipulation.
#

package provide proven::url 0.4.0
package require Tcl 8.6

namespace eval ::proven::url {
    namespace export parse is_valid get_domain get_scheme get_port
    namespace export get_path get_query get_fragment
    namespace export build normalize is_secure has_credentials
    namespace ensemble create

    # Parse URL into components
    # Returns dict with {scheme username password host port path query fragment ok error}
    proc parse {url} {
        set trimmedUrl [string trim $url]

        if {$trimmedUrl eq ""} {
            return [dict create scheme "" host "" path "" ok 0 error "URL is empty"]
        }

        # Extract scheme
        set schemeEnd [string first "://" $trimmedUrl]
        if {$schemeEnd < 0} {
            return [dict create scheme "" host "" path "" ok 0 error "Missing scheme"]
        }

        set scheme [string tolower [string range $trimmedUrl 0 [expr {$schemeEnd - 1}]]]
        set rest [string range $trimmedUrl [expr {$schemeEnd + 3}] end]

        # Extract fragment
        set fragment ""
        set fragPos [string last "#" $rest]
        if {$fragPos >= 0} {
            set fragment [string range $rest [expr {$fragPos + 1}] end]
            set rest [string range $rest 0 [expr {$fragPos - 1}]]
        }

        # Extract query
        set query ""
        set queryPos [string first "?" $rest]
        if {$queryPos >= 0} {
            set query [string range $rest [expr {$queryPos + 1}] end]
            set rest [string range $rest 0 [expr {$queryPos - 1}]]
        }

        # Extract path
        set path "/"
        set pathPos [string first "/" $rest]
        if {$pathPos >= 0} {
            set path [string range $rest $pathPos end]
            set rest [string range $rest 0 [expr {$pathPos - 1}]]
        }

        # Extract userinfo
        set username ""
        set password ""
        set atPos [string last "@" $rest]
        if {$atPos >= 0} {
            set userinfo [string range $rest 0 [expr {$atPos - 1}]]
            set rest [string range $rest [expr {$atPos + 1}] end]

            set colonPos [string first ":" $userinfo]
            if {$colonPos >= 0} {
                set username [string range $userinfo 0 [expr {$colonPos - 1}]]
                set password [string range $userinfo [expr {$colonPos + 1}] end]
            } else {
                set username $userinfo
            }
        }

        # Extract port
        set port ""
        set host $rest

        # Handle IPv6
        if {[string index $rest 0] eq "\["} {
            set bracketEnd [string first "\]" $rest]
            if {$bracketEnd >= 0} {
                set host [string range $rest 1 [expr {$bracketEnd - 1}]]
                set afterBracket [string range $rest [expr {$bracketEnd + 1}] end]
                if {[string index $afterBracket 0] eq ":"} {
                    set port [string range $afterBracket 1 end]
                }
            }
        } else {
            set colonPos [string last ":" $rest]
            if {$colonPos >= 0} {
                set potentialPort [string range $rest [expr {$colonPos + 1}] end]
                if {[string is integer -strict $potentialPort]} {
                    set port $potentialPort
                    set host [string range $rest 0 [expr {$colonPos - 1}]]
                }
            }
        }

        # Validate port if present
        if {$port ne "" && (![string is integer -strict $port] || $port < 1 || $port > 65535)} {
            return [dict create scheme "" host "" path "" ok 0 error "Invalid port number"]
        }

        return [dict create \
            scheme $scheme \
            username $username \
            password $password \
            host $host \
            port $port \
            path $path \
            query $query \
            fragment $fragment \
            ok 1 \
            error ""]
    }

    # Check if URL is valid
    proc is_valid {url} {
        set result [parse $url]
        return [dict get $result ok]
    }

    # Get domain from URL
    proc get_domain {url} {
        set result [parse $url]
        if {![dict get $result ok]} {
            return ""
        }
        return [dict get $result host]
    }

    # Get scheme from URL
    proc get_scheme {url} {
        set result [parse $url]
        if {![dict get $result ok]} {
            return ""
        }
        return [dict get $result scheme]
    }

    # Get port from URL (returns default for scheme if not specified)
    proc get_port {url} {
        set result [parse $url]
        if {![dict get $result ok]} {
            return 0
        }

        set port [dict get $result port]
        if {$port ne ""} {
            return $port
        }

        # Return default port for scheme
        set scheme [dict get $result scheme]
        switch -exact -- $scheme {
            http { return 80 }
            https { return 443 }
            ftp { return 21 }
            ssh { return 22 }
            default { return 0 }
        }
    }

    # Get path from URL
    proc get_path {url} {
        set result [parse $url]
        if {![dict get $result ok]} {
            return ""
        }
        return [dict get $result path]
    }

    # Get query string from URL
    proc get_query {url} {
        set result [parse $url]
        if {![dict get $result ok]} {
            return ""
        }
        return [dict get $result query]
    }

    # Get fragment from URL
    proc get_fragment {url} {
        set result [parse $url]
        if {![dict get $result ok]} {
            return ""
        }
        return [dict get $result fragment]
    }

    # Build URL from components
    proc build {scheme host {path "/"} {query ""} {fragment ""} {port ""} {username ""} {password ""}} {
        set url "${scheme}://"

        if {$username ne ""} {
            append url $username
            if {$password ne ""} {
                append url ":${password}"
            }
            append url "@"
        }

        append url $host

        if {$port ne ""} {
            append url ":${port}"
        }

        append url $path

        if {$query ne ""} {
            append url "?${query}"
        }

        if {$fragment ne ""} {
            append url "#${fragment}"
        }

        return $url
    }

    # Normalize URL (lowercase scheme and host, remove default ports)
    proc normalize {url} {
        set result [parse $url]
        if {![dict get $result ok]} {
            return $url
        }

        set scheme [string tolower [dict get $result scheme]]
        set host [string tolower [dict get $result host]]
        set port [dict get $result port]
        set path [dict get $result path]
        set query [dict get $result query]
        set fragment [dict get $result fragment]
        set username [dict get $result username]
        set password [dict get $result password]

        # Remove default ports
        if {($scheme eq "http" && $port eq "80") ||
            ($scheme eq "https" && $port eq "443")} {
            set port ""
        }

        # Normalize empty path to /
        if {$path eq ""} {
            set path "/"
        }

        return [build $scheme $host $path $query $fragment $port $username $password]
    }

    # Check if URL uses secure scheme
    proc is_secure {url} {
        set result [parse $url]
        if {![dict get $result ok]} {
            return 0
        }
        set scheme [dict get $result scheme]
        return [expr {$scheme eq "https" || $scheme eq "wss" || $scheme eq "sftp" || $scheme eq "ssh"}]
    }

    # Check if URL has credentials
    proc has_credentials {url} {
        set result [parse $url]
        if {![dict get $result ok]} {
            return 0
        }
        return [expr {[dict get $result username] ne "" || [dict get $result password] ne ""}]
    }
}
