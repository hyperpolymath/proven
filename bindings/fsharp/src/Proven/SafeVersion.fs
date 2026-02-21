// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

namespace Proven

/// Safe semantic version parsing, validation, and comparison (SemVer 2.0.0).
module SafeVersion =
    open System
    open System.Text.RegularExpressions

    /// Version parsing errors.
    type VersionError =
        | InvalidFormat of string
        | InvalidMajor of string
        | InvalidMinor of string
        | InvalidPatch of string
        | InvalidPrerelease of string
        | InvalidBuildMetadata of string
        | NegativeNumber of string
        | LeadingZero of string
        | EmptyInput

    /// Semantic version representation.
    type SemVer = {
        Major: int
        Minor: int
        Patch: int
        Prerelease: string list
        BuildMetadata: string list
    }

    /// Create a semantic version.
    let create (major: int) (minor: int) (patch: int) : Result<SemVer, VersionError> =
        if major < 0 then Error(NegativeNumber "major")
        elif minor < 0 then Error(NegativeNumber "minor")
        elif patch < 0 then Error(NegativeNumber "patch")
        else Ok { Major = major; Minor = minor; Patch = patch; Prerelease = []; BuildMetadata = [] }

    /// Create version with prerelease.
    let createWithPrerelease (major: int) (minor: int) (patch: int) (prerelease: string list) : Result<SemVer, VersionError> =
        match create major minor patch with
        | Error e -> Error e
        | Ok ver -> Ok { ver with Prerelease = prerelease }

    /// Create version with build metadata.
    let createWithBuild (major: int) (minor: int) (patch: int) (buildMetadata: string list) : Result<SemVer, VersionError> =
        match create major minor patch with
        | Error e -> Error e
        | Ok ver -> Ok { ver with BuildMetadata = buildMetadata }

    let private prereleasePattern = Regex(@"^(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*$")
    let private buildPattern = Regex(@"^[0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*$")
    let private semverPattern = Regex(@"^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(?:-((?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\+([0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?$")

    /// Parse a semantic version string.
    let parse (input: string) : Result<SemVer, VersionError> =
        if String.IsNullOrWhiteSpace(input) then
            Error EmptyInput
        else
            let trimmed = input.Trim()
            let trimmed = if trimmed.StartsWith("v") || trimmed.StartsWith("V") then trimmed.Substring(1) else trimmed
            let matchResult = semverPattern.Match(trimmed)
            if not matchResult.Success then
                Error(InvalidFormat trimmed)
            else
                let major = Int32.Parse(matchResult.Groups.[1].Value)
                let minor = Int32.Parse(matchResult.Groups.[2].Value)
                let patch = Int32.Parse(matchResult.Groups.[3].Value)
                let prerelease =
                    if matchResult.Groups.[4].Success && matchResult.Groups.[4].Value <> "" then
                        matchResult.Groups.[4].Value.Split('.') |> Array.toList
                    else []
                let buildMetadata =
                    if matchResult.Groups.[5].Success && matchResult.Groups.[5].Value <> "" then
                        matchResult.Groups.[5].Value.Split('.') |> Array.toList
                    else []
                Ok { Major = major; Minor = minor; Patch = patch; Prerelease = prerelease; BuildMetadata = buildMetadata }

    /// Parse version, returning Option.
    let tryParse (input: string) : SemVer option =
        match parse input with
        | Ok ver -> Some ver
        | Error _ -> None

    /// Check if string is a valid semantic version.
    let isValid (input: string) : bool =
        (tryParse input).IsSome

    /// Format version as string.
    let format (version: SemVer) : string =
        let core = sprintf "%d.%d.%d" version.Major version.Minor version.Patch
        let withPrerelease =
            if List.isEmpty version.Prerelease then core
            else sprintf "%s-%s" core (String.concat "." version.Prerelease)
        if List.isEmpty version.BuildMetadata then withPrerelease
        else sprintf "%s+%s" withPrerelease (String.concat "." version.BuildMetadata)

    /// Format version with v prefix.
    let formatWithPrefix (version: SemVer) : string =
        "v" + format version

    /// Check if version is prerelease.
    let isPrerelease (version: SemVer) : bool =
        not (List.isEmpty version.Prerelease)

    /// Check if version is stable (1.0.0 or higher, no prerelease).
    let isStable (version: SemVer) : bool =
        version.Major >= 1 && List.isEmpty version.Prerelease

    /// Compare prerelease identifiers.
    let private comparePrereleaseIdentifier (a: string) (b: string) : int =
        match Int32.TryParse(a), Int32.TryParse(b) with
        | (true, numA), (true, numB) -> compare numA numB
        | (true, _), (false, _) -> -1  // Numeric identifiers have lower precedence
        | (false, _), (true, _) -> 1
        | (false, _), (false, _) -> String.Compare(a, b, StringComparison.Ordinal)

    /// Compare prerelease lists.
    let private comparePrereleases (a: string list) (b: string list) : int =
        match a, b with
        | [], [] -> 0
        | [], _ -> 1   // No prerelease has higher precedence
        | _, [] -> -1
        | _ ->
            let rec compareRec aList bList =
                match aList, bList with
                | [], [] -> 0
                | [], _ -> -1  // Shorter prerelease has lower precedence
                | _, [] -> 1
                | aHead :: aTail, bHead :: bTail ->
                    let cmp = comparePrereleaseIdentifier aHead bHead
                    if cmp <> 0 then cmp else compareRec aTail bTail
            compareRec a b

    /// Compare two versions (ignores build metadata per SemVer spec).
    let compare (a: SemVer) (b: SemVer) : int =
        let majorCmp = Operators.compare a.Major b.Major
        if majorCmp <> 0 then majorCmp
        else
            let minorCmp = Operators.compare a.Minor b.Minor
            if minorCmp <> 0 then minorCmp
            else
                let patchCmp = Operators.compare a.Patch b.Patch
                if patchCmp <> 0 then patchCmp
                else comparePrereleases a.Prerelease b.Prerelease

    /// Check equality (ignores build metadata).
    let equals (a: SemVer) (b: SemVer) : bool =
        compare a b = 0

    /// Check if a > b.
    let isGreaterThan (a: SemVer) (b: SemVer) : bool =
        compare a b > 0

    /// Check if a >= b.
    let isGreaterOrEqual (a: SemVer) (b: SemVer) : bool =
        compare a b >= 0

    /// Check if a < b.
    let isLessThan (a: SemVer) (b: SemVer) : bool =
        compare a b < 0

    /// Check if a <= b.
    let isLessOrEqual (a: SemVer) (b: SemVer) : bool =
        compare a b <= 0

    /// Increment major version (resets minor and patch to 0).
    let incrementMajor (version: SemVer) : SemVer =
        { Major = version.Major + 1; Minor = 0; Patch = 0; Prerelease = []; BuildMetadata = [] }

    /// Increment minor version (resets patch to 0).
    let incrementMinor (version: SemVer) : SemVer =
        { version with Minor = version.Minor + 1; Patch = 0; Prerelease = []; BuildMetadata = [] }

    /// Increment patch version.
    let incrementPatch (version: SemVer) : SemVer =
        { version with Patch = version.Patch + 1; Prerelease = []; BuildMetadata = [] }

    /// Set prerelease identifiers.
    let setPrerelease (prerelease: string list) (version: SemVer) : SemVer =
        { version with Prerelease = prerelease }

    /// Clear prerelease identifiers.
    let clearPrerelease (version: SemVer) : SemVer =
        { version with Prerelease = [] }

    /// Set build metadata.
    let setBuildMetadata (metadata: string list) (version: SemVer) : SemVer =
        { version with BuildMetadata = metadata }

    /// Clear build metadata.
    let clearBuildMetadata (version: SemVer) : SemVer =
        { version with BuildMetadata = [] }

    /// Check if version satisfies a range (basic: ^1.2.3 compatible, ~1.2.3 approximately equal).
    let satisfiesRange (range: string) (version: SemVer) : bool =
        let trimmedRange = range.Trim()
        if trimmedRange.StartsWith("^") then
            // Caret: compatible with (same major, >= minor.patch)
            match tryParse (trimmedRange.Substring(1)) with
            | Some rangeVer ->
                version.Major = rangeVer.Major &&
                compare version rangeVer >= 0
            | None -> false
        elif trimmedRange.StartsWith("~") then
            // Tilde: approximately equal (same major.minor, >= patch)
            match tryParse (trimmedRange.Substring(1)) with
            | Some rangeVer ->
                version.Major = rangeVer.Major &&
                version.Minor = rangeVer.Minor &&
                compare version rangeVer >= 0
            | None -> false
        elif trimmedRange.StartsWith(">=") then
            match tryParse (trimmedRange.Substring(2).Trim()) with
            | Some rangeVer -> compare version rangeVer >= 0
            | None -> false
        elif trimmedRange.StartsWith("<=") then
            match tryParse (trimmedRange.Substring(2).Trim()) with
            | Some rangeVer -> compare version rangeVer <= 0
            | None -> false
        elif trimmedRange.StartsWith(">") then
            match tryParse (trimmedRange.Substring(1).Trim()) with
            | Some rangeVer -> compare version rangeVer > 0
            | None -> false
        elif trimmedRange.StartsWith("<") then
            match tryParse (trimmedRange.Substring(1).Trim()) with
            | Some rangeVer -> compare version rangeVer < 0
            | None -> false
        elif trimmedRange.StartsWith("=") then
            match tryParse (trimmedRange.Substring(1).Trim()) with
            | Some rangeVer -> equals version rangeVer
            | None -> false
        else
            // Exact match
            match tryParse trimmedRange with
            | Some rangeVer -> equals version rangeVer
            | None -> false

    /// Get the minimum of two versions.
    let min (a: SemVer) (b: SemVer) : SemVer =
        if compare a b <= 0 then a else b

    /// Get the maximum of two versions.
    let max (a: SemVer) (b: SemVer) : SemVer =
        if compare a b >= 0 then a else b

    /// Sort versions in ascending order.
    let sort (versions: SemVer list) : SemVer list =
        versions |> List.sortWith compare

    /// Sort versions in descending order.
    let sortDescending (versions: SemVer list) : SemVer list =
        versions |> List.sortWith (fun a b -> compare b a)

    /// Get the latest stable version from a list.
    let latestStable (versions: SemVer list) : SemVer option =
        versions
        |> List.filter isStable
        |> List.sortWith (fun a b -> compare b a)
        |> List.tryHead

    /// Get the latest version (including prerelease) from a list.
    let latest (versions: SemVer list) : SemVer option =
        versions
        |> List.sortWith (fun a b -> compare b a)
        |> List.tryHead

    /// Common version constants.
    let zero : SemVer = { Major = 0; Minor = 0; Patch = 0; Prerelease = []; BuildMetadata = [] }
    let v1_0_0 : SemVer = { Major = 1; Minor = 0; Patch = 0; Prerelease = []; BuildMetadata = [] }
