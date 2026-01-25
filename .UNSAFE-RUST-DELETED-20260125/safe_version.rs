// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe semantic versioning parsing and comparison.
//!
//! Provides SemVer parsing with validation and safe comparison
//! operations following the Semantic Versioning 2.0.0 specification.

use crate::{Error, Result};
use std::cmp::Ordering;

/// Semantic version.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Version {
    pub major: u64,
    pub minor: u64,
    pub patch: u64,
    pub prerelease: Option<String>,
    pub build_metadata: Option<String>,
}

impl Version {
    /// Create a new version.
    pub fn new(major: u64, minor: u64, patch: u64) -> Self {
        Self {
            major,
            minor,
            patch,
            prerelease: None,
            build_metadata: None,
        }
    }

    /// Create with prerelease.
    pub fn with_prerelease(mut self, prerelease: &str) -> Self {
        self.prerelease = Some(prerelease.to_string());
        self
    }

    /// Create with build metadata.
    pub fn with_build(mut self, build: &str) -> Self {
        self.build_metadata = Some(build.to_string());
        self
    }

    /// Parse a version string.
    pub fn parse(s: &str) -> Result<Self> {
        let s = s.trim().strip_prefix('v').unwrap_or(s);

        // Split off build metadata
        let (version_pre, build) = match s.split_once('+') {
            Some((v, b)) => (v, Some(b.to_string())),
            None => (s, None),
        };

        // Split off prerelease
        let (version, prerelease) = match version_pre.split_once('-') {
            Some((v, p)) => (v, Some(p.to_string())),
            None => (version_pre, None),
        };

        // Parse major.minor.patch
        let parts: Vec<&str> = version.split('.').collect();
        if parts.len() != 3 {
            return Err(Error::InvalidInput(
                "Version must have major.minor.patch".into(),
            ));
        }

        let major = parts[0]
            .parse()
            .map_err(|_| Error::InvalidInput("Invalid major version".into()))?;
        let minor = parts[1]
            .parse()
            .map_err(|_| Error::InvalidInput("Invalid minor version".into()))?;
        let patch = parts[2]
            .parse()
            .map_err(|_| Error::InvalidInput("Invalid patch version".into()))?;

        Ok(Self {
            major,
            minor,
            patch,
            prerelease,
            build_metadata: build,
        })
    }

    /// Format as string.
    pub fn to_string(&self) -> String {
        let mut s = format!("{}.{}.{}", self.major, self.minor, self.patch);
        if let Some(ref pre) = self.prerelease {
            s.push('-');
            s.push_str(pre);
        }
        if let Some(ref build) = self.build_metadata {
            s.push('+');
            s.push_str(build);
        }
        s
    }

    /// Check if this is a prerelease version.
    pub fn is_prerelease(&self) -> bool {
        self.prerelease.is_some()
    }

    /// Check if this is a stable release (>= 1.0.0 and no prerelease).
    pub fn is_stable(&self) -> bool {
        self.major >= 1 && self.prerelease.is_none()
    }

    /// Increment major version.
    pub fn bump_major(&self) -> Self {
        Self::new(self.major + 1, 0, 0)
    }

    /// Increment minor version.
    pub fn bump_minor(&self) -> Self {
        Self::new(self.major, self.minor + 1, 0)
    }

    /// Increment patch version.
    pub fn bump_patch(&self) -> Self {
        Self::new(self.major, self.minor, self.patch + 1)
    }
}

impl PartialOrd for Version {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Version {
    fn cmp(&self, other: &Self) -> Ordering {
        // Compare major.minor.patch
        match self.major.cmp(&other.major) {
            Ordering::Equal => {}
            ord => return ord,
        }
        match self.minor.cmp(&other.minor) {
            Ordering::Equal => {}
            ord => return ord,
        }
        match self.patch.cmp(&other.patch) {
            Ordering::Equal => {}
            ord => return ord,
        }

        // Prerelease comparison
        match (&self.prerelease, &other.prerelease) {
            (None, None) => Ordering::Equal,
            (None, Some(_)) => Ordering::Greater, // No prerelease > prerelease
            (Some(_), None) => Ordering::Less,
            (Some(a), Some(b)) => a.cmp(b),
        }
        // Build metadata is ignored in comparison per SemVer spec
    }
}

/// Check if version satisfies a constraint.
pub fn satisfies(version: &Version, constraint: &str) -> Result<bool> {
    let constraint = constraint.trim();

    if constraint.starts_with(">=") {
        let target = Version::parse(&constraint[2..])?;
        return Ok(version >= &target);
    }
    if constraint.starts_with("<=") {
        let target = Version::parse(&constraint[2..])?;
        return Ok(version <= &target);
    }
    if constraint.starts_with('>') {
        let target = Version::parse(&constraint[1..])?;
        return Ok(version > &target);
    }
    if constraint.starts_with('<') {
        let target = Version::parse(&constraint[1..])?;
        return Ok(version < &target);
    }
    if constraint.starts_with('=') {
        let target = Version::parse(&constraint[1..])?;
        return Ok(version == &target);
    }
    if constraint.starts_with('^') {
        // Caret: compatible with version (same major, if major > 0)
        let target = Version::parse(&constraint[1..])?;
        if target.major == 0 {
            return Ok(version.major == 0 && version.minor == target.minor && version >= &target);
        }
        return Ok(version.major == target.major && version >= &target);
    }
    if constraint.starts_with('~') {
        // Tilde: same major.minor
        let target = Version::parse(&constraint[1..])?;
        return Ok(version.major == target.major
            && version.minor == target.minor
            && version >= &target);
    }

    // Exact match
    let target = Version::parse(constraint)?;
    Ok(version == &target)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let v = Version::parse("1.2.3").unwrap();
        assert_eq!(v.major, 1);
        assert_eq!(v.minor, 2);
        assert_eq!(v.patch, 3);

        let v = Version::parse("1.2.3-alpha.1+build.123").unwrap();
        assert_eq!(v.prerelease, Some("alpha.1".into()));
        assert_eq!(v.build_metadata, Some("build.123".into()));
    }

    #[test]
    fn test_comparison() {
        assert!(Version::parse("1.0.0").unwrap() > Version::parse("0.9.9").unwrap());
        assert!(Version::parse("1.0.0").unwrap() > Version::parse("1.0.0-alpha").unwrap());
        assert!(Version::parse("1.0.0-beta").unwrap() > Version::parse("1.0.0-alpha").unwrap());
    }

    #[test]
    fn test_satisfies() {
        let v = Version::parse("1.2.3").unwrap();
        assert!(satisfies(&v, ">=1.0.0").unwrap());
        assert!(satisfies(&v, "^1.0.0").unwrap());
        assert!(satisfies(&v, "~1.2.0").unwrap());
        assert!(!satisfies(&v, ">=2.0.0").unwrap());
    }
}
