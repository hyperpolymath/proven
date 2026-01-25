// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
//! Safe path operations with traversal prevention.

use crate::core::{Error, Result};
use std::path::{Component, Path, PathBuf};

/// Safe path operations.
pub struct SafePath;

impl SafePath {
    /// Check if a path contains traversal sequences.
    pub fn has_traversal(path: &str) -> bool {
        let p = Path::new(path);
        for component in p.components() {
            if matches!(component, Component::ParentDir) {
                return true;
            }
        }
        false
    }

    /// Normalize a path, returning error if traversal detected.
    pub fn normalize(path: &str) -> Result<PathBuf> {
        if Self::has_traversal(path) {
            return Err(Error::PathTraversal(path.to_string()));
        }

        let p = Path::new(path);
        let mut result = PathBuf::new();

        for component in p.components() {
            match component {
                Component::Normal(s) => result.push(s),
                Component::RootDir => result.push("/"),
                Component::CurDir => {}
                Component::ParentDir => {
                    return Err(Error::PathTraversal(path.to_string()));
                }
                Component::Prefix(p) => result.push(p.as_os_str()),
            }
        }

        Ok(result)
    }

    /// Join paths safely, preventing traversal.
    pub fn join(base: &str, child: &str) -> Result<PathBuf> {
        if Self::has_traversal(child) {
            return Err(Error::PathTraversal(child.to_string()));
        }

        let base_path = Path::new(base);
        let child_path = Path::new(child);

        // Don't allow absolute child paths
        if child_path.is_absolute() {
            return Err(Error::PathTraversal("Absolute path not allowed".to_string()));
        }

        Ok(base_path.join(child_path))
    }

    /// Check if a path is within a base directory.
    pub fn is_within(base: &str, path: &str) -> bool {
        let base = match std::fs::canonicalize(base) {
            Ok(p) => p,
            Err(_) => return false,
        };
        let path = match std::fs::canonicalize(path) {
            Ok(p) => p,
            Err(_) => return false,
        };

        path.starts_with(&base)
    }

    /// Get the file extension safely.
    pub fn get_extension(path: &str) -> Option<String> {
        Path::new(path)
            .extension()
            .and_then(|e| e.to_str())
            .map(|s| s.to_string())
    }

    /// Get the filename without extension.
    pub fn get_stem(path: &str) -> Option<String> {
        Path::new(path)
            .file_stem()
            .and_then(|s| s.to_str())
            .map(|s| s.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_has_traversal() {
        assert!(SafePath::has_traversal("../etc/passwd"));
        assert!(SafePath::has_traversal("foo/../bar"));
        assert!(!SafePath::has_traversal("foo/bar"));
        assert!(!SafePath::has_traversal("./foo/bar"));
    }

    #[test]
    fn test_normalize() {
        assert!(SafePath::normalize("../etc/passwd").is_err());
        assert!(SafePath::normalize("foo/bar").is_ok());
    }

    #[test]
    fn test_join() {
        let result = SafePath::join("/base", "child/file.txt");
        assert!(result.is_ok());

        let result = SafePath::join("/base", "../etc/passwd");
        assert!(result.is_err());
    }
}
