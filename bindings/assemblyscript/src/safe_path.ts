// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafePath - Safe filesystem path operations
 *
 * Provides path normalization, traversal attack prevention,
 * and safe path manipulation.
 */

import { Result, Option, ErrorCode } from "./common";

/**
 * SafePath provides safe path operations.
 */
export class SafePath {
  private _path: string;

  constructor(path: string) {
    this._path = SafePath.normalize(path);
  }

  /**
   * Get the normalized path.
   */
  get path(): string {
    return this._path;
  }

  /**
   * Normalize a path by resolving . and .. components.
   */
  static normalize(path: string): string {
    if (path.length == 0) return ".";

    const isAbsolute = path.charCodeAt(0) == 47;  // /
    const parts = path.split("/");
    const stack: string[] = [];

    for (let i = 0; i < parts.length; i++) {
      const part = parts[i];
      if (part == "" || part == ".") continue;
      if (part == "..") {
        if (stack.length > 0 && stack[stack.length - 1] != "..") {
          stack.pop();
        } else if (!isAbsolute) {
          stack.push("..");
        }
      } else {
        stack.push(part);
      }
    }

    let result = stack.join("/");
    if (isAbsolute) result = "/" + result;
    return result.length > 0 ? result : ".";
  }

  /**
   * Check if path contains traversal sequences.
   */
  static containsTraversal(path: string): bool {
    const normalized = SafePath.normalize(path);
    return normalized.startsWith("..") || normalized.indexOf("/../") >= 0;
  }

  /**
   * Check if path is within a base directory.
   */
  static isWithinBase(path: string, base: string): bool {
    const normalizedPath = SafePath.normalize(path);
    const normalizedBase = SafePath.normalize(base);

    // Absolute paths
    if (normalizedPath.startsWith("/") && normalizedBase.startsWith("/")) {
      return normalizedPath.startsWith(normalizedBase + "/") || normalizedPath == normalizedBase;
    }

    // Relative path shouldn't escape
    if (!normalizedPath.startsWith("/") && !normalizedPath.startsWith("..")) {
      return true;
    }

    return false;
  }

  /**
   * Safely join path components.
   */
  static join(base: string, ...parts: string[]): Result<string> {
    let result = base;

    for (let i = 0; i < parts.length; i++) {
      const part = parts[i];

      // Check for absolute path in component (potential attack)
      if (part.startsWith("/")) {
        return Result.err<string>(ErrorCode.InvalidPath);
      }

      // Check for traversal
      if (SafePath.containsTraversal(part)) {
        return Result.err<string>(ErrorCode.InvalidPath);
      }

      // Join
      if (result.endsWith("/")) {
        result = result + part;
      } else {
        result = result + "/" + part;
      }
    }

    return Result.ok<string>(SafePath.normalize(result));
  }

  /**
   * Get parent directory.
   */
  static parent(path: string): Option<string> {
    const normalized = SafePath.normalize(path);
    const lastSlash = normalized.lastIndexOf("/");

    if (lastSlash < 0) {
      return normalized == ".." ? Option.some<string>("../..") : Option.some<string>(".");
    }

    if (lastSlash == 0) {
      return normalized == "/" ? Option.none<string>() : Option.some<string>("/");
    }

    return Option.some<string>(normalized.substring(0, lastSlash));
  }

  /**
   * Get filename (last component).
   */
  static filename(path: string): string {
    const normalized = SafePath.normalize(path);
    const lastSlash = normalized.lastIndexOf("/");
    if (lastSlash < 0) return normalized;
    return normalized.substring(lastSlash + 1);
  }

  /**
   * Get file extension.
   */
  static extension(path: string): Option<string> {
    const name = SafePath.filename(path);
    const lastDot = name.lastIndexOf(".");

    if (lastDot <= 0) {
      return Option.none<string>();
    }

    return Option.some<string>(name.substring(lastDot + 1));
  }

  /**
   * Get filename without extension (stem).
   */
  static stem(path: string): string {
    const name = SafePath.filename(path);
    const lastDot = name.lastIndexOf(".");

    if (lastDot <= 0) return name;
    return name.substring(0, lastDot);
  }

  /**
   * Check if path is absolute.
   */
  static isAbsolute(path: string): bool {
    return path.length > 0 && path.charCodeAt(0) == 47;  // /
  }

  /**
   * Check if path is relative.
   */
  static isRelative(path: string): bool {
    return !SafePath.isAbsolute(path);
  }

  /**
   * Make path relative to base.
   */
  static relativeTo(path: string, base: string): Result<string> {
    const normalizedPath = SafePath.normalize(path);
    const normalizedBase = SafePath.normalize(base);

    if (normalizedPath.startsWith(normalizedBase + "/")) {
      return Result.ok<string>(normalizedPath.substring(normalizedBase.length + 1));
    }

    if (normalizedPath == normalizedBase) {
      return Result.ok<string>(".");
    }

    return Result.err<string>(ErrorCode.InvalidPath);
  }

  /**
   * Sanitize path by removing dangerous characters.
   */
  static sanitize(path: string): string {
    let result = "";
    for (let i = 0; i < path.length; i++) {
      const code = path.charCodeAt(i);
      // Allow alphanumeric, /, ., -, _
      if ((code >= 48 && code <= 57) ||   // 0-9
          (code >= 65 && code <= 90) ||   // A-Z
          (code >= 97 && code <= 122) ||  // a-z
          code == 47 ||  // /
          code == 46 ||  // .
          code == 45 ||  // -
          code == 95) {  // _
        result += String.fromCharCode(code);
      }
    }
    return SafePath.normalize(result);
  }

  /**
   * Check if path has a specific extension.
   */
  static hasExtension(path: string, ext: string): bool {
    const pathExt = SafePath.extension(path);
    if (pathExt.isNone) return false;
    return pathExt.unwrap().toLowerCase() == ext.toLowerCase();
  }

  /**
   * Change file extension.
   */
  static withExtension(path: string, newExt: string): string {
    const dir = SafePath.parent(path);
    const name = SafePath.stem(path);

    const ext = newExt.startsWith(".") ? newExt.substring(1) : newExt;
    const newName = name + "." + ext;

    if (dir.isNone) return newName;
    return dir.unwrap() + "/" + newName;
  }

  // Instance methods
  join(component: string): Result<SafePath> {
    const result = SafePath.join(this._path, component);
    if (result.isErr()) {
      return Result.err<SafePath>(result.error);
    }
    return Result.ok<SafePath>(new SafePath(result.unwrap()));
  }

  getParent(): Option<SafePath> {
    const parent = SafePath.parent(this._path);
    if (parent.isNone) return Option.none<SafePath>();
    return Option.some<SafePath>(new SafePath(parent.unwrap()));
  }

  getFilename(): string {
    return SafePath.filename(this._path);
  }

  getExtension(): Option<string> {
    return SafePath.extension(this._path);
  }

  getStem(): string {
    return SafePath.stem(this._path);
  }
}
