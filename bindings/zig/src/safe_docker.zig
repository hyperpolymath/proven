// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe Docker image and container name validation that cannot crash.
//! Provides validation for Docker image references, container names,
//! registry URLs, and related identifiers per OCI distribution spec.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Error types for Docker operations.
pub const DockerError = error{
    InvalidImageName,
    InvalidTag,
    InvalidDigest,
    InvalidRegistry,
    InvalidContainerName,
    InvalidRepository,
    InvalidNamespace,
    NameTooLong,
    OutOfMemory,
};

/// Maximum lengths per Docker/OCI specifications.
pub const MAX_IMAGE_NAME_LENGTH: usize = 128;
pub const MAX_TAG_LENGTH: usize = 128;
pub const MAX_CONTAINER_NAME_LENGTH: usize = 63;
pub const MAX_REPOSITORY_LENGTH: usize = 256;

/// Parsed Docker image reference.
pub const ImageReference = struct {
    /// Registry hostname (e.g., "docker.io", "gcr.io").
    registry: ?[]const u8,
    /// Namespace/organization (e.g., "library", "myorg").
    namespace: ?[]const u8,
    /// Repository name (e.g., "nginx", "my-app").
    repository: []const u8,
    /// Tag (e.g., "latest", "v1.0.0").
    tag: ?[]const u8,
    /// Digest (e.g., "sha256:abc123...").
    digest: ?[]const u8,

    const Self = @This();

    /// Get the full image name without tag or digest.
    pub fn fullName(self: Self, buffer: []u8) ![]const u8 {
        var pos: usize = 0;

        if (self.registry) |reg| {
            @memcpy(buffer[pos..][0..reg.len], reg);
            pos += reg.len;
            buffer[pos] = '/';
            pos += 1;
        }

        if (self.namespace) |ns| {
            @memcpy(buffer[pos..][0..ns.len], ns);
            pos += ns.len;
            buffer[pos] = '/';
            pos += 1;
        }

        @memcpy(buffer[pos..][0..self.repository.len], self.repository);
        pos += self.repository.len;

        return buffer[0..pos];
    }

    /// Get the full reference including tag or digest.
    pub fn fullReference(self: Self, buffer: []u8) ![]const u8 {
        var pos: usize = 0;

        // Name part
        const name = try self.fullName(buffer);
        pos = name.len;

        // Tag or digest
        if (self.digest) |dig| {
            buffer[pos] = '@';
            pos += 1;
            @memcpy(buffer[pos..][0..dig.len], dig);
            pos += dig.len;
        } else if (self.tag) |t| {
            buffer[pos] = ':';
            pos += 1;
            @memcpy(buffer[pos..][0..t.len], t);
            pos += t.len;
        }

        return buffer[0..pos];
    }

    /// Check if this is an official Docker Hub image (library/).
    pub fn isOfficial(self: Self) bool {
        if (self.registry != null) {
            if (!std.mem.eql(u8, self.registry.?, "docker.io") and
                !std.mem.eql(u8, self.registry.?, "index.docker.io"))
            {
                return false;
            }
        }
        return self.namespace == null or std.mem.eql(u8, self.namespace.?, "library");
    }

    /// Check if the image has a specific tag.
    pub fn hasTag(self: Self) bool {
        return self.tag != null;
    }

    /// Check if the image is pinned by digest.
    pub fn isPinned(self: Self) bool {
        return self.digest != null;
    }

    /// Check if using latest tag (explicit or implicit).
    pub fn isLatest(self: Self) bool {
        if (self.digest != null) return false;
        if (self.tag) |t| {
            return std.mem.eql(u8, t, "latest");
        }
        return true; // No tag = implicit latest
    }
};

/// Validate a Docker image name component (repository name).
pub fn isValidRepositoryName(name: []const u8) bool {
    if (name.len == 0 or name.len > MAX_REPOSITORY_LENGTH) return false;

    // Must start with lowercase alphanumeric
    if (!std.ascii.isLower(name[0]) and !std.ascii.isDigit(name[0])) return false;

    var prev_separator = false;
    for (name) |c| {
        const is_separator = (c == '.' or c == '_' or c == '-');
        const is_alnum = std.ascii.isLower(c) or std.ascii.isDigit(c);

        if (!is_alnum and !is_separator) return false;

        // No consecutive separators
        if (is_separator and prev_separator) return false;

        prev_separator = is_separator;
    }

    // Must not end with separator
    const last = name[name.len - 1];
    return last != '.' and last != '_' and last != '-';
}

/// Validate a Docker image tag.
pub fn isValidTag(tag: []const u8) bool {
    if (tag.len == 0 or tag.len > MAX_TAG_LENGTH) return false;

    // Must start with alphanumeric or underscore
    const first = tag[0];
    if (!std.ascii.isAlphanumeric(first) and first != '_') return false;

    // Rest can be alphanumeric, underscore, hyphen, or period
    for (tag) |c| {
        if (!std.ascii.isAlphanumeric(c) and c != '_' and c != '-' and c != '.') {
            return false;
        }
    }

    return true;
}

/// Validate a Docker image digest.
pub fn isValidDigest(digest: []const u8) bool {
    // Format: algorithm:hex
    const colon_pos = std.mem.indexOfScalar(u8, digest, ':') orelse return false;

    const algorithm = digest[0..colon_pos];
    const hash = digest[colon_pos + 1 ..];

    // Validate algorithm (sha256, sha384, sha512)
    const valid_algorithm = std.mem.eql(u8, algorithm, "sha256") or
        std.mem.eql(u8, algorithm, "sha384") or
        std.mem.eql(u8, algorithm, "sha512");

    if (!valid_algorithm) return false;

    // Validate hash length
    const expected_len: usize = if (std.mem.eql(u8, algorithm, "sha256"))
        64
    else if (std.mem.eql(u8, algorithm, "sha384"))
        96
    else
        128; // sha512

    if (hash.len != expected_len) return false;

    // Validate hex characters
    for (hash) |c| {
        if (!std.ascii.isHex(c)) return false;
    }

    return true;
}

/// Validate a Docker container name.
pub fn isValidContainerName(name: []const u8) bool {
    if (name.len == 0 or name.len > MAX_CONTAINER_NAME_LENGTH) return false;

    // Must match [a-zA-Z0-9][a-zA-Z0-9_.-]+
    // Must start with alphanumeric
    if (!std.ascii.isAlphanumeric(name[0])) return false;

    for (name) |c| {
        if (!std.ascii.isAlphanumeric(c) and c != '_' and c != '.' and c != '-') {
            return false;
        }
    }

    return true;
}

/// Validate a registry hostname.
pub fn isValidRegistry(registry: []const u8) bool {
    if (registry.len == 0) return false;

    // Check for port number
    var hostname: []const u8 = registry;
    if (std.mem.lastIndexOfScalar(u8, registry, ':')) |colon_pos| {
        const port_str = registry[colon_pos + 1 ..];
        // Validate port number
        const port = std.fmt.parseInt(u16, port_str, 10) catch return false;
        if (port == 0) return false;
        hostname = registry[0..colon_pos];
    }

    // Validate hostname
    if (hostname.len == 0) return false;

    // Must contain a dot or be localhost
    if (!std.mem.eql(u8, hostname, "localhost") and std.mem.indexOfScalar(u8, hostname, '.') == null) {
        return false;
    }

    // Validate hostname characters
    var prev_dot = false;
    for (hostname) |c| {
        const is_dot = (c == '.');
        const valid_char = std.ascii.isAlphanumeric(c) or c == '-' or c == '.';
        if (!valid_char) return false;
        if (is_dot and prev_dot) return false;
        prev_dot = is_dot;
    }

    // Must not start or end with dot or hyphen
    const first = hostname[0];
    const last = hostname[hostname.len - 1];
    return first != '.' and first != '-' and last != '.' and last != '-';
}

/// Validate a namespace (organization) name.
pub fn isValidNamespace(namespace: []const u8) bool {
    if (namespace.len == 0 or namespace.len > 255) return false;

    // Must start with alphanumeric
    if (!std.ascii.isAlphanumeric(namespace[0])) return false;

    for (namespace) |c| {
        if (!std.ascii.isAlphanumeric(c) and c != '_' and c != '-') {
            return false;
        }
    }

    return true;
}

/// Parse a Docker image reference string.
pub fn parseImageReference(input: []const u8) DockerError!ImageReference {
    if (input.len == 0) return error.InvalidImageName;

    var remaining = input;
    var registry: ?[]const u8 = null;
    var namespace: ?[]const u8 = null;
    var tag: ?[]const u8 = null;
    var digest: ?[]const u8 = null;

    // Check for digest (@sha256:...)
    if (std.mem.indexOfScalar(u8, remaining, '@')) |at_pos| {
        digest = remaining[at_pos + 1 ..];
        if (!isValidDigest(digest.?)) return error.InvalidDigest;
        remaining = remaining[0..at_pos];
    }

    // Check for tag (:tag)
    if (std.mem.lastIndexOfScalar(u8, remaining, ':')) |colon_pos| {
        // Make sure it's not a port number (would be followed by /)
        const after_colon = remaining[colon_pos + 1 ..];
        if (std.mem.indexOfScalar(u8, after_colon, '/') == null) {
            tag = after_colon;
            if (tag.?.len > 0 and !isValidTag(tag.?)) return error.InvalidTag;
            remaining = remaining[0..colon_pos];
        }
    }

    // Split by /
    var parts: [16][]const u8 = undefined;
    var part_count: usize = 0;

    var it = std.mem.splitScalar(u8, remaining, '/');
    while (it.next()) |part| {
        if (part_count >= 16) return error.InvalidImageName;
        parts[part_count] = part;
        part_count += 1;
    }

    if (part_count == 0) return error.InvalidImageName;

    // Determine registry vs namespace vs repository
    if (part_count == 1) {
        // Just repository (e.g., "nginx")
        const repository = parts[0];
        if (!isValidRepositoryName(repository)) return error.InvalidRepository;
        return ImageReference{
            .registry = null,
            .namespace = null,
            .repository = repository,
            .tag = tag,
            .digest = digest,
        };
    } else if (part_count == 2) {
        // Either registry/repo or namespace/repo
        const first = parts[0];
        const second = parts[1];

        // Check if first looks like a registry (has dot or port)
        if (std.mem.indexOfScalar(u8, first, '.') != null or
            std.mem.indexOfScalar(u8, first, ':') != null or
            std.mem.eql(u8, first, "localhost"))
        {
            if (!isValidRegistry(first)) return error.InvalidRegistry;
            if (!isValidRepositoryName(second)) return error.InvalidRepository;
            registry = first;
            return ImageReference{
                .registry = registry,
                .namespace = null,
                .repository = second,
                .tag = tag,
                .digest = digest,
            };
        } else {
            // namespace/repo
            if (!isValidNamespace(first)) return error.InvalidNamespace;
            if (!isValidRepositoryName(second)) return error.InvalidRepository;
            return ImageReference{
                .registry = null,
                .namespace = first,
                .repository = second,
                .tag = tag,
                .digest = digest,
            };
        }
    } else {
        // registry/namespace/repo or registry/namespace/.../repo
        const first = parts[0];

        // First part should be registry
        if (!isValidRegistry(first) and !std.mem.eql(u8, first, "docker.io")) {
            // Could be docker.io implicit, treat first as namespace
            if (!isValidNamespace(first)) return error.InvalidNamespace;
            namespace = first;
        } else {
            registry = first;
            if (part_count >= 3) {
                namespace = parts[1];
                if (!isValidNamespace(namespace.?)) return error.InvalidNamespace;
            }
        }

        // Last part is repository
        const repository = parts[part_count - 1];
        if (!isValidRepositoryName(repository)) return error.InvalidRepository;

        return ImageReference{
            .registry = registry,
            .namespace = namespace,
            .repository = repository,
            .tag = tag,
            .digest = digest,
        };
    }
}

/// Check if a string is a valid Docker image reference.
pub fn isValidImageReference(input: []const u8) bool {
    _ = parseImageReference(input) catch return false;
    return true;
}

/// Normalize an image reference (add docker.io and library if needed).
pub fn normalizeImageReference(allocator: Allocator, input: []const u8) DockerError![]u8 {
    const ref = try parseImageReference(input);

    var result = std.ArrayList(u8).init(allocator);
    errdefer result.deinit();

    // Add registry
    const registry = ref.registry orelse "docker.io";
    result.appendSlice(registry) catch return error.OutOfMemory;
    result.append('/') catch return error.OutOfMemory;

    // Add namespace
    const namespace = ref.namespace orelse "library";
    result.appendSlice(namespace) catch return error.OutOfMemory;
    result.append('/') catch return error.OutOfMemory;

    // Add repository
    result.appendSlice(ref.repository) catch return error.OutOfMemory;

    // Add tag or digest
    if (ref.digest) |dig| {
        result.append('@') catch return error.OutOfMemory;
        result.appendSlice(dig) catch return error.OutOfMemory;
    } else {
        result.append(':') catch return error.OutOfMemory;
        result.appendSlice(ref.tag orelse "latest") catch return error.OutOfMemory;
    }

    return result.toOwnedSlice() catch error.OutOfMemory;
}

/// Common Docker registries.
pub const Registry = struct {
    pub const DOCKER_HUB = "docker.io";
    pub const GCR = "gcr.io";
    pub const GHCR = "ghcr.io";
    pub const ECR_PUBLIC = "public.ecr.aws";
    pub const QUAY = "quay.io";
    pub const ACR = "azurecr.io";
};

test "isValidRepositoryName" {
    try std.testing.expect(isValidRepositoryName("nginx"));
    try std.testing.expect(isValidRepositoryName("my-app"));
    try std.testing.expect(isValidRepositoryName("my_app"));
    try std.testing.expect(isValidRepositoryName("my.app"));
    try std.testing.expect(!isValidRepositoryName("")); // empty
    try std.testing.expect(!isValidRepositoryName("-nginx")); // starts with hyphen
    try std.testing.expect(!isValidRepositoryName("nginx-")); // ends with hyphen
    try std.testing.expect(!isValidRepositoryName("NGINX")); // uppercase
}

test "isValidTag" {
    try std.testing.expect(isValidTag("latest"));
    try std.testing.expect(isValidTag("v1.0.0"));
    try std.testing.expect(isValidTag("1.0"));
    try std.testing.expect(isValidTag("_internal"));
    try std.testing.expect(!isValidTag("")); // empty
    try std.testing.expect(!isValidTag("-invalid")); // starts with hyphen
}

test "isValidDigest" {
    try std.testing.expect(isValidDigest("sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"));
    try std.testing.expect(!isValidDigest("sha256:tooshort"));
    try std.testing.expect(!isValidDigest("md5:abc123")); // invalid algorithm
    try std.testing.expect(!isValidDigest("sha256")); // missing hash
}

test "isValidContainerName" {
    try std.testing.expect(isValidContainerName("my-container"));
    try std.testing.expect(isValidContainerName("web_server"));
    try std.testing.expect(isValidContainerName("app.v1"));
    try std.testing.expect(!isValidContainerName("")); // empty
    try std.testing.expect(!isValidContainerName("-invalid")); // starts with hyphen
    try std.testing.expect(!isValidContainerName("has space")); // space
}

test "isValidRegistry" {
    try std.testing.expect(isValidRegistry("docker.io"));
    try std.testing.expect(isValidRegistry("gcr.io"));
    try std.testing.expect(isValidRegistry("localhost:5000"));
    try std.testing.expect(isValidRegistry("my-registry.example.com:8080"));
    try std.testing.expect(!isValidRegistry("")); // empty
    try std.testing.expect(!isValidRegistry("nodot")); // no dot (not localhost)
}

test "parseImageReference" {
    // Simple image
    const nginx = try parseImageReference("nginx");
    try std.testing.expectEqualStrings("nginx", nginx.repository);
    try std.testing.expect(nginx.isOfficial());
    try std.testing.expect(nginx.isLatest());

    // With tag
    const nginx_tagged = try parseImageReference("nginx:1.21");
    try std.testing.expectEqualStrings("nginx", nginx_tagged.repository);
    try std.testing.expectEqualStrings("1.21", nginx_tagged.tag.?);

    // With namespace
    const myorg = try parseImageReference("myorg/myapp");
    try std.testing.expectEqualStrings("myorg", myorg.namespace.?);
    try std.testing.expectEqualStrings("myapp", myorg.repository);

    // Full reference
    const gcr = try parseImageReference("gcr.io/my-project/my-image:v1");
    try std.testing.expectEqualStrings("gcr.io", gcr.registry.?);
    try std.testing.expectEqualStrings("my-project", gcr.namespace.?);
    try std.testing.expectEqualStrings("my-image", gcr.repository);
    try std.testing.expectEqualStrings("v1", gcr.tag.?);

    // With digest
    const pinned = try parseImageReference("nginx@sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855");
    try std.testing.expect(pinned.isPinned());
    try std.testing.expect(!pinned.isLatest());
}

test "normalizeImageReference" {
    const allocator = std.testing.allocator;

    const normalized = try normalizeImageReference(allocator, "nginx");
    defer allocator.free(normalized);
    try std.testing.expectEqualStrings("docker.io/library/nginx:latest", normalized);

    const normalized2 = try normalizeImageReference(allocator, "myorg/myapp:v1");
    defer allocator.free(normalized2);
    try std.testing.expectEqualStrings("docker.io/myorg/myapp:v1", normalized2);
}
