// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe capability-based security tokens that cannot crash.
//!
//! This module implements capability-based security tokens following the
//! principle of least privilege. Capabilities are unforgeable tokens that
//! grant specific permissions to resources. All operations are designed
//! to fail safely rather than expose security vulnerabilities.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Error types for capability operations.
pub const CapabilityError = error{
    InvalidToken,
    ExpiredCapability,
    InsufficientPermissions,
    InvalidResource,
    InvalidSignature,
    TokenTooLong,
    TokenTooShort,
    MalformedToken,
    OutOfMemory,
    CapabilityRevoked,
    DelegationNotAllowed,
    MaxDelegationDepthExceeded,
};

/// Permission flags for capabilities.
pub const Permission = packed struct {
    read: bool = false,
    write: bool = false,
    execute: bool = false,
    delete: bool = false,
    admin: bool = false,
    delegate: bool = false,
    revoke: bool = false,
    _reserved: u1 = 0,

    /// No permissions.
    pub const none = Permission{};

    /// Read-only permission.
    pub const read_only = Permission{ .read = true };

    /// Read and write permissions.
    pub const read_write = Permission{ .read = true, .write = true };

    /// Full permissions including admin.
    pub const full = Permission{
        .read = true,
        .write = true,
        .execute = true,
        .delete = true,
        .admin = true,
        .delegate = true,
        .revoke = true,
    };

    /// Check if this permission set includes another.
    pub fn includes(self: Permission, other: Permission) bool {
        if (other.read and !self.read) return false;
        if (other.write and !self.write) return false;
        if (other.execute and !self.execute) return false;
        if (other.delete and !self.delete) return false;
        if (other.admin and !self.admin) return false;
        if (other.delegate and !self.delegate) return false;
        if (other.revoke and !self.revoke) return false;
        return true;
    }

    /// Combine two permission sets (union).
    pub fn combine(self: Permission, other: Permission) Permission {
        return .{
            .read = self.read or other.read,
            .write = self.write or other.write,
            .execute = self.execute or other.execute,
            .delete = self.delete or other.delete,
            .admin = self.admin or other.admin,
            .delegate = self.delegate or other.delegate,
            .revoke = self.revoke or other.revoke,
        };
    }

    /// Intersect two permission sets.
    pub fn intersect(self: Permission, other: Permission) Permission {
        return .{
            .read = self.read and other.read,
            .write = self.write and other.write,
            .execute = self.execute and other.execute,
            .delete = self.delete and other.delete,
            .admin = self.admin and other.admin,
            .delegate = self.delegate and other.delegate,
            .revoke = self.revoke and other.revoke,
        };
    }

    /// Convert to byte representation.
    pub fn toByte(self: Permission) u8 {
        return @bitCast(self);
    }

    /// Create from byte representation.
    pub fn fromByte(byte: u8) Permission {
        return @bitCast(byte);
    }

    /// Check if any permission is set.
    pub fn any(self: Permission) bool {
        return self.read or self.write or self.execute or
            self.delete or self.admin or self.delegate or self.revoke;
    }

    /// Check if no permissions are set.
    pub fn isEmpty(self: Permission) bool {
        return !self.any();
    }
};

/// Resource identifier for capabilities.
pub const ResourceId = struct {
    namespace: [16]u8,
    identifier: [32]u8,

    /// Create from namespace and identifier strings.
    pub fn fromStrings(namespace_str: []const u8, id_str: []const u8) ResourceId {
        var result = ResourceId{
            .namespace = [_]u8{0} ** 16,
            .identifier = [_]u8{0} ** 32,
        };

        const ns_len = @min(namespace_str.len, 16);
        @memcpy(result.namespace[0..ns_len], namespace_str[0..ns_len]);

        const id_len = @min(id_str.len, 32);
        @memcpy(result.identifier[0..id_len], id_str[0..id_len]);

        return result;
    }

    /// Create a wildcard resource matching all in a namespace.
    pub fn wildcard(namespace_str: []const u8) ResourceId {
        return fromStrings(namespace_str, "*");
    }

    /// Check if this resource matches another (with wildcard support).
    pub fn matches(self: *const ResourceId, other: *const ResourceId) bool {
        // Namespace must match exactly
        if (!std.mem.eql(u8, &self.namespace, &other.namespace)) {
            return false;
        }

        // Check for wildcard
        if (self.identifier[0] == '*' and self.identifier[1] == 0) {
            return true;
        }

        return std.mem.eql(u8, &self.identifier, &other.identifier);
    }

    /// Check equality.
    pub fn eql(self: *const ResourceId, other: *const ResourceId) bool {
        return std.mem.eql(u8, &self.namespace, &other.namespace) and
            std.mem.eql(u8, &self.identifier, &other.identifier);
    }
};

/// A capability token granting permissions on a resource.
pub const Capability = struct {
    /// Unique capability identifier (random 128-bit value).
    id: [16]u8,
    /// Resource this capability grants access to.
    resource: ResourceId,
    /// Permissions granted.
    permissions: Permission,
    /// Unix timestamp when capability was created.
    created_at: i64,
    /// Unix timestamp when capability expires (0 = never).
    expires_at: i64,
    /// How many times this capability has been delegated.
    delegation_depth: u8,
    /// Maximum allowed delegation depth.
    max_delegation_depth: u8,
    /// Whether this capability has been revoked.
    revoked: bool,
    /// HMAC signature for integrity verification.
    signature: [32]u8,

    /// Check if capability is expired.
    pub fn isExpired(self: *const Capability) bool {
        if (self.expires_at == 0) return false;
        const now = std.time.timestamp();
        return now > self.expires_at;
    }

    /// Check if capability is valid (not expired or revoked).
    pub fn isValid(self: *const Capability) bool {
        return !self.revoked and !self.isExpired();
    }

    /// Check if capability grants a specific permission.
    pub fn hasPermission(self: *const Capability, perm: Permission) bool {
        return self.permissions.includes(perm);
    }

    /// Check if capability can be delegated.
    pub fn canDelegate(self: *const Capability) bool {
        return self.permissions.delegate and
            self.delegation_depth < self.max_delegation_depth;
    }

    /// Get remaining time in seconds (0 if expired or no expiry).
    pub fn remainingTime(self: *const Capability) i64 {
        if (self.expires_at == 0) return 0;
        const now = std.time.timestamp();
        const remaining = self.expires_at - now;
        return if (remaining > 0) remaining else 0;
    }

    /// Format capability ID as hex string.
    pub fn formatId(self: *const Capability, buf: []u8) CapabilityError![]const u8 {
        if (buf.len < 32) return error.TokenTooShort;
        const hex_chars = "0123456789abcdef";
        for (self.id, 0..) |byte, i| {
            buf[i * 2] = hex_chars[byte >> 4];
            buf[i * 2 + 1] = hex_chars[byte & 0x0F];
        }
        return buf[0..32];
    }
};

/// Capability factory for creating and managing capabilities.
pub const CapabilityFactory = struct {
    /// Secret key for signing capabilities.
    secret_key: [32]u8,
    /// Default expiration time in seconds (0 = never).
    default_expiry: i64,
    /// Default maximum delegation depth.
    default_max_delegation: u8,

    /// Initialize factory with a secret key.
    pub fn init(secret_key: [32]u8) CapabilityFactory {
        return .{
            .secret_key = secret_key,
            .default_expiry = 3600, // 1 hour default
            .default_max_delegation = 3,
        };
    }

    /// Initialize factory with random secret key.
    pub fn initRandom() CapabilityFactory {
        var secret_key: [32]u8 = undefined;
        std.crypto.random.bytes(&secret_key);
        return init(secret_key);
    }

    /// Create a new capability.
    pub fn create(
        self: *const CapabilityFactory,
        resource: ResourceId,
        permissions: Permission,
    ) Capability {
        return self.createWithOptions(resource, permissions, self.default_expiry, self.default_max_delegation);
    }

    /// Create a capability with custom options.
    pub fn createWithOptions(
        self: *const CapabilityFactory,
        resource: ResourceId,
        permissions: Permission,
        expiry_seconds: i64,
        max_delegation: u8,
    ) Capability {
        var capability = Capability{
            .id = undefined,
            .resource = resource,
            .permissions = permissions,
            .created_at = std.time.timestamp(),
            .expires_at = if (expiry_seconds > 0) std.time.timestamp() + expiry_seconds else 0,
            .delegation_depth = 0,
            .max_delegation_depth = max_delegation,
            .revoked = false,
            .signature = undefined,
        };

        // Generate random ID
        std.crypto.random.bytes(&capability.id);

        // Sign the capability
        capability.signature = self.sign(&capability);

        return capability;
    }

    /// Delegate a capability with reduced permissions.
    pub fn delegate(
        self: *const CapabilityFactory,
        parent: *const Capability,
        new_permissions: Permission,
    ) CapabilityError!Capability {
        if (!parent.isValid()) return error.ExpiredCapability;
        if (!parent.canDelegate()) return error.DelegationNotAllowed;
        if (!parent.permissions.includes(new_permissions)) return error.InsufficientPermissions;

        var child = Capability{
            .id = undefined,
            .resource = parent.resource,
            .permissions = parent.permissions.intersect(new_permissions),
            .created_at = std.time.timestamp(),
            .expires_at = parent.expires_at, // Inherit parent expiry
            .delegation_depth = parent.delegation_depth + 1,
            .max_delegation_depth = parent.max_delegation_depth,
            .revoked = false,
            .signature = undefined,
        };

        std.crypto.random.bytes(&child.id);
        child.signature = self.sign(&child);

        return child;
    }

    /// Verify a capability's signature.
    pub fn verify(self: *const CapabilityFactory, capability: *const Capability) bool {
        const expected_signature = self.sign(capability);
        return std.crypto.utils.timingSafeEql(expected_signature, capability.signature);
    }

    /// Validate a capability for a specific resource and permission.
    pub fn validate(
        self: *const CapabilityFactory,
        capability: *const Capability,
        resource: ResourceId,
        required_permission: Permission,
    ) CapabilityError!void {
        // Verify signature
        if (!self.verify(capability)) {
            return error.InvalidSignature;
        }

        // Check if revoked
        if (capability.revoked) {
            return error.CapabilityRevoked;
        }

        // Check expiration
        if (capability.isExpired()) {
            return error.ExpiredCapability;
        }

        // Check resource match
        if (!capability.resource.matches(&resource)) {
            return error.InvalidResource;
        }

        // Check permissions
        if (!capability.hasPermission(required_permission)) {
            return error.InsufficientPermissions;
        }
    }

    /// Sign a capability (internal).
    fn sign(self: *const CapabilityFactory, capability: *const Capability) [32]u8 {
        var hmac = std.crypto.auth.hmac.HmacSha256.init(&self.secret_key);

        // Include all relevant fields in signature
        hmac.update(&capability.id);
        hmac.update(&capability.resource.namespace);
        hmac.update(&capability.resource.identifier);
        hmac.update(&[_]u8{capability.permissions.toByte()});
        hmac.update(std.mem.asBytes(&capability.created_at));
        hmac.update(std.mem.asBytes(&capability.expires_at));
        hmac.update(&[_]u8{ capability.delegation_depth, capability.max_delegation_depth });

        return hmac.finalResult();
    }
};

/// Token serialization and deserialization.
pub const TokenCodec = struct {
    /// Serialize a capability to a binary token.
    pub fn encode(capability: *const Capability, buffer: []u8) CapabilityError![]u8 {
        // Token format:
        // 16 bytes: id
        // 16 bytes: resource.namespace
        // 32 bytes: resource.identifier
        // 1 byte: permissions
        // 8 bytes: created_at
        // 8 bytes: expires_at
        // 1 byte: delegation_depth
        // 1 byte: max_delegation_depth
        // 1 byte: revoked flag
        // 32 bytes: signature
        // Total: 116 bytes

        const required_size: usize = 116;
        if (buffer.len < required_size) return error.TokenTooShort;

        var offset: usize = 0;

        @memcpy(buffer[offset..][0..16], &capability.id);
        offset += 16;

        @memcpy(buffer[offset..][0..16], &capability.resource.namespace);
        offset += 16;

        @memcpy(buffer[offset..][0..32], &capability.resource.identifier);
        offset += 32;

        buffer[offset] = capability.permissions.toByte();
        offset += 1;

        std.mem.writeInt(i64, buffer[offset..][0..8], capability.created_at, .little);
        offset += 8;

        std.mem.writeInt(i64, buffer[offset..][0..8], capability.expires_at, .little);
        offset += 8;

        buffer[offset] = capability.delegation_depth;
        offset += 1;

        buffer[offset] = capability.max_delegation_depth;
        offset += 1;

        buffer[offset] = if (capability.revoked) 1 else 0;
        offset += 1;

        @memcpy(buffer[offset..][0..32], &capability.signature);
        offset += 32;

        return buffer[0..offset];
    }

    /// Deserialize a capability from a binary token.
    pub fn decode(token: []const u8) CapabilityError!Capability {
        const required_size: usize = 116;
        if (token.len < required_size) return error.TokenTooShort;

        var offset: usize = 0;
        var capability: Capability = undefined;

        @memcpy(&capability.id, token[offset..][0..16]);
        offset += 16;

        @memcpy(&capability.resource.namespace, token[offset..][0..16]);
        offset += 16;

        @memcpy(&capability.resource.identifier, token[offset..][0..32]);
        offset += 32;

        capability.permissions = Permission.fromByte(token[offset]);
        offset += 1;

        capability.created_at = std.mem.readInt(i64, token[offset..][0..8], .little);
        offset += 8;

        capability.expires_at = std.mem.readInt(i64, token[offset..][0..8], .little);
        offset += 8;

        capability.delegation_depth = token[offset];
        offset += 1;

        capability.max_delegation_depth = token[offset];
        offset += 1;

        capability.revoked = token[offset] != 0;
        offset += 1;

        @memcpy(&capability.signature, token[offset..][0..32]);

        return capability;
    }

    /// Encode capability to base64.
    pub fn encodeBase64(capability: *const Capability, buffer: []u8) CapabilityError![]u8 {
        var binary_buffer: [116]u8 = undefined;
        const binary = try encode(capability, &binary_buffer);

        const encoded_len = std.base64.standard.Encoder.calcSize(binary.len);
        if (buffer.len < encoded_len) return error.TokenTooShort;

        const encoded = std.base64.standard.Encoder.encode(buffer[0..encoded_len], binary);
        return @constCast(encoded);
    }

    /// Decode capability from base64.
    pub fn decodeBase64(encoded: []const u8) CapabilityError!Capability {
        var binary_buffer: [116]u8 = undefined;
        const decoded_len = std.base64.standard.Decoder.calcSizeForSlice(encoded) catch return error.MalformedToken;
        if (decoded_len > binary_buffer.len) return error.TokenTooLong;

        std.base64.standard.Decoder.decode(&binary_buffer, encoded) catch return error.MalformedToken;
        return decode(binary_buffer[0..decoded_len]);
    }
};

/// Check if a permission request is allowed by a capability.
pub fn checkAccess(
    factory: *const CapabilityFactory,
    capability: *const Capability,
    resource: ResourceId,
    required: Permission,
) bool {
    factory.validate(capability, resource, required) catch return false;
    return true;
}

test "Permission operations" {
    const read_write = Permission.read_write;
    const read_only = Permission.read_only;

    try std.testing.expect(read_write.includes(read_only));
    try std.testing.expect(!read_only.includes(read_write));
    try std.testing.expect(Permission.full.includes(read_write));
}

test "ResourceId matching" {
    const resource1 = ResourceId.fromStrings("files", "doc123");
    const resource2 = ResourceId.fromStrings("files", "doc123");
    const resource3 = ResourceId.fromStrings("files", "doc456");
    const wildcard_res = ResourceId.wildcard("files");

    try std.testing.expect(resource1.eql(&resource2));
    try std.testing.expect(!resource1.eql(&resource3));
    try std.testing.expect(wildcard_res.matches(&resource1));
    try std.testing.expect(wildcard_res.matches(&resource3));
}

test "Capability creation and validation" {
    var factory = CapabilityFactory.initRandom();
    const resource = ResourceId.fromStrings("api", "endpoint1");

    const capability = factory.create(resource, Permission.read_write);

    try std.testing.expect(capability.isValid());
    try std.testing.expect(capability.hasPermission(Permission.read_only));
    try std.testing.expect(factory.verify(&capability));

    try factory.validate(&capability, resource, Permission.read_only);
}

test "Capability delegation" {
    var factory = CapabilityFactory.initRandom();
    const resource = ResourceId.fromStrings("api", "endpoint1");

    const parent = factory.create(resource, Permission.full);
    const child = try factory.delegate(&parent, Permission.read_only);

    try std.testing.expect(child.isValid());
    try std.testing.expect(child.delegation_depth == 1);
    try std.testing.expect(child.hasPermission(Permission.read_only));
    try std.testing.expect(!child.hasPermission(Permission.read_write));
}

test "Capability insufficient permissions" {
    var factory = CapabilityFactory.initRandom();
    const resource = ResourceId.fromStrings("api", "endpoint1");

    const capability = factory.create(resource, Permission.read_only);

    try std.testing.expectError(
        error.InsufficientPermissions,
        factory.validate(&capability, resource, Permission.read_write),
    );
}

test "TokenCodec round-trip" {
    var factory = CapabilityFactory.initRandom();
    const resource = ResourceId.fromStrings("test", "resource");
    const original = factory.create(resource, Permission.read_write);

    var buffer: [116]u8 = undefined;
    const encoded = try TokenCodec.encode(&original, &buffer);
    const decoded = try TokenCodec.decode(encoded);

    try std.testing.expect(std.mem.eql(u8, &original.id, &decoded.id));
    try std.testing.expect(original.permissions.toByte() == decoded.permissions.toByte());
    try std.testing.expect(factory.verify(&decoded));
}

test "checkAccess helper" {
    var factory = CapabilityFactory.initRandom();
    const resource = ResourceId.fromStrings("data", "item1");
    const capability = factory.create(resource, Permission.read_only);

    try std.testing.expect(checkAccess(&factory, &capability, resource, Permission.read_only));
    try std.testing.expect(!checkAccess(&factory, &capability, resource, Permission.read_write));
}
