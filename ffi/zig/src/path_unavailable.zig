// SPDX-License-Identifier: MPL-2.0
pub fn detect(_: []const u8) error{BackendUnavailable}!bool {
    return error.BackendUnavailable;
}
