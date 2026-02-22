// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeEmail.vala -- Email address validation.
 *
 * Delegates to libproven via C extern calls declared in lib_proven.vapi.
 * No email validation logic is reimplemented here.
 */
namespace Proven {

    /**
     * Email address validation (RFC 5321 simplified).
     */
    public class SafeEmail : GLib.Object {

        /**
         * Validate an email address.
         *
         * @param email Email address string to validate.
         * @return true if the address is valid, false if not; null on internal error.
         */
        public static bool? is_valid (string email) {
            unowned uint8[] data = (uint8[]) email.data;
            LibProven.BoolResult r = LibProven.email_is_valid (data);
            if (r.status != 0) {
                return null;
            }
            return r.value;
        }
    }
}
