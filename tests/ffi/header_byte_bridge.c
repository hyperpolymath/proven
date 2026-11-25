/* SPDX-License-Identifier: MPL-2.0
 * Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
 * Compile against the public C header and call the real shared-library ABI.
 */
#include "proven.h"

int main(void) {
    const uint8_t bytes[] = {255, 0, 13, 128};
    for (size_t len = 0; len <= sizeof bytes; ++len) {
        ProvenBoolResult r = proven_header_has_crlf(bytes, len);
        if (r.status != 0 || r.value != (len >= 3)) return 1;
    }
    const uint8_t lf[] = {10};
    ProvenBoolResult r = proven_header_has_crlf(lf, sizeof lf);
    if (r.status != 0 || !r.value) return 2;
    r = proven_header_has_crlf(NULL, 0);
    if (r.status != -1 || r.value) return 3;
    return 0;
}
