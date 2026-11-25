/* SPDX-License-Identifier: MPL-2.0 */
/* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk> */
/* Compile against the owning C header, not a copied test-only declaration. */
#include "proven.h"
#include <pthread.h>
#include <stdio.h>
#include <string.h>

static int require(int condition, const char *message) {
    if (condition) return 0;
    fprintf(stderr, "FAIL: %s\n", message);
    return 1;
}

static char worker_failure;
static void *worker(void *unused) {
    (void)unused;
    for (unsigned i = 0; i < 256; ++i) {
        if (proven_init() != PROVEN_OK) return &worker_failure;
        (void)proven_is_initialized(); /* Other callers may deinitialize. */
        proven_deinit();
    }
    return NULL;
}

static int sequential(int32_t expected, int ready_expected) {
    proven_deinit();
    if (require(!proven_is_initialized(), "initially stopped")) return 1;
    for (unsigned i = 0; i < 256; ++i) {
        if (require(proven_init() == expected, "init status")) return 1;
        if (require(proven_init() == expected, "repeated init status")) return 1;
        if (require(proven_is_initialized() == ready_expected, "readiness")) return 1;
        proven_deinit();
        proven_deinit();
        if (require(!proven_is_initialized(), "idempotent stop")) return 1;
    }
    return 0;
}

static int concurrent(void) {
    pthread_t threads[4];
    unsigned created = 0;
    for (; created < 4; ++created) {
        if (pthread_create(&threads[created], NULL, worker, NULL) != 0) break;
    }
    int failed = created != 4;
    for (unsigned i = 0; i < created; ++i) {
        void *result = NULL;
        if (pthread_join(threads[i], &result) != 0 || result != NULL) failed = 1;
    }
    if (require(!failed, "concurrent lifecycle calls")) return 1;
    if (require(proven_init() == PROVEN_OK && proven_is_initialized(),
                "ready after concurrent calls")) return 1;
    proven_deinit();
    return require(!proven_is_initialized(), "stopped after concurrent calls");
}

int main(int argc, char **argv) {
    /* Reject the known incompatible ABI before making any wrongly typed call. */
    if (require(proven_ffi_abi_version() == PROVEN_FFI_ABI_VERSION, "ABI version must be 2")) return 1;
    int32_t expected = PROVEN_OK;
    const char *message = "PASS: typed C lifecycle, idempotence and concurrency";
    if (argc == 2 && strcmp(argv[1], "--unavailable") == 0) {
        expected = PROVEN_ERR_NOT_IMPLEMENTED;
        message = "PASS: standalone reports unavailable";
    } else if (argc == 2 && strcmp(argv[1], "--backend-failure") == 0) {
        expected = PROVEN_ERR_VALIDATION_FAILED;
        message = "PASS: invalid backend result leaves lifecycle unready";
    }
    if (sequential(expected, expected == PROVEN_OK)) return 1;
    if (expected == PROVEN_OK && concurrent()) return 1;
    puts(message);
    return 0;
}
