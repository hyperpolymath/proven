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

int main(int argc, char **argv) {
    /* Reject the known incompatible ABI before making any wrongly typed call. */
    if (require(proven_ffi_abi_version() == PROVEN_FFI_ABI_VERSION, "ABI version must be 2")) return 1;
    const int unavailable = argc == 2 && strcmp(argv[1], "--unavailable") == 0;
    const int backend_failure = argc == 2 && strcmp(argv[1], "--backend-failure") == 0;
    const int ready_expected = !unavailable && !backend_failure;
    proven_deinit();
    if (require(!proven_is_initialized(), "initially stopped")) return 1;
    for (unsigned i = 0; i < 256; ++i) {
        const int32_t expected = unavailable ? PROVEN_ERR_NOT_IMPLEMENTED :
            backend_failure ? PROVEN_ERR_VALIDATION_FAILED : PROVEN_OK;
        if (require(proven_init() == expected, "init status")) return 1;
        if (require(proven_init() == expected, "repeated init status")) return 1;
        if (require(proven_is_initialized() == ready_expected, "readiness")) return 1;
        proven_deinit();
        proven_deinit();
        if (require(!proven_is_initialized(), "idempotent stop")) return 1;
    }
    if (ready_expected) {
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
        if (require(!proven_is_initialized(), "stopped after concurrent calls")) return 1;
    }
    puts(backend_failure ? "PASS: invalid backend result leaves lifecycle unready" :
         unavailable ? "PASS: standalone reports unavailable" :
                       "PASS: typed C lifecycle, idempotence and concurrency");
    return 0;
}
