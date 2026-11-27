/* SPDX-License-Identifier: MPL-2.0 */
#include "proven.h"
#include <stdio.h>
#include <string.h>

static size_t assertions;
static int oracle(const uint8_t *p, size_t n) {
    size_t start = 0;
    for (size_t i = 0; i <= n; ++i) {
        if (i == n || p[i] == 0 || p[i] == '/' || p[i] == '\\') {
            if (i - start == 2 && p[start] == '.' && p[start + 1] == '.') return 1;
            start = i + 1;
        }
    }
    return 0;
}

static int check(const uint8_t *p, size_t n) {
    uint8_t before[8192];
    if (n > sizeof before) return 1;
    memcpy(before, p, n);
    const int expected = oracle(before, n);
    ProvenBoolResult r = proven_path_has_traversal(p, n);
    ++assertions;
    if (r.status != 0 || r.value != expected || memcmp(before, p, n) != 0) {
        fprintf(stderr, "FAIL: path oracle at case %zu length %zu status %d\n", assertions, n, r.status);
        return 1;
    }
    return 0;
}

static int check_corpus(void) {
    const uint8_t alphabet[] = {0, '.', '/', '\\', 255};
    uint8_t bytes[7] = {0};
    size_t count = 1;
    for (size_t len = 0; len <= 6; ++len) {
        for (size_t index = 0; index < count; ++index) {
            size_t value = index;
            for (size_t j = 0; j < len; ++j) {
                bytes[j] = alphabet[value % 5];
                value /= 5;
            }
            if (check(bytes, len)) return 1;
        }
        count *= 5;
    }
    for (size_t left = 0; left < 256; ++left) {
        for (size_t right = 0; right < 256; ++right) {
            const uint8_t pair[] = {(uint8_t)left, '.', '.', (uint8_t)right};
            if (check(pair, sizeof pair)) return 1;
        }
    }
    return 0;
}

int main(int argc, char **argv) {
    const uint8_t windows[] = {'a', '\\', '.', '.', '\\', 'b'};
    if (argc == 2) {
        const int expected = strcmp(argv[1], "unavailable") == 0 ? -99 : -7;
        ProvenBoolResult r = proven_path_has_traversal(windows, sizeof windows);
        if (r.status != expected || r.value) {
            fprintf(stderr, "FAIL: path refusal expected %d, got %d/%d\n", expected, r.status, r.value);
            return 1;
        }
        puts("PASS: path backend refusal");
        return 0;
    }
    /* First case deliberately reproduces the pre-fix false negative. */
    if (check(windows, sizeof windows)) return 1;
    ProvenBoolResult r = proven_path_has_traversal(NULL, 0);
    if (r.status != -1 || r.value) return 2;
    r = proven_path_has_traversal(NULL, 4096);
    if (r.status != -1 || r.value) return 2;
    uint8_t guarded[] = {'x', 0, '.', '.', '\\', 255};
    for (size_t n = 0; n <= sizeof guarded; ++n) if (check(guarded, n)) return 1;
    uint8_t long_path[8192];
    memset(long_path, 'a', sizeof long_path);
    memcpy(long_path + sizeof long_path - 4, "/../", 4);
    if (check(long_path, sizeof long_path) || check(long_path, sizeof long_path - 2)) return 1;
    if (check_corpus()) return 1;
    printf("PASS: %zu actual C ABI path oracle cases and null controls\n", assertions);
    return 0;
}
