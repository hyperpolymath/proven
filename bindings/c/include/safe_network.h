/* SPDX-License-Identifier: PMPL-1.0 */
/* SPDX-FileCopyrightText: 2025 Hyperpolymath */

/**
 * @file safe_network.h
 * @brief IP address parsing and classification
 *
 * Provides safe parsing and validation of IPv4 addresses,
 * including classification as private, loopback, etc.
 */

#ifndef SAFE_NETWORK_H
#define SAFE_NETWORK_H

#include "proven_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Network Types
 * ============================================================================ */

/**
 * @brief IPv4 address structure
 */
typedef struct ProvenIPv4Address {
    uint8_t octets[4];
} ProvenIPv4Address;

/**
 * @brief Result for IPv4 parsing
 */
typedef struct ProvenIPv4Result {
    ProvenStatus status;
    ProvenIPv4Address address;
} ProvenIPv4Result;

/* ============================================================================
 * IPv4 Operations
 * ============================================================================ */

/**
 * @brief Parse IPv4 address string
 * @param ptr Pointer to address string (e.g., "192.168.1.1")
 * @param len Length of string
 * @return Result with parsed address
 */
ProvenIPv4Result proven_network_parse_ipv4(const uint8_t* ptr, size_t len);

/**
 * @brief Check if IPv4 address is private (RFC 1918)
 * @param addr IPv4 address
 * @return true if private (10.x.x.x, 172.16-31.x.x, 192.168.x.x)
 */
bool proven_network_ipv4_is_private(ProvenIPv4Address addr);

/**
 * @brief Check if IPv4 address is loopback (127.0.0.0/8)
 * @param addr IPv4 address
 * @return true if loopback
 */
bool proven_network_ipv4_is_loopback(ProvenIPv4Address addr);

#ifdef __cplusplus
}
#endif

#endif /* SAFE_NETWORK_H */
