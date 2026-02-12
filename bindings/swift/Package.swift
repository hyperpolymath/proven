// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

// swift-tools-version:5.9

import PackageDescription

let package = Package(
    name: "Proven",
    platforms: [
        .macOS(.v12),
        .iOS(.v15),
        .tvOS(.v15),
        .watchOS(.v8)
    ],
    products: [
        .library(
            name: "Proven",
            targets: ["Proven"]
        ),
    ],
    dependencies: [],
    targets: [
        .target(
            name: "Proven",
            dependencies: [],
            path: "Sources/Proven"
        ),
        .testTarget(
            name: "ProvenTests",
            dependencies: ["Proven"],
            path: "Tests/ProvenTests"
        ),
    ]
)
