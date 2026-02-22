// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

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
        .systemLibrary(
            name: "CProven",
            pkgConfig: "proven",
            providers: [
                .brew(["proven"]),
                .apt(["libproven-dev"]),
            ]
        ),
        .target(
            name: "Proven",
            dependencies: ["CProven"],
            path: "Sources/Proven"
        ),
        .testTarget(
            name: "ProvenTests",
            dependencies: ["Proven"],
            path: "Tests/ProvenTests"
        ),
    ]
)
