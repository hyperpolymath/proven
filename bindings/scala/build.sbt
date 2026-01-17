// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

ThisBuild / version := "0.4.0"
ThisBuild / scalaVersion := "3.3.1"
ThisBuild / organization := "com.hyperpolymath"

lazy val root = (project in file("."))
  .settings(
    name := "proven",
    description := "Safe operations library with 38 modules for security, validation, and data handling",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.17" % Test
    )
  )

// Module count: 38
// Core (11): SafeMath, SafeString, SafePath, SafeEmail, SafeUrl, SafeNetwork, SafeCrypto, SafeUUID, SafeCurrency, SafePhone, SafeHex
// Data (7): SafeJson, SafeDateTime, SafeFloat, SafeVersion, SafeColor, SafeAngle, SafeUnit
// Data Structures (5): SafeBuffer, SafeQueue, SafeBloom, SafeLRU, SafeGraph
// Resilience (4): SafeRateLimiter, SafeCircuitBreaker, SafeRetry, SafeMonotonic
// State (2): SafeStateMachine, SafeCalculator
// Algorithm (4): SafeGeo, SafeProbability, SafeChecksum, SafeTensor
// Security (2): SafePassword, SafeMl
// HTTP (3): SafeHeader, SafeCookie, SafeContentType
