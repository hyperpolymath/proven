// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

ThisBuild / version := "0.9.0"
ThisBuild / scalaVersion := "3.3.1"
ThisBuild / organization := "io.github.hyperpolymath"

lazy val root = (project in file("."))
  .settings(
    name := "proven",
    description := "Scala JNA bindings for libproven - formally verified safe operations",
    libraryDependencies ++= Seq(
      "net.java.dev.jna" % "jna" % "5.14.0",
      "org.scalatest" %% "scalatest" % "3.2.17" % Test
    ),
    // Publish settings
    pomExtra := {
      <url>https://github.com/hyperpolymath/proven</url>
      <licenses>
        <license>
          <name>PMPL-1.0-or-later</name>
          <url>https://github.com/hyperpolymath/proven/blob/main/LICENSE</url>
        </license>
      </licenses>
      <developers>
        <developer>
          <id>hyperpolymath</id>
          <name>Jonathan D.A. Jewell</name>
          <email>jonathan.jewell@open.ac.uk</email>
        </developer>
      </developers>
    }
  )
