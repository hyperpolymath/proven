// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

plugins {
    kotlin("jvm") version "1.9.22"
    id("org.jetbrains.dokka") version "1.9.10"
    `maven-publish`
}

group = "io.github.hyperpolymath"
version = "0.9.0"

repositories {
    mavenCentral()
}

dependencies {
    implementation("net.java.dev.jna:jna:5.14.0")
    testImplementation(kotlin("test"))
}

tasks.test {
    useJUnitPlatform()
}

kotlin {
    jvmToolchain(17)
}

publishing {
    publications {
        create<MavenPublication>("maven") {
            groupId = "io.github.hyperpolymath"
            artifactId = "proven"
            version = "0.9.0"
            from(components["java"])

            pom {
                name.set("proven")
                description.set("Kotlin FFI bindings for libproven - formally verified safe operations")
                url.set("https://github.com/hyperpolymath/proven")
                licenses {
                    license {
                        name.set("PMPL-1.0-or-later")
                        url.set("https://github.com/hyperpolymath/proven/blob/main/LICENSE")
                    }
                }
                developers {
                    developer {
                        id.set("hyperpolymath")
                        name.set("Jonathan D.A. Jewell")
                        email.set("jonathan.jewell@open.ac.uk")
                    }
                }
            }
        }
    }
}
