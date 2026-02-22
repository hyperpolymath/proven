# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

cmake_minimum_required(VERSION 3.14)
project(proven_cpp VERSION 1.0.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Header-only INTERFACE library
add_library(proven_cpp INTERFACE)

target_include_directories(proven_cpp INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>
)

# Link against libproven (the Zig FFI bridge to Idris2 core)
target_link_libraries(proven_cpp INTERFACE proven)
