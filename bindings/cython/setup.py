# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
Build configuration for the proven Cython extension modules.

Compiles .pyx files into native C extensions that link directly
against libproven. All computation is performed by the formally
verified Idris 2 core via the Zig FFI bridge.

Build:
    python setup.py build_ext --inplace

Install:
    pip install -e .
"""

import os
from setuptools import setup, Extension

# Try to import Cython; fall back to pre-generated C files if unavailable
try:
    from Cython.Build import cythonize
    USE_CYTHON = True
except ImportError:
    USE_CYTHON = False

# Locate the proven header and library
REPO_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
INCLUDE_DIR = os.path.join(REPO_ROOT, "bindings", "c", "include")
LIB_DIR = os.path.join(REPO_ROOT, "ffi", "zig", "zig-out", "lib")

# Common extension kwargs
ext_kwargs = {
    "include_dirs": [INCLUDE_DIR],
    "library_dirs": [LIB_DIR, "/usr/local/lib", "/usr/lib"],
    "libraries": ["proven"],
    "language": "c",
}


def make_ext(name, source):
    """Create an Extension object for a given module."""
    return Extension(
        name=name,
        sources=[source],
        **ext_kwargs,
    )


# Define all extension modules
ext_suffix = ".pyx" if USE_CYTHON else ".c"

extensions = [
    make_ext("proven.__init__",      f"proven/__init__{ext_suffix}"),
    make_ext("proven.safe_math",     f"proven/safe_math{ext_suffix}"),
    make_ext("proven.safe_string",   f"proven/safe_string{ext_suffix}"),
    make_ext("proven.safe_path",     f"proven/safe_path{ext_suffix}"),
    make_ext("proven.safe_email",    f"proven/safe_email{ext_suffix}"),
    make_ext("proven.safe_url",      f"proven/safe_url{ext_suffix}"),
    make_ext("proven.safe_crypto",   f"proven/safe_crypto{ext_suffix}"),
    make_ext("proven.safe_json",     f"proven/safe_json{ext_suffix}"),
    make_ext("proven.safe_datetime", f"proven/safe_datetime{ext_suffix}"),
]

if USE_CYTHON:
    extensions = cythonize(
        extensions,
        compiler_directives={
            "language_level": "3",
            "boundscheck": True,
            "wraparound": False,
            "cdivision": False,
        },
    )


setup(
    name="proven-cython",
    version="1.0.0",
    description="Cython bindings for libproven - code that cannot crash",
    long_description=open("README.md").read() if os.path.exists("README.md") else "",
    long_description_content_type="text/markdown",
    author="Jonathan D.A. Jewell",
    author_email="jonathan.jewell@open.ac.uk",
    url="https://github.com/hyperpolymath/proven",
    license="PMPL-1.0-or-later",
    ext_modules=extensions,
    packages=["proven"],
    package_data={"proven": ["*.pxd", "*.pyx"]},
    python_requires=">=3.9",
    setup_requires=["Cython>=3.0"],
    classifiers=[
        "Development Status :: 4 - Beta",
        "Intended Audience :: Developers",
        "Topic :: Software Development :: Libraries",
        "Programming Language :: Cython",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
        "Programming Language :: Python :: 3.12",
    ],
)
