# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
Proven - Code that cannot crash.

A verified safety library providing:
- SafeMath: Arithmetic without overflow/underflow/division-by-zero
- SafeString: UTF-8 and escaping without exceptions
- SafeJson: Parsing without JSONDecodeError
- SafeUrl: URL parsing without malformed URL crashes
- SafeEmail: Validation without regex catastrophic backtracking
- SafePath: Filesystem ops without traversal attacks
- SafeCrypto: Cryptographic primitives done right
- SafePassword: Authentication without security holes
"""

from .safe_math import SafeMath
from .safe_string import SafeString
from .safe_path import SafePath
from .safe_crypto import SafeCrypto
from .safe_url import SafeUrl
from .safe_email import SafeEmail
from .safe_network import SafeNetwork
from .core import ProvenError, ProvenStatus

__version__ = "0.3.0"
__all__ = [
    "SafeMath",
    "SafeString",
    "SafePath",
    "SafeCrypto",
    "SafeUrl",
    "SafeEmail",
    "SafeNetwork",
    "ProvenError",
    "ProvenStatus",
]
