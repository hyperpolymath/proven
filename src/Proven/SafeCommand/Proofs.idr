-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| Safety proofs for shell command construction
|||
||| This module provides type-level proofs that:
||| - Escaped arguments cannot break out of quoting
||| - Command names don't contain injection characters
||| - Arguments don't contain shell metacharacters after escaping
module Proven.SafeCommand.Proofs