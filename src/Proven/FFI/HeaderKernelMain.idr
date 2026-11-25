-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
module Proven.FFI.HeaderKernelMain
import Proven.SafeHeader.ByteKernel
import Proven.SafePath.ByteKernel

-- Dynamic inputs retain the noinline, two-argument headerStep in generated C.
-- This executable is a code-generation root; the library never invokes main.
main : IO ()
main = do
  a <- getLine
  b <- getLine
  printLn (headerStep (cast (length a)) (cast (length b)))
  printLn (pathFinish (pathStep (cast (length a)) (cast (length b))))
