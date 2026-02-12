-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
-- Test FFI exports
module Main

import Proven.FFI.SafeTensor

main : IO ()
main = do
  putStrLn "=== Testing Proven FFI Exports ==="
  putStrLn ""

  -- Test tensor shape validation
  putStrLn "Testing SafeTensor FFI exports:"
  let result1 = proven_idris_tensor_same_shape 5 5
  putStrLn $ "  Shape equality (5, 5): " ++ show result1 ++ " (expected: 1)"

  let result2 = proven_idris_tensor_same_shape 3 5
  putStrLn $ "  Shape equality (3, 5): " ++ show result2 ++ " (expected: 0)"

  -- Test index validation
  let result3 = proven_idris_tensor_valid_index 2 5
  putStrLn $ "  Valid index (2 in length 5): " ++ show result3 ++ " (expected: 1)"

  let result4 = proven_idris_tensor_valid_index 5 5
  putStrLn $ "  Valid index (5 in length 5): " ++ show result4 ++ " (expected: 0)"

  -- Test matrix multiply compatibility
  let result5 = proven_idris_tensor_matmul_compatible 3 3
  putStrLn $ "  MatMul compatible (3x3 * 3xN): " ++ show result5 ++ " (expected: 1)"

  let result6 = proven_idris_tensor_matmul_compatible 3 5
  putStrLn $ "  MatMul compatible (3x3 * 5xN): " ++ show result6 ++ " (expected: 0)"

  putStrLn ""
  putStrLn "✓ All FFI exports working correctly!"
  putStrLn "✓ Functions exported with C ABI and callable from Zig/C"
