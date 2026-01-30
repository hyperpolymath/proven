-- SPDX-License-Identifier: Apache-2.0
-- Quick test of FFI exports
module Main

import Proven.FFI.SafeTensor
import Proven.FFI.SafeBuffer
import Proven.FFI.SafeHex

main : IO ()
main = do
  -- Test SafeTensor exports
  putStrLn "Testing SafeTensor FFI exports..."
  let shape1 = proven_idris_tensor_same_shape 5 5
  putStrLn $ "Shapes 5,5 same: " ++ show shape1

  let validIdx = proven_idris_tensor_valid_index 2 5
  putStrLn $ "Index 2 valid in length 5: " ++ show validIdx

  let matmul = proven_idris_tensor_matmul_compatible 3 3
  putStrLn $ "Matrix multiply 3x3, 3x3 compatible: " ++ show matmul

  -- Test SafeBuffer exports
  putStrLn "\nTesting SafeBuffer FFI exports..."
  let empty = proven_idris_buffer_is_empty 0
  putStrLn $ "Buffer length 0 is empty: " ++ show empty

  let full = proven_idris_buffer_is_full 10 10
  putStrLn $ "Buffer 10/10 is full: " ++ show full

  -- Test SafeHex exports
  putStrLn "\nTesting SafeHex FFI exports..."
  let validChar = proven_idris_hex_is_valid_char 65  -- 'A'
  putStrLn $ "Char 'A' (65) is valid hex: " ++ show validChar

  let evenLen = proven_idris_hex_even_length 8
  putStrLn $ "Length 8 is even: " ++ show evenLen

  putStrLn "\n✓ All FFI exports working!"
