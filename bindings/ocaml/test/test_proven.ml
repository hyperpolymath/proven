(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2025 Hyperpolymath *)

open Proven

let test_safe_div () =
  Alcotest.(check (option int)) "10/2=5" (Some 5) (Safe_math.safe_div 10 2);
  Alcotest.(check (option int)) "10/0=None" None (Safe_math.safe_div 10 0)

let test_safe_mod () =
  Alcotest.(check (option int)) "10%3=1" (Some 1) (Safe_math.safe_mod 10 3);
  Alcotest.(check (option int)) "10%0=None" None (Safe_math.safe_mod 10 0)

let test_safe_add () =
  Alcotest.(check (option int)) "1+2=3" (Some 3) (Safe_math.safe_add 1 2)

let test_safe_sub () =
  Alcotest.(check (option int)) "5-3=2" (Some 2) (Safe_math.safe_sub 5 3)

let test_safe_mul () =
  Alcotest.(check (option int)) "3*4=12" (Some 12) (Safe_math.safe_mul 3 4)

let test_escape_html () =
  Alcotest.(check string) "escape <" "&lt;script&gt;" (Safe_string.escape_html "<script>");
  Alcotest.(check string) "escape &" "a &amp; b" (Safe_string.escape_html "a & b")

let test_escape_sql () =
  Alcotest.(check string) "escape '" "it''s" (Safe_string.escape_sql "it's")

let test_truncate_safe () =
  Alcotest.(check string) "truncate" "he..." (Safe_string.truncate_safe "hello world" 5);
  Alcotest.(check string) "no truncate" "hi" (Safe_string.truncate_safe "hi" 10)

let test_has_traversal () =
  Alcotest.(check bool) "../ detected" true (Safe_path.has_traversal "../etc");
  Alcotest.(check bool) "~ detected" true (Safe_path.has_traversal "~/file");
  Alcotest.(check bool) "safe path" false (Safe_path.has_traversal "normal/path")

let test_email_valid () =
  Alcotest.(check bool) "valid email" true (Safe_email.is_valid "user@example.com");
  Alcotest.(check bool) "invalid email" false (Safe_email.is_valid "not-an-email")

let test_email_normalize () =
  Alcotest.(check (option string)) "normalize"
    (Some "User@example.com")
    (Safe_email.normalize "User@EXAMPLE.COM")

let test_ipv4_valid () =
  Alcotest.(check bool) "valid ipv4" true (Safe_network.is_valid_ipv4 "192.168.1.1");
  Alcotest.(check bool) "invalid ipv4" false (Safe_network.is_valid_ipv4 "256.1.1.1")

let test_ipv4_private () =
  Alcotest.(check bool) "192.168 private" true (Safe_network.is_private "192.168.1.1");
  Alcotest.(check bool) "8.8.8.8 public" false (Safe_network.is_private "8.8.8.8")

let test_constant_time_compare () =
  Alcotest.(check bool) "equal" true (Safe_crypto.constant_time_compare "secret" "secret");
  Alcotest.(check bool) "not equal" false (Safe_crypto.constant_time_compare "secret" "other!")

let test_secure_zero () =
  let zeroed = Safe_crypto.secure_zero 4 in
  Alcotest.(check int) "length" 4 (String.length zeroed);
  Alcotest.(check string) "content" "\000\000\000\000" zeroed

let () =
  Alcotest.run "Proven" [
    "Safe_math", [
      Alcotest.test_case "safe_div" `Quick test_safe_div;
      Alcotest.test_case "safe_mod" `Quick test_safe_mod;
      Alcotest.test_case "safe_add" `Quick test_safe_add;
      Alcotest.test_case "safe_sub" `Quick test_safe_sub;
      Alcotest.test_case "safe_mul" `Quick test_safe_mul;
    ];
    "Safe_string", [
      Alcotest.test_case "escape_html" `Quick test_escape_html;
      Alcotest.test_case "escape_sql" `Quick test_escape_sql;
      Alcotest.test_case "truncate_safe" `Quick test_truncate_safe;
    ];
    "Safe_path", [
      Alcotest.test_case "has_traversal" `Quick test_has_traversal;
    ];
    "Safe_email", [
      Alcotest.test_case "is_valid" `Quick test_email_valid;
      Alcotest.test_case "normalize" `Quick test_email_normalize;
    ];
    "Safe_network", [
      Alcotest.test_case "is_valid_ipv4" `Quick test_ipv4_valid;
      Alcotest.test_case "is_private" `Quick test_ipv4_private;
    ];
    "Safe_crypto", [
      Alcotest.test_case "constant_time_compare" `Quick test_constant_time_compare;
      Alcotest.test_case "secure_zero" `Quick test_secure_zero;
    ];
  ]
