(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> *)

(** Tests for the Proven OCaml FFI bindings.

    These tests call through to libproven via ctypes.  They require
    [libproven.so] to be loadable at runtime (e.g., via LD_LIBRARY_PATH). *)

(* -------------------------------------------------------------------------- *)
(* Test helpers                                                               *)
(* -------------------------------------------------------------------------- *)

let option_int    = Alcotest.(option int)
let option_string = Alcotest.(option string)
let option_float  = Alcotest.(option (float 1e-6))

(* -------------------------------------------------------------------------- *)
(* Lifecycle                                                                  *)
(* -------------------------------------------------------------------------- *)

let test_init () =
  let ok = Proven.Lifecycle.init () in
  Alcotest.(check bool) "init succeeds" true ok;
  Alcotest.(check bool) "is_initialized" true (Proven.Lifecycle.is_initialized ())

(* -------------------------------------------------------------------------- *)
(* SafeMath                                                                   *)
(* -------------------------------------------------------------------------- *)

let test_math_div () =
  Alcotest.(check option_int) "10/2=5" (Some 5) (Proven.SafeMath.div 10 2);
  Alcotest.(check option_int) "10/0=None" None (Proven.SafeMath.div 10 0)

let test_math_mod () =
  Alcotest.(check option_int) "10%3=1" (Some 1) (Proven.SafeMath.modulo 10 3);
  Alcotest.(check option_int) "10%0=None" None (Proven.SafeMath.modulo 10 0)

let test_math_add () =
  Alcotest.(check option_int) "1+2=3" (Some 3) (Proven.SafeMath.add 1 2)

let test_math_sub () =
  Alcotest.(check option_int) "5-3=2" (Some 2) (Proven.SafeMath.sub 5 3)

let test_math_mul () =
  Alcotest.(check option_int) "3*4=12" (Some 12) (Proven.SafeMath.mul 3 4)

let test_math_abs () =
  Alcotest.(check option_int) "abs(-7)=7" (Some 7) (Proven.SafeMath.abs (-7));
  Alcotest.(check option_int) "abs(0)=0" (Some 0) (Proven.SafeMath.abs 0)

let test_math_clamp () =
  Alcotest.(check int) "clamp 5 [0,10]" 5 (Proven.SafeMath.clamp ~lo:0 ~hi:10 5);
  Alcotest.(check int) "clamp -1 [0,10]" 0 (Proven.SafeMath.clamp ~lo:0 ~hi:10 (-1));
  Alcotest.(check int) "clamp 20 [0,10]" 10 (Proven.SafeMath.clamp ~lo:0 ~hi:10 20)

let test_math_pow () =
  Alcotest.(check option_int) "2^10=1024" (Some 1024) (Proven.SafeMath.pow 2 10)

(* -------------------------------------------------------------------------- *)
(* SafeString                                                                 *)
(* -------------------------------------------------------------------------- *)

let test_string_escape_html () =
  Alcotest.(check option_string) "escape <"
    (Some "&lt;script&gt;")
    (Proven.SafeString.escape_html "<script>")

let test_string_escape_sql () =
  Alcotest.(check option_string) "escape '"
    (Some "it''s")
    (Proven.SafeString.escape_sql "it's")

let test_string_utf8 () =
  Alcotest.(check bool) "valid utf8" true
    (Proven.SafeString.is_valid_utf8 "hello")

(* -------------------------------------------------------------------------- *)
(* SafePath                                                                   *)
(* -------------------------------------------------------------------------- *)

let test_path_traversal () =
  Alcotest.(check bool) "../ detected" true (Proven.SafePath.has_traversal "../etc");
  Alcotest.(check bool) "safe path" false (Proven.SafePath.has_traversal "normal/path")

(* -------------------------------------------------------------------------- *)
(* SafeEmail                                                                  *)
(* -------------------------------------------------------------------------- *)

let test_email_valid () =
  Alcotest.(check bool) "valid" true (Proven.SafeEmail.is_valid "user@example.com");
  Alcotest.(check bool) "invalid" false (Proven.SafeEmail.is_valid "not-an-email")

(* -------------------------------------------------------------------------- *)
(* SafeCrypto                                                                 *)
(* -------------------------------------------------------------------------- *)

let test_crypto_constant_time_eq () =
  Alcotest.(check bool) "equal" true (Proven.SafeCrypto.constant_time_eq "secret" "secret");
  Alcotest.(check bool) "not equal" false (Proven.SafeCrypto.constant_time_eq "secret" "other!")

let test_crypto_random_bytes () =
  match Proven.SafeCrypto.random_bytes 16 with
  | Some s -> Alcotest.(check int) "16 bytes" 16 (String.length s)
  | None -> Alcotest.fail "random_bytes returned None"

(* -------------------------------------------------------------------------- *)
(* SafeFloat                                                                  *)
(* -------------------------------------------------------------------------- *)

let test_float_div () =
  Alcotest.(check option_float) "10/2=5" (Some 5.0) (Proven.SafeFloat.div 10.0 2.0);
  Alcotest.(check option_float) "10/0=None" None (Proven.SafeFloat.div 10.0 0.0)

let test_float_sqrt () =
  Alcotest.(check option_float) "sqrt(4)=2" (Some 2.0) (Proven.SafeFloat.sqrt 4.0);
  Alcotest.(check option_float) "sqrt(-1)=None" None (Proven.SafeFloat.sqrt (-1.0))

let test_float_ln () =
  Alcotest.(check option_float) "ln(e)~1" (Some 1.0) (Proven.SafeFloat.ln (exp 1.0));
  Alcotest.(check option_float) "ln(-1)=None" None (Proven.SafeFloat.ln (-1.0))

let test_float_finite () =
  Alcotest.(check bool) "1.0 finite" true (Proven.SafeFloat.is_finite 1.0);
  Alcotest.(check bool) "nan not finite" false (Proven.SafeFloat.is_finite Float.nan)

(* -------------------------------------------------------------------------- *)
(* SafeDateTime                                                               *)
(* -------------------------------------------------------------------------- *)

let test_datetime_leap_year () =
  Alcotest.(check bool) "2000 leap" true (Proven.SafeDateTime.is_leap_year 2000);
  Alcotest.(check bool) "1900 not leap" false (Proven.SafeDateTime.is_leap_year 1900);
  Alcotest.(check bool) "2024 leap" true (Proven.SafeDateTime.is_leap_year 2024)

let test_datetime_days_in_month () =
  Alcotest.(check int) "Feb 2024" 29 (Proven.SafeDateTime.days_in_month ~year:2024 ~month:2);
  Alcotest.(check int) "Feb 2023" 28 (Proven.SafeDateTime.days_in_month ~year:2023 ~month:2);
  Alcotest.(check int) "Jan" 31 (Proven.SafeDateTime.days_in_month ~year:2023 ~month:1)

(* -------------------------------------------------------------------------- *)
(* SafeAngle                                                                  *)
(* -------------------------------------------------------------------------- *)

let test_angle_conversion () =
  let rad = Proven.SafeAngle.deg_to_rad 180.0 in
  Alcotest.(check (float 1e-6)) "180 deg = pi rad" Float.pi rad;
  let deg = Proven.SafeAngle.rad_to_deg Float.pi in
  Alcotest.(check (float 1e-6)) "pi rad = 180 deg" 180.0 deg

let test_angle_normalize () =
  let n = Proven.SafeAngle.normalize_degrees 450.0 in
  Alcotest.(check (float 1e-6)) "450 -> 90" 90.0 n;
  let n2 = Proven.SafeAngle.normalize_degrees (-90.0) in
  Alcotest.(check (float 1e-6)) "-90 -> 270" 270.0 n2

(* -------------------------------------------------------------------------- *)
(* SafeProbability                                                            *)
(* -------------------------------------------------------------------------- *)

let test_probability () =
  let p = Proven.SafeProbability.create 0.5 in
  Alcotest.(check (float 1e-6)) "create 0.5" 0.5 p;
  let clamped = Proven.SafeProbability.create 1.5 in
  Alcotest.(check (float 1e-6)) "clamp 1.5 -> 1.0" 1.0 clamped;
  let comp = Proven.SafeProbability.complement 0.3 in
  Alcotest.(check (float 1e-6)) "not 0.3 = 0.7" 0.7 comp

(* -------------------------------------------------------------------------- *)
(* SafeML                                                                     *)
(* -------------------------------------------------------------------------- *)

let test_ml_sigmoid () =
  let s = Proven.SafeMl.sigmoid 0.0 in
  Alcotest.(check (float 1e-6)) "sigmoid(0)=0.5" 0.5 s

let test_ml_relu () =
  Alcotest.(check (float 1e-6)) "relu(5)=5" 5.0 (Proven.SafeMl.relu 5.0);
  Alcotest.(check (float 1e-6)) "relu(-5)=0" 0.0 (Proven.SafeMl.relu (-5.0))

(* -------------------------------------------------------------------------- *)
(* Version                                                                    *)
(* -------------------------------------------------------------------------- *)

let test_version_info () =
  let m = Proven.Version.major () in
  Alcotest.(check bool) "major >= 0" true (m >= 0);
  let mc = Proven.Version.module_count () in
  Alcotest.(check bool) "modules > 0" true (mc > 0)

(* -------------------------------------------------------------------------- *)
(* Test runner                                                                *)
(* -------------------------------------------------------------------------- *)

let () =
  Alcotest.run "Proven FFI" [
    "Lifecycle", [
      Alcotest.test_case "init" `Quick test_init;
    ];
    "SafeMath", [
      Alcotest.test_case "div" `Quick test_math_div;
      Alcotest.test_case "mod" `Quick test_math_mod;
      Alcotest.test_case "add" `Quick test_math_add;
      Alcotest.test_case "sub" `Quick test_math_sub;
      Alcotest.test_case "mul" `Quick test_math_mul;
      Alcotest.test_case "abs" `Quick test_math_abs;
      Alcotest.test_case "clamp" `Quick test_math_clamp;
      Alcotest.test_case "pow" `Quick test_math_pow;
    ];
    "SafeString", [
      Alcotest.test_case "escape_html" `Quick test_string_escape_html;
      Alcotest.test_case "escape_sql" `Quick test_string_escape_sql;
      Alcotest.test_case "utf8" `Quick test_string_utf8;
    ];
    "SafePath", [
      Alcotest.test_case "traversal" `Quick test_path_traversal;
    ];
    "SafeEmail", [
      Alcotest.test_case "is_valid" `Quick test_email_valid;
    ];
    "SafeCrypto", [
      Alcotest.test_case "constant_time_eq" `Quick test_crypto_constant_time_eq;
      Alcotest.test_case "random_bytes" `Quick test_crypto_random_bytes;
    ];
    "SafeFloat", [
      Alcotest.test_case "div" `Quick test_float_div;
      Alcotest.test_case "sqrt" `Quick test_float_sqrt;
      Alcotest.test_case "ln" `Quick test_float_ln;
      Alcotest.test_case "finite" `Quick test_float_finite;
    ];
    "SafeDateTime", [
      Alcotest.test_case "leap_year" `Quick test_datetime_leap_year;
      Alcotest.test_case "days_in_month" `Quick test_datetime_days_in_month;
    ];
    "SafeAngle", [
      Alcotest.test_case "conversion" `Quick test_angle_conversion;
      Alcotest.test_case "normalize" `Quick test_angle_normalize;
    ];
    "SafeProbability", [
      Alcotest.test_case "basic" `Quick test_probability;
    ];
    "SafeML", [
      Alcotest.test_case "sigmoid" `Quick test_ml_sigmoid;
      Alcotest.test_case "relu" `Quick test_ml_relu;
    ];
    "Version", [
      Alcotest.test_case "info" `Quick test_version_info;
    ];
  ]
