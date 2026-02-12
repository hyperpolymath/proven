-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeTemplate operations
|||
||| This module exports template safety operations to the C ABI via Idris2's RefC backend.
||| All functions are proven total and prevent Server-Side Template Injection (SSTI).
|||
||| Return conventions:
||| - Bool checks -> Int (0 = false, 1 = true)
||| - Rendering -> (status: Int, value: String)
module Proven.FFI.SafeTemplate

import Proven.SafeTemplate

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

--------------------------------------------------------------------------------
-- SSTI Detection
--------------------------------------------------------------------------------

export
proven_idris_template_has_dangerous_pattern : String -> Int
proven_idris_template_has_dangerous_pattern = encodeBool . hasDangerousPattern

export
proven_idris_template_is_valid_var_name : String -> Int
proven_idris_template_is_valid_var_name = encodeBool . isValidVarName

export
proven_idris_template_mk_safe_var : String -> (Int, String)
proven_idris_template_mk_safe_var s = case mkSafeVarName s of
  Nothing => (1, "Unsafe variable name")
  Just (MkSafeVarName n) => (0, n)

--------------------------------------------------------------------------------
-- HTML Escaping
--------------------------------------------------------------------------------

export
proven_idris_template_html_escape : String -> String
proven_idris_template_html_escape = htmlEscape

--------------------------------------------------------------------------------
-- Template Value Truthiness
--------------------------------------------------------------------------------

export
proven_idris_template_is_truthy_string : String -> Int
proven_idris_template_is_truthy_string s = encodeBool (isTruthy (TString s))

export
proven_idris_template_is_truthy_int : Int -> Int
proven_idris_template_is_truthy_int n = encodeBool (isTruthy (TInt (cast n)))

export
proven_idris_template_is_truthy_bool : Int -> Int
proven_idris_template_is_truthy_bool 0 = encodeBool (isTruthy (TBool False))
proven_idris_template_is_truthy_bool _ = encodeBool (isTruthy (TBool True))

--------------------------------------------------------------------------------
-- Simple Template Rendering
--------------------------------------------------------------------------------

export
proven_idris_template_render_literal : String -> String
proven_idris_template_render_literal s = renderExpr (Literal s) []

export
proven_idris_template_render_variable : String -> String -> String
proven_idris_template_render_variable varName value =
  renderExpr (Variable varName) [(varName, TString value)]

export
proven_idris_template_render_escaped_variable : String -> String -> String
proven_idris_template_render_escaped_variable varName value =
  renderExpr (Escape (Variable varName)) [(varName, TString value)]
