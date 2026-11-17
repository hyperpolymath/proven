-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeCookieProps

import Proven.Core
import Proven.SafeCookie
import Data.List
import Data.String
import System
import KnownDefects

%default total

-- Real API: `validName : String -> CookieResult CookieName`,
-- `validValue : String -> String -> CookieResult CookieValue`,
-- `validDomain`, `validPath`, `cookieWith : String -> String ->
-- CookieAttributes -> CookieResult Cookie`, attribute setters
-- (`httpOnly`, `secure`, `sameSite`, `maxAge`) and `build` (Set-Cookie).
-- The validators in SafeCookie.Parser are `export`, not `public export`, so
-- closed results do not reduce at the type level; the public-export
-- character predicates they rely on DO reduce and are proven below.

--------------------------------------------------------------------------------
-- Type-level proofs (public-export predicates from SafeCookie.Types)
--------------------------------------------------------------------------------

||| '=' and ';' are not cookie-name characters (validName rejects them).
prop_equalsNotNameChar : isValidNameChar '=' = False
prop_equalsNotNameChar = Refl

prop_semicolonNotNameChar : isValidNameChar ';' = False
prop_semicolonNotNameChar = Refl

||| A plain value has no injection character (validValue's gate).
prop_plainValueNoInjection : hasInjectionChar "abc123" = False
prop_plainValueNoInjection = Refl

||| Semicolon attribute smuggling is an injection.
prop_semicolonValueInjection : hasInjectionChar "value;HttpOnly" = True
prop_semicolonValueInjection = Refl

||| Header splitting via newline is an injection.
prop_newlineValueInjection : hasInjectionChar "value\nSet-Cookie: evil" = True
prop_newlineValueInjection = Refl

--------------------------------------------------------------------------------
-- Execution checks
--------------------------------------------------------------------------------

private
check : String -> Bool -> IO ()
check label True = putStrLn ("PASS (execution): " ++ label)
check label False = do
  putStrLn ("FAIL: " ++ label)
  exitFailure

||| Attributes with every optional flag off, so setters are observable.
private
bareAttrs : CookieAttributes
bareAttrs = { secure := False, httpOnly := False, sameSite := Nothing } defaultAttributes

private
cookieHas : (Cookie -> Bool) -> CookieResult Cookie -> Bool
cookieHas p (Ok c) = p c
cookieHas _ (Err _) = False

||| Test runner for cookie properties
export
runCookieProperties : IO ()
runCookieProperties = do
  putStrLn "SafeCookie Property Tests"
  putStrLn "========================="
  putStrLn "prop_{equals,semicolon}NotNameChar: PASS (proven by type)"
  putStrLn "prop_plainValueNoInjection, prop_{semicolon,newline}ValueInjection: PASS (proven by type)"
  check "valid cookie name session_id" (isOk (validName "session_id"))
  check "empty cookie name fails" (isErr (validName ""))
  check "cookie name with '=' fails" (isErr (validName "name=value"))
  check "cookie name with ';' fails" (isErr (validName "name;evil"))
  check "valid cookie value abc123" (isOk (validValue "n" "abc123"))
  check "cookie value with ';' fails" (isErr (validValue "n" "value;HttpOnly"))
  check "cookie value with newline fails" (isErr (validValue "n" "value\nSet-Cookie: evil"))
  check "HttpOnly attribute set and emitted"
    (cookieHas (\c => c.attributes.httpOnly && isInfixOf "HttpOnly" (build c))
               (cookieWith "name" "value" (httpOnly bareAttrs)))
  check "HttpOnly absent when not requested"
    (cookieHas (\c => not (isInfixOf "HttpOnly" (build c)))
               (cookieWith "name" "value" bareAttrs))
  check "Secure attribute set and emitted"
    (cookieHas (\c => c.attributes.secure && isInfixOf "Secure" (build c))
               (cookieWith "name" "value" (secure bareAttrs)))
  check "SameSite=Strict attribute set and emitted"
    (cookieHas (\c => c.attributes.sameSite == Just Strict &&
                      isInfixOf "SameSite=Strict" (build c))
               (cookieWith "name" "value" (sameSite Strict bareAttrs)))
  check "SameSite=None without Secure rejected"
    (isErr (cookieWith "name" "value" (sameSite None bareAttrs)))
  check "domain validation: example.com accepted" (isOk (validDomain "example.com"))
  check "domain validation: consecutive dots rejected" (isErr (validDomain "a..com"))
  check "path validation: /api/v1 accepted" (isOk (validPath "/api/v1"))
  check "path validation: relative path rejected" (isErr (validPath "api/v1"))
  check "Max-Age 3600 accepted and emitted"
    (cookieHas (\c => c.attributes.maxAge == Just 3600 && isInfixOf "Max-Age=3600" (build c))
               (cookieWith "name" "value" (maxAge 3600 defaultAttributes)))
  -- Known defect: no Max-Age validation; build emits "Max-Age=-1".
  knownDefect CookieNegativeMaxAge "negative Max-Age rejected (RFC 6265 max-age-av is non-zero-digit *DIGIT)"
    (isErr (cookieWith "name" "value" (maxAge (-1) defaultAttributes)))
  putStrLn ""
