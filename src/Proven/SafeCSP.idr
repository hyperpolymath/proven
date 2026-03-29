-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| SafeCSP - Content Security Policy construction and validation
|||
||| Provides type-safe CSP header generation per W3C CSP Level 3.
||| Prevents: XSS via script injection, data exfiltration, clickjacking,
||| mixed content, unsafe inline styles/scripts.
module Proven.SafeCSP

import Data.String
import Data.List
import Data.Nat

%default total

-- ============================================================================
-- SOURCE EXPRESSIONS
-- ============================================================================

||| CSP source expression (what's allowed to load)
public export
data Source =
    Self                    -- 'self'
  | None                    -- 'none'
  | UnsafeInline            -- 'unsafe-inline' (dangerous)
  | UnsafeEval              -- 'unsafe-eval' (dangerous)
  | StrictDynamic           -- 'strict-dynamic'
  | UnsafeHashes            -- 'unsafe-hashes'
  | Nonce String            -- 'nonce-<base64>'
  | Hash String String      -- '<algorithm>-<base64>' (e.g., sha256)
  | Host String             -- https://example.com
  | Scheme String           -- https: data: blob:
  | WildcardHost            -- *

public export
Show Source where
  show Self          = "'self'"
  show None          = "'none'"
  show UnsafeInline  = "'unsafe-inline'"
  show UnsafeEval    = "'unsafe-eval'"
  show StrictDynamic = "'strict-dynamic'"
  show UnsafeHashes  = "'unsafe-hashes'"
  show (Nonce n)     = "'nonce-" ++ n ++ "'"
  show (Hash alg h)  = "'" ++ alg ++ "-" ++ h ++ "'"
  show (Host h)      = h
  show (Scheme s)    = s
  show WildcardHost  = "*"

public export
Eq Source where
  Self == Self = True
  None == None = True
  UnsafeInline == UnsafeInline = True
  UnsafeEval == UnsafeEval = True
  StrictDynamic == StrictDynamic = True
  UnsafeHashes == UnsafeHashes = True
  (Nonce a) == (Nonce b) = a == b
  (Hash a1 h1) == (Hash a2 h2) = a1 == a2 && h1 == h2
  (Host a) == (Host b) = a == b
  (Scheme a) == (Scheme b) = a == b
  WildcardHost == WildcardHost = True
  _ == _ = False

-- ============================================================================
-- DIRECTIVES
-- ============================================================================

||| CSP directive types
public export
data Directive =
    DefaultSrc (List Source)
  | ScriptSrc (List Source)
  | StyleSrc (List Source)
  | ImgSrc (List Source)
  | FontSrc (List Source)
  | ConnectSrc (List Source)
  | MediaSrc (List Source)
  | ObjectSrc (List Source)
  | FrameSrc (List Source)
  | ChildSrc (List Source)
  | WorkerSrc (List Source)
  | ManifestSrc (List Source)
  | BaseUri (List Source)
  | FormAction (List Source)
  | FrameAncestors (List Source)
  | UpgradeInsecureRequests
  | BlockAllMixedContent
  | ReportUri String
  | ReportTo String

||| Render a directive to its header string fragment
public export
renderDirective : Directive -> String
renderDirective (DefaultSrc srcs)  = "default-src " ++ renderSources srcs
renderDirective (ScriptSrc srcs)   = "script-src " ++ renderSources srcs
renderDirective (StyleSrc srcs)    = "style-src " ++ renderSources srcs
renderDirective (ImgSrc srcs)      = "img-src " ++ renderSources srcs
renderDirective (FontSrc srcs)     = "font-src " ++ renderSources srcs
renderDirective (ConnectSrc srcs)  = "connect-src " ++ renderSources srcs
renderDirective (MediaSrc srcs)    = "media-src " ++ renderSources srcs
renderDirective (ObjectSrc srcs)   = "object-src " ++ renderSources srcs
renderDirective (FrameSrc srcs)    = "frame-src " ++ renderSources srcs
renderDirective (ChildSrc srcs)    = "child-src " ++ renderSources srcs
renderDirective (WorkerSrc srcs)   = "worker-src " ++ renderSources srcs
renderDirective (ManifestSrc srcs) = "manifest-src " ++ renderSources srcs
renderDirective (BaseUri srcs)     = "base-uri " ++ renderSources srcs
renderDirective (FormAction srcs)  = "form-action " ++ renderSources srcs
renderDirective (FrameAncestors srcs) = "frame-ancestors " ++ renderSources srcs
renderDirective UpgradeInsecureRequests = "upgrade-insecure-requests"
renderDirective BlockAllMixedContent   = "block-all-mixed-content"
renderDirective (ReportUri uri)    = "report-uri " ++ uri
renderDirective (ReportTo group)   = "report-to " ++ group
  where
    renderSources : List Source -> String
    renderSources srcs = fastConcat (intersperse " " (map show srcs))

-- ============================================================================
-- CSP POLICY
-- ============================================================================

||| A complete CSP policy
public export
record CSPPolicy where
  constructor MkCSPPolicy
  directives : List Directive

||| Render the full CSP header value
public export
renderPolicy : CSPPolicy -> String
renderPolicy policy =
  fastConcat (intersperse "; " (map renderDirective policy.directives))

||| Empty policy
public export
emptyPolicy : CSPPolicy
emptyPolicy = MkCSPPolicy []

||| Add a directive to a policy
public export
addDirective : Directive -> CSPPolicy -> CSPPolicy
addDirective d policy = { directives $= (d ::) } policy

-- ============================================================================
-- SAFETY CHECKS
-- ============================================================================

||| Unsafe source expressions that weaken CSP
public export
isUnsafeSource : Source -> Bool
isUnsafeSource UnsafeInline = True
isUnsafeSource UnsafeEval   = True
isUnsafeSource WildcardHost  = True
isUnsafeSource (Scheme "data:") = True
isUnsafeSource _ = False

||| CSP policy warnings
public export
data CSPWarning =
    UnsafeInlineUsed String   -- directive name
  | UnsafeEvalUsed String
  | WildcardUsed String
  | MissingDefaultSrc
  | MissingObjectSrc
  | MissingBaseUri

public export
Show CSPWarning where
  show (UnsafeInlineUsed d) = "unsafe-inline in " ++ d
  show (UnsafeEvalUsed d)   = "unsafe-eval in " ++ d
  show (WildcardUsed d)     = "wildcard (*) in " ++ d
  show MissingDefaultSrc    = "No default-src directive"
  show MissingObjectSrc     = "No object-src (plugin vector)"
  show MissingBaseUri       = "No base-uri (base tag hijack vector)"

||| Check a directive for unsafe sources
checkDirectiveSafety : String -> List Source -> List CSPWarning
checkDirectiveSafety name srcs =
  let inlineWarn = if any (== UnsafeInline) srcs then [UnsafeInlineUsed name] else []
      evalWarn   = if any (== UnsafeEval) srcs then [UnsafeEvalUsed name] else []
      wildWarn   = if any (== WildcardHost) srcs then [WildcardUsed name] else []
  in inlineWarn ++ evalWarn ++ wildWarn

||| Audit a CSP policy for security weaknesses
public export
auditPolicy : CSPPolicy -> List CSPWarning
auditPolicy policy =
  let directiveWarnings = concatMap auditDirective policy.directives
      hasDefault = any isDefault policy.directives
      hasObject  = any isObject policy.directives
      hasBaseUri = any isBaseUri policy.directives
      missingWarnings = (if hasDefault then [] else [MissingDefaultSrc]) ++
                        (if hasObject then [] else [MissingObjectSrc]) ++
                        (if hasBaseUri then [] else [MissingBaseUri])
  in directiveWarnings ++ missingWarnings
  where
    auditDirective : Directive -> List CSPWarning
    auditDirective (ScriptSrc srcs) = checkDirectiveSafety "script-src" srcs
    auditDirective (StyleSrc srcs)  = checkDirectiveSafety "style-src" srcs
    auditDirective (DefaultSrc srcs) = checkDirectiveSafety "default-src" srcs
    auditDirective _ = []

    isDefault : Directive -> Bool
    isDefault (DefaultSrc _) = True
    isDefault _ = False

    isObject : Directive -> Bool
    isObject (ObjectSrc _) = True
    isObject _ = False

    isBaseUri : Directive -> Bool
    isBaseUri (BaseUri _) = True
    isBaseUri _ = False

||| Check if a policy has no warnings
public export
isSafePolicy : CSPPolicy -> Bool
isSafePolicy = isNil . auditPolicy

-- ============================================================================
-- PRESET POLICIES
-- ============================================================================

||| Strict CSP (recommended baseline)
public export
strictPolicy : CSPPolicy
strictPolicy = MkCSPPolicy
  [ DefaultSrc [Self]
  , ScriptSrc [Self]
  , StyleSrc [Self]
  , ImgSrc [Self]
  , FontSrc [Self]
  , ConnectSrc [Self]
  , ObjectSrc [None]
  , BaseUri [Self]
  , FormAction [Self]
  , FrameAncestors [None]
  , UpgradeInsecureRequests
  ]

||| Nonce-based CSP for dynamic scripts
public export
noncePolicy : String -> CSPPolicy
noncePolicy nonce = MkCSPPolicy
  [ DefaultSrc [Self]
  , ScriptSrc [StrictDynamic, Nonce nonce]
  , StyleSrc [Self, UnsafeInline]  -- styles often need inline
  , ImgSrc [Self, Scheme "data:"]
  , ObjectSrc [None]
  , BaseUri [None]
  , FrameAncestors [None]
  ]
