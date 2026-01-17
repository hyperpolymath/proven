// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeOauth - OAuth 2.0 token and scope validation that cannot crash.
 *
 * Provides validation for OAuth 2.0 tokens, scopes, grant types, and
 * related constructs according to RFC 6749 and RFC 6750.
 */

/** OAuth error types */
type oauthError =
  | InvalidToken
  | TokenExpired
  | UnsupportedTokenType
  | InvalidScope
  | ScopeNotAllowed
  | UnsupportedGrantType
  | InvalidClientId
  | InvalidRedirectUri
  | InvalidState
  | InvalidAuthorizationCode
  | InvalidRefreshToken
  | InvalidCodeVerifier
  | InvalidCodeChallenge

/** OAuth 2.0 token types */
type tokenType = Bearer | Mac | DPoP

/** OAuth 2.0 grant types */
type grantType =
  | AuthorizationCode
  | ClientCredentials
  | RefreshToken
  | Password
  | DeviceCode
  | JwtBearer

/** PKCE code challenge methods */
type codeChallengeMethod = Plain | S256

/** OAuth 2.0 error codes */
type errorCode =
  | InvalidRequest
  | InvalidClient
  | InvalidGrant
  | UnauthorizedClient
  | UnsupportedGrantTypeError
  | InvalidScopeError
  | AccessDenied
  | UnsupportedResponseType
  | ServerError
  | TemporarilyUnavailable

/** Maximum token length */
let maxTokenLength = 4096

/** Maximum scope length (total) */
let maxScopeLength = 1024

/** Maximum individual scope name length */
let maxScopeNameLength = 256

/** Maximum client ID length */
let maxClientIdLength = 256

/** Minimum state parameter length */
let minStateLength = 8

/** Maximum state parameter length */
let maxStateLength = 512

/** PKCE code verifier minimum length (RFC 7636) */
let pkceMinVerifierLength = 43

/** PKCE code verifier maximum length (RFC 7636) */
let pkceMaxVerifierLength = 128

/** Common OpenID Connect scopes */
module CommonScopes = {
  let openid = "openid"
  let profile = "profile"
  let email = "email"
  let address = "address"
  let phone = "phone"
  let offlineAccess = "offline_access"
}

/** Parse token type from string */
let tokenTypeFromString = (s: string): option<tokenType> => {
  let lower = Js.String2.toLowerCase(s)
  switch lower {
  | "bearer" => Some(Bearer)
  | "mac" => Some(Mac)
  | "dpop" => Some(DPoP)
  | _ => None
  }
}

/** Convert token type to string */
let tokenTypeToString = (tt: tokenType): string => {
  switch tt {
  | Bearer => "Bearer"
  | Mac => "MAC"
  | DPoP => "DPoP"
  }
}

/** Parse grant type from string */
let grantTypeFromString = (s: string): option<grantType> => {
  switch s {
  | "authorization_code" => Some(AuthorizationCode)
  | "client_credentials" => Some(ClientCredentials)
  | "refresh_token" => Some(RefreshToken)
  | "password" => Some(Password)
  | "urn:ietf:params:oauth:grant-type:device_code" => Some(DeviceCode)
  | "urn:ietf:params:oauth:grant-type:jwt-bearer" => Some(JwtBearer)
  | _ => None
  }
}

/** Convert grant type to string */
let grantTypeToString = (gt: grantType): string => {
  switch gt {
  | AuthorizationCode => "authorization_code"
  | ClientCredentials => "client_credentials"
  | RefreshToken => "refresh_token"
  | Password => "password"
  | DeviceCode => "urn:ietf:params:oauth:grant-type:device_code"
  | JwtBearer => "urn:ietf:params:oauth:grant-type:jwt-bearer"
  }
}

/** Parse code challenge method from string */
let codeChallengeMethodFromString = (s: string): option<codeChallengeMethod> => {
  switch s {
  | "plain" => Some(Plain)
  | "S256" => Some(S256)
  | _ => None
  }
}

/** Convert code challenge method to string */
let codeChallengeMethodToString = (ccm: codeChallengeMethod): string => {
  switch ccm {
  | Plain => "plain"
  | S256 => "S256"
  }
}

/** Parse error code from string */
let errorCodeFromString = (s: string): option<errorCode> => {
  switch s {
  | "invalid_request" => Some(InvalidRequest)
  | "invalid_client" => Some(InvalidClient)
  | "invalid_grant" => Some(InvalidGrant)
  | "unauthorized_client" => Some(UnauthorizedClient)
  | "unsupported_grant_type" => Some(UnsupportedGrantTypeError)
  | "invalid_scope" => Some(InvalidScopeError)
  | "access_denied" => Some(AccessDenied)
  | "unsupported_response_type" => Some(UnsupportedResponseType)
  | "server_error" => Some(ServerError)
  | "temporarily_unavailable" => Some(TemporarilyUnavailable)
  | _ => None
  }
}

/** Convert error code to string */
let errorCodeToString = (ec: errorCode): string => {
  switch ec {
  | InvalidRequest => "invalid_request"
  | InvalidClient => "invalid_client"
  | InvalidGrant => "invalid_grant"
  | UnauthorizedClient => "unauthorized_client"
  | UnsupportedGrantTypeError => "unsupported_grant_type"
  | InvalidScopeError => "invalid_scope"
  | AccessDenied => "access_denied"
  | UnsupportedResponseType => "unsupported_response_type"
  | ServerError => "server_error"
  | TemporarilyUnavailable => "temporarily_unavailable"
  }
}

/** Check if character is valid for token (RFC 6750 VSCHAR) */
let isValidTokenChar = (charCode: float): bool => {
  // VSCHAR = %x21-7E (printable ASCII excluding space)
  charCode >= 33.0 && charCode <= 126.0
}

/** Check if character is NQCHAR (RFC 6749 Appendix A) */
let isNqchar = (charCode: float): bool => {
  // NQCHAR = %x21 / %x23-5B / %x5D-7E
  charCode == 33.0 || (charCode >= 35.0 && charCode <= 91.0) || (charCode >= 93.0 && charCode <= 126.0)
}

/** Check if character is unreserved for PKCE (RFC 7636) */
let isPkceUnreservedChar = (charCode: float): bool => {
  // alphanumeric, -, ., _, ~
  (charCode >= 65.0 && charCode <= 90.0) ||
  (charCode >= 97.0 && charCode <= 122.0) ||
  (charCode >= 48.0 && charCode <= 57.0) ||
  charCode == 45.0 ||
  charCode == 46.0 ||
  charCode == 95.0 ||
  charCode == 126.0
}

/** Check if character is base64url */
let isBase64UrlChar = (charCode: float): bool => {
  (charCode >= 65.0 && charCode <= 90.0) ||
  (charCode >= 97.0 && charCode <= 122.0) ||
  (charCode >= 48.0 && charCode <= 57.0) ||
  charCode == 45.0 ||
  charCode == 95.0 ||
  charCode == 61.0
}

/** Validate an access token format */
let validateAccessToken = (token: string): result<unit, oauthError> => {
  let len = Js.String2.length(token)
  if len == 0 || len > maxTokenLength {
    Error(InvalidToken)
  } else {
    let chars = Js.String2.split(token, "")
    let allValid = Js.Array2.every(chars, c => {
      let code = Js.String2.charCodeAt(c, 0)
      isValidTokenChar(code)
    })
    if allValid {
      Ok()
    } else {
      Error(InvalidToken)
    }
  }
}

/** Validate a refresh token format */
let validateRefreshToken = (token: string): result<unit, oauthError> => {
  let len = Js.String2.length(token)
  if len == 0 || len > maxTokenLength {
    Error(InvalidRefreshToken)
  } else {
    let chars = Js.String2.split(token, "")
    let allValid = Js.Array2.every(chars, c => {
      let code = Js.String2.charCodeAt(c, 0)
      isValidTokenChar(code)
    })
    if allValid {
      Ok()
    } else {
      Error(InvalidRefreshToken)
    }
  }
}

/** Validate an authorization code format */
let validateAuthorizationCode = (code: string): result<unit, oauthError> => {
  let len = Js.String2.length(code)
  if len == 0 || len > maxTokenLength {
    Error(InvalidAuthorizationCode)
  } else {
    let chars = Js.String2.split(code, "")
    let allValid = Js.Array2.every(chars, c => {
      let code = Js.String2.charCodeAt(c, 0)
      isValidTokenChar(code)
    })
    if allValid {
      Ok()
    } else {
      Error(InvalidAuthorizationCode)
    }
  }
}

/** Validate a single scope name */
let validateScopeName = (scope: string): result<unit, oauthError> => {
  let len = Js.String2.length(scope)
  if len == 0 || len > maxScopeNameLength {
    Error(InvalidScope)
  } else {
    let chars = Js.String2.split(scope, "")
    let allValid = Js.Array2.every(chars, c => {
      let code = Js.String2.charCodeAt(c, 0)
      isNqchar(code)
    })
    if allValid {
      Ok()
    } else {
      Error(InvalidScope)
    }
  }
}

/** Validate a space-separated scope string */
let validateScopeString = (scopeString: string): result<unit, oauthError> => {
  let len = Js.String2.length(scopeString)
  if len > maxScopeLength {
    Error(InvalidScope)
  } else if len == 0 {
    Ok()
  } else {
    let scopes = Js.String2.split(scopeString, " ")
    // Check for empty scopes (double space or leading/trailing space)
    let hasEmpty = Js.Array2.some(scopes, s => Js.String2.length(s) == 0)
    if hasEmpty {
      Error(InvalidScope)
    } else {
      let results = Js.Array2.map(scopes, validateScopeName)
      let hasError = Js.Array2.some(results, r =>
        switch r {
        | Error(_) => true
        | Ok(_) => false
        }
      )
      if hasError {
        Error(InvalidScope)
      } else {
        Ok()
      }
    }
  }
}

/** Parse a space-separated scope string into a list */
let parseScopes = (scopeString: string): result<array<string>, oauthError> => {
  switch validateScopeString(scopeString) {
  | Error(e) => Error(e)
  | Ok(_) =>
    if Js.String2.length(scopeString) == 0 {
      Ok([])
    } else {
      Ok(Js.String2.split(scopeString, " "))
    }
  }
}

/** Join scopes into a space-separated string */
let joinScopes = (scopes: array<string>): result<string, oauthError> => {
  if Js.Array2.length(scopes) == 0 {
    Ok("")
  } else {
    let results = Js.Array2.map(scopes, validateScopeName)
    let hasError = Js.Array2.some(results, r =>
      switch r {
      | Error(_) => true
      | Ok(_) => false
      }
    )
    if hasError {
      Error(InvalidScope)
    } else {
      let joined = Js.Array2.joinWith(scopes, " ")
      if Js.String2.length(joined) > maxScopeLength {
        Error(InvalidScope)
      } else {
        Ok(joined)
      }
    }
  }
}

/** Check if scope list contains a specific scope */
let scopesContain = (scopes: array<string>, scope: string): bool => {
  Js.Array2.includes(scopes, scope)
}

/** Check if scope list has all required scopes */
let scopesHaveAll = (scopes: array<string>, required: array<string>): bool => {
  Js.Array2.every(required, r => Js.Array2.includes(scopes, r))
}

/** Check if scope list has any of the candidate scopes */
let scopesHaveAny = (scopes: array<string>, candidates: array<string>): bool => {
  Js.Array2.some(candidates, c => Js.Array2.includes(scopes, c))
}

/** Validate a client ID */
let validateClientId = (clientId: string): result<unit, oauthError> => {
  let len = Js.String2.length(clientId)
  if len == 0 || len > maxClientIdLength {
    Error(InvalidClientId)
  } else {
    let chars = Js.String2.split(clientId, "")
    let allValid = Js.Array2.every(chars, c => {
      let code = Js.String2.charCodeAt(c, 0)
      code >= 32.0 && code <= 126.0
    })
    if allValid {
      Ok()
    } else {
      Error(InvalidClientId)
    }
  }
}

/** Validate a state parameter */
let validateState = (state: string): result<unit, oauthError> => {
  let len = Js.String2.length(state)
  if len < minStateLength || len > maxStateLength {
    Error(InvalidState)
  } else {
    let chars = Js.String2.split(state, "")
    let allValid = Js.Array2.every(chars, c => {
      let code = Js.String2.charCodeAt(c, 0)
      code >= 32.0 && code <= 126.0
    })
    if allValid {
      Ok()
    } else {
      Error(InvalidState)
    }
  }
}

/** Validate a PKCE code verifier */
let validateCodeVerifier = (verifier: string): result<unit, oauthError> => {
  let len = Js.String2.length(verifier)
  if len < pkceMinVerifierLength || len > pkceMaxVerifierLength {
    Error(InvalidCodeVerifier)
  } else {
    let chars = Js.String2.split(verifier, "")
    let allValid = Js.Array2.every(chars, c => {
      let code = Js.String2.charCodeAt(c, 0)
      isPkceUnreservedChar(code)
    })
    if allValid {
      Ok()
    } else {
      Error(InvalidCodeVerifier)
    }
  }
}

/** Validate a PKCE code challenge */
let validateCodeChallenge = (challenge: string): result<unit, oauthError> => {
  let len = Js.String2.length(challenge)
  if len < 43 || len > 128 {
    Error(InvalidCodeChallenge)
  } else {
    let chars = Js.String2.split(challenge, "")
    let allValid = Js.Array2.every(chars, c => {
      let code = Js.String2.charCodeAt(c, 0)
      isBase64UrlChar(code)
    })
    if allValid {
      Ok()
    } else {
      Error(InvalidCodeChallenge)
    }
  }
}

/** Validate a redirect URI */
let validateRedirectUri = (uri: string): result<unit, oauthError> => {
  if Js.String2.length(uri) == 0 {
    Error(InvalidRedirectUri)
  } else {
    let httpsPrefix = "https://"
    let httpLocalhost = "http://localhost"
    let http127 = "http://127.0.0.1"

    let isSecure = Js.String2.startsWith(uri, httpsPrefix)
    let isLocalhost =
      Js.String2.startsWith(uri, httpLocalhost) || Js.String2.startsWith(uri, http127)

    if !isSecure && !isLocalhost {
      Error(InvalidRedirectUri)
    } else if Js.String2.includes(uri, "#") {
      Error(InvalidRedirectUri)
    } else {
      Ok()
    }
  }
}

/** Check if a scope is an OpenID Connect scope */
let isOpenIdScope = (scope: string): bool => {
  switch scope {
  | "openid" | "profile" | "email" | "address" | "phone" | "offline_access" => true
  | _ => false
  }
}

/** Extract token from Authorization header (Bearer scheme) */
let extractBearerToken = (header: string): result<string, oauthError> => {
  let prefix = "Bearer "
  if !Js.String2.startsWith(header, prefix) {
    Error(UnsupportedTokenType)
  } else {
    let token = Js.String2.trim(Js.String2.sliceToEnd(header, ~from=Js.String2.length(prefix)))
    switch validateAccessToken(token) {
    | Error(e) => Error(e)
    | Ok(_) => Ok(token)
    }
  }
}

/** Build Authorization header value */
let buildBearerHeader = (token: string): result<string, oauthError> => {
  switch validateAccessToken(token) {
  | Error(e) => Error(e)
  | Ok(_) => Ok("Bearer " ++ token)
  }
}
