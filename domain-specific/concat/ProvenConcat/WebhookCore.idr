-- SPDX-License-Identifier: PMPL-1.0
module ProvenConcat.WebhookCore

import Proven.SafeHTTP
import Proven.SafeHeader
import Proven.SafeUrl
import Proven.SafeContentType
import Proven.SafeJson
import Proven.SafeJWT
import Proven.SafeRateLimiter
import Proven.SafeLog

%default total

public export
record WebhookRequest where
  constructor MkWebhookRequest
  method : Method
  url : String
  headers : List (String, String)
  body : String

public export
record WebhookResult where
  constructor MkWebhookResult
  payload : String

public export
data WebhookError
  = InvalidMethod String
  | InvalidUrl String
  | InvalidHeaders String
  | InvalidContentType String
  | InvalidPayload String
  | InvalidToken String

public export
validateWebhook : WebhookRequest -> Either WebhookError WebhookResult
validateWebhook _ = Left (InvalidPayload "unimplemented")
