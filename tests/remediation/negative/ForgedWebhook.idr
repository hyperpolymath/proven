-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
module ForgedWebhook
import Proven.SafeWebhook
%default total
forged : (key : String) -> (event : WebhookEvent) -> (now : Nat) ->
         (tracker : IdempotencyTracker) -> WebhookVerified key event now tracker
forged key event now tracker = MkWebhookVerified Refl
