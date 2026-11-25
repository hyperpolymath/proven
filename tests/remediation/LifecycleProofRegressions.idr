-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
module LifecycleProofRegressions
import Proven.ABI.Lifecycle
%default total

allTransitionsRefine : (state : LifecycleState) -> (command : LifecycleCommand) ->
  lifecycleCommand (requestCode state command) = stateCode (transition command state)
allTransitionsRefine = wireRefinesTransition

repeatedCommands : (command : LifecycleCommand) -> (state : LifecycleState) ->
  transition command (transition command state) = transition command state
repeatedCommands = transitionIdempotent

allMalformedRequests : (request : Bits64) ->
  (request == 0) = False -> (request == 1) = False ->
  (request == 2) = False -> (request == 3) = False ->
  lifecycleCommand request = 2
allMalformedRequests = malformedRequestRejected

cannotStartWithInvalidMessage : lifecycleCommand 4 = 2
cannotStartWithInvalidMessage = Refl

positiveStart : lifecycleCommand (requestCode Stopped Start) = 1
positiveStart = wireRefinesTransition Stopped Start

positiveStop : lifecycleCommand (requestCode Started Stop) = 0
positiveStop = wireRefinesTransition Started Stop
