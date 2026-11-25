-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
module Proven.ABI.Lifecycle

%default total

public export
data LifecycleState = Stopped | Started

public export
data LifecycleCommand = Stop | Start

public export
transition : LifecycleCommand -> LifecycleState -> LifecycleState
transition Stop _ = Stopped
transition Start _ = Started

public export
stateCode : LifecycleState -> Bits64
stateCode Stopped = 0
stateCode Started = 1

||| Wire requests include the previous state, so the wrapper cannot silently
||| substitute a different state transition protocol. All other codes fail.
public export
requestCode : LifecycleState -> LifecycleCommand -> Bits64
requestCode Stopped Stop = 0
requestCode Stopped Start = 1
requestCode Started Stop = 2
requestCode Started Start = 3

||| Called by the production Zig lifecycle adapter. 2 is an invalid response.
public export
%noinline
lifecycleCommand : Bits64 -> Bits64
lifecycleCommand request =
  if request == 0 then stateCode (transition Stop Stopped)
  else if request == 1 then stateCode (transition Start Stopped)
  else if request == 2 then stateCode (transition Stop Started)
  else if request == 3 then stateCode (transition Start Started)
  else 2

||| Refinement of the actual generated entrypoint for every state/command pair.
public export
wireRefinesTransition : (state : LifecycleState) -> (command : LifecycleCommand) ->
  lifecycleCommand (requestCode state command) = stateCode (transition command state)
wireRefinesTransition Stopped Stop = Refl
wireRefinesTransition Stopped Start = Refl
wireRefinesTransition Started Stop = Refl
wireRefinesTransition Started Start = Refl

public export
transitionIdempotent : (command : LifecycleCommand) -> (state : LifecycleState) ->
  transition command (transition command state) = transition command state
transitionIdempotent Stop state = Refl
transitionIdempotent Start state = Refl

public export
startMakesReady : (state : LifecycleState) -> transition Start state = Started
startMakesReady state = Refl

public export
stopMakesUnready : (state : LifecycleState) -> transition Stop state = Stopped
stopMakesUnready state = Refl

public export
malformedRequestRejected : (request : Bits64) ->
  (request == 0) = False -> (request == 1) = False ->
  (request == 2) = False -> (request == 3) = False ->
  lifecycleCommand request = 2
malformedRequestRejected request notZero notOne notTwo notThree =
  rewrite notZero in rewrite notOne in rewrite notTwo in rewrite notThree in Refl
