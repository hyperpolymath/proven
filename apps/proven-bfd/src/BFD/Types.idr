-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Core protocol types for BFD (RFC 5880 - Bidirectional Forwarding Detection).
||| All types are closed sum types with Show instances.
module BFD.Types

%default total

---------------------------------------------------------------------------
-- BFD State (RFC 5880 Section 4.1)
---------------------------------------------------------------------------

||| BFD session state machine states.
public export
data State : Type where
  AdminDown : State
  Down      : State
  Init      : State
  Up        : State

public export
Show State where
  show AdminDown = "AdminDown"
  show Down      = "Down"
  show Init      = "Init"
  show Up        = "Up"

---------------------------------------------------------------------------
-- Diagnostic Code (RFC 5880 Section 4.1)
---------------------------------------------------------------------------

||| BFD diagnostic codes indicating reason for state change.
public export
data Diagnostic : Type where
  NoDiagnostic                    : Diagnostic
  ControlDetectionTimeExpired     : Diagnostic
  EchoFunctionFailed              : Diagnostic
  NeighborSignaledSessionDown     : Diagnostic
  ForwardingPlaneReset            : Diagnostic
  PathDown                        : Diagnostic
  ConcatenatedPathDown            : Diagnostic
  AdministrativelyDown            : Diagnostic
  ReverseConcatenatedPathDown     : Diagnostic

public export
Show Diagnostic where
  show NoDiagnostic                    = "No Diagnostic"
  show ControlDetectionTimeExpired     = "Control Detection Time Expired"
  show EchoFunctionFailed              = "Echo Function Failed"
  show NeighborSignaledSessionDown     = "Neighbor Signaled Session Down"
  show ForwardingPlaneReset            = "Forwarding Plane Reset"
  show PathDown                        = "Path Down"
  show ConcatenatedPathDown            = "Concatenated Path Down"
  show AdministrativelyDown            = "Administratively Down"
  show ReverseConcatenatedPathDown     = "Reverse Concatenated Path Down"

---------------------------------------------------------------------------
-- Session Mode
---------------------------------------------------------------------------

||| BFD session operating modes.
public export
data SessionMode : Type where
  AsyncMode  : SessionMode
  DemandMode : SessionMode

public export
Show SessionMode where
  show AsyncMode  = "Asynchronous"
  show DemandMode = "Demand"
