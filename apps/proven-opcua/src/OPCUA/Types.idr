-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Core protocol types for OPC UA (Industrial IoT).
||| All types are closed sum types with Show instances.
module OPCUA.Types

%default total

---------------------------------------------------------------------------
-- Service Type
---------------------------------------------------------------------------

||| OPC UA service request types.
public export
data ServiceType : Type where
  Read               : ServiceType
  Write              : ServiceType
  Browse             : ServiceType
  Subscribe          : ServiceType
  Publish            : ServiceType
  Call               : ServiceType
  CreateSession      : ServiceType
  ActivateSession    : ServiceType
  CloseSession       : ServiceType
  CreateSubscription : ServiceType
  DeleteSubscription : ServiceType

public export
Show ServiceType where
  show Read               = "Read"
  show Write              = "Write"
  show Browse             = "Browse"
  show Subscribe          = "Subscribe"
  show Publish            = "Publish"
  show Call               = "Call"
  show CreateSession      = "CreateSession"
  show ActivateSession    = "ActivateSession"
  show CloseSession       = "CloseSession"
  show CreateSubscription = "CreateSubscription"
  show DeleteSubscription = "DeleteSubscription"

---------------------------------------------------------------------------
-- Node Class
---------------------------------------------------------------------------

||| OPC UA address space node classes.
public export
data NodeClass : Type where
  Object        : NodeClass
  Variable      : NodeClass
  Method        : NodeClass
  ObjectType    : NodeClass
  VariableType  : NodeClass
  ReferenceType : NodeClass
  DataType      : NodeClass
  View          : NodeClass

public export
Show NodeClass where
  show Object        = "Object"
  show Variable      = "Variable"
  show Method        = "Method"
  show ObjectType    = "ObjectType"
  show VariableType  = "VariableType"
  show ReferenceType = "ReferenceType"
  show DataType      = "DataType"
  show View          = "View"

---------------------------------------------------------------------------
-- Status Code
---------------------------------------------------------------------------

||| OPC UA status codes for operation results.
public export
data StatusCode : Type where
  Good                       : StatusCode
  Uncertain                  : StatusCode
  Bad                        : StatusCode
  BadNodeIdUnknown           : StatusCode
  BadAttributeIdInvalid      : StatusCode
  BadNotReadable             : StatusCode
  BadNotWritable             : StatusCode
  BadOutOfRange              : StatusCode
  BadTypeMismatch            : StatusCode
  BadSessionIdInvalid        : StatusCode
  BadSubscriptionIdInvalid   : StatusCode
  BadTimeout                 : StatusCode

public export
Show StatusCode where
  show Good                       = "Good"
  show Uncertain                  = "Uncertain"
  show Bad                        = "Bad"
  show BadNodeIdUnknown           = "Bad_NodeIdUnknown"
  show BadAttributeIdInvalid      = "Bad_AttributeIdInvalid"
  show BadNotReadable             = "Bad_NotReadable"
  show BadNotWritable             = "Bad_NotWritable"
  show BadOutOfRange              = "Bad_OutOfRange"
  show BadTypeMismatch            = "Bad_TypeMismatch"
  show BadSessionIdInvalid        = "Bad_SessionIdInvalid"
  show BadSubscriptionIdInvalid   = "Bad_SubscriptionIdInvalid"
  show BadTimeout                 = "Bad_Timeout"

---------------------------------------------------------------------------
-- Security Mode
---------------------------------------------------------------------------

||| OPC UA message security modes.
public export
data SecurityMode : Type where
  None           : SecurityMode
  Sign           : SecurityMode
  SignAndEncrypt  : SecurityMode

public export
Show SecurityMode where
  show None          = "None"
  show Sign          = "Sign"
  show SignAndEncrypt = "SignAndEncrypt"
