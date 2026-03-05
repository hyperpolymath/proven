-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Core protocol types for RFC 4120 Kerberos V5.
-- | Defines message types, encryption types, error codes, and ticket
-- | flags as closed sum types with Show instances.

module Kerberos.Types

%default total

||| Kerberos message types per RFC 4120 Section 5.4.1.
public export
data MessageType : Type where
  AS_REQ    : MessageType
  AS_REP    : MessageType
  TGS_REQ   : MessageType
  TGS_REP   : MessageType
  AP_REQ    : MessageType
  AP_REP    : MessageType
  KRB_ERROR : MessageType
  KRB_SAFE  : MessageType
  KRB_PRIV  : MessageType
  KRB_CRED  : MessageType

public export
Show MessageType where
  show AS_REQ    = "AS_REQ"
  show AS_REP    = "AS_REP"
  show TGS_REQ   = "TGS_REQ"
  show TGS_REP   = "TGS_REP"
  show AP_REQ    = "AP_REQ"
  show AP_REP    = "AP_REP"
  show KRB_ERROR = "KRB_ERROR"
  show KRB_SAFE  = "KRB_SAFE"
  show KRB_PRIV  = "KRB_PRIV"
  show KRB_CRED  = "KRB_CRED"

||| Kerberos encryption types per RFC 3962 / RFC 8009.
public export
data EncryptionType : Type where
  AES256_CTS_HMAC_SHA1   : EncryptionType
  AES128_CTS_HMAC_SHA1   : EncryptionType
  AES256_CTS_HMAC_SHA384 : EncryptionType
  RC4_HMAC               : EncryptionType
  DES3_CBC_SHA1          : EncryptionType

public export
Show EncryptionType where
  show AES256_CTS_HMAC_SHA1   = "AES256_CTS_HMAC_SHA1"
  show AES128_CTS_HMAC_SHA1   = "AES128_CTS_HMAC_SHA1"
  show AES256_CTS_HMAC_SHA384 = "AES256_CTS_HMAC_SHA384"
  show RC4_HMAC               = "RC4_HMAC"
  show DES3_CBC_SHA1          = "DES3_CBC_SHA1"

||| Kerberos error codes per RFC 4120 Section 7.5.9.
public export
data ErrorCode : Type where
  KDC_ERR_NONE                : ErrorCode
  KDC_ERR_NAME_EXP            : ErrorCode
  KDC_ERR_SERVICE_EXP         : ErrorCode
  KDC_ERR_BAD_PVNO            : ErrorCode
  KDC_ERR_C_OLD_MAST_KVNO     : ErrorCode
  KDC_ERR_S_OLD_MAST_KVNO     : ErrorCode
  KDC_ERR_C_PRINCIPAL_UNKNOWN : ErrorCode
  KDC_ERR_S_PRINCIPAL_UNKNOWN : ErrorCode
  KDC_ERR_PREAUTH_FAILED      : ErrorCode
  KDC_ERR_PREAUTH_REQUIRED    : ErrorCode

public export
Show ErrorCode where
  show KDC_ERR_NONE                = "KDC_ERR_NONE"
  show KDC_ERR_NAME_EXP            = "KDC_ERR_NAME_EXP"
  show KDC_ERR_SERVICE_EXP         = "KDC_ERR_SERVICE_EXP"
  show KDC_ERR_BAD_PVNO            = "KDC_ERR_BAD_PVNO"
  show KDC_ERR_C_OLD_MAST_KVNO     = "KDC_ERR_C_OLD_MAST_KVNO"
  show KDC_ERR_S_OLD_MAST_KVNO     = "KDC_ERR_S_OLD_MAST_KVNO"
  show KDC_ERR_C_PRINCIPAL_UNKNOWN = "KDC_ERR_C_PRINCIPAL_UNKNOWN"
  show KDC_ERR_S_PRINCIPAL_UNKNOWN = "KDC_ERR_S_PRINCIPAL_UNKNOWN"
  show KDC_ERR_PREAUTH_FAILED      = "KDC_ERR_PREAUTH_FAILED"
  show KDC_ERR_PREAUTH_REQUIRED    = "KDC_ERR_PREAUTH_REQUIRED"

||| Kerberos ticket flags per RFC 4120 Section 5.3.
public export
data TicketFlag : Type where
  Forwardable  : TicketFlag
  Forwarded    : TicketFlag
  Proxiable    : TicketFlag
  Proxy        : TicketFlag
  Renewable    : TicketFlag
  PreAuthent   : TicketFlag
  HWAuthent    : TicketFlag

public export
Show TicketFlag where
  show Forwardable = "Forwardable"
  show Forwarded   = "Forwarded"
  show Proxiable   = "Proxiable"
  show Proxy       = "Proxy"
  show Renewable   = "Renewable"
  show PreAuthent  = "PreAuthent"
  show HWAuthent   = "HWAuthent"
