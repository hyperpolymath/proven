-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
module Proven.SafeJWT.Containment

import Proven.SafeJWT

%default total

||| No header, claims, key or output can satisfy successful token creation.
public export
creationNeverSucceeds : (header : JWTHeader) -> (claims : JWTClaims) ->
                        (key : SigningKey) -> (token : String) ->
                        createToken header claims key = Ok token -> Void
creationNeverSucceeds header claims key token Refl impossible

public export
hs256CreationUnavailable : (secret : String) -> (claims : JWTClaims) ->
                           createHS256Token secret claims = Err (CryptoUnavailable HS256)
hs256CreationUnavailable secret claims = Refl

public export
signatureNeverVerified : (key : SigningKey) -> (jwt : DecodedJWT) ->
                         verifySignature key jwt = Ok () -> Void
signatureNeverVerified key jwt Refl impossible

||| Claims checks cannot bypass the unavailable authenticator.
public export
validationNeverSucceeds : (opts : ValidationOptions) -> (key : SigningKey) ->
                          (now : Integer) -> (jwt : DecodedJWT) ->
                          (validated : ValidatedJWT) ->
                          validate opts key now jwt = Ok validated -> Void
validationNeverSucceeds opts key now jwt validated accepted with (validateDecoded opts now jwt)
  validationNeverSucceeds opts key now jwt validated Refl | Err err impossible
  validationNeverSucceeds opts key now jwt validated Refl | Ok () impossible

public export
hmacUnavailable : (alg : JWTAlgorithm) -> (secret : List Bits8) ->
                   (input : String) -> (signature : List Bits8) ->
                   verifyHMAC alg secret input signature = False
hmacUnavailable alg secret input signature = Refl

public export
rsaUnavailable : (alg : JWTAlgorithm) -> (key : SigningKey) ->
                  (input : String) -> (signature : List Bits8) ->
                  verifyRSA alg key input signature = False
rsaUnavailable alg key input signature = Refl

public export
ecdsaUnavailable : (alg : JWTAlgorithm) -> (key : SigningKey) ->
                    (input : String) -> (signature : List Bits8) ->
                    verifyECDSA alg key input signature = False
ecdsaUnavailable alg key input signature = Refl

public export
eddsaUnavailable : (key : SigningKey) -> (input : String) ->
                    (signature : List Bits8) ->
                    verifyEdDSA key input signature = False
eddsaUnavailable key input signature = Refl
