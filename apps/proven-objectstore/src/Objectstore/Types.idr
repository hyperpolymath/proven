-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Core protocol types for the proven-objectstore S3-compatible object storage server.
||| Defines closed sum types for API operations, storage classes, access control
||| lists, and error codes.
module Objectstore.Types

%default total

---------------------------------------------------------------------------
-- Operation: S3-compatible API operations supported by the object store.
---------------------------------------------------------------------------

||| Enumerates the S3-compatible API operations that proven-objectstore
||| handles. Covers object CRUD, bucket management, and multipart uploads.
public export
data Operation
  = PutObject               -- ^ Upload an object to a bucket
  | GetObject               -- ^ Retrieve an object from a bucket
  | DeleteObject            -- ^ Delete an object from a bucket
  | ListObjects             -- ^ List objects within a bucket (with prefix/delimiter)
  | HeadObject              -- ^ Retrieve object metadata without the body
  | CopyObject              -- ^ Server-side copy of an object between keys/buckets
  | CreateBucket            -- ^ Create a new storage bucket
  | DeleteBucket            -- ^ Delete an empty bucket
  | ListBuckets             -- ^ List all buckets owned by the authenticated user
  | InitMultipartUpload     -- ^ Initiate a multipart upload session
  | UploadPart              -- ^ Upload a single part in a multipart upload
  | CompleteMultipartUpload -- ^ Finalise a multipart upload by assembling parts

||| Display a human-readable label for each operation.
public export
Show Operation where
  show PutObject               = "PutObject"
  show GetObject               = "GetObject"
  show DeleteObject            = "DeleteObject"
  show ListObjects             = "ListObjects"
  show HeadObject              = "HeadObject"
  show CopyObject              = "CopyObject"
  show CreateBucket            = "CreateBucket"
  show DeleteBucket            = "DeleteBucket"
  show ListBuckets             = "ListBuckets"
  show InitMultipartUpload     = "InitMultipartUpload"
  show UploadPart              = "UploadPart"
  show CompleteMultipartUpload = "CompleteMultipartUpload"

---------------------------------------------------------------------------
-- StorageClass: Durability and retrieval trade-off tiers.
---------------------------------------------------------------------------

||| Determines the storage tier for an object, trading off retrieval
||| latency and cost against storage cost and durability guarantees.
public export
data StorageClass
  = Standard         -- ^ High durability, low latency, highest storage cost
  | InfrequentAccess -- ^ Lower storage cost, retrieval fee applies
  | Glacier          -- ^ Archive tier, minutes-to-hours retrieval
  | DeepArchive      -- ^ Lowest cost, hours-to-days retrieval
  | OneZone          -- ^ Single availability zone, lower durability guarantee

||| Display a human-readable label for each storage class.
public export
Show StorageClass where
  show Standard         = "Standard"
  show InfrequentAccess = "InfrequentAccess"
  show Glacier          = "Glacier"
  show DeepArchive      = "DeepArchive"
  show OneZone          = "OneZone"

---------------------------------------------------------------------------
-- ACL: Canned access control list presets.
---------------------------------------------------------------------------

||| Predefined access control list (ACL) presets that can be applied to
||| buckets and objects for coarse-grained permission control.
public export
data ACL
  = Private           -- ^ Owner has full control; no public access
  | PublicRead        -- ^ Owner has full control; anyone can read
  | PublicReadWrite   -- ^ Anyone can read and write (use with extreme caution)
  | AuthenticatedRead -- ^ Any authenticated user can read

||| Display a human-readable label for each ACL preset.
public export
Show ACL where
  show Private           = "Private"
  show PublicRead        = "PublicRead"
  show PublicReadWrite   = "PublicReadWrite"
  show AuthenticatedRead = "AuthenticatedRead"

---------------------------------------------------------------------------
-- ErrorCode: S3-compatible error responses.
---------------------------------------------------------------------------

||| Error codes returned by the object store API, modelled after the
||| S3 error code vocabulary for client compatibility.
public export
data ErrorCode
  = NoSuchBucket       -- ^ The specified bucket does not exist
  | NoSuchKey          -- ^ The specified object key does not exist
  | BucketAlreadyExists -- ^ A bucket with this name already exists
  | BucketNotEmpty     -- ^ Cannot delete a bucket that contains objects
  | AccessDenied       -- ^ Caller lacks permission for the requested operation
  | EntityTooLarge     -- ^ Object or part exceeds the maximum allowed size
  | InvalidPart        -- ^ One or more parts in a multipart completion are invalid
  | IncompleteBody     -- ^ Request body was shorter than Content-Length

||| Display a human-readable label for each error code.
public export
Show ErrorCode where
  show NoSuchBucket       = "NoSuchBucket"
  show NoSuchKey          = "NoSuchKey"
  show BucketAlreadyExists = "BucketAlreadyExists"
  show BucketNotEmpty     = "BucketNotEmpty"
  show AccessDenied       = "AccessDenied"
  show EntityTooLarge     = "EntityTooLarge"
  show InvalidPart        = "InvalidPart"
  show IncompleteBody     = "IncompleteBody"
