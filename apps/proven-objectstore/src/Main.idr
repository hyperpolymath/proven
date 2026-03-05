-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Entry point for the proven-objectstore S3-compatible object storage server.
||| Prints the server identity, port, and enumerates all type constructors
||| to verify the type definitions are correctly linked.
module Main

import Objectstore

%default total

||| Print all constructors of a type as a comma-separated list.
printConstructors : String -> List String -> IO ()
printConstructors label vals = putStrLn $ "  " ++ label ++ ": " ++ showList vals
  where
    showList : List String -> String
    showList []        = "(none)"
    showList [x]       = x
    showList (x :: xs) = x ++ ", " ++ showList xs

||| All Operation constructors.
allOperations : List Operation
allOperations = [ PutObject, GetObject, DeleteObject, ListObjects, HeadObject
                , CopyObject, CreateBucket, DeleteBucket, ListBuckets
                , InitMultipartUpload, UploadPart, CompleteMultipartUpload ]

||| All StorageClass constructors.
allStorageClasses : List StorageClass
allStorageClasses = [Standard, InfrequentAccess, Glacier, DeepArchive, OneZone]

||| All ACL constructors.
allACLs : List ACL
allACLs = [Private, PublicRead, PublicReadWrite, AuthenticatedRead]

||| All ErrorCode constructors.
allErrorCodes : List ErrorCode
allErrorCodes = [ NoSuchBucket, NoSuchKey, BucketAlreadyExists, BucketNotEmpty
                , AccessDenied, EntityTooLarge, InvalidPart, IncompleteBody ]

||| Entry point. Prints server name, default port, and all type constructors.
main : IO ()
main = do
  putStrLn "proven-objectstore — S3-Compatible Object Storage"
  putStrLn $ "Default port: " ++ show objectstorePort
  putStrLn "Types:"
  printConstructors "Operation" (map show allOperations)
  printConstructors "StorageClass" (map show allStorageClasses)
  printConstructors "ACL" (map show allACLs)
  printConstructors "ErrorCode" (map show allErrorCodes)
