-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- proven-dns: Main entry point
--
-- A DNS resolver implementation that cannot crash on malformed queries.
-- Uses proven's type-safe approach to ensure all name parsing, query
-- construction, and response generation is total.
--
-- Usage:
--   idris2 --build proven-dns.ipkg
--   ./build/exec/proven-dns

module Main

import DNS
import DNS.Name
import DNS.RecordType
import DNS.Query
import DNS.Response
import DNS.Zone

%default total

-- ============================================================================
-- Demo: domain name parsing
-- ============================================================================

||| Demonstrate domain name parsing with valid and invalid inputs.
covering
demoNameParsing : IO ()
demoNameParsing = do
  putStrLn "\n--- DNS Domain Name Parsing Demo ---\n"

  -- Valid names
  let validNames = [ "www.example.com"
                   , "mail.example.org"
                   , "sub.domain.example.co.uk"
                   , "single"
                   ]
  traverse_ (\s => case parseName s of
    Right name => putStrLn $ "  OK: " ++ s ++ " -> " ++ show name
                             ++ " (" ++ show (labelCount name) ++ " labels)"
    Left err   => putStrLn $ "  ERR: " ++ s ++ " -> " ++ show err
  ) validNames

  putStrLn ""

  -- Invalid names
  let tooLong = pack (replicate 64 'a')
  case parseName tooLong of
    Left err => putStrLn $ "  Rejected (label too long): " ++ show err
    Right _  => putStrLn $ "  Unexpected success for 64-char label"

  case parseName "" of
    Left err => putStrLn $ "  Rejected (empty): " ++ show err
    Right _  => putStrLn $ "  Unexpected success for empty name"

-- ============================================================================
-- Demo: query construction
-- ============================================================================

||| Demonstrate building various DNS queries.
covering
demoQueryConstruction : IO ()
demoQueryConstruction = do
  putStrLn "\n--- DNS Query Construction Demo ---\n"

  -- Simple A record query
  case simpleQuery 1234 "www.example.com" of
    Left err => putStrLn $ "  Query failed: " ++ show err
    Right q  => do
      putStrLn $ "  A query: " ++ show q
      traverse_ (\qn => putStrLn $ "    Question: " ++ show qn) q.questions

  -- AAAA record query
  case typedQuery 1235 "ipv6.example.com" AAAA of
    Left err => putStrLn $ "  Query failed: " ++ show err
    Right q  => putStrLn $ "  AAAA query: " ++ show q

  -- MX record query
  case typedQuery 1236 "example.com" MX of
    Left err => putStrLn $ "  Query failed: " ++ show err
    Right q  => putStrLn $ "  MX query: " ++ show q

  -- Reverse DNS query
  case reverseQuery 1237 "192.168.1.1" of
    Left err => putStrLn $ "  Reverse query failed: " ++ show err
    Right q  => do
      putStrLn $ "  PTR query: " ++ show q
      traverse_ (\qn => putStrLn $ "    Reverse: " ++ show qn) q.questions

-- ============================================================================
-- Demo: response construction
-- ============================================================================

||| Demonstrate building DNS responses with resource records.
covering
demoResponse : IO ()
demoResponse = do
  putStrLn "\n--- DNS Response Construction Demo ---\n"

  let exampleName = unsafeMkName ["www", "example", "com"]
  let exampleOrg  = unsafeMkName ["example", "com"]

  -- Build an A record answer
  let aRecord = MkRR
        { rrName  = exampleName
        , rrType  = A
        , rrClass = IN
        , rrTTL   = 300
        , rrData  = RDataA 93 184 216 34
        }

  -- Build a successful response
  let question = MkQuestion exampleName A IN
  let resp = successResponse 1234 [question] [aRecord]

  putStrLn $ "  Response: " ++ show resp
  putStrLn $ "  Answer:   " ++ show aRecord
  putStrLn $ "  Success:  " ++ show (isSuccessful resp)
  putStrLn $ "  Records:  " ++ show (totalRecords resp)

  -- Build an NXDOMAIN response
  let badName  = unsafeMkName ["doesnotexist", "example", "com"]
  let badQ     = MkQuestion badName A IN
  let nxResp   = nxdomainResponse 1235 [badQ]

  putStrLn $ "\n  NXDOMAIN: " ++ show nxResp
  putStrLn $ "  Success:  " ++ show (isSuccessful nxResp)

-- ============================================================================
-- Demo: zone management
-- ============================================================================

||| Demonstrate zone creation and validation.
covering
demoZone : IO ()
demoZone = do
  putStrLn "\n--- DNS Zone Management Demo ---\n"

  let origin = unsafeMkName ["example", "com"]
  let ns1    = unsafeMkName ["ns1", "example", "com"]
  let ns2    = unsafeMkName ["ns2", "example", "com"]
  let mname  = ns1
  let rname  = unsafeMkName ["admin", "example", "com"]

  -- Build a zone
  let zone0 = mkZone origin mname rname 2026022801 [ns1, ns2]

  -- Add some records
  let www  = unsafeMkName ["www", "example", "com"]
  let mail = unsafeMkName ["mail", "example", "com"]

  let aRec = MkRR www A IN 300 (RDataA 93 184 216 34)
  let mxRec = MkRR origin MX IN 3600 (RDataMX 10 mail)
  let mailA = MkRR mail A IN 300 (RDataA 93 184 216 35)

  let zone = addRecord mailA (addRecord mxRec (addRecord aRec zone0))

  putStrLn $ "  Zone: " ++ show zone
  putStrLn $ "  Total records: " ++ show (totalZoneRecords zone)

  -- Validate the zone
  let errors = validateZone zone
  case errors of
    [] => putStrLn "  Validation: PASSED (no errors)"
    _  => traverse_ (\e => putStrLn $ "  Validation error: " ++ show e) errors

  -- Look up records
  let results = lookupInZone www A zone
  putStrLn $ "\n  Lookup www.example.com A: " ++ show (length results) ++ " records"
  traverse_ (\rr => putStrLn $ "    " ++ show rr) results

  let mxResults = lookupInZone origin MX zone
  putStrLn $ "  Lookup example.com MX: " ++ show (length mxResults) ++ " records"
  traverse_ (\rr => putStrLn $ "    " ++ show rr) mxResults

-- ============================================================================
-- Main
-- ============================================================================

covering
main : IO ()
main = do
  putStrLn "proven-dns v0.1.0 -- DNS that cannot crash"
  putStrLn "Powered by proven (Idris 2 formal verification)"
  putStrLn ""
  putStrLn $ "DNS port: " ++ show (cast {to=Nat} dnsPort)
  putStrLn $ "Max UDP size: " ++ show maxUdpSize ++ " bytes"
  putStrLn $ "Max label length: " ++ show maxLabelLength ++ " chars"
  putStrLn $ "Max name length: " ++ show maxNameLength ++ " chars"

  demoNameParsing
  demoQueryConstruction
  demoResponse
  demoZone

  putStrLn "\n--- All parsing proven safe at compile time ---"
  putStrLn "Build with: idris2 --build proven-dns.ipkg"
  putStrLn "Run with:   ./build/exec/proven-dns"
