{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- SPDX-FileCopyrightText: 2025 Hyperpolymath -}

import Test.Hspec
import Proven

main :: IO ()
main = hspec $ do
  describe "SafeMath" $ do
    it "safeDiv divides correctly" $
      safeDiv 10 2 `shouldBe` Just 5

    it "safeDiv returns Nothing on division by zero" $
      safeDiv 10 0 `shouldBe` Nothing

    it "safeMod computes modulo correctly" $
      safeMod 10 3 `shouldBe` Just 1

    it "safeMod returns Nothing on mod by zero" $
      safeMod 10 0 `shouldBe` Nothing

    it "safeAdd adds correctly" $
      safeAdd 1 2 `shouldBe` Just 3

    it "safeSub subtracts correctly" $
      safeSub 5 3 `shouldBe` Just 2

    it "safeMul multiplies correctly" $
      safeMul 3 4 `shouldBe` Just 12

  describe "SafeString" $ do
    it "escapeHtml escapes HTML entities" $ do
      escapeHtml "<script>" `shouldBe` "&lt;script&gt;"
      escapeHtml "a & b" `shouldBe` "a &amp; b"

    it "escapeSql escapes single quotes" $
      escapeSql "it's" `shouldBe` "it''s"

    it "truncateSafe truncates with suffix" $ do
      truncateSafe "hello world" 5 "..." `shouldBe` "he..."
      truncateSafe "hi" 10 "..." `shouldBe` "hi"

  describe "SafePath" $ do
    it "hasTraversal detects traversal sequences" $ do
      hasTraversal "../etc/passwd" `shouldBe` True
      hasTraversal "~/file" `shouldBe` True
      hasTraversal "normal/path" `shouldBe` False

    it "isSafe validates safe paths" $ do
      isSafe "safe/path" `shouldBe` True
      isSafe "../unsafe" `shouldBe` False

  describe "SafeEmail" $ do
    it "isValid validates email addresses" $ do
      isValid "user@example.com" `shouldBe` True
      isValid "not-an-email" `shouldBe` False
      isValid "@invalid.com" `shouldBe` False

    it "normalize lowercases domain" $
      normalize "User@EXAMPLE.COM" `shouldBe` Just "User@example.com"

  describe "SafeNetwork" $ do
    it "isValidIPv4 validates IPv4 addresses" $ do
      isValidIPv4 "192.168.1.1" `shouldBe` True
      isValidIPv4 "invalid" `shouldBe` False
      isValidIPv4 "256.1.1.1" `shouldBe` False

    it "isPrivate detects private addresses" $ do
      isPrivate "192.168.1.1" `shouldBe` True
      isPrivate "10.0.0.1" `shouldBe` True
      isPrivate "8.8.8.8" `shouldBe` False

    it "isLoopback detects loopback addresses" $ do
      isLoopback "127.0.0.1" `shouldBe` True
      isLoopback "192.168.1.1" `shouldBe` False

  describe "SafeCrypto" $ do
    it "constantTimeCompare compares strings correctly" $ do
      constantTimeCompare "secret" "secret" `shouldBe` True
      constantTimeCompare "secret" "other!" `shouldBe` False
      constantTimeCompare "" "" `shouldBe` True

    it "secureZero creates zeroed string" $ do
      length (secureZero 4) `shouldBe` 4
      secureZero 4 `shouldBe` "\0\0\0\0"
