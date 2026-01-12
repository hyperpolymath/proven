-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafePhone - Phone number validation and formatting
|||
||| This module provides:
||| - Phone number parsing and validation
||| - E.164 format support
||| - Country code detection
||| - Phone number formatting
||| - Basic phone number type classification
|||
||| Follows ITU-T E.164 recommendation for international numbers
module Proven.SafePhone

import public Proven.Core
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Phone Number Types
--------------------------------------------------------------------------------

||| Phone number type classification
public export
data PhoneNumberType
  = Mobile
  | FixedLine
  | TollFree
  | PremiumRate
  | SharedCost
  | VoIP
  | PersonalNumber
  | Pager
  | UAN  -- Universal Access Number
  | Unknown

public export
Show PhoneNumberType where
  show Mobile = "Mobile"
  show FixedLine = "Fixed Line"
  show TollFree = "Toll Free"
  show PremiumRate = "Premium Rate"
  show SharedCost = "Shared Cost"
  show VoIP = "VoIP"
  show PersonalNumber = "Personal Number"
  show Pager = "Pager"
  show UAN = "UAN"
  show Unknown = "Unknown"

public export
Eq PhoneNumberType where
  Mobile == Mobile = True
  FixedLine == FixedLine = True
  TollFree == TollFree = True
  PremiumRate == PremiumRate = True
  SharedCost == SharedCost = True
  VoIP == VoIP = True
  PersonalNumber == PersonalNumber = True
  Pager == Pager = True
  UAN == UAN = True
  Unknown == Unknown = True
  _ == _ = False

--------------------------------------------------------------------------------
-- Country Codes
--------------------------------------------------------------------------------

||| ITU country calling codes (common ones)
public export
data CountryCallingCode
  = CC1     -- USA, Canada, Caribbean
  | CC7     -- Russia, Kazakhstan
  | CC20    -- Egypt
  | CC27    -- South Africa
  | CC30    -- Greece
  | CC31    -- Netherlands
  | CC32    -- Belgium
  | CC33    -- France
  | CC34    -- Spain
  | CC36    -- Hungary
  | CC39    -- Italy
  | CC40    -- Romania
  | CC41    -- Switzerland
  | CC43    -- Austria
  | CC44    -- UK
  | CC45    -- Denmark
  | CC46    -- Sweden
  | CC47    -- Norway
  | CC48    -- Poland
  | CC49    -- Germany
  | CC51    -- Peru
  | CC52    -- Mexico
  | CC53    -- Cuba
  | CC54    -- Argentina
  | CC55    -- Brazil
  | CC56    -- Chile
  | CC57    -- Colombia
  | CC58    -- Venezuela
  | CC60    -- Malaysia
  | CC61    -- Australia
  | CC62    -- Indonesia
  | CC63    -- Philippines
  | CC64    -- New Zealand
  | CC65    -- Singapore
  | CC66    -- Thailand
  | CC81    -- Japan
  | CC82    -- South Korea
  | CC84    -- Vietnam
  | CC86    -- China
  | CC90    -- Turkey
  | CC91    -- India
  | CC92    -- Pakistan
  | CC93    -- Afghanistan
  | CC94    -- Sri Lanka
  | CC95    -- Myanmar
  | CC98    -- Iran
  | CC212   -- Morocco
  | CC213   -- Algeria
  | CC216   -- Tunisia
  | CC218   -- Libya
  | CC220   -- Gambia
  | CC221   -- Senegal
  | CC234   -- Nigeria
  | CC254   -- Kenya
  | CC255   -- Tanzania
  | CC256   -- Uganda
  | CC260   -- Zambia
  | CC263   -- Zimbabwe
  | CC351   -- Portugal
  | CC352   -- Luxembourg
  | CC353   -- Ireland
  | CC354   -- Iceland
  | CC358   -- Finland
  | CC380   -- Ukraine
  | CC381   -- Serbia
  | CC385   -- Croatia
  | CC386   -- Slovenia
  | CC420   -- Czech Republic
  | CC421   -- Slovakia
  | CC852   -- Hong Kong
  | CC853   -- Macau
  | CC855   -- Cambodia
  | CC856   -- Laos
  | CC880   -- Bangladesh
  | CC886   -- Taiwan
  | CC960   -- Maldives
  | CC961   -- Lebanon
  | CC962   -- Jordan
  | CC963   -- Syria
  | CC964   -- Iraq
  | CC965   -- Kuwait
  | CC966   -- Saudi Arabia
  | CC967   -- Yemen
  | CC968   -- Oman
  | CC971   -- UAE
  | CC972   -- Israel
  | CC973   -- Bahrain
  | CC974   -- Qatar
  | CC975   -- Bhutan
  | CC976   -- Mongolia
  | CC977   -- Nepal
  | CC992   -- Tajikistan
  | CC993   -- Turkmenistan
  | CC994   -- Azerbaijan
  | CC995   -- Georgia
  | CC996   -- Kyrgyzstan
  | CC998   -- Uzbekistan
  | CCUnknown

public export
Show CountryCallingCode where
  show CC1 = "+1"
  show CC7 = "+7"
  show CC20 = "+20"
  show CC27 = "+27"
  show CC30 = "+30"
  show CC31 = "+31"
  show CC32 = "+32"
  show CC33 = "+33"
  show CC34 = "+34"
  show CC36 = "+36"
  show CC39 = "+39"
  show CC40 = "+40"
  show CC41 = "+41"
  show CC43 = "+43"
  show CC44 = "+44"
  show CC45 = "+45"
  show CC46 = "+46"
  show CC47 = "+47"
  show CC48 = "+48"
  show CC49 = "+49"
  show CC51 = "+51"
  show CC52 = "+52"
  show CC53 = "+53"
  show CC54 = "+54"
  show CC55 = "+55"
  show CC56 = "+56"
  show CC57 = "+57"
  show CC58 = "+58"
  show CC60 = "+60"
  show CC61 = "+61"
  show CC62 = "+62"
  show CC63 = "+63"
  show CC64 = "+64"
  show CC65 = "+65"
  show CC66 = "+66"
  show CC81 = "+81"
  show CC82 = "+82"
  show CC84 = "+84"
  show CC86 = "+86"
  show CC90 = "+90"
  show CC91 = "+91"
  show CC92 = "+92"
  show CC93 = "+93"
  show CC94 = "+94"
  show CC95 = "+95"
  show CC98 = "+98"
  show CC212 = "+212"
  show CC213 = "+213"
  show CC216 = "+216"
  show CC218 = "+218"
  show CC220 = "+220"
  show CC221 = "+221"
  show CC234 = "+234"
  show CC254 = "+254"
  show CC255 = "+255"
  show CC256 = "+256"
  show CC260 = "+260"
  show CC263 = "+263"
  show CC351 = "+351"
  show CC352 = "+352"
  show CC353 = "+353"
  show CC354 = "+354"
  show CC358 = "+358"
  show CC380 = "+380"
  show CC381 = "+381"
  show CC385 = "+385"
  show CC386 = "+386"
  show CC420 = "+420"
  show CC421 = "+421"
  show CC852 = "+852"
  show CC853 = "+853"
  show CC855 = "+855"
  show CC856 = "+856"
  show CC880 = "+880"
  show CC886 = "+886"
  show CC960 = "+960"
  show CC961 = "+961"
  show CC962 = "+962"
  show CC963 = "+963"
  show CC964 = "+964"
  show CC965 = "+965"
  show CC966 = "+966"
  show CC967 = "+967"
  show CC968 = "+968"
  show CC971 = "+971"
  show CC972 = "+972"
  show CC973 = "+973"
  show CC974 = "+974"
  show CC975 = "+975"
  show CC976 = "+976"
  show CC977 = "+977"
  show CC992 = "+992"
  show CC993 = "+993"
  show CC994 = "+994"
  show CC995 = "+995"
  show CC996 = "+996"
  show CC998 = "+998"
  show CCUnknown = "+?"

||| Get country calling code value
public export
ccValue : CountryCallingCode -> Nat
ccValue CC1 = 1
ccValue CC7 = 7
ccValue CC20 = 20
ccValue CC27 = 27
ccValue CC30 = 30
ccValue CC31 = 31
ccValue CC32 = 32
ccValue CC33 = 33
ccValue CC34 = 34
ccValue CC36 = 36
ccValue CC39 = 39
ccValue CC40 = 40
ccValue CC41 = 41
ccValue CC43 = 43
ccValue CC44 = 44
ccValue CC45 = 45
ccValue CC46 = 46
ccValue CC47 = 47
ccValue CC48 = 48
ccValue CC49 = 49
ccValue CC51 = 51
ccValue CC52 = 52
ccValue CC53 = 53
ccValue CC54 = 54
ccValue CC55 = 55
ccValue CC56 = 56
ccValue CC57 = 57
ccValue CC58 = 58
ccValue CC60 = 60
ccValue CC61 = 61
ccValue CC62 = 62
ccValue CC63 = 63
ccValue CC64 = 64
ccValue CC65 = 65
ccValue CC66 = 66
ccValue CC81 = 81
ccValue CC82 = 82
ccValue CC84 = 84
ccValue CC86 = 86
ccValue CC90 = 90
ccValue CC91 = 91
ccValue CC92 = 92
ccValue CC93 = 93
ccValue CC94 = 94
ccValue CC95 = 95
ccValue CC98 = 98
ccValue CC212 = 212
ccValue CC213 = 213
ccValue CC216 = 216
ccValue CC218 = 218
ccValue CC220 = 220
ccValue CC221 = 221
ccValue CC234 = 234
ccValue CC254 = 254
ccValue CC255 = 255
ccValue CC256 = 256
ccValue CC260 = 260
ccValue CC263 = 263
ccValue CC351 = 351
ccValue CC352 = 352
ccValue CC353 = 353
ccValue CC354 = 354
ccValue CC358 = 358
ccValue CC380 = 380
ccValue CC381 = 381
ccValue CC385 = 385
ccValue CC386 = 386
ccValue CC420 = 420
ccValue CC421 = 421
ccValue CC852 = 852
ccValue CC853 = 853
ccValue CC855 = 855
ccValue CC856 = 856
ccValue CC880 = 880
ccValue CC886 = 886
ccValue CC960 = 960
ccValue CC961 = 961
ccValue CC962 = 962
ccValue CC963 = 963
ccValue CC964 = 964
ccValue CC965 = 965
ccValue CC966 = 966
ccValue CC967 = 967
ccValue CC968 = 968
ccValue CC971 = 971
ccValue CC972 = 972
ccValue CC973 = 973
ccValue CC974 = 974
ccValue CC975 = 975
ccValue CC976 = 976
ccValue CC977 = 977
ccValue CC992 = 992
ccValue CC993 = 993
ccValue CC994 = 994
ccValue CC995 = 995
ccValue CC996 = 996
ccValue CC998 = 998
ccValue CCUnknown = 0

--------------------------------------------------------------------------------
-- Phone Number Structure
--------------------------------------------------------------------------------

||| Validated phone number in E.164 format
||| Stores the number as digits only (no + prefix stored, but implied)
public export
data PhoneNumber : Type where
  MkPhoneNumber : (countryCode : CountryCallingCode) ->
                  (nationalNumber : String) ->
                  PhoneNumber

||| Phone number parsing errors
public export
data PhoneError
  = InvalidCharacter Char
  | TooShort Nat
  | TooLong Nat
  | InvalidCountryCode String
  | InvalidNationalNumber String
  | EmptyInput

public export
Show PhoneError where
  show (InvalidCharacter c) = "Invalid character: " ++ singleton c
  show (TooShort n) = "Phone number too short: " ++ show n ++ " digits"
  show (TooLong n) = "Phone number too long: " ++ show n ++ " digits"
  show (InvalidCountryCode s) = "Invalid country code: " ++ s
  show (InvalidNationalNumber s) = "Invalid national number: " ++ s
  show EmptyInput = "Empty input"

--------------------------------------------------------------------------------
-- Phone Number Parsing
--------------------------------------------------------------------------------

||| Check if character is a digit
isDigitChar : Char -> Bool
isDigitChar c = c >= '0' && c <= '9'

||| Extract only digits from string
extractDigits : String -> String
extractDigits s = pack (filter isDigitChar (unpack s))

||| Parse country code from digit string
parseCountryCode : String -> Maybe (CountryCallingCode, String)
parseCountryCode s =
  -- Try 3-digit codes first, then 2-digit, then 1-digit
  tryParse 3 s <|> tryParse 2 s <|> tryParse 1 s
  where
    codeFromNat : Nat -> Maybe CountryCallingCode
    codeFromNat 1 = Just CC1
    codeFromNat 7 = Just CC7
    codeFromNat 20 = Just CC20
    codeFromNat 27 = Just CC27
    codeFromNat 30 = Just CC30
    codeFromNat 31 = Just CC31
    codeFromNat 32 = Just CC32
    codeFromNat 33 = Just CC33
    codeFromNat 34 = Just CC34
    codeFromNat 36 = Just CC36
    codeFromNat 39 = Just CC39
    codeFromNat 40 = Just CC40
    codeFromNat 41 = Just CC41
    codeFromNat 43 = Just CC43
    codeFromNat 44 = Just CC44
    codeFromNat 45 = Just CC45
    codeFromNat 46 = Just CC46
    codeFromNat 47 = Just CC47
    codeFromNat 48 = Just CC48
    codeFromNat 49 = Just CC49
    codeFromNat 51 = Just CC51
    codeFromNat 52 = Just CC52
    codeFromNat 53 = Just CC53
    codeFromNat 54 = Just CC54
    codeFromNat 55 = Just CC55
    codeFromNat 56 = Just CC56
    codeFromNat 57 = Just CC57
    codeFromNat 58 = Just CC58
    codeFromNat 60 = Just CC60
    codeFromNat 61 = Just CC61
    codeFromNat 62 = Just CC62
    codeFromNat 63 = Just CC63
    codeFromNat 64 = Just CC64
    codeFromNat 65 = Just CC65
    codeFromNat 66 = Just CC66
    codeFromNat 81 = Just CC81
    codeFromNat 82 = Just CC82
    codeFromNat 84 = Just CC84
    codeFromNat 86 = Just CC86
    codeFromNat 90 = Just CC90
    codeFromNat 91 = Just CC91
    codeFromNat 92 = Just CC92
    codeFromNat 93 = Just CC93
    codeFromNat 94 = Just CC94
    codeFromNat 95 = Just CC95
    codeFromNat 98 = Just CC98
    codeFromNat 212 = Just CC212
    codeFromNat 213 = Just CC213
    codeFromNat 216 = Just CC216
    codeFromNat 218 = Just CC218
    codeFromNat 220 = Just CC220
    codeFromNat 221 = Just CC221
    codeFromNat 234 = Just CC234
    codeFromNat 254 = Just CC254
    codeFromNat 255 = Just CC255
    codeFromNat 256 = Just CC256
    codeFromNat 260 = Just CC260
    codeFromNat 263 = Just CC263
    codeFromNat 351 = Just CC351
    codeFromNat 352 = Just CC352
    codeFromNat 353 = Just CC353
    codeFromNat 354 = Just CC354
    codeFromNat 358 = Just CC358
    codeFromNat 380 = Just CC380
    codeFromNat 381 = Just CC381
    codeFromNat 385 = Just CC385
    codeFromNat 386 = Just CC386
    codeFromNat 420 = Just CC420
    codeFromNat 421 = Just CC421
    codeFromNat 852 = Just CC852
    codeFromNat 853 = Just CC853
    codeFromNat 855 = Just CC855
    codeFromNat 856 = Just CC856
    codeFromNat 880 = Just CC880
    codeFromNat 886 = Just CC886
    codeFromNat 960 = Just CC960
    codeFromNat 961 = Just CC961
    codeFromNat 962 = Just CC962
    codeFromNat 963 = Just CC963
    codeFromNat 964 = Just CC964
    codeFromNat 965 = Just CC965
    codeFromNat 966 = Just CC966
    codeFromNat 967 = Just CC967
    codeFromNat 968 = Just CC968
    codeFromNat 971 = Just CC971
    codeFromNat 972 = Just CC972
    codeFromNat 973 = Just CC973
    codeFromNat 974 = Just CC974
    codeFromNat 975 = Just CC975
    codeFromNat 976 = Just CC976
    codeFromNat 977 = Just CC977
    codeFromNat 992 = Just CC992
    codeFromNat 993 = Just CC993
    codeFromNat 994 = Just CC994
    codeFromNat 995 = Just CC995
    codeFromNat 996 = Just CC996
    codeFromNat 998 = Just CC998
    codeFromNat _ = Nothing

    tryParse : Nat -> String -> Maybe (CountryCallingCode, String)
    tryParse n str =
      let prefix = substr 0 n str
          rest = substr n (length str) str
      in case parsePositive {a=Nat} prefix of
           Just num => case codeFromNat num of
             Just cc => Just (cc, rest)
             Nothing => Nothing
           Nothing => Nothing

||| Parse phone number from string (E.164 format with +)
public export
parsePhone : String -> Either PhoneError PhoneNumber
parsePhone input =
  let trimmed = trim input
  in if null trimmed
       then Left EmptyInput
       else parseE164 trimmed
  where
    parseE164 : String -> Either PhoneError PhoneNumber
    parseE164 s =
      let chars = unpack s
      in case chars of
           ('+' :: rest) =>
             let digits = pack (filter isDigitChar rest)
                 len = length digits
             in if len < 7
                  then Left (TooShort len)
                  else if len > 15
                    then Left (TooLong len)
                    else case parseCountryCode digits of
                      Just (cc, national) =>
                        if length national < 4
                          then Left (InvalidNationalNumber national)
                          else Right (MkPhoneNumber cc national)
                      Nothing => Left (InvalidCountryCode (substr 0 3 digits))
           _ =>
             let digits = extractDigits s
                 len = length digits
             in if len < 7
                  then Left (TooShort len)
                  else if len > 15
                    then Left (TooLong len)
                    else case parseCountryCode digits of
                      Just (cc, national) =>
                        if length national < 4
                          then Left (InvalidNationalNumber national)
                          else Right (MkPhoneNumber cc national)
                      Nothing => Left (InvalidCountryCode (substr 0 3 digits))

||| Parse phone number, returning Maybe
public export
parsePhone' : String -> Maybe PhoneNumber
parsePhone' s = either (const Nothing) Just (parsePhone s)

--------------------------------------------------------------------------------
-- Phone Number Formatting
--------------------------------------------------------------------------------

||| Format phone number in E.164 format (+CCNNNN...)
public export
formatE164 : PhoneNumber -> String
formatE164 (MkPhoneNumber cc national) =
  "+" ++ show (ccValue cc) ++ national

||| Format phone number with spaces (e.g., +1 555 123 4567)
public export
formatInternational : PhoneNumber -> String
formatInternational (MkPhoneNumber cc national) =
  "+" ++ show (ccValue cc) ++ " " ++ formatNational national
  where
    formatNational : String -> String
    formatNational n =
      let len = length n
      in if len <= 4 then n
         else if len <= 7 then substr 0 3 n ++ " " ++ substr 3 len n
         else if len <= 10 then substr 0 3 n ++ " " ++ substr 3 3 n ++ " " ++ substr 6 len n
         else n  -- Just return as-is for very long numbers

||| Format for display (varies by country, simplified version)
public export
formatDisplay : PhoneNumber -> String
formatDisplay = formatInternational

public export
Show PhoneNumber where
  show = formatE164

public export
Eq PhoneNumber where
  (MkPhoneNumber cc1 n1) == (MkPhoneNumber cc2 n2) =
    ccValue cc1 == ccValue cc2 && n1 == n2

--------------------------------------------------------------------------------
-- Phone Number Validation
--------------------------------------------------------------------------------

||| Check if string is valid phone number
public export
isValidPhone : String -> Bool
isValidPhone s = isRight (parsePhone s)

||| Get the country code from a phone number
public export
getCountryCode : PhoneNumber -> CountryCallingCode
getCountryCode (MkPhoneNumber cc _) = cc

||| Get the national number from a phone number
public export
getNationalNumber : PhoneNumber -> String
getNationalNumber (MkPhoneNumber _ n) = n

||| Get the full number as digits only
public export
getDigits : PhoneNumber -> String
getDigits (MkPhoneNumber cc national) = show (ccValue cc) ++ national

||| Get total length of phone number (country code + national)
public export
phoneLength : PhoneNumber -> Nat
phoneLength phone = length (getDigits phone)
