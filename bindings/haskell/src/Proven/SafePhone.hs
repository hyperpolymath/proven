{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- SPDX-FileCopyrightText: 2025 Hyperpolymath -}

-- | Safe phone number parsing and formatting.
--
-- Provides:
--
-- * ITU-T E.164 format support
-- * Country calling code detection
-- * Phone number parsing and validation
-- * Multiple output formats (E.164, international, display)
module Proven.SafePhone
  ( -- * Types
    PhoneNumber(..)
  , CountryCode(..)
  , PhoneNumberType(..)
  , PhoneError(..)
    -- * Parsing
  , parsePhone
  , parsePhone'
  , isValidPhone
    -- * Properties
  , getCountryCode
  , getNationalNumber
  , getDigits
  , phoneLength
  , countryCodeValue
    -- * Formatting
  , formatE164
  , formatInternational
  , formatDisplay
  ) where

import Data.Char (isDigit)
import Text.Read (readMaybe)

-- | Phone number type classification.
data PhoneNumberType
  = Mobile
  | FixedLine
  | TollFree
  | PremiumRate
  | SharedCost
  | VoIP
  | PersonalNumber
  | Pager
  | UAN  -- ^ Universal Access Number
  | UnknownType
  deriving (Show, Eq, Ord)

-- | ITU country calling codes (common ones).
data CountryCode
  = CC1     -- ^ USA, Canada, Caribbean
  | CC7     -- ^ Russia, Kazakhstan
  | CC20    -- ^ Egypt
  | CC27    -- ^ South Africa
  | CC30    -- ^ Greece
  | CC31    -- ^ Netherlands
  | CC32    -- ^ Belgium
  | CC33    -- ^ France
  | CC34    -- ^ Spain
  | CC36    -- ^ Hungary
  | CC39    -- ^ Italy
  | CC40    -- ^ Romania
  | CC41    -- ^ Switzerland
  | CC43    -- ^ Austria
  | CC44    -- ^ UK
  | CC45    -- ^ Denmark
  | CC46    -- ^ Sweden
  | CC47    -- ^ Norway
  | CC48    -- ^ Poland
  | CC49    -- ^ Germany
  | CC51    -- ^ Peru
  | CC52    -- ^ Mexico
  | CC53    -- ^ Cuba
  | CC54    -- ^ Argentina
  | CC55    -- ^ Brazil
  | CC56    -- ^ Chile
  | CC57    -- ^ Colombia
  | CC58    -- ^ Venezuela
  | CC60    -- ^ Malaysia
  | CC61    -- ^ Australia
  | CC62    -- ^ Indonesia
  | CC63    -- ^ Philippines
  | CC64    -- ^ New Zealand
  | CC65    -- ^ Singapore
  | CC66    -- ^ Thailand
  | CC81    -- ^ Japan
  | CC82    -- ^ South Korea
  | CC84    -- ^ Vietnam
  | CC86    -- ^ China
  | CC90    -- ^ Turkey
  | CC91    -- ^ India
  | CC92    -- ^ Pakistan
  | CC93    -- ^ Afghanistan
  | CC94    -- ^ Sri Lanka
  | CC95    -- ^ Myanmar
  | CC98    -- ^ Iran
  | CC212   -- ^ Morocco
  | CC213   -- ^ Algeria
  | CC216   -- ^ Tunisia
  | CC218   -- ^ Libya
  | CC234   -- ^ Nigeria
  | CC254   -- ^ Kenya
  | CC351   -- ^ Portugal
  | CC352   -- ^ Luxembourg
  | CC353   -- ^ Ireland
  | CC354   -- ^ Iceland
  | CC358   -- ^ Finland
  | CC380   -- ^ Ukraine
  | CC381   -- ^ Serbia
  | CC385   -- ^ Croatia
  | CC386   -- ^ Slovenia
  | CC420   -- ^ Czech Republic
  | CC421   -- ^ Slovakia
  | CC852   -- ^ Hong Kong
  | CC853   -- ^ Macau
  | CC855   -- ^ Cambodia
  | CC856   -- ^ Laos
  | CC880   -- ^ Bangladesh
  | CC886   -- ^ Taiwan
  | CC961   -- ^ Lebanon
  | CC962   -- ^ Jordan
  | CC963   -- ^ Syria
  | CC964   -- ^ Iraq
  | CC965   -- ^ Kuwait
  | CC966   -- ^ Saudi Arabia
  | CC967   -- ^ Yemen
  | CC968   -- ^ Oman
  | CC971   -- ^ UAE
  | CC972   -- ^ Israel
  | CC973   -- ^ Bahrain
  | CC974   -- ^ Qatar
  | CC977   -- ^ Nepal
  | CC992   -- ^ Tajikistan
  | CC993   -- ^ Turkmenistan
  | CC994   -- ^ Azerbaijan
  | CC995   -- ^ Georgia
  | CC996   -- ^ Kyrgyzstan
  | CC998   -- ^ Uzbekistan
  | CCUnknown
  deriving (Eq, Ord)

instance Show CountryCode where
  show cc = "+" ++ show (countryCodeValue cc)

-- | Get the numeric value of a country code.
countryCodeValue :: CountryCode -> Int
countryCodeValue CC1    = 1
countryCodeValue CC7    = 7
countryCodeValue CC20   = 20
countryCodeValue CC27   = 27
countryCodeValue CC30   = 30
countryCodeValue CC31   = 31
countryCodeValue CC32   = 32
countryCodeValue CC33   = 33
countryCodeValue CC34   = 34
countryCodeValue CC36   = 36
countryCodeValue CC39   = 39
countryCodeValue CC40   = 40
countryCodeValue CC41   = 41
countryCodeValue CC43   = 43
countryCodeValue CC44   = 44
countryCodeValue CC45   = 45
countryCodeValue CC46   = 46
countryCodeValue CC47   = 47
countryCodeValue CC48   = 48
countryCodeValue CC49   = 49
countryCodeValue CC51   = 51
countryCodeValue CC52   = 52
countryCodeValue CC53   = 53
countryCodeValue CC54   = 54
countryCodeValue CC55   = 55
countryCodeValue CC56   = 56
countryCodeValue CC57   = 57
countryCodeValue CC58   = 58
countryCodeValue CC60   = 60
countryCodeValue CC61   = 61
countryCodeValue CC62   = 62
countryCodeValue CC63   = 63
countryCodeValue CC64   = 64
countryCodeValue CC65   = 65
countryCodeValue CC66   = 66
countryCodeValue CC81   = 81
countryCodeValue CC82   = 82
countryCodeValue CC84   = 84
countryCodeValue CC86   = 86
countryCodeValue CC90   = 90
countryCodeValue CC91   = 91
countryCodeValue CC92   = 92
countryCodeValue CC93   = 93
countryCodeValue CC94   = 94
countryCodeValue CC95   = 95
countryCodeValue CC98   = 98
countryCodeValue CC212  = 212
countryCodeValue CC213  = 213
countryCodeValue CC216  = 216
countryCodeValue CC218  = 218
countryCodeValue CC234  = 234
countryCodeValue CC254  = 254
countryCodeValue CC351  = 351
countryCodeValue CC352  = 352
countryCodeValue CC353  = 353
countryCodeValue CC354  = 354
countryCodeValue CC358  = 358
countryCodeValue CC380  = 380
countryCodeValue CC381  = 381
countryCodeValue CC385  = 385
countryCodeValue CC386  = 386
countryCodeValue CC420  = 420
countryCodeValue CC421  = 421
countryCodeValue CC852  = 852
countryCodeValue CC853  = 853
countryCodeValue CC855  = 855
countryCodeValue CC856  = 856
countryCodeValue CC880  = 880
countryCodeValue CC886  = 886
countryCodeValue CC961  = 961
countryCodeValue CC962  = 962
countryCodeValue CC963  = 963
countryCodeValue CC964  = 964
countryCodeValue CC965  = 965
countryCodeValue CC966  = 966
countryCodeValue CC967  = 967
countryCodeValue CC968  = 968
countryCodeValue CC971  = 971
countryCodeValue CC972  = 972
countryCodeValue CC973  = 973
countryCodeValue CC974  = 974
countryCodeValue CC977  = 977
countryCodeValue CC992  = 992
countryCodeValue CC993  = 993
countryCodeValue CC994  = 994
countryCodeValue CC995  = 995
countryCodeValue CC996  = 996
countryCodeValue CC998  = 998
countryCodeValue CCUnknown = 0

-- | Phone number parsing errors.
data PhoneError
  = InvalidCharacter Char
  | TooShort Int
  | TooLong Int
  | InvalidCountryCode String
  | InvalidNationalNumber String
  | EmptyInput
  deriving (Show, Eq)

-- | Validated phone number in E.164 format.
data PhoneNumber = PhoneNumber
  { phoneCountryCode    :: CountryCode
  , phoneNationalNumber :: String
  } deriving (Eq)

instance Show PhoneNumber where
  show = formatE164

--------------------------------------------------------------------------------
-- Country Code Parsing
--------------------------------------------------------------------------------

-- | Parse country code from digits.
codeFromInt :: Int -> Maybe CountryCode
codeFromInt 1   = Just CC1
codeFromInt 7   = Just CC7
codeFromInt 20  = Just CC20
codeFromInt 27  = Just CC27
codeFromInt 30  = Just CC30
codeFromInt 31  = Just CC31
codeFromInt 32  = Just CC32
codeFromInt 33  = Just CC33
codeFromInt 34  = Just CC34
codeFromInt 36  = Just CC36
codeFromInt 39  = Just CC39
codeFromInt 40  = Just CC40
codeFromInt 41  = Just CC41
codeFromInt 43  = Just CC43
codeFromInt 44  = Just CC44
codeFromInt 45  = Just CC45
codeFromInt 46  = Just CC46
codeFromInt 47  = Just CC47
codeFromInt 48  = Just CC48
codeFromInt 49  = Just CC49
codeFromInt 51  = Just CC51
codeFromInt 52  = Just CC52
codeFromInt 53  = Just CC53
codeFromInt 54  = Just CC54
codeFromInt 55  = Just CC55
codeFromInt 56  = Just CC56
codeFromInt 57  = Just CC57
codeFromInt 58  = Just CC58
codeFromInt 60  = Just CC60
codeFromInt 61  = Just CC61
codeFromInt 62  = Just CC62
codeFromInt 63  = Just CC63
codeFromInt 64  = Just CC64
codeFromInt 65  = Just CC65
codeFromInt 66  = Just CC66
codeFromInt 81  = Just CC81
codeFromInt 82  = Just CC82
codeFromInt 84  = Just CC84
codeFromInt 86  = Just CC86
codeFromInt 90  = Just CC90
codeFromInt 91  = Just CC91
codeFromInt 92  = Just CC92
codeFromInt 93  = Just CC93
codeFromInt 94  = Just CC94
codeFromInt 95  = Just CC95
codeFromInt 98  = Just CC98
codeFromInt 212 = Just CC212
codeFromInt 213 = Just CC213
codeFromInt 216 = Just CC216
codeFromInt 218 = Just CC218
codeFromInt 234 = Just CC234
codeFromInt 254 = Just CC254
codeFromInt 351 = Just CC351
codeFromInt 352 = Just CC352
codeFromInt 353 = Just CC353
codeFromInt 354 = Just CC354
codeFromInt 358 = Just CC358
codeFromInt 380 = Just CC380
codeFromInt 381 = Just CC381
codeFromInt 385 = Just CC385
codeFromInt 386 = Just CC386
codeFromInt 420 = Just CC420
codeFromInt 421 = Just CC421
codeFromInt 852 = Just CC852
codeFromInt 853 = Just CC853
codeFromInt 855 = Just CC855
codeFromInt 856 = Just CC856
codeFromInt 880 = Just CC880
codeFromInt 886 = Just CC886
codeFromInt 961 = Just CC961
codeFromInt 962 = Just CC962
codeFromInt 963 = Just CC963
codeFromInt 964 = Just CC964
codeFromInt 965 = Just CC965
codeFromInt 966 = Just CC966
codeFromInt 967 = Just CC967
codeFromInt 968 = Just CC968
codeFromInt 971 = Just CC971
codeFromInt 972 = Just CC972
codeFromInt 973 = Just CC973
codeFromInt 974 = Just CC974
codeFromInt 977 = Just CC977
codeFromInt 992 = Just CC992
codeFromInt 993 = Just CC993
codeFromInt 994 = Just CC994
codeFromInt 995 = Just CC995
codeFromInt 996 = Just CC996
codeFromInt 998 = Just CC998
codeFromInt _   = Nothing

-- | Try to parse country code from digit string (try 3, 2, 1 digits).
parseCountryCodeFromDigits :: String -> Maybe (CountryCode, String)
parseCountryCodeFromDigits s =
  tryParse 3 s `orElse` tryParse 2 s `orElse` tryParse 1 s
  where
    orElse Nothing b = b
    orElse a _       = a

    tryParse :: Int -> String -> Maybe (CountryCode, String)
    tryParse n str
      | length str < n = Nothing
      | otherwise =
          let prefix = take n str
              rest = drop n str
          in case readMaybe prefix :: Maybe Int of
               Nothing  -> Nothing
               Just num -> case codeFromInt num of
                 Just cc -> Just (cc, rest)
                 Nothing -> Nothing

--------------------------------------------------------------------------------
-- Phone Parsing
--------------------------------------------------------------------------------

-- | Extract only digits from string.
extractDigits :: String -> String
extractDigits = filter isDigit

-- | Parse phone number from string (E.164 format with + or plain digits).
parsePhone :: String -> Either PhoneError PhoneNumber
parsePhone input =
  let trimmed = dropWhile (== ' ') $ reverse $ dropWhile (== ' ') $ reverse input
  in if null trimmed
       then Left EmptyInput
       else parseE164 trimmed
  where
    parseE164 :: String -> Either PhoneError PhoneNumber
    parseE164 ('+':rest) = parseDigitsOnly (extractDigits rest)
    parseE164 s          = parseDigitsOnly (extractDigits s)

    parseDigitsOnly :: String -> Either PhoneError PhoneNumber
    parseDigitsOnly digits
      | len < 7   = Left (TooShort len)
      | len > 15  = Left (TooLong len)
      | otherwise = case parseCountryCodeFromDigits digits of
          Nothing -> Left (InvalidCountryCode (take 3 digits))
          Just (cc, national)
            | length national < 4 -> Left (InvalidNationalNumber national)
            | otherwise -> Right (PhoneNumber cc national)
      where
        len = length digits

-- | Parse phone number, returning Maybe.
parsePhone' :: String -> Maybe PhoneNumber
parsePhone' s = either (const Nothing) Just (parsePhone s)

-- | Check if string is valid phone number.
isValidPhone :: String -> Bool
isValidPhone s = either (const False) (const True) (parsePhone s)

--------------------------------------------------------------------------------
-- Phone Properties
--------------------------------------------------------------------------------

-- | Get the country code from a phone number.
getCountryCode :: PhoneNumber -> CountryCode
getCountryCode = phoneCountryCode

-- | Get the national number from a phone number.
getNationalNumber :: PhoneNumber -> String
getNationalNumber = phoneNationalNumber

-- | Get the full number as digits only.
getDigits :: PhoneNumber -> String
getDigits (PhoneNumber cc national) = show (countryCodeValue cc) ++ national

-- | Get total length of phone number (country code + national).
phoneLength :: PhoneNumber -> Int
phoneLength phone = length (getDigits phone)

--------------------------------------------------------------------------------
-- Phone Formatting
--------------------------------------------------------------------------------

-- | Format phone number in E.164 format (+CCNNNN...).
formatE164 :: PhoneNumber -> String
formatE164 (PhoneNumber cc national) = "+" ++ show (countryCodeValue cc) ++ national

-- | Format phone number with spaces (e.g., +1 555 123 4567).
formatInternational :: PhoneNumber -> String
formatInternational (PhoneNumber cc national) =
  "+" ++ show (countryCodeValue cc) ++ " " ++ formatNational national
  where
    formatNational :: String -> String
    formatNational n
      | len <= 4  = n
      | len <= 7  = take 3 n ++ " " ++ drop 3 n
      | len <= 10 = take 3 n ++ " " ++ take 3 (drop 3 n) ++ " " ++ drop 6 n
      | otherwise = n
      where
        len = length n

-- | Format for display (uses international format).
formatDisplay :: PhoneNumber -> String
formatDisplay = formatInternational
