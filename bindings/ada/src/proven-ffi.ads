--  SPDX-License-Identifier: MPL-2.0
--  (PMPL-1.0-or-later preferred; MPL-2.0 required for GNAT ecosystem)
--  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

--  Proven.FFI: Low-level C ABI declarations for libproven.
--
--  This package mirrors the C types and function signatures from
--  bindings/c/include/proven.h and the per-module headers. All types use
--  pragma Convention(C, ...) and all functions use pragma Import(C, ...).
--
--  Users should prefer the high-level Safe_* packages over calling
--  these FFI primitives directly.

with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package Proven.FFI is

   ---------------------------------------------------------------------------
   --  Status Codes (mirrors ProvenStatus)
   ---------------------------------------------------------------------------

   PROVEN_OK                    : constant int := 0;
   PROVEN_ERR_NULL_POINTER      : constant int := -1;
   PROVEN_ERR_INVALID_ARGUMENT  : constant int := -2;
   PROVEN_ERR_OVERFLOW          : constant int := -3;
   PROVEN_ERR_UNDERFLOW         : constant int := -4;
   PROVEN_ERR_DIVISION_BY_ZERO  : constant int := -5;
   PROVEN_ERR_PARSE_FAILURE     : constant int := -6;
   PROVEN_ERR_VALIDATION_FAILED : constant int := -7;
   PROVEN_ERR_OUT_OF_BOUNDS     : constant int := -8;
   PROVEN_ERR_ENCODING_ERROR    : constant int := -9;
   PROVEN_ERR_ALLOCATION_FAILED : constant int := -10;
   PROVEN_ERR_NOT_IMPLEMENTED   : constant int := -99;

   ---------------------------------------------------------------------------
   --  Core Result Types
   ---------------------------------------------------------------------------

   type Int_Result is record
      Status : int;
      Value  : long;
   end record;
   pragma Convention (C, Int_Result);

   type Bool_Result is record
      Status : int;
      Value  : C_bool;
   end record;
   pragma Convention (C, Bool_Result);

   type String_Result is record
      Status : int;
      Value  : chars_ptr;
      Length : size_t;
   end record;
   pragma Convention (C, String_Result);

   type Float_Result is record
      Status : int;
      Value  : double;
   end record;
   pragma Convention (C, Float_Result);

   ---------------------------------------------------------------------------
   --  Network Types
   ---------------------------------------------------------------------------

   type IPv4_Octets is array (0 .. 3) of unsigned_char;
   pragma Convention (C, IPv4_Octets);

   type IPv4_Address is record
      Octets : IPv4_Octets;
   end record;
   pragma Convention (C, IPv4_Address);

   type IPv4_Result is record
      Status  : int;
      Address : IPv4_Address;
   end record;
   pragma Convention (C, IPv4_Result);

   ---------------------------------------------------------------------------
   --  URL Types
   ---------------------------------------------------------------------------

   type Url_Components is record
      Scheme       : chars_ptr;
      Scheme_Len   : size_t;
      Host         : chars_ptr;
      Host_Len     : size_t;
      Port         : unsigned_short;
      Has_Port     : C_bool;
      Path         : chars_ptr;
      Path_Len     : size_t;
      Query        : chars_ptr;
      Query_Len    : size_t;
      Fragment     : chars_ptr;
      Fragment_Len : size_t;
   end record;
   pragma Convention (C, Url_Components);

   type Url_Result is record
      Status     : int;
      Components : Url_Components;
   end record;
   pragma Convention (C, Url_Result);

   ---------------------------------------------------------------------------
   --  DateTime Types
   ---------------------------------------------------------------------------

   type C_DateTime is record
      Year              : int;
      Month             : unsigned_char;
      Day               : unsigned_char;
      Hour              : unsigned_char;
      Minute            : unsigned_char;
      Second            : unsigned_char;
      Nanosecond        : unsigned;
      Tz_Offset_Minutes : short;
   end record;
   pragma Convention (C, C_DateTime);

   type DateTime_Result is record
      Status   : int;
      Datetime : C_DateTime;
   end record;
   pragma Convention (C, DateTime_Result);

   ---------------------------------------------------------------------------
   --  Version Types
   ---------------------------------------------------------------------------

   type C_Semantic_Version is record
      Major          : unsigned;
      Minor          : unsigned;
      Patch          : unsigned;
      Prerelease_Len : size_t;
      Prerelease     : chars_ptr;
   end record;
   pragma Convention (C, C_Semantic_Version);

   type Version_Result is record
      Status  : int;
      Version : C_Semantic_Version;
   end record;
   pragma Convention (C, Version_Result);

   ---------------------------------------------------------------------------
   --  JSON Types
   ---------------------------------------------------------------------------

   PROVEN_JSON_NULL    : constant int := 0;
   PROVEN_JSON_BOOL    : constant int := 1;
   PROVEN_JSON_NUMBER  : constant int := 2;
   PROVEN_JSON_STRING  : constant int := 3;
   PROVEN_JSON_ARRAY   : constant int := 4;
   PROVEN_JSON_OBJECT  : constant int := 5;
   PROVEN_JSON_INVALID : constant int := -1;

   ---------------------------------------------------------------------------
   --  UUID Types
   ---------------------------------------------------------------------------

   type UUID_Bytes is array (0 .. 15) of unsigned_char;
   pragma Convention (C, UUID_Bytes);

   type C_Uuid is record
      Bytes : UUID_Bytes;
   end record;
   pragma Convention (C, C_Uuid);

   UUID_OK                 : constant int := 0;
   UUID_ERR_NULL_POINTER   : constant int := -1;
   UUID_ERR_INVALID_FORMAT : constant int := -2;
   UUID_ERR_INVALID_LENGTH : constant int := -3;
   UUID_ERR_INVALID_HEX   : constant int := -4;
   UUID_ERR_BUFFER_SMALL   : constant int := -5;
   UUID_ERR_RANDOM_FAILED  : constant int := -6;

   UUID_STRING_LEN : constant := 36;
   UUID_URN_LEN    : constant := 45;
   UUID_BYTES_LEN  : constant := 16;

   ---------------------------------------------------------------------------
   --  Hex Types
   ---------------------------------------------------------------------------

   HEX_OK                : constant int := 0;
   HEX_ERR_NULL_POINTER  : constant int := -1;
   HEX_ERR_INVALID_CHAR  : constant int := -2;
   HEX_ERR_ODD_LENGTH    : constant int := -3;
   HEX_ERR_BUFFER_SMALL  : constant int := -4;
   HEX_ERR_OVERFLOW      : constant int := -5;

   type Hex_Decode_Result is record
      Status        : int;
      Bytes_Written : size_t;
   end record;
   pragma Convention (C, Hex_Decode_Result);

   type Hex_Encode_Result is record
      Status        : int;
      Chars_Written : size_t;
   end record;
   pragma Convention (C, Hex_Encode_Result);

   type Hex_Int_Result is record
      Status : int;
      Value  : unsigned_long;
   end record;
   pragma Convention (C, Hex_Int_Result);

   ---------------------------------------------------------------------------
   --  Currency Types
   ---------------------------------------------------------------------------

   CURRENCY_OK                  : constant int := 0;
   CURRENCY_ERR_INVALID_CODE    : constant int := -2;
   CURRENCY_ERR_MISMATCH        : constant int := -3;
   CURRENCY_ERR_OVERFLOW        : constant int := -4;
   CURRENCY_ERR_DIV_BY_ZERO     : constant int := -6;
   CURRENCY_ERR_BUFFER_SMALL    : constant int := -7;

   type C_Money is record
      Minor_Units : long;
      Currency    : int;
   end record;
   pragma Convention (C, C_Money);

   type Money_Result is record
      Status : int;
      Value  : C_Money;
   end record;
   pragma Convention (C, Money_Result);

   type Currency_Code_Result is record
      Status : int;
      Code   : int;
   end record;
   pragma Convention (C, Currency_Code_Result);

   ---------------------------------------------------------------------------
   --  Phone Types
   ---------------------------------------------------------------------------

   PHONE_OK              : constant int := 0;
   PHONE_MAX_DIGITS      : constant := 15;
   PHONE_FORMAT_BUF_SIZE : constant := 32;

   type Phone_National_Buf is array (0 .. PHONE_MAX_DIGITS) of char;
   pragma Convention (C, Phone_National_Buf);

   type C_Phone_Number is record
      Country_Code        : int;
      National_Number     : Phone_National_Buf;
      National_Number_Len : unsigned_char;
   end record;
   pragma Convention (C, C_Phone_Number);

   type Phone_Result is record
      Status : int;
      Number : C_Phone_Number;
   end record;
   pragma Convention (C, Phone_Result);

   ---------------------------------------------------------------------------
   --  Digest Types
   ---------------------------------------------------------------------------

   type C_Digest is record
      Algorithm : unsigned_char;
      Value     : chars_ptr;
      Value_Len : size_t;
   end record;
   pragma Convention (C, C_Digest);

   type Digest_Result is record
      Status : int;
      Digest : C_Digest;
   end record;
   pragma Convention (C, Digest_Result);

   ---------------------------------------------------------------------------
   --  Registry Types
   ---------------------------------------------------------------------------

   type C_Image_Reference is record
      Registry       : chars_ptr;
      Registry_Len   : size_t;
      Repository     : chars_ptr;
      Repository_Len : size_t;
      Tag            : chars_ptr;
      Tag_Len        : size_t;
      Digest         : chars_ptr;
      Digest_Len     : size_t;
   end record;
   pragma Convention (C, C_Image_Reference);

   type Image_Ref_Result is record
      Status    : int;
      Reference : C_Image_Reference;
   end record;
   pragma Convention (C, Image_Ref_Result);

   ---------------------------------------------------------------------------
   --  Runtime Management
   ---------------------------------------------------------------------------

   function Proven_Init return int;
   pragma Import (C, Proven_Init, "proven_init");

   procedure Proven_Deinit;
   pragma Import (C, Proven_Deinit, "proven_deinit");

   function Proven_Is_Initialized return C_bool;
   pragma Import (C, Proven_Is_Initialized, "proven_is_initialized");

   function Proven_FFI_ABI_Version return unsigned;
   pragma Import (C, Proven_FFI_ABI_Version, "proven_ffi_abi_version");

   ---------------------------------------------------------------------------
   --  Memory Management
   ---------------------------------------------------------------------------

   procedure Proven_Free_String (Ptr : chars_ptr);
   pragma Import (C, Proven_Free_String, "proven_free_string");

   procedure Proven_Url_Free (Components : access Url_Components);
   pragma Import (C, Proven_Url_Free, "proven_url_free");

   procedure Proven_Version_Free (Version : access C_Semantic_Version);
   pragma Import (C, Proven_Version_Free, "proven_version_free");

   ---------------------------------------------------------------------------
   --  SafeMath
   ---------------------------------------------------------------------------

   function Math_Div (Numerator, Denominator : long) return Int_Result;
   pragma Import (C, Math_Div, "proven_math_div");

   function Math_Mod (Numerator, Denominator : long) return Int_Result;
   pragma Import (C, Math_Mod, "proven_math_mod");

   function Math_Add_Checked (A, B : long) return Int_Result;
   pragma Import (C, Math_Add_Checked, "proven_math_add_checked");

   function Math_Sub_Checked (A, B : long) return Int_Result;
   pragma Import (C, Math_Sub_Checked, "proven_math_sub_checked");

   function Math_Mul_Checked (A, B : long) return Int_Result;
   pragma Import (C, Math_Mul_Checked, "proven_math_mul_checked");

   function Math_Pow_Checked (Base : long; Exp : unsigned) return Int_Result;
   pragma Import (C, Math_Pow_Checked, "proven_math_pow_checked");

   function Math_Abs_Safe (N : long) return Int_Result;
   pragma Import (C, Math_Abs_Safe, "proven_math_abs_safe");

   function Math_Clamp (Lo, Hi, Value : long) return long;
   pragma Import (C, Math_Clamp, "proven_math_clamp");

   ---------------------------------------------------------------------------
   --  SafeString
   ---------------------------------------------------------------------------

   function String_Is_Valid_UTF8
     (Ptr : access unsigned_char; Len : size_t) return Bool_Result;
   pragma Import (C, String_Is_Valid_UTF8, "proven_string_is_valid_utf8");

   function String_Escape_SQL
     (Ptr : access unsigned_char; Len : size_t) return String_Result;
   pragma Import (C, String_Escape_SQL, "proven_string_escape_sql");

   function String_Escape_HTML
     (Ptr : access unsigned_char; Len : size_t) return String_Result;
   pragma Import (C, String_Escape_HTML, "proven_string_escape_html");

   function String_Escape_JS
     (Ptr : access unsigned_char; Len : size_t) return String_Result;
   pragma Import (C, String_Escape_JS, "proven_string_escape_js");

   ---------------------------------------------------------------------------
   --  SafePath
   ---------------------------------------------------------------------------

   function Path_Has_Traversal
     (Ptr : access unsigned_char; Len : size_t) return Bool_Result;
   pragma Import (C, Path_Has_Traversal, "proven_path_has_traversal");

   function Path_Sanitize_Filename
     (Ptr : access unsigned_char; Len : size_t) return String_Result;
   pragma Import (C, Path_Sanitize_Filename, "proven_path_sanitize_filename");

   ---------------------------------------------------------------------------
   --  SafeEmail
   ---------------------------------------------------------------------------

   function Email_Is_Valid
     (Ptr : access unsigned_char; Len : size_t) return Bool_Result;
   pragma Import (C, Email_Is_Valid, "proven_email_is_valid");

   ---------------------------------------------------------------------------
   --  SafeNetwork
   ---------------------------------------------------------------------------

   function Network_Parse_IPv4
     (Ptr : access unsigned_char; Len : size_t) return IPv4_Result;
   pragma Import (C, Network_Parse_IPv4, "proven_network_parse_ipv4");

   function Network_IPv4_Is_Private (Addr : IPv4_Address) return C_bool;
   pragma Import (C, Network_IPv4_Is_Private, "proven_network_ipv4_is_private");

   function Network_IPv4_Is_Loopback (Addr : IPv4_Address) return C_bool;
   pragma Import (C, Network_IPv4_Is_Loopback,
                  "proven_network_ipv4_is_loopback");

   ---------------------------------------------------------------------------
   --  SafeCrypto
   ---------------------------------------------------------------------------

   function Crypto_Constant_Time_Eq
     (Ptr1 : access unsigned_char; Len1 : size_t;
      Ptr2 : access unsigned_char; Len2 : size_t) return Bool_Result;
   pragma Import (C, Crypto_Constant_Time_Eq,
                  "proven_crypto_constant_time_eq");

   function Crypto_Random_Bytes
     (Ptr : access unsigned_char; Len : size_t) return int;
   pragma Import (C, Crypto_Random_Bytes, "proven_crypto_random_bytes");

   ---------------------------------------------------------------------------
   --  SafeFloat
   ---------------------------------------------------------------------------

   function Float_Div (A, B : double) return Float_Result;
   pragma Import (C, Float_Div, "proven_float_div");

   function Float_Sqrt (X : double) return Float_Result;
   pragma Import (C, Float_Sqrt, "proven_float_sqrt");

   function Float_Ln (X : double) return Float_Result;
   pragma Import (C, Float_Ln, "proven_float_ln");

   function Float_Is_Finite (X : double) return C_bool;
   pragma Import (C, Float_Is_Finite, "proven_float_is_finite");

   function Float_Is_NaN (X : double) return C_bool;
   pragma Import (C, Float_Is_NaN, "proven_float_is_nan");

   ---------------------------------------------------------------------------
   --  SafeUrl
   ---------------------------------------------------------------------------

   function Url_Parse
     (Ptr : access unsigned_char; Len : size_t) return Url_Result;
   pragma Import (C, Url_Parse, "proven_url_parse");

   ---------------------------------------------------------------------------
   --  SafeJson
   ---------------------------------------------------------------------------

   function Json_Is_Valid
     (Ptr : access unsigned_char; Len : size_t) return Bool_Result;
   pragma Import (C, Json_Is_Valid, "proven_json_is_valid");

   function Json_Get_Type
     (Ptr : access unsigned_char; Len : size_t) return int;
   pragma Import (C, Json_Get_Type, "proven_json_get_type");

   ---------------------------------------------------------------------------
   --  SafeDateTime
   ---------------------------------------------------------------------------

   function Datetime_Parse
     (Ptr : access unsigned_char; Len : size_t) return DateTime_Result;
   pragma Import (C, Datetime_Parse, "proven_datetime_parse");

   function Datetime_Format_ISO8601
     (DT : C_DateTime) return String_Result;
   pragma Import (C, Datetime_Format_ISO8601,
                  "proven_datetime_format_iso8601");

   function Datetime_Is_Leap_Year (Year : int) return C_bool;
   pragma Import (C, Datetime_Is_Leap_Year, "proven_datetime_is_leap_year");

   function Datetime_Days_In_Month
     (Year : int; Month : unsigned_char) return unsigned_char;
   pragma Import (C, Datetime_Days_In_Month, "proven_datetime_days_in_month");

   ---------------------------------------------------------------------------
   --  SafeVersion
   ---------------------------------------------------------------------------

   function Version_Parse
     (Ptr : access unsigned_char; Len : size_t) return Version_Result;
   pragma Import (C, Version_Parse, "proven_version_parse");

   function Version_Compare
     (A, B : C_Semantic_Version) return int;
   pragma Import (C, Version_Compare, "proven_version_compare");

   ---------------------------------------------------------------------------
   --  SafeUUID
   ---------------------------------------------------------------------------

   function UUID_V4_Generate (UUID : access C_Uuid) return int;
   pragma Import (C, UUID_V4_Generate, "uuid_v4_generate");

   function UUID_Parse
     (Str : chars_ptr; Len : size_t; UUID : access C_Uuid) return int;
   pragma Import (C, UUID_Parse, "uuid_parse");

   function UUID_To_String
     (UUID : access constant C_Uuid; Buf : chars_ptr;
      Buf_Size : size_t) return int;
   pragma Import (C, UUID_To_String, "uuid_to_string");

   function UUID_To_URN
     (UUID : access constant C_Uuid; Buf : chars_ptr;
      Buf_Size : size_t) return int;
   pragma Import (C, UUID_To_URN, "uuid_to_urn");

   function UUID_Is_Nil (UUID : access constant C_Uuid) return C_bool;
   pragma Import (C, UUID_Is_Nil, "uuid_is_nil");

   function UUID_Equals
     (A, B : access constant C_Uuid) return C_bool;
   pragma Import (C, UUID_Equals, "uuid_equals");

   function UUID_Is_Valid (Str : chars_ptr; Len : size_t) return C_bool;
   pragma Import (C, UUID_Is_Valid, "uuid_is_valid");

   ---------------------------------------------------------------------------
   --  SafeHex
   ---------------------------------------------------------------------------

   function Hex_Encode
     (Bytes     : access unsigned_char; Bytes_Len : size_t;
      Hex       : chars_ptr;            Hex_Size  : size_t)
      return Hex_Encode_Result;
   pragma Import (C, Hex_Encode, "hex_encode");

   function Hex_Decode
     (Hex       : chars_ptr;            Hex_Len    : size_t;
      Bytes     : access unsigned_char; Bytes_Size : size_t)
      return Hex_Decode_Result;
   pragma Import (C, Hex_Decode, "hex_decode");

   function Hex_Is_Valid (Str : chars_ptr; Len : size_t) return C_bool;
   pragma Import (C, Hex_Is_Valid, "hex_is_valid");

   function Hex_Is_Valid_Bytes (Str : chars_ptr; Len : size_t) return C_bool;
   pragma Import (C, Hex_Is_Valid_Bytes, "hex_is_valid_bytes");

   function Hex_Constant_Time_Eq
     (A : chars_ptr; A_Len : size_t;
      B : chars_ptr; B_Len : size_t) return C_bool;
   pragma Import (C, Hex_Constant_Time_Eq, "hex_constant_time_eq");

   function Hex_To_Int (Hex : chars_ptr; Len : size_t) return Hex_Int_Result;
   pragma Import (C, Hex_To_Int, "hex_to_int");

   ---------------------------------------------------------------------------
   --  SafeCurrency
   ---------------------------------------------------------------------------

   function Currency_Parse_Code
     (Str : chars_ptr; Len : size_t) return Currency_Code_Result;
   pragma Import (C, Currency_Parse_Code, "currency_parse_code");

   function Currency_Is_Valid_Code
     (Str : chars_ptr; Len : size_t) return C_bool;
   pragma Import (C, Currency_Is_Valid_Code, "currency_is_valid_code");

   function Currency_Get_Decimals (Code : int) return unsigned_char;
   pragma Import (C, Currency_Get_Decimals, "currency_get_decimals");

   function Currency_Get_Symbol (Code : int) return chars_ptr;
   pragma Import (C, Currency_Get_Symbol, "currency_get_symbol");

   function Currency_Get_Name (Code : int) return chars_ptr;
   pragma Import (C, Currency_Get_Name, "currency_get_name");

   function C_Money_From_Major
     (Amount : long; Currency : int) return C_Money;
   pragma Import (C, C_Money_From_Major, "money_from_major");

   function C_Money_From_Minor
     (Amount : long; Currency : int) return C_Money;
   pragma Import (C, C_Money_From_Minor, "money_from_minor");

   function C_Money_Zero (Currency : int) return C_Money;
   pragma Import (C, C_Money_Zero, "money_zero");

   function C_Money_Add (A, B : C_Money) return Money_Result;
   pragma Import (C, C_Money_Add, "money_add");

   function C_Money_Sub (A, B : C_Money) return Money_Result;
   pragma Import (C, C_Money_Sub, "money_sub");

   function C_Money_Mul (M : C_Money; Scalar : long) return Money_Result;
   pragma Import (C, C_Money_Mul, "money_mul");

   function C_Money_Div (M : C_Money; Divisor : long) return Money_Result;
   pragma Import (C, C_Money_Div, "money_div");

   function C_Money_Is_Zero (M : C_Money) return C_bool;
   pragma Import (C, C_Money_Is_Zero, "money_is_zero");

   function C_Money_Is_Positive (M : C_Money) return C_bool;
   pragma Import (C, C_Money_Is_Positive, "money_is_positive");

   function C_Money_Is_Negative (M : C_Money) return C_bool;
   pragma Import (C, C_Money_Is_Negative, "money_is_negative");

   function C_Money_Get_Major (M : C_Money) return long;
   pragma Import (C, C_Money_Get_Major, "money_get_major");

   function C_Money_Get_Minor (M : C_Money) return long;
   pragma Import (C, C_Money_Get_Minor, "money_get_minor");

   function C_Money_Format
     (M : C_Money; Buf : chars_ptr; Buf_Size : size_t) return int;
   pragma Import (C, C_Money_Format, "money_format");

   ---------------------------------------------------------------------------
   --  SafePhone
   ---------------------------------------------------------------------------

   function Phone_Parse
     (Str : chars_ptr; Len : size_t) return Phone_Result;
   pragma Import (C, Phone_Parse, "phone_parse");

   function Phone_Is_Valid
     (Str : chars_ptr; Len : size_t) return C_bool;
   pragma Import (C, Phone_Is_Valid, "phone_is_valid");

   function Phone_Format_E164
     (Number : access constant C_Phone_Number; Buf : chars_ptr;
      Buf_Size : size_t) return int;
   pragma Import (C, Phone_Format_E164, "phone_format_e164");

   function Phone_Format_International
     (Number : access constant C_Phone_Number; Buf : chars_ptr;
      Buf_Size : size_t) return int;
   pragma Import (C, Phone_Format_International, "phone_format_international");

   function Phone_Get_Calling_Code (Code : int) return unsigned_short;
   pragma Import (C, Phone_Get_Calling_Code, "phone_get_calling_code");

   function Phone_Digit_Count
     (Number : access constant C_Phone_Number) return size_t;
   pragma Import (C, Phone_Digit_Count, "phone_digit_count");

   ---------------------------------------------------------------------------
   --  SafeDigest
   ---------------------------------------------------------------------------

   function Digest_Parse
     (Input : chars_ptr; Len : size_t) return Digest_Result;
   pragma Import (C, Digest_Parse, "proven_digest_parse");

   function Digest_Verify
     (Expected, Actual : access C_Digest) return Bool_Result;
   pragma Import (C, Digest_Verify, "proven_digest_verify");

   ---------------------------------------------------------------------------
   --  SafeRegistry
   ---------------------------------------------------------------------------

   function Registry_Parse
     (Input : chars_ptr; Len : size_t) return Image_Ref_Result;
   pragma Import (C, Registry_Parse, "proven_registry_parse");

end Proven.FFI;
