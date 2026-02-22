{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}

-- | Raw FFI imports for every function exported by @libproven@.
--
-- This module contains __only__ @foreign import ccall@ declarations.
-- No Haskell logic is implemented here; all computation is delegated
-- to the Idris 2 verified core via the Zig FFI layer.
--
-- For safe Haskell wrappers, see the @Proven.Safe*@ modules.
module Proven.FFI
  ( -- * Lifecycle
    c_proven_init
  , c_proven_deinit
  , c_proven_is_initialized
  , c_proven_ffi_abi_version
    -- * Memory management
  , c_proven_free_string
    -- * Version info
  , c_proven_version_major
  , c_proven_version_minor
  , c_proven_version_patch
  , c_proven_module_count
    -- * SafeMath
  , c_proven_math_div
  , c_proven_math_mod
  , c_proven_math_add_checked
  , c_proven_math_sub_checked
  , c_proven_math_mul_checked
  , c_proven_math_abs_safe
  , c_proven_math_clamp
  , c_proven_math_pow_checked
    -- * SafeString
  , c_proven_string_is_valid_utf8
  , c_proven_string_escape_sql
  , c_proven_string_escape_html
  , c_proven_string_escape_js
    -- * SafePath
  , c_proven_path_has_traversal
  , c_proven_path_sanitize_filename
    -- * SafeCrypto
  , c_proven_crypto_constant_time_eq
  , c_proven_crypto_random_bytes
    -- * SafeEmail
  , c_proven_email_is_valid
    -- * SafeNetwork
  , c_proven_network_parse_ipv4
  , c_proven_network_ipv4_is_private
  , c_proven_network_ipv4_is_loopback
    -- * SafeUrl
  , c_proven_url_parse
  , c_proven_url_free
    -- * SafeHeader
  , c_proven_header_has_crlf
  , c_proven_header_is_valid_name
  , c_proven_header_is_dangerous
  , c_proven_header_render
  , c_proven_header_build_csp
  , c_proven_header_build_hsts
    -- * SafeCookie
  , c_proven_cookie_has_injection
  , c_proven_cookie_validate_name
  , c_proven_cookie_validate_value
  , c_proven_cookie_get_prefix
  , c_proven_cookie_build_set_cookie
  , c_proven_cookie_build_delete
    -- * SafeContentType
  , c_proven_content_type_parse
  , c_proven_content_type_free
  , c_proven_content_type_can_sniff_dangerous
  , c_proven_content_type_render
  , c_proven_content_type_is_json
  , c_proven_content_type_is_xml
    -- * SafeHex
  , c_proven_hex_encode
  , c_proven_hex_decode
  , c_proven_hex_free
    -- * SafeUUID
  , c_proven_uuid_v4
  , c_proven_uuid_to_string
  , c_proven_uuid_parse
  , c_proven_uuid_is_nil
  , c_proven_uuid_version
    -- * SafeCurrency
  , c_proven_currency_parse
  , c_proven_currency_format
    -- * SafePhone
  , c_proven_phone_parse
  , c_proven_phone_format_e164
    -- * SafeJson
  , c_proven_json_is_valid
  , c_proven_json_get_type
    -- * SafeDateTime
  , c_proven_datetime_parse
  , c_proven_datetime_format_iso8601
  , c_proven_datetime_is_leap_year
  , c_proven_datetime_days_in_month
    -- * SafeFloat
  , c_proven_float_div
  , c_proven_float_is_finite
  , c_proven_float_is_nan
  , c_proven_float_sqrt
  , c_proven_float_ln
    -- * SafeVersion
  , c_proven_version_parse
  , c_proven_version_compare
  , c_proven_version_free
    -- * SafeGeo
  , c_proven_geo_validate
  , c_proven_geo_distance
  , c_proven_geo_in_bounds
    -- * SafeChecksum
  , c_proven_checksum_crc32
  , c_proven_checksum_verify_crc32
    -- * SafeProbability
  , c_proven_probability_create
  , c_proven_probability_and
  , c_proven_probability_or_exclusive
  , c_proven_probability_not
    -- * SafeCalculator
  , c_proven_calculator_eval
    -- * SafePassword
  , c_proven_password_validate
  , c_proven_password_is_common
    -- * SafeColor
  , c_proven_color_parse_hex
  , c_proven_color_rgb_to_hsl
  , c_proven_color_to_hex
    -- * SafeAngle
  , c_proven_angle_deg_to_rad
  , c_proven_angle_rad_to_deg
  , c_proven_angle_normalize_degrees
  , c_proven_angle_normalize_radians
    -- * SafeUnit
  , c_proven_unit_convert_length
  , c_proven_unit_convert_temp
    -- * SafeML
  , c_proven_ml_sigmoid
  , c_proven_ml_relu
  , c_proven_ml_leaky_relu
  , c_proven_ml_clamp
    -- * SafeRetry
  , c_proven_retry_delay
  , c_proven_retry_should_retry
    -- * Re-exports
  , module Proven.FFI.Types
  ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Proven.FFI.Types

-- ============================================================================
-- Lifecycle
-- ============================================================================

foreign import ccall "proven_init"
  c_proven_init :: IO CInt

foreign import ccall "proven_deinit"
  c_proven_deinit :: IO ()

foreign import ccall "proven_is_initialized"
  c_proven_is_initialized :: IO CChar

foreign import ccall "proven_ffi_abi_version"
  c_proven_ffi_abi_version :: IO Word32

-- ============================================================================
-- Memory management
-- ============================================================================

foreign import ccall "proven_free_string"
  c_proven_free_string :: Ptr CChar -> IO ()

-- ============================================================================
-- Version info
-- ============================================================================

foreign import ccall "proven_version_major"
  c_proven_version_major :: IO Word32

foreign import ccall "proven_version_minor"
  c_proven_version_minor :: IO Word32

foreign import ccall "proven_version_patch"
  c_proven_version_patch :: IO Word32

foreign import ccall "proven_module_count"
  c_proven_module_count :: IO Word32

-- ============================================================================
-- SafeMath
-- ============================================================================

foreign import ccall "proven_math_div"
  c_proven_math_div :: CLLong -> CLLong -> IO IntResult

foreign import ccall "proven_math_mod"
  c_proven_math_mod :: CLLong -> CLLong -> IO IntResult

foreign import ccall "proven_math_add_checked"
  c_proven_math_add_checked :: CLLong -> CLLong -> IO IntResult

foreign import ccall "proven_math_sub_checked"
  c_proven_math_sub_checked :: CLLong -> CLLong -> IO IntResult

foreign import ccall "proven_math_mul_checked"
  c_proven_math_mul_checked :: CLLong -> CLLong -> IO IntResult

foreign import ccall "proven_math_abs_safe"
  c_proven_math_abs_safe :: CLLong -> IO IntResult

foreign import ccall "proven_math_clamp"
  c_proven_math_clamp :: CLLong -> CLLong -> CLLong -> IO CLLong

foreign import ccall "proven_math_pow_checked"
  c_proven_math_pow_checked :: CLLong -> Word32 -> IO IntResult

-- ============================================================================
-- SafeString
-- ============================================================================

foreign import ccall "proven_string_is_valid_utf8"
  c_proven_string_is_valid_utf8 :: Ptr Word8 -> CSize -> IO BoolResult

foreign import ccall "proven_string_escape_sql"
  c_proven_string_escape_sql :: Ptr Word8 -> CSize -> IO StringResult

foreign import ccall "proven_string_escape_html"
  c_proven_string_escape_html :: Ptr Word8 -> CSize -> IO StringResult

foreign import ccall "proven_string_escape_js"
  c_proven_string_escape_js :: Ptr Word8 -> CSize -> IO StringResult

-- ============================================================================
-- SafePath
-- ============================================================================

foreign import ccall "proven_path_has_traversal"
  c_proven_path_has_traversal :: Ptr Word8 -> CSize -> IO BoolResult

foreign import ccall "proven_path_sanitize_filename"
  c_proven_path_sanitize_filename :: Ptr Word8 -> CSize -> IO StringResult

-- ============================================================================
-- SafeCrypto
-- ============================================================================

foreign import ccall "proven_crypto_constant_time_eq"
  c_proven_crypto_constant_time_eq :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> IO BoolResult

foreign import ccall "proven_crypto_random_bytes"
  c_proven_crypto_random_bytes :: Ptr Word8 -> CSize -> IO CInt

-- ============================================================================
-- SafeEmail
-- ============================================================================

foreign import ccall "proven_email_is_valid"
  c_proven_email_is_valid :: Ptr Word8 -> CSize -> IO BoolResult

-- ============================================================================
-- SafeNetwork
-- ============================================================================

foreign import ccall "proven_network_parse_ipv4"
  c_proven_network_parse_ipv4 :: Ptr Word8 -> CSize -> IO IPv4Result

foreign import ccall "proven_network_ipv4_is_private"
  c_proven_network_ipv4_is_private :: IPv4Address -> IO CChar

foreign import ccall "proven_network_ipv4_is_loopback"
  c_proven_network_ipv4_is_loopback :: IPv4Address -> IO CChar

-- ============================================================================
-- SafeUrl (opaque UrlResult returned; uses proven_url_free)
-- ============================================================================

foreign import ccall "proven_url_parse"
  c_proven_url_parse :: Ptr Word8 -> CSize -> IO (Ptr ())

foreign import ccall "proven_url_free"
  c_proven_url_free :: Ptr () -> IO ()

-- ============================================================================
-- SafeHeader
-- ============================================================================

foreign import ccall "proven_header_has_crlf"
  c_proven_header_has_crlf :: Ptr Word8 -> CSize -> IO BoolResult

foreign import ccall "proven_header_is_valid_name"
  c_proven_header_is_valid_name :: Ptr Word8 -> CSize -> IO BoolResult

foreign import ccall "proven_header_is_dangerous"
  c_proven_header_is_dangerous :: Ptr Word8 -> CSize -> IO BoolResult

foreign import ccall "proven_header_render"
  c_proven_header_render :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> IO StringResult

foreign import ccall "proven_header_build_csp"
  c_proven_header_build_csp :: Ptr Word8 -> CSize -> IO StringResult

foreign import ccall "proven_header_build_hsts"
  c_proven_header_build_hsts :: CLLong -> CChar -> CChar -> IO StringResult

-- ============================================================================
-- SafeCookie
-- ============================================================================

foreign import ccall "proven_cookie_has_injection"
  c_proven_cookie_has_injection :: Ptr Word8 -> CSize -> IO BoolResult

foreign import ccall "proven_cookie_validate_name"
  c_proven_cookie_validate_name :: Ptr Word8 -> CSize -> IO BoolResult

foreign import ccall "proven_cookie_validate_value"
  c_proven_cookie_validate_value :: Ptr Word8 -> CSize -> IO BoolResult

foreign import ccall "proven_cookie_get_prefix"
  c_proven_cookie_get_prefix :: Ptr Word8 -> CSize -> IO IntResult

foreign import ccall "proven_cookie_build_set_cookie"
  c_proven_cookie_build_set_cookie :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr CookieAttributes -> IO StringResult

foreign import ccall "proven_cookie_build_delete"
  c_proven_cookie_build_delete :: Ptr Word8 -> CSize -> IO StringResult

-- ============================================================================
-- SafeContentType
-- ============================================================================

foreign import ccall "proven_content_type_parse"
  c_proven_content_type_parse :: Ptr Word8 -> CSize -> IO ContentTypeResult

foreign import ccall "proven_content_type_free"
  c_proven_content_type_free :: Ptr ContentTypeResult -> IO ()

foreign import ccall "proven_content_type_can_sniff_dangerous"
  c_proven_content_type_can_sniff_dangerous :: Ptr Word8 -> CSize -> IO BoolResult

foreign import ccall "proven_content_type_render"
  c_proven_content_type_render :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> CharsetC -> CChar -> IO StringResult

foreign import ccall "proven_content_type_is_json"
  c_proven_content_type_is_json :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> IO BoolResult

foreign import ccall "proven_content_type_is_xml"
  c_proven_content_type_is_xml :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> IO BoolResult

-- ============================================================================
-- SafeHex
-- ============================================================================

foreign import ccall "proven_hex_encode"
  c_proven_hex_encode :: Ptr Word8 -> CSize -> CChar -> IO StringResult

foreign import ccall "proven_hex_decode"
  c_proven_hex_decode :: Ptr Word8 -> CSize -> IO (Ptr ())

foreign import ccall "proven_hex_free"
  c_proven_hex_free :: Ptr () -> IO ()

-- ============================================================================
-- SafeUUID
-- ============================================================================

foreign import ccall "proven_uuid_v4"
  c_proven_uuid_v4 :: IO UUIDResult

foreign import ccall "proven_uuid_to_string"
  c_proven_uuid_to_string :: FFI_UUID -> IO StringResult

foreign import ccall "proven_uuid_parse"
  c_proven_uuid_parse :: Ptr Word8 -> CSize -> IO UUIDResult

foreign import ccall "proven_uuid_is_nil"
  c_proven_uuid_is_nil :: FFI_UUID -> IO CChar

foreign import ccall "proven_uuid_version"
  c_proven_uuid_version :: FFI_UUID -> IO Word8

-- ============================================================================
-- SafeCurrency
-- ============================================================================

foreign import ccall "proven_currency_parse"
  c_proven_currency_parse :: Ptr Word8 -> CSize -> IO CurrencyResult

foreign import ccall "proven_currency_format"
  c_proven_currency_format :: CLLong -> Word8 -> Word8 -> Word8 -> Word8 -> IO StringResult

-- ============================================================================
-- SafePhone
-- ============================================================================

foreign import ccall "proven_phone_parse"
  c_proven_phone_parse :: Ptr Word8 -> CSize -> IO PhoneResult

foreign import ccall "proven_phone_format_e164"
  c_proven_phone_format_e164 :: Word16 -> Word64 -> IO StringResult

-- ============================================================================
-- SafeJson
-- ============================================================================

foreign import ccall "proven_json_is_valid"
  c_proven_json_is_valid :: Ptr Word8 -> CSize -> IO BoolResult

foreign import ccall "proven_json_get_type"
  c_proven_json_get_type :: Ptr Word8 -> CSize -> IO JsonType

-- ============================================================================
-- SafeDateTime
-- ============================================================================

foreign import ccall "proven_datetime_parse"
  c_proven_datetime_parse :: Ptr Word8 -> CSize -> IO DateTimeResult

foreign import ccall "proven_datetime_format_iso8601"
  c_proven_datetime_format_iso8601 :: Ptr FFI_DateTime -> IO StringResult

foreign import ccall "proven_datetime_is_leap_year"
  c_proven_datetime_is_leap_year :: CInt -> IO CChar

foreign import ccall "proven_datetime_days_in_month"
  c_proven_datetime_days_in_month :: CInt -> Word8 -> IO Word8

-- ============================================================================
-- SafeFloat
-- ============================================================================

foreign import ccall "proven_float_div"
  c_proven_float_div :: CDouble -> CDouble -> IO FloatResult

foreign import ccall "proven_float_is_finite"
  c_proven_float_is_finite :: CDouble -> IO CChar

foreign import ccall "proven_float_is_nan"
  c_proven_float_is_nan :: CDouble -> IO CChar

foreign import ccall "proven_float_sqrt"
  c_proven_float_sqrt :: CDouble -> IO FloatResult

foreign import ccall "proven_float_ln"
  c_proven_float_ln :: CDouble -> IO FloatResult

-- ============================================================================
-- SafeVersion
-- ============================================================================

foreign import ccall "proven_version_parse"
  c_proven_version_parse :: Ptr Word8 -> CSize -> IO VersionResult

foreign import ccall "proven_version_compare"
  c_proven_version_compare :: Ptr FFI_SemanticVersion -> Ptr FFI_SemanticVersion -> IO CInt

foreign import ccall "proven_version_free"
  c_proven_version_free :: Ptr FFI_SemanticVersion -> IO ()

-- ============================================================================
-- SafeGeo
-- ============================================================================

foreign import ccall "proven_geo_validate"
  c_proven_geo_validate :: CDouble -> CDouble -> IO GeoResult

foreign import ccall "proven_geo_distance"
  c_proven_geo_distance :: Ptr GeoCoordinate -> Ptr GeoCoordinate -> IO FloatResult

foreign import ccall "proven_geo_in_bounds"
  c_proven_geo_in_bounds :: Ptr GeoCoordinate -> CDouble -> CDouble -> CDouble -> CDouble -> IO CChar

-- ============================================================================
-- SafeChecksum
-- ============================================================================

foreign import ccall "proven_checksum_crc32"
  c_proven_checksum_crc32 :: Ptr Word8 -> CSize -> IO IntResult

foreign import ccall "proven_checksum_verify_crc32"
  c_proven_checksum_verify_crc32 :: Ptr Word8 -> CSize -> Word32 -> IO BoolResult

-- ============================================================================
-- SafeProbability
-- ============================================================================

foreign import ccall "proven_probability_create"
  c_proven_probability_create :: CDouble -> IO CDouble

foreign import ccall "proven_probability_and"
  c_proven_probability_and :: CDouble -> CDouble -> IO CDouble

foreign import ccall "proven_probability_or_exclusive"
  c_proven_probability_or_exclusive :: CDouble -> CDouble -> IO CDouble

foreign import ccall "proven_probability_not"
  c_proven_probability_not :: CDouble -> IO CDouble

-- ============================================================================
-- SafeCalculator
-- ============================================================================

foreign import ccall "proven_calculator_eval"
  c_proven_calculator_eval :: Ptr Word8 -> CSize -> IO FloatResult

-- ============================================================================
-- SafePassword
-- ============================================================================

foreign import ccall "proven_password_validate"
  c_proven_password_validate :: Ptr Word8 -> CSize -> IO PasswordResult

foreign import ccall "proven_password_is_common"
  c_proven_password_is_common :: Ptr Word8 -> CSize -> IO CChar

-- ============================================================================
-- SafeColor
-- ============================================================================

foreign import ccall "proven_color_parse_hex"
  c_proven_color_parse_hex :: Ptr Word8 -> CSize -> IO ColorParseResult

foreign import ccall "proven_color_rgb_to_hsl"
  c_proven_color_rgb_to_hsl :: RGBColor -> IO HSLColor

foreign import ccall "proven_color_to_hex"
  c_proven_color_to_hex :: RGBColor -> IO StringResult

-- ============================================================================
-- SafeAngle
-- ============================================================================

foreign import ccall "proven_angle_deg_to_rad"
  c_proven_angle_deg_to_rad :: CDouble -> IO CDouble

foreign import ccall "proven_angle_rad_to_deg"
  c_proven_angle_rad_to_deg :: CDouble -> IO CDouble

foreign import ccall "proven_angle_normalize_degrees"
  c_proven_angle_normalize_degrees :: CDouble -> IO CDouble

foreign import ccall "proven_angle_normalize_radians"
  c_proven_angle_normalize_radians :: CDouble -> IO CDouble

-- ============================================================================
-- SafeUnit
-- ============================================================================

foreign import ccall "proven_unit_convert_length"
  c_proven_unit_convert_length :: CDouble -> LengthUnitC -> LengthUnitC -> IO FloatResult

foreign import ccall "proven_unit_convert_temp"
  c_proven_unit_convert_temp :: CDouble -> TempUnitC -> TempUnitC -> IO FloatResult

-- ============================================================================
-- SafeML
-- ============================================================================

foreign import ccall "proven_ml_sigmoid"
  c_proven_ml_sigmoid :: CDouble -> IO CDouble

foreign import ccall "proven_ml_relu"
  c_proven_ml_relu :: CDouble -> IO CDouble

foreign import ccall "proven_ml_leaky_relu"
  c_proven_ml_leaky_relu :: CDouble -> CDouble -> IO CDouble

foreign import ccall "proven_ml_clamp"
  c_proven_ml_clamp :: CDouble -> CDouble -> CDouble -> IO CDouble

-- ============================================================================
-- SafeRetry
-- ============================================================================

foreign import ccall "proven_retry_delay"
  c_proven_retry_delay :: Ptr RetryConfig -> Word32 -> IO Word64

foreign import ccall "proven_retry_should_retry"
  c_proven_retry_should_retry :: Ptr RetryConfig -> Word32 -> IO CChar
