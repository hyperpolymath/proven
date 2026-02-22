# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Proven.NIF do
  @moduledoc """
  NIF (Native Implemented Function) bridge to libproven.

  This module contains all NIF function declarations that call into the
  Rustler-compiled native code, which in turn calls `libproven` (the
  Zig/Idris 2 formally verified core). No computation is performed here;
  every function is a direct pass-through to the native library.

  These functions are NOT intended to be called directly by users.
  Use the domain-specific modules (e.g., `Proven.SafeMath`) instead.
  """

  use Rustler,
    otp_app: :proven,
    crate: :proven_nif

  # --- SafeMath ---
  @doc false
  def nif_math_add(_a, _b), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_math_sub(_a, _b), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_math_mul(_a, _b), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_math_div(_a, _b), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_math_mod(_a, _b), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_math_abs(_n), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_math_clamp(_lo, _hi, _value), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_math_pow(_base, _exp), do: :erlang.nif_error(:nif_not_loaded)

  # --- SafeString ---
  @doc false
  def nif_string_escape_html(_input), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_string_escape_sql(_input), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_string_escape_js(_input), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_string_is_valid_utf8(_input), do: :erlang.nif_error(:nif_not_loaded)

  # --- SafePath ---
  @doc false
  def nif_path_has_traversal(_path), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_path_sanitize_filename(_filename), do: :erlang.nif_error(:nif_not_loaded)

  # --- SafeEmail ---
  @doc false
  def nif_email_is_valid(_email), do: :erlang.nif_error(:nif_not_loaded)

  # --- SafeNetwork ---
  @doc false
  def nif_network_parse_ipv4(_address), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_network_ipv4_is_private(_a, _b, _c, _d), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_network_ipv4_is_loopback(_a, _b, _c, _d), do: :erlang.nif_error(:nif_not_loaded)

  # --- SafeCrypto ---
  @doc false
  def nif_crypto_constant_time_eq(_a, _b), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_crypto_random_bytes(_n), do: :erlang.nif_error(:nif_not_loaded)

  # --- SafeFloat ---
  @doc false
  def nif_float_div(_a, _b), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_float_is_finite(_x), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_float_is_nan(_x), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_float_sqrt(_x), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_float_ln(_x), do: :erlang.nif_error(:nif_not_loaded)

  # --- SafeHex ---
  @doc false
  def nif_hex_encode(_data, _uppercase), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_hex_decode(_hex_string), do: :erlang.nif_error(:nif_not_loaded)

  # --- SafeUUID ---
  @doc false
  def nif_uuid_v4(), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_uuid_parse(_uuid_string), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_uuid_is_nil(_uuid_string), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_uuid_version(_uuid_string), do: :erlang.nif_error(:nif_not_loaded)

  # --- SafeHeader ---
  @doc false
  def nif_header_has_crlf(_s), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_header_is_valid_name(_name), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_header_is_dangerous(_name), do: :erlang.nif_error(:nif_not_loaded)

  # --- SafeCookie ---
  @doc false
  def nif_cookie_has_injection(_s), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_cookie_get_prefix(_name), do: :erlang.nif_error(:nif_not_loaded)

  # --- SafeContentType ---
  @doc false
  def nif_content_type_can_sniff_dangerous(_s), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_content_type_is_json(_subtype, _suffix), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_content_type_is_xml(_subtype, _suffix), do: :erlang.nif_error(:nif_not_loaded)

  # --- SafePassword ---
  @doc false
  def nif_password_validate(_password), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_password_is_common(_password), do: :erlang.nif_error(:nif_not_loaded)

  # --- SafeColor ---
  @doc false
  def nif_color_parse_hex(_hex), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_color_rgb_to_hsl(_r, _g, _b), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_color_to_hex(_r, _g, _b), do: :erlang.nif_error(:nif_not_loaded)

  # --- SafeAngle ---
  @doc false
  def nif_angle_deg_to_rad(_degrees), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_angle_rad_to_deg(_radians), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_angle_normalize_degrees(_degrees), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_angle_normalize_radians(_radians), do: :erlang.nif_error(:nif_not_loaded)

  # --- SafeUnit ---
  @doc false
  def nif_unit_convert_length(_value, _from, _to), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_unit_convert_temp(_value, _from, _to), do: :erlang.nif_error(:nif_not_loaded)

  # --- SafeGeo ---
  @doc false
  def nif_geo_validate(_lat, _lon), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_geo_distance(_lat1, _lon1, _lat2, _lon2), do: :erlang.nif_error(:nif_not_loaded)

  # --- SafeChecksum ---
  @doc false
  def nif_checksum_crc32(_data), do: :erlang.nif_error(:nif_not_loaded)

  # --- SafeProbability ---
  @doc false
  def nif_probability_create(_value), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_probability_and(_a, _b), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_probability_or(_a, _b), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_probability_not(_p), do: :erlang.nif_error(:nif_not_loaded)

  # --- SafeML ---
  @doc false
  def nif_ml_sigmoid(_x), do: :erlang.nif_error(:nif_not_loaded)
  @doc false
  def nif_ml_relu(_x), do: :erlang.nif_error(:nif_not_loaded)

  # --- SafeJson ---
  @doc false
  def nif_json_is_valid(_json_string), do: :erlang.nif_error(:nif_not_loaded)
end
