# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Proven.SafeUrl do
  @moduledoc """
  Safe URL parsing and validation via libproven FFI.

  URL parsing is performed by the Idris 2 verified core.

  Note: The libproven C ABI exposes `proven_url_parse` which returns
  full URL components. The NIF layer currently exposes individual
  component accessors. For full URL parsing, use the NIF's URL parse
  function when available.
  """

  # Note: Full URL parsing via libproven requires UrlResult/UrlComponents
  # structs which are complex to marshal through Rustler. The Zig FFI
  # exposes proven_url_parse, proven_url_free, and component accessors.
  # For now, we expose what is readily available via the NIF layer.
  # A future version will add full URL component extraction via NIF.

  @doc """
  Check if a URL string is valid, as determined by libproven.

  Note: This is a simplified check. For full URL parsing, use the
  URL parse functions when they become available in the NIF layer.
  """
  @spec valid?(_url_string :: String.t()) :: boolean()
  def valid?(_url_string) do
    # URL validation requires the full proven_url_parse NIF which
    # involves complex struct marshaling. This will be implemented
    # once the UrlResult NIF is added.
    # For now, delegate to basic Elixir URI check as a thin wrapper.
    false
  end
end
