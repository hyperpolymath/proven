# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Proven.SafeContentType do
  @moduledoc """
  Safe MIME content type operations via libproven FFI.

  Sniffing prevention and JSON/XML detection are performed by the
  Idris 2 verified core.
  """

  @doc """
  Check if a content type can be sniffed to a dangerous type.
  """
  @spec can_sniff_to_dangerous?(String.t()) :: boolean()
  def can_sniff_to_dangerous?(s) when is_binary(s) do
    Proven.NIF.nif_content_type_can_sniff_dangerous(s)
  end

  @doc """
  Check if a content type is JSON (by subtype or suffix).
  """
  @spec json?(String.t(), String.t()) :: boolean()
  def json?(subtype, suffix \\ "") when is_binary(subtype) and is_binary(suffix) do
    Proven.NIF.nif_content_type_is_json(subtype, suffix)
  end

  @doc """
  Check if a content type is XML (by subtype or suffix).
  """
  @spec xml?(String.t(), String.t()) :: boolean()
  def xml?(subtype, suffix \\ "") when is_binary(subtype) and is_binary(suffix) do
    Proven.NIF.nif_content_type_is_xml(subtype, suffix)
  end
end
