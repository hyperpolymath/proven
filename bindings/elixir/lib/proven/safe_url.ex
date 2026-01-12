# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

defmodule Proven.SafeUrl do
  @moduledoc """
  Safe URL parsing and validation operations.
  """

  defmodule Parsed do
    @moduledoc """
    Represents the parsed components of a URL.
    """
    defstruct [:scheme, :host, :port, :path, :query, :fragment]

    @type t :: %__MODULE__{
            scheme: String.t(),
            host: String.t(),
            port: non_neg_integer() | nil,
            path: String.t(),
            query: String.t() | nil,
            fragment: String.t() | nil
          }
  end

  @doc """
  Parse a URL into its components.
  Returns {:ok, parsed} or {:error, :invalid_url}.
  """
  @spec parse(String.t()) :: {:ok, Parsed.t()} | {:error, :invalid_url}
  def parse(url_string) do
    case URI.parse(url_string) do
      %URI{scheme: scheme, host: host} when is_binary(scheme) and is_binary(host) and host != "" ->
        {:ok,
         %Parsed{
           scheme: String.downcase(scheme),
           host: host,
           port: URI.default_port(scheme) |> then(&if(&1, do: nil, else: URI.parse(url_string).port)),
           path: URI.parse(url_string).path || "/",
           query: URI.parse(url_string).query,
           fragment: URI.parse(url_string).fragment
         }}

      _ ->
        {:error, :invalid_url}
    end
  end

  @doc """
  Check if a string is a valid URL.
  """
  @spec valid?(String.t()) :: boolean()
  def valid?(url_string) do
    case parse(url_string) do
      {:ok, _} -> true
      {:error, _} -> false
    end
  end

  @doc """
  Extract the host from a URL.
  """
  @spec get_host(String.t()) :: {:ok, String.t()} | {:error, :invalid_url}
  def get_host(url_string) do
    case parse(url_string) do
      {:ok, parsed} -> {:ok, parsed.host}
      error -> error
    end
  end

  @doc """
  Extract the path from a URL.
  """
  @spec get_path(String.t()) :: {:ok, String.t()} | {:error, :invalid_url}
  def get_path(url_string) do
    case parse(url_string) do
      {:ok, parsed} -> {:ok, parsed.path}
      error -> error
    end
  end

  @doc """
  Check if a URL uses HTTPS.
  """
  @spec https?(String.t()) :: boolean()
  def https?(url_string) do
    case parse(url_string) do
      {:ok, parsed} -> parsed.scheme == "https"
      {:error, _} -> false
    end
  end

  @doc """
  Check if a URL uses a secure scheme (https, wss).
  """
  @spec secure?(String.t()) :: boolean()
  def secure?(url_string) do
    case parse(url_string) do
      {:ok, parsed} -> parsed.scheme in ["https", "wss"]
      {:error, _} -> false
    end
  end
end
