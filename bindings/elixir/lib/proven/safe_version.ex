# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

defmodule Proven.SafeVersion do
  @moduledoc """
  Safe semantic version parsing and comparison.

  Provides validated SemVer 2.0 parsing and comparison with proper
  handling of pre-release and build metadata.
  """

  @type version_tuple :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}
  @type version_result :: {:ok, Version.t()} | {:error, atom()}
  @type comparison_result :: :lt | :eq | :gt

  @doc """
  Parse a semantic version string.

  Returns `{:ok, version}` or `{:error, reason}`.

  ## Examples

      iex> Proven.SafeVersion.parse("1.2.3")
      {:ok, %Version{major: 1, minor: 2, patch: 3}}

      iex> Proven.SafeVersion.parse("invalid")
      {:error, :invalid_version}
  """
  @spec parse(String.t()) :: version_result()
  def parse(version_string) when is_binary(version_string) do
    # Strip leading 'v' if present
    normalized = String.trim_leading(version_string, "v")

    case Version.parse(normalized) do
      {:ok, version} -> {:ok, version}
      :error -> {:error, :invalid_version}
    end
  end

  @doc """
  Parse a version string, raising on error.
  """
  @spec parse!(String.t()) :: Version.t()
  def parse!(version_string) do
    case parse(version_string) do
      {:ok, version} -> version
      {:error, reason} -> raise ArgumentError, "Invalid version: #{inspect(reason)}"
    end
  end

  @doc """
  Compare two version strings.

  Returns `:lt`, `:eq`, or `:gt`.

  ## Examples

      iex> Proven.SafeVersion.compare("1.0.0", "2.0.0")
      :lt

      iex> Proven.SafeVersion.compare("2.0.0", "1.0.0")
      :gt
  """
  @spec compare(String.t(), String.t()) :: {:ok, comparison_result()} | {:error, atom()}
  def compare(version1, version2) do
    with {:ok, v1} <- parse(version1),
         {:ok, v2} <- parse(version2) do
      {:ok, Version.compare(v1, v2)}
    end
  end

  @doc """
  Check if version satisfies a requirement.

  ## Examples

      iex> Proven.SafeVersion.satisfies?("1.5.0", ">= 1.0.0 and < 2.0.0")
      true
  """
  @spec satisfies?(String.t(), String.t()) :: boolean()
  def satisfies?(version_string, requirement_string) do
    with {:ok, version} <- parse(version_string),
         {:ok, requirement} <- parse_requirement(requirement_string) do
      Version.match?(version, requirement)
    else
      _ -> false
    end
  end

  @doc """
  Parse a version requirement string.

  Returns `{:ok, requirement}` or `{:error, reason}`.
  """
  @spec parse_requirement(String.t()) :: {:ok, Version.Requirement.t()} | {:error, atom()}
  def parse_requirement(requirement_string) when is_binary(requirement_string) do
    case Version.parse_requirement(requirement_string) do
      {:ok, requirement} -> {:ok, requirement}
      :error -> {:error, :invalid_requirement}
    end
  end

  @doc """
  Increment the major version.

  Returns `{:ok, new_version}` or `{:error, reason}`.
  """
  @spec increment_major(String.t()) :: {:ok, String.t()} | {:error, atom()}
  def increment_major(version_string) do
    with {:ok, version} <- parse(version_string) do
      new_version = "#{version.major + 1}.0.0"
      {:ok, new_version}
    end
  end

  @doc """
  Increment the minor version.

  Returns `{:ok, new_version}` or `{:error, reason}`.
  """
  @spec increment_minor(String.t()) :: {:ok, String.t()} | {:error, atom()}
  def increment_minor(version_string) do
    with {:ok, version} <- parse(version_string) do
      new_version = "#{version.major}.#{version.minor + 1}.0"
      {:ok, new_version}
    end
  end

  @doc """
  Increment the patch version.

  Returns `{:ok, new_version}` or `{:error, reason}`.
  """
  @spec increment_patch(String.t()) :: {:ok, String.t()} | {:error, atom()}
  def increment_patch(version_string) do
    with {:ok, version} <- parse(version_string) do
      new_version = "#{version.major}.#{version.minor}.#{version.patch + 1}"
      {:ok, new_version}
    end
  end

  @doc """
  Check if a version is a pre-release.
  """
  @spec pre_release?(String.t()) :: boolean()
  def pre_release?(version_string) do
    case parse(version_string) do
      {:ok, version} -> version.pre != []
      {:error, _} -> false
    end
  end

  @doc """
  Get version components as a tuple.

  Returns `{:ok, {major, minor, patch}}` or `{:error, reason}`.
  """
  @spec to_tuple(String.t()) :: {:ok, version_tuple()} | {:error, atom()}
  def to_tuple(version_string) do
    with {:ok, version} <- parse(version_string) do
      {:ok, {version.major, version.minor, version.patch}}
    end
  end

  @doc """
  Create a version from a tuple.

  Returns `{:ok, version_string}` or `{:error, reason}`.
  """
  @spec from_tuple(version_tuple()) :: {:ok, String.t()} | {:error, atom()}
  def from_tuple({major, minor, patch})
      when is_integer(major) and major >= 0 and
           is_integer(minor) and minor >= 0 and
           is_integer(patch) and patch >= 0 do
    {:ok, "#{major}.#{minor}.#{patch}"}
  end

  def from_tuple(_), do: {:error, :invalid_tuple}

  @doc """
  Check if version1 is compatible with version2 (same major, >= minor.patch).
  """
  @spec compatible?(String.t(), String.t()) :: boolean()
  def compatible?(version1, version2) do
    with {:ok, v1} <- parse(version1),
         {:ok, v2} <- parse(version2) do
      v1.major == v2.major and Version.compare(v1, v2) != :lt
    else
      _ -> false
    end
  end

  @doc """
  Get the latest version from a list.

  Returns `{:ok, version}` or `{:error, reason}`.
  """
  @spec latest([String.t()]) :: {:ok, String.t()} | {:error, atom()}
  def latest([]), do: {:error, :empty_list}

  def latest(versions) when is_list(versions) do
    parsed =
      versions
      |> Enum.map(&parse/1)
      |> Enum.filter(&match?({:ok, _}, &1))
      |> Enum.map(fn {:ok, v} -> v end)

    case parsed do
      [] -> {:error, :no_valid_versions}
      _ ->
        latest_version = Enum.max(parsed, Version)
        {:ok, to_string(latest_version)}
    end
  end
end
