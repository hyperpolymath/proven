# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

defmodule Proven.MixProject do
  use Mix.Project

  @version "0.10.0"
  @source_url "https://github.com/hyperpolymath/proven"

  def project do
    [
      app: :proven,
      version: @version,
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      description: "Safety-first utility functions with formal verification guarantees - 38 modules covering math, strings, data structures, networking, security, and ML",
      package: package(),
      source_url: @source_url,
      docs: docs(),

      # Rustler configuration for NIF bindings
      compilers: Mix.compilers(),
      rustler_crates: [
        proven_nif: [
          path: "native/proven_nif",
          mode: if(Mix.env() == :prod, do: :release, else: :debug)
        ]
      ]
    ]
  end

  def application do
    [
      extra_applications: [:logger, :crypto]
    ]
  end

  defp deps do
    [
      # JSON support (optional — used by SafeJson if available)
      {:jason, "~> 1.4", optional: true},

      # NIF bindings via Rustler
      {:rustler, "~> 0.35.0", optional: true},
      {:rustler_precompiled, "~> 0.8", optional: true},

      # Development dependencies
      {:ex_doc, "~> 0.31", only: :dev, runtime: false},
      {:dialyxir, "~> 1.4", only: [:dev, :test], runtime: false},
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false}
    ]
  end

  defp package do
    [
      name: "proven",
      licenses: ["PMPL-1.0"],
      links: %{
        "GitHub" => @source_url,
        "Changelog" => "#{@source_url}/blob/main/CHANGELOG.md"
      },
      files: ~w(lib native .formatter.exs mix.exs README* LICENSE* CHANGELOG*),
      maintainers: ["Hyperpolymath"]
    ]
  end

  defp docs do
    [
      main: "Proven",
      source_ref: "v#{@version}",
      source_url: @source_url,
      extras: ["README.md", "CHANGELOG.md"],
      groups_for_modules: [
        "Core Modules": [
          Proven.SafeMath,
          Proven.SafeString,
          Proven.SafePath,
          Proven.SafeEmail,
          Proven.SafeUrl,
          Proven.SafeNetwork,
          Proven.SafeCrypto,
          Proven.SafeUuid,
          Proven.SafeCurrency,
          Proven.SafePhone,
          Proven.SafeHex
        ],
        "Data Modules": [
          Proven.SafeJson,
          Proven.SafeDateTime,
          Proven.SafeFloat,
          Proven.SafeVersion,
          Proven.SafeColor,
          Proven.SafeAngle,
          Proven.SafeUnit
        ],
        "Data Structure Modules": [
          Proven.SafeBuffer,
          Proven.SafeQueue,
          Proven.SafeBloom,
          Proven.SafeLru,
          Proven.SafeGraph
        ],
        "Resilience Modules": [
          Proven.SafeRateLimiter,
          Proven.SafeCircuitBreaker,
          Proven.SafeRetry,
          Proven.SafeMonotonic
        ],
        "State Modules": [
          Proven.SafeStateMachine,
          Proven.SafeCalculator
        ],
        "Algorithm Modules": [
          Proven.SafeGeo,
          Proven.SafeProbability,
          Proven.SafeChecksum,
          Proven.SafeTensor
        ],
        "Security Modules": [
          Proven.SafePassword,
          Proven.SafeMl
        ],
        "HTTP Modules": [
          Proven.SafeHeader,
          Proven.SafeCookie,
          Proven.SafeContentType
        ]
      ]
    ]
  end
end
