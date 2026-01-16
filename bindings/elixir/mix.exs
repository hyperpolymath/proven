# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

defmodule Proven.MixProject do
  use Mix.Project

  def project do
    [
      app: :proven,
      version: "0.9.0",
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      description: "Safety-first utility functions with formal verification guarantees",
      package: package(),
      source_url: "https://github.com/hyperpolymath/proven"
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:ex_doc, "~> 0.31", only: :dev, runtime: false}
    ]
  end

  defp package do
    [
      licenses: ["PMPL-1.0"],
      links: %{"GitHub" => "https://github.com/hyperpolymath/proven"}
    ]
  end
end
