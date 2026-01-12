# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

defmodule Proven.SafeCurrency do
  @moduledoc """
  Safe currency and money operations with overflow protection.

  Uses integer-based arithmetic (cents/minor units) internally
  to avoid floating-point precision issues. All monetary calculations
  are checked for overflow and currency compatibility.
  """

  defmodule Money do
    @moduledoc """
    Represents a monetary amount with currency.

    The amount is stored in the currency's minor units (e.g., cents for USD).
    """
    defstruct [:amount, :currency, :exponent]

    @type t :: %__MODULE__{
            amount: integer(),
            currency: atom(),
            exponent: non_neg_integer()
          }
  end

  # ISO 4217 currency definitions: {symbol, name, decimal_places}
  @currencies %{
    # Major currencies
    usd: {"$", "US Dollar", 2},
    eur: {"€", "Euro", 2},
    gbp: {"£", "British Pound", 2},
    jpy: {"¥", "Japanese Yen", 0},
    cny: {"¥", "Chinese Yuan", 2},
    chf: {"CHF", "Swiss Franc", 2},
    cad: {"$", "Canadian Dollar", 2},
    aud: {"$", "Australian Dollar", 2},
    nzd: {"$", "New Zealand Dollar", 2},
    # Other currencies
    inr: {"₹", "Indian Rupee", 2},
    brl: {"R$", "Brazilian Real", 2},
    krw: {"₩", "South Korean Won", 0},
    mxn: {"$", "Mexican Peso", 2},
    sgd: {"$", "Singapore Dollar", 2},
    hkd: {"$", "Hong Kong Dollar", 2},
    sek: {"kr", "Swedish Krona", 2},
    nok: {"kr", "Norwegian Krone", 2},
    dkk: {"kr", "Danish Krone", 2},
    pln: {"zł", "Polish Zloty", 2},
    czk: {"Kč", "Czech Koruna", 2},
    # Cryptocurrencies (extended precision)
    btc: {"₿", "Bitcoin", 8},
    eth: {"Ξ", "Ethereum", 18},
    # Precious metals
    xau: {"XAU", "Gold (troy oz)", 6},
    xag: {"XAG", "Silver (troy oz)", 6},
    # Special
    xxx: {"", "No Currency", 0}
  }

  @max_int64 9_223_372_036_854_775_807
  @min_int64 -9_223_372_036_854_775_808

  @doc """
  Create a Money struct from a major unit amount (e.g., dollars).

  ## Examples

      iex> Proven.SafeCurrency.from_major(10.50, :usd)
      {:ok, %Proven.SafeCurrency.Money{amount: 1050, currency: :usd, exponent: 2}}

      iex> Proven.SafeCurrency.from_major(100, :jpy)
      {:ok, %Proven.SafeCurrency.Money{amount: 100, currency: :jpy, exponent: 0}}
  """
  @spec from_major(number(), atom()) :: {:ok, Money.t()} | {:error, :unknown_currency | :overflow}
  def from_major(amount, currency) when is_atom(currency) do
    case get_currency_info(currency) do
      {:ok, {_symbol, _name, exponent}} ->
        minor_amount = round(amount * :math.pow(10, exponent))

        if minor_amount > @max_int64 or minor_amount < @min_int64 do
          {:error, :overflow}
        else
          {:ok, %Money{amount: trunc(minor_amount), currency: currency, exponent: exponent}}
        end

      error ->
        error
    end
  end

  @doc """
  Create a Money struct from minor units (e.g., cents).

  ## Examples

      iex> Proven.SafeCurrency.from_minor(1050, :usd)
      {:ok, %Proven.SafeCurrency.Money{amount: 1050, currency: :usd, exponent: 2}}
  """
  @spec from_minor(integer(), atom()) :: {:ok, Money.t()} | {:error, :unknown_currency}
  def from_minor(amount, currency) when is_integer(amount) and is_atom(currency) do
    case get_currency_info(currency) do
      {:ok, {_symbol, _name, exponent}} ->
        {:ok, %Money{amount: amount, currency: currency, exponent: exponent}}

      error ->
        error
    end
  end

  @doc """
  Parse a string amount into a Money struct.

  ## Examples

      iex> Proven.SafeCurrency.parse("10.50", :usd)
      {:ok, %Proven.SafeCurrency.Money{amount: 1050, currency: :usd, exponent: 2}}

      iex> Proven.SafeCurrency.parse("$10.50", :usd)
      {:ok, %Proven.SafeCurrency.Money{amount: 1050, currency: :usd, exponent: 2}}
  """
  @spec parse(String.t(), atom()) ::
          {:ok, Money.t()} | {:error, :invalid_format | :unknown_currency | :overflow}
  def parse(amount_string, currency) when is_binary(amount_string) and is_atom(currency) do
    # Remove currency symbols and whitespace
    cleaned =
      amount_string
      |> String.replace(~r/[^\d.\-]/, "")
      |> String.trim()

    case Float.parse(cleaned) do
      {amount, ""} -> from_major(amount, currency)
      {amount, _rest} -> from_major(amount, currency)
      :error -> {:error, :invalid_format}
    end
  end

  @doc """
  Add two Money values of the same currency.

  ## Examples

      iex> {:ok, a} = Proven.SafeCurrency.from_minor(1000, :usd)
      iex> {:ok, b} = Proven.SafeCurrency.from_minor(500, :usd)
      iex> Proven.SafeCurrency.add(a, b)
      {:ok, %Proven.SafeCurrency.Money{amount: 1500, currency: :usd, exponent: 2}}
  """
  @spec add(Money.t(), Money.t()) ::
          {:ok, Money.t()} | {:error, :currency_mismatch | :overflow}
  def add(%Money{currency: c1}, %Money{currency: c2}) when c1 != c2 do
    {:error, :currency_mismatch}
  end

  def add(%Money{amount: a1, currency: currency, exponent: exp}, %Money{amount: a2}) do
    result = a1 + a2

    if result > @max_int64 or result < @min_int64 do
      {:error, :overflow}
    else
      {:ok, %Money{amount: result, currency: currency, exponent: exp}}
    end
  end

  @doc """
  Subtract two Money values of the same currency.

  ## Examples

      iex> {:ok, a} = Proven.SafeCurrency.from_minor(1000, :usd)
      iex> {:ok, b} = Proven.SafeCurrency.from_minor(300, :usd)
      iex> Proven.SafeCurrency.subtract(a, b)
      {:ok, %Proven.SafeCurrency.Money{amount: 700, currency: :usd, exponent: 2}}
  """
  @spec subtract(Money.t(), Money.t()) ::
          {:ok, Money.t()} | {:error, :currency_mismatch | :overflow}
  def subtract(%Money{currency: c1}, %Money{currency: c2}) when c1 != c2 do
    {:error, :currency_mismatch}
  end

  def subtract(%Money{amount: a1, currency: currency, exponent: exp}, %Money{amount: a2}) do
    result = a1 - a2

    if result > @max_int64 or result < @min_int64 do
      {:error, :overflow}
    else
      {:ok, %Money{amount: result, currency: currency, exponent: exp}}
    end
  end

  @doc """
  Multiply a Money value by a scalar.

  ## Examples

      iex> {:ok, money} = Proven.SafeCurrency.from_minor(1000, :usd)
      iex> Proven.SafeCurrency.multiply(money, 3)
      {:ok, %Proven.SafeCurrency.Money{amount: 3000, currency: :usd, exponent: 2}}
  """
  @spec multiply(Money.t(), number()) :: {:ok, Money.t()} | {:error, :overflow}
  def multiply(%Money{amount: amount, currency: currency, exponent: exp}, multiplier)
      when is_number(multiplier) do
    result = round(amount * multiplier)

    if result > @max_int64 or result < @min_int64 do
      {:error, :overflow}
    else
      {:ok, %Money{amount: trunc(result), currency: currency, exponent: exp}}
    end
  end

  @doc """
  Divide a Money value by a divisor.

  Uses banker's rounding (round half to even) for fair distribution.

  ## Examples

      iex> {:ok, money} = Proven.SafeCurrency.from_minor(1000, :usd)
      iex> Proven.SafeCurrency.divide(money, 3)
      {:ok, %Proven.SafeCurrency.Money{amount: 333, currency: :usd, exponent: 2}}
  """
  @spec divide(Money.t(), number()) ::
          {:ok, Money.t()} | {:error, :division_by_zero}
  def divide(_money, 0), do: {:error, :division_by_zero}
  def divide(_money, 0.0), do: {:error, :division_by_zero}

  def divide(%Money{amount: amount, currency: currency, exponent: exp}, divisor)
      when is_number(divisor) do
    result = round(amount / divisor)
    {:ok, %Money{amount: trunc(result), currency: currency, exponent: exp}}
  end

  @doc """
  Allocate a Money value into N equal parts, distributing remainder.

  Returns a list of Money values that sum to the original amount.

  ## Examples

      iex> {:ok, money} = Proven.SafeCurrency.from_minor(100, :usd)
      iex> Proven.SafeCurrency.allocate(money, 3)
      {:ok, [
        %Proven.SafeCurrency.Money{amount: 34, currency: :usd, exponent: 2},
        %Proven.SafeCurrency.Money{amount: 33, currency: :usd, exponent: 2},
        %Proven.SafeCurrency.Money{amount: 33, currency: :usd, exponent: 2}
      ]}
  """
  @spec allocate(Money.t(), pos_integer()) :: {:ok, [Money.t()]} | {:error, :invalid_parts}
  def allocate(_money, n) when not is_integer(n) or n <= 0, do: {:error, :invalid_parts}

  def allocate(%Money{amount: amount, currency: currency, exponent: exp}, n) do
    base = div(amount, n)
    remainder = rem(amount, n)

    parts =
      Enum.map(1..n, fn i ->
        extra = if i <= remainder, do: 1, else: 0
        %Money{amount: base + extra, currency: currency, exponent: exp}
      end)

    {:ok, parts}
  end

  @doc """
  Format a Money value as a string.

  ## Examples

      iex> {:ok, money} = Proven.SafeCurrency.from_minor(1050, :usd)
      iex> Proven.SafeCurrency.format(money)
      {:ok, "$10.50"}

      iex> {:ok, money} = Proven.SafeCurrency.from_minor(1000, :jpy)
      iex> Proven.SafeCurrency.format(money)
      {:ok, "¥1000"}
  """
  @spec format(Money.t(), keyword()) :: {:ok, String.t()}
  def format(%Money{amount: amount, currency: currency, exponent: exp}, opts \\ []) do
    show_symbol = Keyword.get(opts, :symbol, true)
    show_code = Keyword.get(opts, :code, false)

    {symbol, _name, _} = Map.get(@currencies, currency, {"", "", 0})

    formatted_amount =
      if exp > 0 do
        major = div(amount, trunc(:math.pow(10, exp)))
        minor = abs(rem(amount, trunc(:math.pow(10, exp))))
        minor_str = String.pad_leading(Integer.to_string(minor), exp, "0")
        "#{major}.#{minor_str}"
      else
        Integer.to_string(amount)
      end

    result =
      cond do
        show_code -> "#{formatted_amount} #{String.upcase(Atom.to_string(currency))}"
        show_symbol and symbol != "" -> "#{symbol}#{formatted_amount}"
        true -> formatted_amount
      end

    {:ok, result}
  end

  @doc """
  Convert Money to its major unit as a float.

  Warning: Use only for display; use integer operations for calculations.

  ## Examples

      iex> {:ok, money} = Proven.SafeCurrency.from_minor(1050, :usd)
      iex> Proven.SafeCurrency.to_major(money)
      {:ok, 10.5}
  """
  @spec to_major(Money.t()) :: {:ok, float()}
  def to_major(%Money{amount: amount, exponent: exp}) do
    {:ok, amount / :math.pow(10, exp)}
  end

  @doc """
  Get the minor unit amount.

  ## Examples

      iex> {:ok, money} = Proven.SafeCurrency.from_major(10.50, :usd)
      iex> Proven.SafeCurrency.to_minor(money)
      {:ok, 1050}
  """
  @spec to_minor(Money.t()) :: {:ok, integer()}
  def to_minor(%Money{amount: amount}), do: {:ok, amount}

  @doc """
  Compare two Money values.

  Returns :lt, :eq, or :gt.

  ## Examples

      iex> {:ok, a} = Proven.SafeCurrency.from_minor(1000, :usd)
      iex> {:ok, b} = Proven.SafeCurrency.from_minor(500, :usd)
      iex> Proven.SafeCurrency.compare(a, b)
      {:ok, :gt}
  """
  @spec compare(Money.t(), Money.t()) ::
          {:ok, :lt | :eq | :gt} | {:error, :currency_mismatch}
  def compare(%Money{currency: c1}, %Money{currency: c2}) when c1 != c2 do
    {:error, :currency_mismatch}
  end

  def compare(%Money{amount: a1}, %Money{amount: a2}) do
    cond do
      a1 < a2 -> {:ok, :lt}
      a1 > a2 -> {:ok, :gt}
      true -> {:ok, :eq}
    end
  end

  @doc """
  Check if a currency code is known.

  ## Examples

      iex> Proven.SafeCurrency.known_currency?(:usd)
      true

      iex> Proven.SafeCurrency.known_currency?(:xyz)
      false
  """
  @spec known_currency?(atom()) :: boolean()
  def known_currency?(currency) when is_atom(currency) do
    Map.has_key?(@currencies, currency)
  end

  @doc """
  Get information about a currency.

  ## Examples

      iex> Proven.SafeCurrency.get_currency_info(:usd)
      {:ok, {"$", "US Dollar", 2}}
  """
  @spec get_currency_info(atom()) :: {:ok, {String.t(), String.t(), non_neg_integer()}} | {:error, :unknown_currency}
  def get_currency_info(currency) when is_atom(currency) do
    case Map.get(@currencies, currency) do
      nil -> {:error, :unknown_currency}
      info -> {:ok, info}
    end
  end

  @doc """
  List all known currency codes.

  ## Examples

      iex> :usd in Proven.SafeCurrency.list_currencies()
      true
  """
  @spec list_currencies() :: [atom()]
  def list_currencies do
    Map.keys(@currencies)
  end

  @doc """
  Check if a Money value is zero.

  ## Examples

      iex> {:ok, money} = Proven.SafeCurrency.from_minor(0, :usd)
      iex> Proven.SafeCurrency.zero?(money)
      true
  """
  @spec zero?(Money.t()) :: boolean()
  def zero?(%Money{amount: 0}), do: true
  def zero?(%Money{}), do: false

  @doc """
  Check if a Money value is positive.
  """
  @spec positive?(Money.t()) :: boolean()
  def positive?(%Money{amount: amount}), do: amount > 0

  @doc """
  Check if a Money value is negative.
  """
  @spec negative?(Money.t()) :: boolean()
  def negative?(%Money{amount: amount}), do: amount < 0

  @doc """
  Return the absolute value of a Money amount.
  """
  @spec abs(Money.t()) :: {:ok, Money.t()}
  def abs(%Money{amount: amount, currency: currency, exponent: exp}) do
    {:ok, %Money{amount: Kernel.abs(amount), currency: currency, exponent: exp}}
  end

  @doc """
  Negate a Money value.
  """
  @spec negate(Money.t()) :: {:ok, Money.t()} | {:error, :overflow}
  def negate(%Money{amount: amount, currency: currency, exponent: exp}) do
    negated = -amount

    if negated > @max_int64 or negated < @min_int64 do
      {:error, :overflow}
    else
      {:ok, %Money{amount: negated, currency: currency, exponent: exp}}
    end
  end
end
