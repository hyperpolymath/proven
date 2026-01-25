// SPDX-License-Identifier: PMPL-1.0

package proven

/**
 * Unit categories for type safety.
 */
enum UnitCategory:
  case Length
  case Mass
  case Time
  case Temperature
  case DataSize
  case Speed
  case Area
  case Volume

/**
 * Quantity with unit for type-safe operations.
 */
case class Quantity[U <: UnitCategory](value: Double, unit: String, category: U):

  /**
   * Add two quantities (must be same category).
   */
  def +(other: Quantity[U]): Option[Quantity[U]] =
    if category != other.category then None
    else
      // Convert other to this unit
      SafeUnit.convert(other.value, other.unit, unit).map { converted =>
        Quantity(value + converted, unit, category)
      }

  /**
   * Subtract two quantities (must be same category).
   */
  def -(other: Quantity[U]): Option[Quantity[U]] =
    if category != other.category then None
    else
      SafeUnit.convert(other.value, other.unit, unit).map { converted =>
        Quantity(value - converted, unit, category)
      }

  /**
   * Multiply by scalar.
   */
  def *(scalar: Double): Quantity[U] = Quantity(value * scalar, unit, category)

  /**
   * Divide by scalar.
   */
  def /(scalar: Double): Option[Quantity[U]] =
    if scalar.abs < 1e-10 then None
    else Some(Quantity(value / scalar, unit, category))

  /**
   * Convert to another unit.
   */
  def to(targetUnit: String): Option[Quantity[U]] =
    SafeUnit.convert(value, unit, targetUnit).map(Quantity(_, targetUnit, category))

  override def toString: String = s"$value $unit"

/**
 * Safe unit conversion operations.
 */
object SafeUnit:

  // Conversion factors to base units
  private val lengthFactors: Map[String, Double] = Map(
    "m" -> 1.0,
    "km" -> 1000.0,
    "cm" -> 0.01,
    "mm" -> 0.001,
    "mi" -> 1609.344,
    "yd" -> 0.9144,
    "ft" -> 0.3048,
    "in" -> 0.0254,
    "nm" -> 1852.0,  // nautical mile
    "um" -> 1e-6,    // micrometer
    "nm" -> 1e-9     // nanometer (overrides nautical mile - use "nmi" for nautical)
  )

  private val massFactors: Map[String, Double] = Map(
    "kg" -> 1.0,
    "g" -> 0.001,
    "mg" -> 1e-6,
    "lb" -> 0.453592,
    "oz" -> 0.0283495,
    "t" -> 1000.0,
    "st" -> 6.35029  // stone
  )

  private val timeFactors: Map[String, Double] = Map(
    "s" -> 1.0,
    "ms" -> 0.001,
    "us" -> 1e-6,
    "ns" -> 1e-9,
    "min" -> 60.0,
    "h" -> 3600.0,
    "d" -> 86400.0,
    "wk" -> 604800.0
  )

  private val dataSizeFactors: Map[String, Double] = Map(
    "B" -> 1.0,
    "KB" -> 1024.0,
    "MB" -> 1024.0 * 1024,
    "GB" -> 1024.0 * 1024 * 1024,
    "TB" -> 1024.0 * 1024 * 1024 * 1024,
    "PB" -> 1024.0 * 1024 * 1024 * 1024 * 1024,
    "KiB" -> 1024.0,
    "MiB" -> 1024.0 * 1024,
    "GiB" -> 1024.0 * 1024 * 1024,
    "TiB" -> 1024.0 * 1024 * 1024 * 1024,
    "kB" -> 1000.0,
    "mB" -> 1000.0 * 1000,
    "gB" -> 1000.0 * 1000 * 1000
  )

  private val areaFactors: Map[String, Double] = Map(
    "m2" -> 1.0,
    "km2" -> 1e6,
    "cm2" -> 1e-4,
    "mm2" -> 1e-6,
    "ha" -> 10000.0,
    "acre" -> 4046.86,
    "sqft" -> 0.092903,
    "sqmi" -> 2.59e6
  )

  private val volumeFactors: Map[String, Double] = Map(
    "L" -> 1.0,
    "mL" -> 0.001,
    "m3" -> 1000.0,
    "cm3" -> 0.001,
    "gal" -> 3.78541,
    "qt" -> 0.946353,
    "pt" -> 0.473176,
    "floz" -> 0.0295735
  )

  /**
   * Convert between units.
   */
  def convert(value: Double, fromUnit: String, toUnit: String): Option[Double] =
    if fromUnit == toUnit then return Some(value)

    // Try each category
    convertWithFactors(value, fromUnit, toUnit, lengthFactors)
      .orElse(convertWithFactors(value, fromUnit, toUnit, massFactors))
      .orElse(convertWithFactors(value, fromUnit, toUnit, timeFactors))
      .orElse(convertWithFactors(value, fromUnit, toUnit, dataSizeFactors))
      .orElse(convertWithFactors(value, fromUnit, toUnit, areaFactors))
      .orElse(convertWithFactors(value, fromUnit, toUnit, volumeFactors))
      .orElse(convertTemperature(value, fromUnit, toUnit))

  private def convertWithFactors(
      value: Double,
      fromUnit: String,
      toUnit: String,
      factors: Map[String, Double]
  ): Option[Double] =
    for
      fromFactor <- factors.get(fromUnit)
      toFactor <- factors.get(toUnit)
    yield value * fromFactor / toFactor

  private def convertTemperature(
      value: Double,
      fromUnit: String,
      toUnit: String
  ): Option[Double] =
    // Convert to Kelvin first, then to target
    val kelvin: Option[Double] = fromUnit match
      case "K" => Some(value)
      case "C" | "degC" => Some(value + 273.15)
      case "F" | "degF" => Some((value - 32) * 5 / 9 + 273.15)
      case "R" => Some(value * 5 / 9)  // Rankine
      case _ => None

    kelvin.flatMap { k =>
      toUnit match
        case "K" => Some(k)
        case "C" | "degC" => Some(k - 273.15)
        case "F" | "degF" => Some((k - 273.15) * 9 / 5 + 32)
        case "R" => Some(k * 9 / 5)
        case _ => None
    }

  /**
   * Create a length quantity.
   */
  def length(value: Double, unit: String): Quantity[UnitCategory.Length.type] =
    Quantity(value, unit, UnitCategory.Length)

  /**
   * Create a mass quantity.
   */
  def mass(value: Double, unit: String): Quantity[UnitCategory.Mass.type] =
    Quantity(value, unit, UnitCategory.Mass)

  /**
   * Create a time quantity.
   */
  def time(value: Double, unit: String): Quantity[UnitCategory.Time.type] =
    Quantity(value, unit, UnitCategory.Time)

  /**
   * Create a data size quantity.
   */
  def dataSize(value: Double, unit: String): Quantity[UnitCategory.DataSize.type] =
    Quantity(value, unit, UnitCategory.DataSize)

  // Convenience conversions
  def metersToFeet(meters: Double): Double = meters / 0.3048
  def feetToMeters(feet: Double): Double = feet * 0.3048
  def milesToKm(miles: Double): Double = miles * 1.609344
  def kmToMiles(km: Double): Double = km / 1.609344
  def kgToPounds(kg: Double): Double = kg / 0.453592
  def poundsToKg(pounds: Double): Double = pounds * 0.453592
  def celsiusToFahrenheit(c: Double): Double = c * 9 / 5 + 32
  def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5 / 9
  def litersToGallons(liters: Double): Double = liters / 3.78541
  def gallonsToLiters(gallons: Double): Double = gallons * 3.78541

  /**
   * Format bytes as human-readable string.
   */
  def formatBytes(bytes: Long): String =
    if bytes < 1024 then s"$bytes B"
    else if bytes < 1024 * 1024 then f"${bytes / 1024.0}%.1f KB"
    else if bytes < 1024 * 1024 * 1024 then f"${bytes / (1024.0 * 1024)}%.1f MB"
    else if bytes < 1024L * 1024 * 1024 * 1024 then f"${bytes / (1024.0 * 1024 * 1024)}%.1f GB"
    else f"${bytes / (1024.0 * 1024 * 1024 * 1024)}%.1f TB"

  /**
   * Format duration as human-readable string.
   */
  def formatDuration(seconds: Long): String =
    if seconds < 60 then s"${seconds}s"
    else if seconds < 3600 then s"${seconds / 60}m ${seconds % 60}s"
    else if seconds < 86400 then s"${seconds / 3600}h ${(seconds % 3600) / 60}m"
    else s"${seconds / 86400}d ${(seconds % 86400) / 3600}h"

  /**
   * Parse a value with unit (e.g., "10 kg", "5.5m").
   */
  def parse(s: String): Option[(Double, String)] =
    val pattern = """^(-?\d+\.?\d*)\s*([a-zA-Z]+\d*)$""".r
    s.trim match
      case pattern(value, unit) => Some((value.toDouble, unit))
      case _ => None
