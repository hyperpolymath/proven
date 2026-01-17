// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

export enum LengthUnit {
  Meters = 'meters',
  Kilometers = 'kilometers',
  Miles = 'miles',
  Feet = 'feet',
  Inches = 'inches',
  Centimeters = 'centimeters',
  Millimeters = 'millimeters',
  Yards = 'yards',
  NauticalMiles = 'nautical_miles',
}

const LENGTH_TO_METERS: Record<LengthUnit, number> = {
  [LengthUnit.Meters]: 1,
  [LengthUnit.Kilometers]: 1000,
  [LengthUnit.Miles]: 1609.344,
  [LengthUnit.Feet]: 0.3048,
  [LengthUnit.Inches]: 0.0254,
  [LengthUnit.Centimeters]: 0.01,
  [LengthUnit.Millimeters]: 0.001,
  [LengthUnit.Yards]: 0.9144,
  [LengthUnit.NauticalMiles]: 1852,
};

/**
 * Length represents a length value with unit.
 */
export class Length {
  readonly value: number;
  readonly unit: LengthUnit;

  constructor(value: number, unit: LengthUnit) {
    this.value = value;
    this.unit = unit;
  }

  toMeters(): number {
    return this.value * LENGTH_TO_METERS[this.unit];
  }

  convert(to: LengthUnit): Length {
    const meters = this.toMeters();
    return new Length(meters / LENGTH_TO_METERS[to], to);
  }

  add(other: Length): Length {
    const meters = this.toMeters() + other.toMeters();
    return new Length(meters / LENGTH_TO_METERS[this.unit], this.unit);
  }

  sub(other: Length): Length {
    const meters = this.toMeters() - other.toMeters();
    return new Length(meters / LENGTH_TO_METERS[this.unit], this.unit);
  }

  toString(): string {
    return `${this.value} ${this.unit}`;
  }
}

export enum MassUnit {
  Kilograms = 'kilograms',
  Grams = 'grams',
  Milligrams = 'milligrams',
  Pounds = 'pounds',
  Ounces = 'ounces',
  Tons = 'tons',
  MetricTons = 'metric_tons',
}

const MASS_TO_KG: Record<MassUnit, number> = {
  [MassUnit.Kilograms]: 1,
  [MassUnit.Grams]: 0.001,
  [MassUnit.Milligrams]: 0.000001,
  [MassUnit.Pounds]: 0.453592,
  [MassUnit.Ounces]: 0.0283495,
  [MassUnit.Tons]: 907.185,
  [MassUnit.MetricTons]: 1000,
};

/**
 * Mass represents a mass value with unit.
 */
export class Mass {
  readonly value: number;
  readonly unit: MassUnit;

  constructor(value: number, unit: MassUnit) {
    this.value = value;
    this.unit = unit;
  }

  toKilograms(): number {
    return this.value * MASS_TO_KG[this.unit];
  }

  convert(to: MassUnit): Mass {
    const kg = this.toKilograms();
    return new Mass(kg / MASS_TO_KG[to], to);
  }

  toString(): string {
    return `${this.value} ${this.unit}`;
  }
}

export enum TemperatureUnit {
  Celsius = 'celsius',
  Fahrenheit = 'fahrenheit',
  Kelvin = 'kelvin',
}

/**
 * Temperature represents a temperature value with unit.
 */
export class Temperature {
  readonly value: number;
  readonly unit: TemperatureUnit;

  constructor(value: number, unit: TemperatureUnit) {
    this.value = value;
    this.unit = unit;
  }

  toCelsius(): number {
    switch (this.unit) {
      case TemperatureUnit.Celsius:
        return this.value;
      case TemperatureUnit.Fahrenheit:
        return (this.value - 32) * (5 / 9);
      case TemperatureUnit.Kelvin:
        return this.value - 273.15;
    }
  }

  convert(to: TemperatureUnit): Temperature {
    const celsius = this.toCelsius();
    let value: number;
    switch (to) {
      case TemperatureUnit.Celsius:
        value = celsius;
        break;
      case TemperatureUnit.Fahrenheit:
        value = celsius * (9 / 5) + 32;
        break;
      case TemperatureUnit.Kelvin:
        value = celsius + 273.15;
        break;
    }
    return new Temperature(value, to);
  }

  toString(): string {
    const symbol =
      this.unit === TemperatureUnit.Celsius ? '°C' :
      this.unit === TemperatureUnit.Fahrenheit ? '°F' : 'K';
    return `${this.value}${symbol}`;
  }
}

export enum TimeUnit {
  Seconds = 'seconds',
  Milliseconds = 'milliseconds',
  Microseconds = 'microseconds',
  Nanoseconds = 'nanoseconds',
  Minutes = 'minutes',
  Hours = 'hours',
  Days = 'days',
  Weeks = 'weeks',
}

const TIME_TO_SECONDS: Record<TimeUnit, number> = {
  [TimeUnit.Seconds]: 1,
  [TimeUnit.Milliseconds]: 0.001,
  [TimeUnit.Microseconds]: 0.000001,
  [TimeUnit.Nanoseconds]: 0.000000001,
  [TimeUnit.Minutes]: 60,
  [TimeUnit.Hours]: 3600,
  [TimeUnit.Days]: 86400,
  [TimeUnit.Weeks]: 604800,
};

/**
 * TimeValue represents a time duration with unit.
 */
export class TimeValue {
  readonly value: number;
  readonly unit: TimeUnit;

  constructor(value: number, unit: TimeUnit) {
    this.value = value;
    this.unit = unit;
  }

  toSeconds(): number {
    return this.value * TIME_TO_SECONDS[this.unit];
  }

  convert(to: TimeUnit): TimeValue {
    const seconds = this.toSeconds();
    return new TimeValue(seconds / TIME_TO_SECONDS[to], to);
  }

  toString(): string {
    return `${this.value} ${this.unit}`;
  }
}

export enum DataUnit {
  Bytes = 'bytes',
  Kilobytes = 'kilobytes',
  Megabytes = 'megabytes',
  Gigabytes = 'gigabytes',
  Terabytes = 'terabytes',
  Kibibytes = 'kibibytes',
  Mebibytes = 'mebibytes',
  Gibibytes = 'gibibytes',
  Tebibytes = 'tebibytes',
}

const DATA_TO_BYTES: Record<DataUnit, number> = {
  [DataUnit.Bytes]: 1,
  [DataUnit.Kilobytes]: 1000,
  [DataUnit.Megabytes]: 1000000,
  [DataUnit.Gigabytes]: 1000000000,
  [DataUnit.Terabytes]: 1000000000000,
  [DataUnit.Kibibytes]: 1024,
  [DataUnit.Mebibytes]: 1048576,
  [DataUnit.Gibibytes]: 1073741824,
  [DataUnit.Tebibytes]: 1099511627776,
};

/**
 * DataSize represents a data size with unit.
 */
export class DataSize {
  readonly value: number;
  readonly unit: DataUnit;

  constructor(value: number, unit: DataUnit) {
    this.value = value;
    this.unit = unit;
  }

  toBytes(): number {
    return this.value * DATA_TO_BYTES[this.unit];
  }

  convert(to: DataUnit): DataSize {
    const bytes = this.toBytes();
    return new DataSize(bytes / DATA_TO_BYTES[to], to);
  }

  /**
   * Format as human-readable string.
   */
  humanize(binary: boolean = true): string {
    const bytes = this.toBytes();
    const units = binary
      ? ['B', 'KiB', 'MiB', 'GiB', 'TiB']
      : ['B', 'KB', 'MB', 'GB', 'TB'];
    const base = binary ? 1024 : 1000;

    let value = bytes;
    let unitIndex = 0;

    while (value >= base && unitIndex < units.length - 1) {
      value /= base;
      unitIndex++;
    }

    return `${value.toFixed(2)} ${units[unitIndex]}`;
  }

  toString(): string {
    return `${this.value} ${this.unit}`;
  }
}

export const SafeUnit = {
  LengthUnit,
  Length,
  MassUnit,
  Mass,
  TemperatureUnit,
  Temperature,
  TimeUnit,
  TimeValue,
  DataUnit,
  DataSize,
};
