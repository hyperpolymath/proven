// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

// LengthUnit represents length units.
type LengthUnit int

const (
	Meters LengthUnit = iota
	Kilometers
	Miles
	Feet
	Inches
	Centimeters
	Millimeters
	Yards
	NauticalMiles
)

// Length represents a length value with unit.
type Length struct {
	value float64
	unit  LengthUnit
}

// NewLength creates a length.
func NewLength(value float64, unit LengthUnit) Length {
	return Length{value: value, unit: unit}
}

// ToMeters converts to meters.
func (l Length) ToMeters() float64 {
	switch l.unit {
	case Meters:
		return l.value
	case Kilometers:
		return l.value * 1000
	case Miles:
		return l.value * 1609.344
	case Feet:
		return l.value * 0.3048
	case Inches:
		return l.value * 0.0254
	case Centimeters:
		return l.value / 100
	case Millimeters:
		return l.value / 1000
	case Yards:
		return l.value * 0.9144
	case NauticalMiles:
		return l.value * 1852
	}
	return l.value
}

// Convert converts to another unit.
func (l Length) Convert(to LengthUnit) Length {
	meters := l.ToMeters()
	var value float64
	switch to {
	case Meters:
		value = meters
	case Kilometers:
		value = meters / 1000
	case Miles:
		value = meters / 1609.344
	case Feet:
		value = meters / 0.3048
	case Inches:
		value = meters / 0.0254
	case Centimeters:
		value = meters * 100
	case Millimeters:
		value = meters * 1000
	case Yards:
		value = meters / 0.9144
	case NauticalMiles:
		value = meters / 1852
	}
	return Length{value: value, unit: to}
}

// MassUnit represents mass units.
type MassUnit int

const (
	Kilograms MassUnit = iota
	Grams
	Milligrams
	Pounds
	Ounces
	Tons
	MetricTons
)

// Mass represents a mass value with unit.
type Mass struct {
	value float64
	unit  MassUnit
}

// NewMass creates a mass.
func NewMass(value float64, unit MassUnit) Mass {
	return Mass{value: value, unit: unit}
}

// ToKilograms converts to kilograms.
func (m Mass) ToKilograms() float64 {
	switch m.unit {
	case Kilograms:
		return m.value
	case Grams:
		return m.value / 1000
	case Milligrams:
		return m.value / 1000000
	case Pounds:
		return m.value * 0.453592
	case Ounces:
		return m.value * 0.0283495
	case Tons:
		return m.value * 907.185
	case MetricTons:
		return m.value * 1000
	}
	return m.value
}

// Convert converts to another unit.
func (m Mass) Convert(to MassUnit) Mass {
	kg := m.ToKilograms()
	var value float64
	switch to {
	case Kilograms:
		value = kg
	case Grams:
		value = kg * 1000
	case Milligrams:
		value = kg * 1000000
	case Pounds:
		value = kg / 0.453592
	case Ounces:
		value = kg / 0.0283495
	case Tons:
		value = kg / 907.185
	case MetricTons:
		value = kg / 1000
	}
	return Mass{value: value, unit: to}
}

// TemperatureUnit represents temperature units.
type TemperatureUnit int

const (
	Celsius TemperatureUnit = iota
	Fahrenheit
	Kelvin
)

// Temperature represents a temperature value with unit.
type Temperature struct {
	value float64
	unit  TemperatureUnit
}

// NewTemperature creates a temperature.
func NewTemperature(value float64, unit TemperatureUnit) Temperature {
	return Temperature{value: value, unit: unit}
}

// ToCelsius converts to Celsius.
func (t Temperature) ToCelsius() float64 {
	switch t.unit {
	case Celsius:
		return t.value
	case Fahrenheit:
		return (t.value - 32) * 5 / 9
	case Kelvin:
		return t.value - 273.15
	}
	return t.value
}

// Convert converts to another unit.
func (t Temperature) Convert(to TemperatureUnit) Temperature {
	c := t.ToCelsius()
	var value float64
	switch to {
	case Celsius:
		value = c
	case Fahrenheit:
		value = c*9/5 + 32
	case Kelvin:
		value = c + 273.15
	}
	return Temperature{value: value, unit: to}
}

// TimeUnit represents time units.
type TimeUnit int

const (
	Seconds TimeUnit = iota
	Milliseconds
	Microseconds
	Nanoseconds
	Minutes
	Hours
	Days
	Weeks
)

// TimeValue represents a time value with unit.
type TimeValue struct {
	value float64
	unit  TimeUnit
}

// NewTimeValue creates a time value.
func NewTimeValue(value float64, unit TimeUnit) TimeValue {
	return TimeValue{value: value, unit: unit}
}

// ToSeconds converts to seconds.
func (t TimeValue) ToSeconds() float64 {
	switch t.unit {
	case Seconds:
		return t.value
	case Milliseconds:
		return t.value / 1000
	case Microseconds:
		return t.value / 1000000
	case Nanoseconds:
		return t.value / 1000000000
	case Minutes:
		return t.value * 60
	case Hours:
		return t.value * 3600
	case Days:
		return t.value * 86400
	case Weeks:
		return t.value * 604800
	}
	return t.value
}

// Convert converts to another unit.
func (t TimeValue) Convert(to TimeUnit) TimeValue {
	s := t.ToSeconds()
	var value float64
	switch to {
	case Seconds:
		value = s
	case Milliseconds:
		value = s * 1000
	case Microseconds:
		value = s * 1000000
	case Nanoseconds:
		value = s * 1000000000
	case Minutes:
		value = s / 60
	case Hours:
		value = s / 3600
	case Days:
		value = s / 86400
	case Weeks:
		value = s / 604800
	}
	return TimeValue{value: value, unit: to}
}

// DataUnit represents data size units.
type DataUnit int

const (
	Bytes DataUnit = iota
	Kilobytes
	Megabytes
	Gigabytes
	Terabytes
	Kibibytes
	Mebibytes
	Gibibytes
	Tebibytes
)

// DataSize represents a data size with unit.
type DataSize struct {
	value float64
	unit  DataUnit
}

// NewDataSize creates a data size.
func NewDataSize(value float64, unit DataUnit) DataSize {
	return DataSize{value: value, unit: unit}
}

// ToBytes converts to bytes.
func (d DataSize) ToBytes() float64 {
	switch d.unit {
	case Bytes:
		return d.value
	case Kilobytes:
		return d.value * 1000
	case Megabytes:
		return d.value * 1000000
	case Gigabytes:
		return d.value * 1000000000
	case Terabytes:
		return d.value * 1000000000000
	case Kibibytes:
		return d.value * 1024
	case Mebibytes:
		return d.value * 1048576
	case Gibibytes:
		return d.value * 1073741824
	case Tebibytes:
		return d.value * 1099511627776
	}
	return d.value
}

// Convert converts to another unit.
func (d DataSize) Convert(to DataUnit) DataSize {
	b := d.ToBytes()
	var value float64
	switch to {
	case Bytes:
		value = b
	case Kilobytes:
		value = b / 1000
	case Megabytes:
		value = b / 1000000
	case Gigabytes:
		value = b / 1000000000
	case Terabytes:
		value = b / 1000000000000
	case Kibibytes:
		value = b / 1024
	case Mebibytes:
		value = b / 1048576
	case Gibibytes:
		value = b / 1073741824
	case Tebibytes:
		value = b / 1099511627776
	}
	return DataSize{value: value, unit: to}
}
