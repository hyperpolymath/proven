// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

open Proven_Bitwise

/**
 * SafeCron - Safe cron expression parsing and validation.
 *
 * Provides functions for parsing, validating, and working with cron expressions.
 * Supports standard 5-field cron format: minute hour day-of-month month day-of-week.
 * All operations are bounds-checked and cannot crash.
 */

/** Cron error types */
type cronError =
  | InvalidFieldCount
  | ValueOutOfRange
  | InvalidStep
  | InvalidRange
  | MalformedField
  | InvalidDayName
  | InvalidMonthName

/** Cron field range */
type fieldRange = {
  min: int,
  max: int,
}

/** Standard cron field definitions */
let minuteRange: fieldRange = {min: 0, max: 59}
let hourRange: fieldRange = {min: 0, max: 23}
let dayOfMonthRange: fieldRange = {min: 1, max: 31}
let monthRange: fieldRange = {min: 1, max: 12}
let dayOfWeekRange: fieldRange = {min: 0, max: 6} // 0 = Sunday

/** Parsed cron expression using bitmasks for efficient matching */
type cronExpression = {
  minutes: float, // 64-bit bitmask for minutes 0-59
  hours: int, // 32-bit bitmask for hours 0-23
  daysOfMonth: int, // 32-bit bitmask for days 1-31
  months: int, // 16-bit bitmask for months 1-12
  daysOfWeek: int, // 8-bit bitmask for days 0-6 (Sunday = 0)
}

/** Day-of-week names for parsing */
let dayNames = ["sun", "mon", "tue", "wed", "thu", "fri", "sat"]

/** Month names for parsing */
let monthNames = ["jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"]

/** Check if a specific time matches a cron expression */
let matches = (
  cronExpr: cronExpression,
  ~minute: int,
  ~hour: int,
  ~day: int,
  ~month: int,
  ~dayOfWeek: int,
): bool => {
  if minute < 0 || minute > 59 {
    false
  } else if hour < 0 || hour > 23 {
    false
  } else if day < 1 || day > 31 {
    false
  } else if month < 1 || month > 12 {
    false
  } else if dayOfWeek < 0 || dayOfWeek > 6 {
    false
  } else {
    // Check each field using bitmask operations
    let minuteBit = Js.Math.pow_float(~base=2.0, ~exp=Belt.Int.toFloat(minute))
    let minuteMatch = mod(Belt.Float.toInt(cronExpr.minutes /. minuteBit), 2) == 1

    let hourMask = lsl(1, hour)
    let hourMatch = land(cronExpr.hours, hourMask) != 0

    let dayMask = lsl(1, day)
    let dayMatch = land(cronExpr.daysOfMonth, dayMask) != 0

    let monthMask = lsl(1, month)
    let monthMatch = land(cronExpr.months, monthMask) != 0

    let dowMask = lsl(1, dayOfWeek)
    let dowMatch = land(cronExpr.daysOfWeek, dowMask) != 0

    minuteMatch && hourMatch && dayMatch && monthMatch && dowMatch
  }
}

/** Check if expression runs every minute */
let isEveryMinute = (cronExpr: cronExpression): bool => {
  // All 60 minutes, all 24 hours, all 31 days, all 12 months, all 7 days
  let allMinutes = Js.Math.pow_float(~base=2.0, ~exp=60.0) -. 1.0
  let allHours = lsl(1, 24) - 1
  let allDays = lsl(1, 32) - 2 // bits 1-31 (bit 0 unused)
  let allMonths = lsl(1, 13) - 2 // bits 1-12 (bit 0 unused)
  let allDaysOfWeek = lsl(1, 7) - 1

  cronExpr.minutes == allMinutes &&
  cronExpr.hours == allHours &&
  cronExpr.daysOfMonth == allDays &&
  cronExpr.months == allMonths &&
  cronExpr.daysOfWeek == allDaysOfWeek
}

/** Count set bits in an integer */
let popCount = (n: int): int => {
  let count = ref(0)
  let value = ref(n)
  while value.contents > 0 {
    count := count.contents + land(value.contents, 1)
    value := lsr(value.contents, 1)
  }
  count.contents
}

/** Count set bits in a float (used for minutes) */
let popCountFloat = (n: float): int => {
  let count = ref(0)
  let value = ref(n)
  while value.contents > 0.0 {
    if mod(Belt.Float.toInt(value.contents), 2) == 1 {
      count := count.contents + 1
    }
    value := Js.Math.floor_float(value.contents /. 2.0)
  }
  count.contents
}

/** Count matching minutes in expression */
let countMinutes = (cronExpr: cronExpression): int => {
  popCountFloat(cronExpr.minutes)
}

/** Count matching hours in expression */
let countHours = (cronExpr: cronExpression): int => {
  popCount(cronExpr.hours)
}

/** Parse a value (number or name) within range */
let parseValue = (
  value: string,
  range: fieldRange,
  names: option<array<string>>,
): result<int, cronError> => {
  // Try to parse as number
  switch Belt.Int.fromString(value) {
  | Some(num) =>
    if num < range.min || num > range.max {
      Error(ValueOutOfRange)
    } else {
      Ok(num)
    }
  | None =>
    // Try to parse as name
    switch names {
    | None => Error(MalformedField)
    | Some(nameList) =>
      let lower = Js.String2.toLowerCase(value)
      let shortName = if Js.String2.length(lower) >= 3 {
        Js.String2.slice(lower, ~from=0, ~to_=3)
      } else {
        lower
      }
      let index = Belt.Array.getIndexBy(nameList, name => name == shortName)
      switch index {
      | None =>
        if range.min == 1 {
          Error(InvalidMonthName)
        } else {
          Error(InvalidDayName)
        }
      | Some(idx) =>
        // Adjust for 1-indexed months
        let resultValue = if range.min == 1 { idx + 1 } else { idx }
        Ok(resultValue)
      }
    }
  }
}

/** Parse a single cron field item (not comma-separated) */
let parseFieldItem = (
  item: string,
  range: fieldRange,
  names: option<array<string>>,
): result<array<int>, cronError> => {
  if Js.String2.length(item) == 0 {
    Error(MalformedField)
  } else {
    // Check for step
    let (mainPart, step) = switch Js.String2.indexOf(item, "/") {
    | -1 => (item, 1)
    | slashPos =>
      let stepPart = Js.String2.sliceToEnd(item, ~from=slashPos + 1)
      switch Belt.Int.fromString(stepPart) {
      | None => (item, 0) // Will error below
      | Some(s) =>
        if s == 0 {
          (item, 0) // Will error below
        } else {
          (Js.String2.slice(item, ~from=0, ~to_=slashPos), s)
        }
      }
    }

    if step == 0 {
      Error(InvalidStep)
    } else {
      let (rangeStart, rangeEnd) = if mainPart == "*" {
        (range.min, range.max)
      } else {
        switch Js.String2.indexOf(mainPart, "-") {
        | -1 =>
          // Single value
          switch parseValue(mainPart, range, names) {
          | Ok(v) => (v, v)
          | Error(e) => (-1, -1) // Will return error
          }
        | dashPos =>
          // Range
          let startPart = Js.String2.slice(mainPart, ~from=0, ~to_=dashPos)
          let endPart = Js.String2.sliceToEnd(mainPart, ~from=dashPos + 1)
          switch (parseValue(startPart, range, names), parseValue(endPart, range, names)) {
          | (Ok(s), Ok(e)) => (s, e)
          | _ => (-1, -1)
          }
        }
      }

      if rangeStart == -1 {
        Error(MalformedField)
      } else if rangeStart > rangeEnd {
        Error(InvalidRange)
      } else {
        // Build array of values
        let values = ref([])
        let current = ref(rangeStart)
        while current.contents <= rangeEnd {
          values := Belt.Array.concat(values.contents, [current.contents])
          current := current.contents + step
        }
        Ok(values.contents)
      }
    }
  }
}

/** Parse a single cron field into values */
let parseField = (
  field: string,
  range: fieldRange,
  names: option<array<string>>,
): result<array<int>, cronError> => {
  // Handle comma-separated list
  let items = Js.String2.split(field, ",")
  let allValues = ref([])
  let error = ref(None)

  Belt.Array.forEach(items, item => {
    if error.contents == None {
      switch parseFieldItem(item, range, names) {
      | Ok(values) => allValues := Belt.Array.concat(allValues.contents, values)
      | Error(e) => error := Some(e)
      }
    }
  })

  switch error.contents {
  | Some(e) => Error(e)
  | None => Ok(allValues.contents)
  }
}

/** Convert array of values to bitmask (as float for 64-bit) */
let valuesToBitmaskFloat = (values: array<int>): float => {
  Belt.Array.reduce(values, 0.0, (acc, v) => {
    acc +. Js.Math.pow_float(~base=2.0, ~exp=Belt.Int.toFloat(v))
  })
}

/** Convert array of values to bitmask (as int for 32-bit) */
let valuesToBitmask = (values: array<int>): int => {
  Belt.Array.reduce(values, 0, (acc, v) => {
    lor(acc, lsl(1, v))
  })
}

/** Parse a cron expression string.
 *
 * Supports standard 5-field format: minute hour day-of-month month day-of-week
 * Each field can contain:
 * - `*` - all values
 * - `N` - specific value
 * - `N-M` - range
 * - `*\/N` - step from start
 * - `N-M/S` - step within range
 * - `N,M,O` - list of values
 */
let parse = (expression: string): result<cronExpression, cronError> => {
  let parts = Js.String2.split(expression, " ")
  let fields = Belt.Array.keep(parts, part => Js.String2.length(part) > 0)

  if Belt.Array.length(fields) != 5 {
    Error(InvalidFieldCount)
  } else {
    let minuteField = Belt.Array.getExn(fields, 0)
    let hourField = Belt.Array.getExn(fields, 1)
    let dayField = Belt.Array.getExn(fields, 2)
    let monthField = Belt.Array.getExn(fields, 3)
    let dowField = Belt.Array.getExn(fields, 4)

    switch parseField(minuteField, minuteRange, None) {
    | Error(e) => Error(e)
    | Ok(minuteValues) =>
      switch parseField(hourField, hourRange, None) {
      | Error(e) => Error(e)
      | Ok(hourValues) =>
        switch parseField(dayField, dayOfMonthRange, None) {
        | Error(e) => Error(e)
        | Ok(dayValues) =>
          switch parseField(monthField, monthRange, Some(monthNames)) {
          | Error(e) => Error(e)
          | Ok(monthValues) =>
            switch parseField(dowField, dayOfWeekRange, Some(dayNames)) {
            | Error(e) => Error(e)
            | Ok(dowValues) =>
              Ok({
                minutes: valuesToBitmaskFloat(minuteValues),
                hours: valuesToBitmask(hourValues),
                daysOfMonth: valuesToBitmask(dayValues),
                months: valuesToBitmask(monthValues),
                daysOfWeek: valuesToBitmask(dowValues),
              })
            }
          }
        }
      }
    }
  }
}

/** Validate a cron expression without fully parsing it */
let isValid = (expression: string): bool => {
  switch parse(expression) {
  | Ok(_) => true
  | Error(_) => false
  }
}

/** Common cron expression presets */
module Presets = {
  /** Every minute: `* * * * *` */
  let everyMinute = "* * * * *"
  /** Every hour at minute 0: `0 * * * *` */
  let hourly = "0 * * * *"
  /** Every day at midnight: `0 0 * * *` */
  let daily = "0 0 * * *"
  /** Every Sunday at midnight: `0 0 * * 0` */
  let weekly = "0 0 * * 0"
  /** First of every month at midnight: `0 0 1 * *` */
  let monthly = "0 0 1 * *"
  /** First of January at midnight: `0 0 1 1 *` */
  let yearly = "0 0 1 1 *"
}

/** Parse a preset or custom expression */
let parseOrPreset = (expression: string): result<cronExpression, cronError> => {
  // Handle @-style presets
  if Js.String2.length(expression) > 0 && Js.String2.charAt(expression, 0) == "@" {
    let preset = Js.String2.sliceToEnd(expression, ~from=1)
    switch preset {
    | "yearly" | "annually" => parse(Presets.yearly)
    | "monthly" => parse(Presets.monthly)
    | "weekly" => parse(Presets.weekly)
    | "daily" | "midnight" => parse(Presets.daily)
    | "hourly" => parse(Presets.hourly)
    | _ => Error(MalformedField)
    }
  } else {
    parse(expression)
  }
}

/** Get a human-readable description of a cron expression */
let describe = (cronExpr: cronExpression): string => {
  if isEveryMinute(cronExpr) {
    "every minute"
  } else {
    let minuteCount = countMinutes(cronExpr)
    let hourCount = countHours(cronExpr)

    if minuteCount == 1 && hourCount == 24 {
      "once per hour"
    } else if minuteCount == 1 && hourCount == 1 {
      "once per day"
    } else {
      "custom schedule"
    }
  }
}

/** Create a cron expression for running at a specific minute of every hour */
let everyHourAt = (minute: int): result<cronExpression, cronError> => {
  if minute < 0 || minute > 59 {
    Error(ValueOutOfRange)
  } else {
    parse(`${Belt.Int.toString(minute)} * * * *`)
  }
}

/** Create a cron expression for running at a specific time every day */
let everyDayAt = (hour: int, minute: int): result<cronExpression, cronError> => {
  if hour < 0 || hour > 23 {
    Error(ValueOutOfRange)
  } else if minute < 0 || minute > 59 {
    Error(ValueOutOfRange)
  } else {
    parse(`${Belt.Int.toString(minute)} ${Belt.Int.toString(hour)} * * *`)
  }
}

/** Create a cron expression for running every N minutes */
let everyNMinutes = (n: int): result<cronExpression, cronError> => {
  if n < 1 || n > 59 {
    Error(InvalidStep)
  } else {
    parse(`*/${Belt.Int.toString(n)} * * * *`)
  }
}

/** Create a cron expression for running every N hours */
let everyNHours = (n: int): result<cronExpression, cronError> => {
  if n < 1 || n > 23 {
    Error(InvalidStep)
  } else {
    parse(`0 */${Belt.Int.toString(n)} * * *`)
  }
}

/** Create a cron expression for running on specific days of the week */
let onDaysOfWeek = (days: array<int>, hour: int, minute: int): result<cronExpression, cronError> => {
  if hour < 0 || hour > 23 {
    Error(ValueOutOfRange)
  } else if minute < 0 || minute > 59 {
    Error(ValueOutOfRange)
  } else if Belt.Array.length(days) == 0 {
    Error(MalformedField)
  } else {
    let invalidDay = Belt.Array.getBy(days, d => d < 0 || d > 6)
    switch invalidDay {
    | Some(_) => Error(ValueOutOfRange)
    | None =>
      let dayList = Js.Array2.joinWith(Belt.Array.map(days, Belt.Int.toString), ",")
      parse(`${Belt.Int.toString(minute)} ${Belt.Int.toString(hour)} * * ${dayList}`)
    }
  }
}

/** Check if a Date matches the cron expression */
let matchesDate = (cronExpr: cronExpression, date: Js.Date.t): bool => {
  let minute = Js.Date.getMinutes(date)->Belt.Float.toInt
  let hour = Js.Date.getHours(date)->Belt.Float.toInt
  let day = Js.Date.getDate(date)->Belt.Float.toInt
  let month = Js.Date.getMonth(date)->Belt.Float.toInt + 1
  let dayOfWeek = Js.Date.getDay(date)->Belt.Float.toInt

  matches(cronExpr, ~minute, ~hour, ~day, ~month, ~dayOfWeek)
}
