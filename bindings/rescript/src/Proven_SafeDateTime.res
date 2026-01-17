// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeDateTime - Safe date and time operations that cannot crash.
 *
 * Provides safe date parsing, formatting, and manipulation.
 */

/** DateTime representation */
type dateTime = {
  year: int,
  month: int,
  day: int,
  hour: int,
  minute: int,
  second: int,
  millisecond: int,
}

/** Create a new DateTime with validation */
let make = (
  ~year: int,
  ~month: int,
  ~day: int,
  ~hour: int=0,
  ~minute: int=0,
  ~second: int=0,
  ~millisecond: int=0,
  (),
): option<dateTime> => {
  // Validate ranges
  if month < 1 || month > 12 {
    None
  } else if day < 1 || day > 31 {
    None
  } else if hour < 0 || hour > 23 {
    None
  } else if minute < 0 || minute > 59 {
    None
  } else if second < 0 || second > 59 {
    None
  } else if millisecond < 0 || millisecond > 999 {
    None
  } else {
    // Check days in month
    let daysInMonth = switch month {
    | 1 | 3 | 5 | 7 | 8 | 10 | 12 => 31
    | 4 | 6 | 9 | 11 => 30
    | 2 =>
      // Leap year check
      let isLeap = mod(year, 4) == 0 && (mod(year, 100) != 0 || mod(year, 400) == 0)
      if isLeap {
        29
      } else {
        28
      }
    | _ => 0
    }
    if day > daysInMonth {
      None
    } else {
      Some({year, month, day, hour, minute, second, millisecond})
    }
  }
}

/** Parse ISO 8601 date string */
let parseIso = (isoString: string): option<dateTime> => {
  try {
    let date = Js.Date.fromString(isoString)
    if Js.Float.isNaN(Js.Date.getTime(date)) {
      None
    } else {
      make(
        ~year=Js.Date.getFullYear(date)->Belt.Float.toInt,
        ~month=Js.Date.getMonth(date)->Belt.Float.toInt + 1,
        ~day=Js.Date.getDate(date)->Belt.Float.toInt,
        ~hour=Js.Date.getHours(date)->Belt.Float.toInt,
        ~minute=Js.Date.getMinutes(date)->Belt.Float.toInt,
        ~second=Js.Date.getSeconds(date)->Belt.Float.toInt,
        ~millisecond=Js.Date.getMilliseconds(date)->Belt.Float.toInt,
        (),
      )
    }
  } catch {
  | _ => None
  }
}

/** Get current date and time */
let now = (): dateTime => {
  let date = Js.Date.make()
  {
    year: Js.Date.getFullYear(date)->Belt.Float.toInt,
    month: Js.Date.getMonth(date)->Belt.Float.toInt + 1,
    day: Js.Date.getDate(date)->Belt.Float.toInt,
    hour: Js.Date.getHours(date)->Belt.Float.toInt,
    minute: Js.Date.getMinutes(date)->Belt.Float.toInt,
    second: Js.Date.getSeconds(date)->Belt.Float.toInt,
    millisecond: Js.Date.getMilliseconds(date)->Belt.Float.toInt,
  }
}

/** Convert to ISO 8601 string */
let toIso = (dt: dateTime): string => {
  let pad2 = n => {
    if n < 10 {
      "0" ++ Belt.Int.toString(n)
    } else {
      Belt.Int.toString(n)
    }
  }
  let pad3 = n => {
    if n < 10 {
      "00" ++ Belt.Int.toString(n)
    } else if n < 100 {
      "0" ++ Belt.Int.toString(n)
    } else {
      Belt.Int.toString(n)
    }
  }
  `${Belt.Int.toString(dt.year)}-${pad2(dt.month)}-${pad2(dt.day)}T${pad2(dt.hour)}:${pad2(
      dt.minute,
    )}:${pad2(dt.second)}.${pad3(dt.millisecond)}Z`
}

/** Convert to Unix timestamp (milliseconds) */
let toTimestamp = (dt: dateTime): float => {
  let jsDate = Js.Date.makeWithYMDHMS(
    ~year=Belt.Int.toFloat(dt.year),
    ~month=Belt.Int.toFloat(dt.month - 1),
    ~date=Belt.Int.toFloat(dt.day),
    ~hours=Belt.Int.toFloat(dt.hour),
    ~minutes=Belt.Int.toFloat(dt.minute),
    ~seconds=Belt.Int.toFloat(dt.second),
    (),
  )
  Js.Date.getTime(jsDate)
}

/** Create from Unix timestamp (milliseconds) */
let fromTimestamp = (timestamp: float): option<dateTime> => {
  if Js.Float.isNaN(timestamp) || !Js.Float.isFinite(timestamp) {
    None
  } else {
    let date = Js.Date.fromFloat(timestamp)
    make(
      ~year=Js.Date.getFullYear(date)->Belt.Float.toInt,
      ~month=Js.Date.getMonth(date)->Belt.Float.toInt + 1,
      ~day=Js.Date.getDate(date)->Belt.Float.toInt,
      ~hour=Js.Date.getHours(date)->Belt.Float.toInt,
      ~minute=Js.Date.getMinutes(date)->Belt.Float.toInt,
      ~second=Js.Date.getSeconds(date)->Belt.Float.toInt,
      ~millisecond=Js.Date.getMilliseconds(date)->Belt.Float.toInt,
      (),
    )
  }
}

/** Add days to a date */
let addDays = (dt: dateTime, days: int): dateTime => {
  let timestamp = toTimestamp(dt) +. Belt.Int.toFloat(days) *. 86400000.0
  fromTimestamp(timestamp)->Belt.Option.getWithDefault(dt)
}

/** Add hours to a date */
let addHours = (dt: dateTime, hours: int): dateTime => {
  let timestamp = toTimestamp(dt) +. Belt.Int.toFloat(hours) *. 3600000.0
  fromTimestamp(timestamp)->Belt.Option.getWithDefault(dt)
}

/** Add minutes to a date */
let addMinutes = (dt: dateTime, minutes: int): dateTime => {
  let timestamp = toTimestamp(dt) +. Belt.Int.toFloat(minutes) *. 60000.0
  fromTimestamp(timestamp)->Belt.Option.getWithDefault(dt)
}

/** Add seconds to a date */
let addSeconds = (dt: dateTime, seconds: int): dateTime => {
  let timestamp = toTimestamp(dt) +. Belt.Int.toFloat(seconds) *. 1000.0
  fromTimestamp(timestamp)->Belt.Option.getWithDefault(dt)
}

/** Compare two dates: -1 if a < b, 0 if equal, 1 if a > b */
let compare = (a: dateTime, b: dateTime): int => {
  let tsA = toTimestamp(a)
  let tsB = toTimestamp(b)
  if tsA < tsB {
    -1
  } else if tsA > tsB {
    1
  } else {
    0
  }
}

/** Check if date a is before date b */
let isBefore = (a: dateTime, b: dateTime): bool => compare(a, b) < 0

/** Check if date a is after date b */
let isAfter = (a: dateTime, b: dateTime): bool => compare(a, b) > 0

/** Check if two dates are equal */
let isEqual = (a: dateTime, b: dateTime): bool => compare(a, b) == 0

/** Get the difference in milliseconds */
let diffMilliseconds = (a: dateTime, b: dateTime): float => {
  toTimestamp(a) -. toTimestamp(b)
}

/** Get the difference in seconds */
let diffSeconds = (a: dateTime, b: dateTime): float => {
  diffMilliseconds(a, b) /. 1000.0
}

/** Get the difference in minutes */
let diffMinutes = (a: dateTime, b: dateTime): float => {
  diffMilliseconds(a, b) /. 60000.0
}

/** Get the difference in hours */
let diffHours = (a: dateTime, b: dateTime): float => {
  diffMilliseconds(a, b) /. 3600000.0
}

/** Get the difference in days */
let diffDays = (a: dateTime, b: dateTime): float => {
  diffMilliseconds(a, b) /. 86400000.0
}

/** Check if a year is a leap year */
let isLeapYear = (year: int): bool => {
  mod(year, 4) == 0 && (mod(year, 100) != 0 || mod(year, 400) == 0)
}

/** Get the day of week (0 = Sunday, 6 = Saturday) */
let dayOfWeek = (dt: dateTime): int => {
  let jsDate = Js.Date.makeWithYMD(
    ~year=Belt.Int.toFloat(dt.year),
    ~month=Belt.Int.toFloat(dt.month - 1),
    ~date=Belt.Int.toFloat(dt.day),
    (),
  )
  Js.Date.getDay(jsDate)->Belt.Float.toInt
}

/** Get the day of year (1-366) */
let dayOfYear = (dt: dateTime): int => {
  let daysInMonths = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  let feb = if isLeapYear(dt.year) {
    29
  } else {
    28
  }
  let days = ref(dt.day)
  for i in 0 to dt.month - 2 {
    let monthDays = if i == 1 {
      feb
    } else {
      Belt.Array.getUnsafe(daysInMonths, i)
    }
    days := days.contents + monthDays
  }
  days.contents
}
