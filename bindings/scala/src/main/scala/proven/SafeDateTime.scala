// SPDX-License-Identifier: PMPL-1.0

package proven

import scala.util.Try

/**
 * Parsed date components.
 */
case class Date(year: Int, month: Int, day: Int):
  require(month >= 1 && month <= 12, "Month must be 1-12")
  require(day >= 1 && day <= SafeDateTime.daysInMonth(year, month).getOrElse(31), "Invalid day for month")

  /**
   * Format as ISO 8601 date (YYYY-MM-DD).
   */
  def formatIso: String = f"$year%04d-$month%02d-$day%02d"

  /**
   * Check if this date is a weekend.
   */
  def isWeekend: Boolean =
    val dayOfWeek = SafeDateTime.dayOfWeek(year, month, day)
    dayOfWeek == 0 || dayOfWeek == 6

  /**
   * Add days to this date.
   */
  def plusDays(days: Int): Date =
    SafeDateTime.addDays(this, days)

  override def toString: String = formatIso

/**
 * Parsed time components.
 */
case class Time(hour: Int, minute: Int, second: Int, nanos: Int = 0):
  require(hour >= 0 && hour <= 23, "Hour must be 0-23")
  require(minute >= 0 && minute <= 59, "Minute must be 0-59")
  require(second >= 0 && second <= 59, "Second must be 0-59")
  require(nanos >= 0 && nanos < 1000000000, "Nanos must be 0-999999999")

  /**
   * Format as ISO 8601 time (HH:MM:SS).
   */
  def formatIso: String =
    if nanos == 0 then f"$hour%02d:$minute%02d:$second%02d"
    else f"$hour%02d:$minute%02d:$second%02d.${nanos}%09d".stripSuffix("0" * (9 - nanos.toString.length))

  /**
   * Convert to total seconds since midnight.
   */
  def toSeconds: Int = hour * 3600 + minute * 60 + second

  override def toString: String = formatIso

/**
 * Combined date and time.
 */
case class DateTime(date: Date, time: Time):
  /**
   * Format as ISO 8601 datetime.
   */
  def formatIso: String = s"${date.formatIso}T${time.formatIso}"

  override def toString: String = formatIso

/**
 * Safe datetime parsing and validation.
 */
object SafeDateTime:

  /**
   * Check if a year is a leap year.
   */
  def isLeapYear(year: Int): Boolean =
    (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0)

  /**
   * Get the number of days in a month.
   */
  def daysInMonth(year: Int, month: Int): Option[Int] = month match
    case 1 | 3 | 5 | 7 | 8 | 10 | 12 => Some(31)
    case 4 | 6 | 9 | 11 => Some(30)
    case 2 => Some(if isLeapYear(year) then 29 else 28)
    case _ => None

  /**
   * Validate and create a date.
   */
  def validateDate(year: Int, month: Int, day: Int): Either[String, Date] =
    if month < 1 || month > 12 then
      Left(s"Invalid month: $month")
    else
      daysInMonth(year, month) match
        case Some(maxDay) if day >= 1 && day <= maxDay =>
          Right(Date(year, month, day))
        case Some(maxDay) =>
          Left(s"Invalid day $day for month $month (max: $maxDay)")
        case None =>
          Left(s"Invalid month: $month")

  /**
   * Validate and create a time.
   */
  def validateTime(hour: Int, minute: Int, second: Int): Either[String, Time] =
    if hour < 0 || hour > 23 then Left(s"Invalid hour: $hour")
    else if minute < 0 || minute > 59 then Left(s"Invalid minute: $minute")
    else if second < 0 || second > 59 then Left(s"Invalid second: $second")
    else Right(Time(hour, minute, second))

  /**
   * Parse ISO 8601 date (YYYY-MM-DD).
   */
  def parseIsoDate(s: String): Either[String, Date] =
    val parts = s.trim.split("-")
    if parts.length != 3 then
      Left("Invalid date format, expected YYYY-MM-DD")
    else
      for
        year <- Try(parts(0).toInt).toEither.left.map(_ => "Invalid year")
        month <- Try(parts(1).toInt).toEither.left.map(_ => "Invalid month")
        day <- Try(parts(2).toInt).toEither.left.map(_ => "Invalid day")
        date <- validateDate(year, month, day)
      yield date

  /**
   * Parse ISO 8601 time (HH:MM:SS or HH:MM:SS.nnn).
   */
  def parseIsoTime(s: String): Either[String, Time] =
    val (timePart, nanos) = s.trim.indexOf('.') match
      case -1 => (s.trim, 0)
      case i =>
        val nanosStr = s.substring(i + 1).padTo(9, '0').take(9)
        (s.substring(0, i), Try(nanosStr.toInt).getOrElse(0))

    val parts = timePart.split(":")
    if parts.length < 2 || parts.length > 3 then
      Left("Invalid time format, expected HH:MM or HH:MM:SS")
    else
      for
        hour <- Try(parts(0).toInt).toEither.left.map(_ => "Invalid hour")
        minute <- Try(parts(1).toInt).toEither.left.map(_ => "Invalid minute")
        second <- if parts.length > 2 then
          Try(parts(2).toInt).toEither.left.map(_ => "Invalid second")
        else Right(0)
        time <- validateTime(hour, minute, second).map(_.copy(nanos = nanos))
      yield time

  /**
   * Parse ISO 8601 datetime (YYYY-MM-DDTHH:MM:SS).
   */
  def parseIsoDateTime(s: String): Either[String, DateTime] =
    val parts = s.trim.split("[T ]")
    if parts.length != 2 then
      Left("Invalid datetime format, expected YYYY-MM-DDTHH:MM:SS")
    else
      for
        date <- parseIsoDate(parts(0))
        time <- parseIsoTime(parts(1).takeWhile(c => c != 'Z' && c != '+' && c != '-'))
      yield DateTime(date, time)

  /**
   * Get day of week (0 = Sunday, 6 = Saturday) using Zeller's congruence.
   */
  def dayOfWeek(year: Int, month: Int, day: Int): Int =
    val m = if month < 3 then month + 12 else month
    val y = if month < 3 then year - 1 else year
    val k = y % 100
    val j = y / 100
    val h = (day + (13 * (m + 1)) / 5 + k + k / 4 + j / 4 - 2 * j) % 7
    ((h + 6) % 7) // Convert to Sunday = 0

  /**
   * Add days to a date.
   */
  def addDays(date: Date, days: Int): Date =
    var year = date.year
    var month = date.month
    var day = date.day + days

    while day > daysInMonth(year, month).getOrElse(28) do
      day -= daysInMonth(year, month).getOrElse(28)
      month += 1
      if month > 12 then
        month = 1
        year += 1

    while day < 1 do
      month -= 1
      if month < 1 then
        month = 12
        year -= 1
      day += daysInMonth(year, month).getOrElse(28)

    Date(year, month, day)

  /**
   * Calculate days between two dates.
   */
  def daysBetween(d1: Date, d2: Date): Int =
    toJulianDay(d2) - toJulianDay(d1)

  /**
   * Convert to Julian day number for calculations.
   */
  private def toJulianDay(date: Date): Int =
    val a = (14 - date.month) / 12
    val y = date.year + 4800 - a
    val m = date.month + 12 * a - 3
    date.day + (153 * m + 2) / 5 + 365 * y + y / 4 - y / 100 + y / 400 - 32045

  /**
   * Check if a date is valid.
   */
  def isValidDate(year: Int, month: Int, day: Int): Boolean =
    validateDate(year, month, day).isRight

  /**
   * Check if a time is valid.
   */
  def isValidTime(hour: Int, minute: Int, second: Int): Boolean =
    validateTime(hour, minute, second).isRight

  /**
   * Get the current date (system timezone).
   */
  def today: Date =
    val now = java.time.LocalDate.now()
    Date(now.getYear, now.getMonthValue, now.getDayOfMonth)

  /**
   * Get the current time (system timezone).
   */
  def now: Time =
    val t = java.time.LocalTime.now()
    Time(t.getHour, t.getMinute, t.getSecond, t.getNano)
