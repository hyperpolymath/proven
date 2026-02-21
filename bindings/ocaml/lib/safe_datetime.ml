(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2025 Hyperpolymath *)

(** Safe date and time operations with timezone awareness. *)

(** Date representation. *)
type date = {
  year: int;
  month: int;  (** 1-12 *)
  day: int;    (** 1-31 *)
}

(** Time representation. *)
type time = {
  hour: int;    (** 0-23 *)
  minute: int;  (** 0-59 *)
  second: int;  (** 0-59 *)
  nanosecond: int;  (** 0-999999999 *)
}

(** DateTime representation. *)
type t = {
  date: date;
  time: time;
  offset_minutes: int;  (** UTC offset in minutes *)
}

type error =
  | Invalid_year
  | Invalid_month
  | Invalid_day
  | Invalid_hour
  | Invalid_minute
  | Invalid_second
  | Invalid_format
  | Overflow

(** Days in each month for a non-leap year. *)
let days_in_month = [| 0; 31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 |]

(** Check if year is a leap year. *)
let is_leap_year year =
  (year mod 4 = 0 && year mod 100 <> 0) || (year mod 400 = 0)

(** Get the number of days in a month. *)
let days_in_month_of year month =
  if month < 1 || month > 12 then 0
  else if month = 2 && is_leap_year year then 29
  else days_in_month.(month)

(** Validate a date. *)
let validate_date year month day =
  if year < 1 || year > 9999 then Error Invalid_year
  else if month < 1 || month > 12 then Error Invalid_month
  else if day < 1 || day > days_in_month_of year month then Error Invalid_day
  else Ok { year; month; day }

(** Validate a time. *)
let validate_time hour minute second nanosecond =
  if hour < 0 || hour > 23 then Error Invalid_hour
  else if minute < 0 || minute > 59 then Error Invalid_minute
  else if second < 0 || second > 59 then Error Invalid_second
  else if nanosecond < 0 || nanosecond > 999999999 then Error Invalid_second
  else Ok { hour; minute; second; nanosecond }

(** Create a datetime. *)
let create ~year ~month ~day ~hour ~minute ~second ?(nanosecond=0) ?(offset_minutes=0) () =
  match validate_date year month day with
  | Error e -> Error e
  | Ok date ->
      match validate_time hour minute second nanosecond with
      | Error e -> Error e
      | Ok time -> Ok { date; time; offset_minutes }

(** Create a date-only value. *)
let of_date ~year ~month ~day =
  match validate_date year month day with
  | Error e -> Error e
  | Ok date -> Ok { date; time = { hour = 0; minute = 0; second = 0; nanosecond = 0 }; offset_minutes = 0 }

(** Get current time (UTC). *)
let now () =
  let tm = Unix.gmtime (Unix.gettimeofday ()) in
  {
    date = { year = tm.tm_year + 1900; month = tm.tm_mon + 1; day = tm.tm_mday };
    time = { hour = tm.tm_hour; minute = tm.tm_min; second = tm.tm_sec; nanosecond = 0 };
    offset_minutes = 0;
  }

(** Get today's date (UTC). *)
let today () =
  let dt = now () in
  dt.date

(** Format date as ISO 8601 (YYYY-MM-DD). *)
let format_date date =
  Printf.sprintf "%04d-%02d-%02d" date.year date.month date.day

(** Format time as ISO 8601 (HH:MM:SS). *)
let format_time time =
  if time.nanosecond = 0 then
    Printf.sprintf "%02d:%02d:%02d" time.hour time.minute time.second
  else
    Printf.sprintf "%02d:%02d:%02d.%09d" time.hour time.minute time.second time.nanosecond

(** Format datetime as ISO 8601. *)
let format dt =
  let date_str = format_date dt.date in
  let time_str = format_time dt.time in
  if dt.offset_minutes = 0 then
    Printf.sprintf "%sT%sZ" date_str time_str
  else
    let sign = if dt.offset_minutes >= 0 then '+' else '-' in
    let abs_offset = abs dt.offset_minutes in
    let hours = abs_offset / 60 in
    let mins = abs_offset mod 60 in
    Printf.sprintf "%sT%s%c%02d:%02d" date_str time_str sign hours mins

(** Format as RFC 3339. *)
let format_rfc3339 = format

(** Format as RFC 2822 (email date format). *)
let format_rfc2822 dt =
  let day_names = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |] in
  let month_names = [| ""; "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
                       "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |] in
  let day_of_week = (dt.date.day + 13 * (dt.date.month + 1) / 5 +
                     dt.date.year + dt.date.year / 4 - dt.date.year / 100 +
                     dt.date.year / 400) mod 7 in
  let tz_str =
    if dt.offset_minutes = 0 then "+0000"
    else
      let sign = if dt.offset_minutes >= 0 then '+' else '-' in
      let abs_offset = abs dt.offset_minutes in
      Printf.sprintf "%c%02d%02d" sign (abs_offset / 60) (abs_offset mod 60)
  in
  Printf.sprintf "%s, %02d %s %04d %02d:%02d:%02d %s"
    day_names.(day_of_week)
    dt.date.day
    month_names.(dt.date.month)
    dt.date.year
    dt.time.hour
    dt.time.minute
    dt.time.second
    tz_str

(** Parse ISO 8601 date. *)
let parse_date str =
  try
    if String.length str < 10 then Error Invalid_format
    else
      let year = int_of_string (String.sub str 0 4) in
      let month = int_of_string (String.sub str 5 2) in
      let day = int_of_string (String.sub str 8 2) in
      validate_date year month day
  with _ -> Error Invalid_format

(** Parse ISO 8601 datetime. *)
let parse str =
  try
    let str = String.trim str in
    if String.length str < 10 then Error Invalid_format
    else
      let date_str = String.sub str 0 10 in
      match parse_date date_str with
      | Error e -> Error e
      | Ok date ->
          if String.length str = 10 then
            Ok { date; time = { hour = 0; minute = 0; second = 0; nanosecond = 0 }; offset_minutes = 0 }
          else if String.length str < 19 then Error Invalid_format
          else
            let hour = int_of_string (String.sub str 11 2) in
            let minute = int_of_string (String.sub str 14 2) in
            let second = int_of_string (String.sub str 17 2) in
            match validate_time hour minute second 0 with
            | Error e -> Error e
            | Ok time ->
                let offset_minutes =
                  if String.length str = 19 then 0
                  else
                    let rest = String.sub str 19 (String.length str - 19) in
                    if String.length rest = 0 then 0
                    else if rest.[0] = 'Z' then 0
                    else if rest.[0] = '+' || rest.[0] = '-' then
                      let sign = if rest.[0] = '+' then 1 else -1 in
                      let hours = int_of_string (String.sub rest 1 2) in
                      let mins =
                        if String.length rest >= 5 then
                          int_of_string (String.sub rest 4 2)
                        else 0
                      in
                      sign * (hours * 60 + mins)
                    else 0
                in
                Ok { date; time; offset_minutes }
  with _ -> Error Invalid_format

(** Convert to Unix timestamp (seconds since epoch). *)
let to_timestamp dt =
  let days_since_epoch =
    let y = dt.date.year - 1970 in
    let leap_years = (dt.date.year - 1) / 4 - (dt.date.year - 1) / 100 + (dt.date.year - 1) / 400 - 477 in
    let days_this_year =
      let rec sum_months m acc =
        if m < dt.date.month then
          sum_months (m + 1) (acc + days_in_month_of dt.date.year m)
        else acc
      in
      sum_months 1 0 + dt.date.day - 1
    in
    y * 365 + leap_years + days_this_year
  in
  let seconds = days_since_epoch * 86400 + dt.time.hour * 3600 + dt.time.minute * 60 + dt.time.second in
  Int64.of_int (seconds - dt.offset_minutes * 60)

(** Create from Unix timestamp. *)
let of_timestamp ?(offset_minutes=0) ts =
  let ts = Int64.add ts (Int64.of_int (offset_minutes * 60)) in
  let days = Int64.to_int (Int64.div ts 86400L) in
  let secs = Int64.to_int (Int64.rem ts 86400L) in
  let secs = if secs < 0 then secs + 86400 else secs in
  let hour = secs / 3600 in
  let minute = (secs mod 3600) / 60 in
  let second = secs mod 60 in
  (* Calculate date from days since epoch *)
  let rec find_year y days =
    let days_in_year = if is_leap_year y then 366 else 365 in
    if days < days_in_year then (y, days)
    else find_year (y + 1) (days - days_in_year)
  in
  let year, day_of_year = find_year 1970 days in
  let rec find_month m days =
    let dim = days_in_month_of year m in
    if days < dim then (m, days + 1)
    else find_month (m + 1) (days - dim)
  in
  let month, day = find_month 1 day_of_year in
  {
    date = { year; month; day };
    time = { hour; minute; second; nanosecond = 0 };
    offset_minutes;
  }

(** Add days to a datetime. *)
let add_days dt days =
  let ts = to_timestamp dt in
  let new_ts = Int64.add ts (Int64.of_int (days * 86400)) in
  of_timestamp ~offset_minutes:dt.offset_minutes new_ts

(** Add hours to a datetime. *)
let add_hours dt hours =
  let ts = to_timestamp dt in
  let new_ts = Int64.add ts (Int64.of_int (hours * 3600)) in
  of_timestamp ~offset_minutes:dt.offset_minutes new_ts

(** Add minutes to a datetime. *)
let add_minutes dt minutes =
  let ts = to_timestamp dt in
  let new_ts = Int64.add ts (Int64.of_int (minutes * 60)) in
  of_timestamp ~offset_minutes:dt.offset_minutes new_ts

(** Add seconds to a datetime. *)
let add_seconds dt seconds =
  let ts = to_timestamp dt in
  let new_ts = Int64.add ts (Int64.of_int seconds) in
  of_timestamp ~offset_minutes:dt.offset_minutes new_ts

(** Get difference between two datetimes in seconds. *)
let diff_seconds a b =
  Int64.sub (to_timestamp a) (to_timestamp b)

(** Get difference in days. *)
let diff_days a b =
  Int64.to_int (Int64.div (diff_seconds a b) 86400L)

(** Compare two datetimes. *)
let compare a b =
  Int64.compare (to_timestamp a) (to_timestamp b)

(** Check if a is before b. *)
let is_before a b = compare a b < 0

(** Check if a is after b. *)
let is_after a b = compare a b > 0

(** Convert to UTC. *)
let to_utc dt =
  { (of_timestamp (to_timestamp dt)) with offset_minutes = 0 }

(** Convert to a different timezone offset. *)
let with_offset dt offset_minutes =
  of_timestamp ~offset_minutes (to_timestamp dt)

(** Get day of week (0 = Sunday, 6 = Saturday). *)
let day_of_week dt =
  let d = dt.date.day in
  let m = if dt.date.month < 3 then dt.date.month + 12 else dt.date.month in
  let y = if dt.date.month < 3 then dt.date.year - 1 else dt.date.year in
  (d + 13 * (m + 1) / 5 + y + y / 4 - y / 100 + y / 400) mod 7

(** Check if date is a weekend. *)
let is_weekend dt =
  let dow = day_of_week dt in
  dow = 0 || dow = 6

(** Get start of day. *)
let start_of_day dt =
  { dt with time = { hour = 0; minute = 0; second = 0; nanosecond = 0 } }

(** Get end of day. *)
let end_of_day dt =
  { dt with time = { hour = 23; minute = 59; second = 59; nanosecond = 999999999 } }

(** Get start of month. *)
let start_of_month dt =
  { dt with date = { dt.date with day = 1 };
            time = { hour = 0; minute = 0; second = 0; nanosecond = 0 } }

(** Get end of month. *)
let end_of_month dt =
  let last_day = days_in_month_of dt.date.year dt.date.month in
  { dt with date = { dt.date with day = last_day };
            time = { hour = 23; minute = 59; second = 59; nanosecond = 999999999 } }
