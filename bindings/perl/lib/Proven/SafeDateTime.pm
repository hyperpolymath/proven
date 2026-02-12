# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

package Proven::SafeDateTime;
use strict;
use warnings;
use Exporter 'import';
use Time::Local qw(timegm timelocal);
use POSIX qw(strftime mktime);

our $VERSION = '0.4.0';
our @EXPORT_OK = qw(
    parse parse_iso8601 format_iso8601 now now_utc
    add_seconds add_minutes add_hours add_days add_months add_years
    diff_seconds diff_days is_leap_year days_in_month
    start_of_day end_of_day start_of_month end_of_month
    to_timestamp from_timestamp format_rfc2822 is_valid_date
);

=head1 NAME

Proven::SafeDateTime - Safe date and time operations without exceptions

=head1 SYNOPSIS

    use Proven::SafeDateTime qw(parse format_iso8601 now add_days);

    my $dt = now();
    my $future = add_days($dt, 30);
    print format_iso8601($future);

=head1 DESCRIPTION

Provides safe date/time operations. All functions return undef
on invalid input instead of throwing exceptions.

DateTime objects are represented as hashrefs with keys:
year, month, day, hour, minute, second, tz_offset

=cut

# DateTime object for OO interface
package Proven::SafeDateTime::DateTime;
use strict;
use warnings;

sub new {
    my ($class, $components) = @_;
    return undef unless ref($components) eq 'HASH';

    # Validate components
    my $year = $components->{year} // 1970;
    my $month = $components->{month} // 1;
    my $day = $components->{day} // 1;
    my $hour = $components->{hour} // 0;
    my $minute = $components->{minute} // 0;
    my $second = $components->{second} // 0;
    my $tz_offset = $components->{tz_offset} // 0;

    return undef if $month < 1 || $month > 12;
    return undef if $day < 1 || $day > 31;
    return undef if $hour < 0 || $hour > 23;
    return undef if $minute < 0 || $minute > 59;
    return undef if $second < 0 || $second > 59;

    return bless {
        year      => int($year),
        month     => int($month),
        day       => int($day),
        hour      => int($hour),
        minute    => int($minute),
        second    => int($second),
        tz_offset => int($tz_offset),
    }, $class;
}

sub year      { shift->{year} }
sub month     { shift->{month} }
sub day       { shift->{day} }
sub hour      { shift->{hour} }
sub minute    { shift->{minute} }
sub second    { shift->{second} }
sub tz_offset { shift->{tz_offset} }

sub to_iso8601 {
    my ($self) = @_;
    return Proven::SafeDateTime::format_iso8601($self);
}

sub to_timestamp {
    my ($self) = @_;
    return Proven::SafeDateTime::to_timestamp($self);
}

sub equals {
    my ($self, $other) = @_;
    return 0 unless ref($other) && $other->isa('Proven::SafeDateTime::DateTime');
    return $self->to_timestamp == $other->to_timestamp;
}

package Proven::SafeDateTime;

=head2 now()

Get the current local time as a DateTime object.

=cut

sub now {
    my @time_parts = localtime(time);
    return Proven::SafeDateTime::DateTime->new({
        year   => $time_parts[5] + 1900,
        month  => $time_parts[4] + 1,
        day    => $time_parts[3],
        hour   => $time_parts[2],
        minute => $time_parts[1],
        second => $time_parts[0],
        tz_offset => _get_tz_offset(),
    });
}

=head2 now_utc()

Get the current UTC time as a DateTime object.

=cut

sub now_utc {
    my @time_parts = gmtime(time);
    return Proven::SafeDateTime::DateTime->new({
        year   => $time_parts[5] + 1900,
        month  => $time_parts[4] + 1,
        day    => $time_parts[3],
        hour   => $time_parts[2],
        minute => $time_parts[1],
        second => $time_parts[0],
        tz_offset => 0,
    });
}

sub _get_tz_offset {
    my $now = time;
    my @local = localtime($now);
    my @gmt = gmtime($now);

    my $offset_seconds = timelocal(@local) - timegm(@gmt);
    return int($offset_seconds / 60);  # Return offset in minutes
}

=head2 parse($string, $format)

Parse a date string with optional format.
Default format is ISO 8601.

=cut

sub parse {
    my ($string, $format) = @_;
    return undef unless defined $string;

    $format //= 'iso8601';

    if ($format eq 'iso8601') {
        return parse_iso8601($string);
    }

    return undef;
}

=head2 parse_iso8601($string)

Parse an ISO 8601 date/time string.
Supports: YYYY-MM-DD, YYYY-MM-DDTHH:MM:SS, with optional timezone

=cut

sub parse_iso8601 {
    my ($string) = @_;
    return undef unless defined $string;

    # Full ISO 8601: 2025-01-15T10:30:45+05:30 or 2025-01-15T10:30:45Z
    if ($string =~ /^(\d{4})-(\d{2})-(\d{2})(?:T(\d{2}):(\d{2}):(\d{2})(?:\.(\d+))?(?:Z|([+-])(\d{2}):?(\d{2}))?)?$/) {
        my ($year, $month, $day, $hour, $minute, $second, $frac, $tz_sign, $tz_hour, $tz_min) =
            ($1, $2, $3, $4 // 0, $5 // 0, $6 // 0, $7 // 0, $8 // '+', $9 // 0, $10 // 0);

        my $tz_offset = 0;
        if (defined $8) {
            $tz_offset = (int($tz_hour) * 60 + int($tz_min));
            $tz_offset = -$tz_offset if $tz_sign eq '-';
        }

        return Proven::SafeDateTime::DateTime->new({
            year      => int($year),
            month     => int($month),
            day       => int($day),
            hour      => int($hour),
            minute    => int($minute),
            second    => int($second),
            tz_offset => $tz_offset,
        });
    }

    return undef;
}

=head2 format_iso8601($datetime)

Format a DateTime object as ISO 8601 string.

=cut

sub format_iso8601 {
    my ($datetime) = @_;
    return undef unless ref($datetime) && $datetime->isa('Proven::SafeDateTime::DateTime');

    my $result = sprintf('%04d-%02d-%02dT%02d:%02d:%02d',
        $datetime->{year}, $datetime->{month}, $datetime->{day},
        $datetime->{hour}, $datetime->{minute}, $datetime->{second});

    if ($datetime->{tz_offset} == 0) {
        $result .= 'Z';
    } else {
        my $sign = $datetime->{tz_offset} >= 0 ? '+' : '-';
        my $abs_offset = abs($datetime->{tz_offset});
        my $tz_hours = int($abs_offset / 60);
        my $tz_minutes = $abs_offset % 60;
        $result .= sprintf('%s%02d:%02d', $sign, $tz_hours, $tz_minutes);
    }

    return $result;
}

=head2 format_rfc2822($datetime)

Format a DateTime object as RFC 2822 string.

=cut

sub format_rfc2822 {
    my ($datetime) = @_;
    return undef unless ref($datetime) && $datetime->isa('Proven::SafeDateTime::DateTime');

    my @day_names = qw(Sun Mon Tue Wed Thu Fri Sat);
    my @month_names = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);

    my $timestamp = to_timestamp($datetime);
    return undef unless defined $timestamp;

    my @tm = gmtime($timestamp);
    my $day_name = $day_names[$tm[6]];
    my $month_name = $month_names[$datetime->{month} - 1];

    my $tz_str = 'GMT';
    if ($datetime->{tz_offset} != 0) {
        my $sign = $datetime->{tz_offset} >= 0 ? '+' : '-';
        my $abs_offset = abs($datetime->{tz_offset});
        $tz_str = sprintf('%s%02d%02d', $sign, int($abs_offset / 60), $abs_offset % 60);
    }

    return sprintf('%s, %02d %s %04d %02d:%02d:%02d %s',
        $day_name, $datetime->{day}, $month_name, $datetime->{year},
        $datetime->{hour}, $datetime->{minute}, $datetime->{second}, $tz_str);
}

=head2 to_timestamp($datetime)

Convert a DateTime object to Unix timestamp.

=cut

sub to_timestamp {
    my ($datetime) = @_;
    return undef unless ref($datetime) && $datetime->isa('Proven::SafeDateTime::DateTime');

    my $result;
    eval {
        $result = timegm(
            $datetime->{second},
            $datetime->{minute},
            $datetime->{hour},
            $datetime->{day},
            $datetime->{month} - 1,
            $datetime->{year} - 1900
        );
        # Adjust for timezone
        $result -= $datetime->{tz_offset} * 60;
    };

    return $@ ? undef : $result;
}

=head2 from_timestamp($timestamp, $utc)

Create a DateTime object from Unix timestamp.
If $utc is true, creates UTC time; otherwise local time.

=cut

sub from_timestamp {
    my ($timestamp, $utc) = @_;
    return undef unless defined $timestamp && $timestamp =~ /^-?\d+$/;

    my @time_parts = $utc ? gmtime($timestamp) : localtime($timestamp);

    return Proven::SafeDateTime::DateTime->new({
        year      => $time_parts[5] + 1900,
        month     => $time_parts[4] + 1,
        day       => $time_parts[3],
        hour      => $time_parts[2],
        minute    => $time_parts[1],
        second    => $time_parts[0],
        tz_offset => $utc ? 0 : _get_tz_offset(),
    });
}

=head2 add_seconds($datetime, $seconds)

Add seconds to a DateTime.

=cut

sub add_seconds {
    my ($datetime, $seconds) = @_;
    return undef unless ref($datetime) && $datetime->isa('Proven::SafeDateTime::DateTime');
    return undef unless defined $seconds && $seconds =~ /^-?\d+$/;

    my $timestamp = to_timestamp($datetime);
    return undef unless defined $timestamp;

    return from_timestamp($timestamp + $seconds, $datetime->{tz_offset} == 0);
}

=head2 add_minutes($datetime, $minutes)

Add minutes to a DateTime.

=cut

sub add_minutes {
    my ($datetime, $minutes) = @_;
    return add_seconds($datetime, $minutes * 60);
}

=head2 add_hours($datetime, $hours)

Add hours to a DateTime.

=cut

sub add_hours {
    my ($datetime, $hours) = @_;
    return add_seconds($datetime, $hours * 3600);
}

=head2 add_days($datetime, $days)

Add days to a DateTime.

=cut

sub add_days {
    my ($datetime, $days) = @_;
    return add_seconds($datetime, $days * 86400);
}

=head2 add_months($datetime, $months)

Add months to a DateTime.

=cut

sub add_months {
    my ($datetime, $months) = @_;
    return undef unless ref($datetime) && $datetime->isa('Proven::SafeDateTime::DateTime');
    return undef unless defined $months && $months =~ /^-?\d+$/;

    my $total_months = ($datetime->{year} * 12 + $datetime->{month} - 1) + $months;
    my $new_year = int($total_months / 12);
    my $new_month = ($total_months % 12) + 1;

    # Handle negative months
    if ($new_month < 1) {
        $new_month += 12;
        $new_year--;
    }

    my $max_day = days_in_month($new_year, $new_month);
    my $new_day = $datetime->{day} > $max_day ? $max_day : $datetime->{day};

    return Proven::SafeDateTime::DateTime->new({
        year      => $new_year,
        month     => $new_month,
        day       => $new_day,
        hour      => $datetime->{hour},
        minute    => $datetime->{minute},
        second    => $datetime->{second},
        tz_offset => $datetime->{tz_offset},
    });
}

=head2 add_years($datetime, $years)

Add years to a DateTime.

=cut

sub add_years {
    my ($datetime, $years) = @_;
    return add_months($datetime, $years * 12);
}

=head2 diff_seconds($datetime1, $datetime2)

Get the difference between two DateTimes in seconds.

=cut

sub diff_seconds {
    my ($datetime1, $datetime2) = @_;

    my $ts1 = to_timestamp($datetime1);
    my $ts2 = to_timestamp($datetime2);

    return undef unless defined $ts1 && defined $ts2;
    return $ts1 - $ts2;
}

=head2 diff_days($datetime1, $datetime2)

Get the difference between two DateTimes in days.

=cut

sub diff_days {
    my ($datetime1, $datetime2) = @_;

    my $seconds = diff_seconds($datetime1, $datetime2);
    return undef unless defined $seconds;
    return int($seconds / 86400);
}

=head2 is_leap_year($year)

Check if a year is a leap year.

=cut

sub is_leap_year {
    my ($year) = @_;
    return 0 unless defined $year && $year =~ /^-?\d+$/;

    return (($year % 4 == 0) && ($year % 100 != 0)) || ($year % 400 == 0) ? 1 : 0;
}

=head2 days_in_month($year, $month)

Get the number of days in a month.

=cut

sub days_in_month {
    my ($year, $month) = @_;
    return undef unless defined $year && defined $month;
    return undef if $month < 1 || $month > 12;

    my @days = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

    if ($month == 2 && is_leap_year($year)) {
        return 29;
    }

    return $days[$month - 1];
}

=head2 start_of_day($datetime)

Get the start of the day (00:00:00).

=cut

sub start_of_day {
    my ($datetime) = @_;
    return undef unless ref($datetime) && $datetime->isa('Proven::SafeDateTime::DateTime');

    return Proven::SafeDateTime::DateTime->new({
        year      => $datetime->{year},
        month     => $datetime->{month},
        day       => $datetime->{day},
        hour      => 0,
        minute    => 0,
        second    => 0,
        tz_offset => $datetime->{tz_offset},
    });
}

=head2 end_of_day($datetime)

Get the end of the day (23:59:59).

=cut

sub end_of_day {
    my ($datetime) = @_;
    return undef unless ref($datetime) && $datetime->isa('Proven::SafeDateTime::DateTime');

    return Proven::SafeDateTime::DateTime->new({
        year      => $datetime->{year},
        month     => $datetime->{month},
        day       => $datetime->{day},
        hour      => 23,
        minute    => 59,
        second    => 59,
        tz_offset => $datetime->{tz_offset},
    });
}

=head2 start_of_month($datetime)

Get the start of the month.

=cut

sub start_of_month {
    my ($datetime) = @_;
    return undef unless ref($datetime) && $datetime->isa('Proven::SafeDateTime::DateTime');

    return Proven::SafeDateTime::DateTime->new({
        year      => $datetime->{year},
        month     => $datetime->{month},
        day       => 1,
        hour      => 0,
        minute    => 0,
        second    => 0,
        tz_offset => $datetime->{tz_offset},
    });
}

=head2 end_of_month($datetime)

Get the end of the month.

=cut

sub end_of_month {
    my ($datetime) = @_;
    return undef unless ref($datetime) && $datetime->isa('Proven::SafeDateTime::DateTime');

    my $last_day = days_in_month($datetime->{year}, $datetime->{month});

    return Proven::SafeDateTime::DateTime->new({
        year      => $datetime->{year},
        month     => $datetime->{month},
        day       => $last_day,
        hour      => 23,
        minute    => 59,
        second    => 59,
        tz_offset => $datetime->{tz_offset},
    });
}

=head2 is_valid_date($year, $month, $day)

Check if a date is valid.

=cut

sub is_valid_date {
    my ($year, $month, $day) = @_;

    return 0 unless defined $year && defined $month && defined $day;
    return 0 if $month < 1 || $month > 12;
    return 0 if $day < 1;

    my $max_day = days_in_month($year, $month);
    return 0 if $day > $max_day;

    return 1;
}

1;

__END__

=head1 AUTHOR

Hyperpolymath

=head1 LICENSE

PMPL-1.0

=cut
