module:    Extracting
synopsis:  Common-Dylan compliance for the Date module
author:    Douglas M. Auclair
copyright: LGPL, 2000

/// This implementation module defines the dates and times.
/// It conforms to section 7.5 of the Date Module description
/// of the "System and I/O" document from Functional Objects, Inc.

define function decode-date(d :: <date>)
 => (year :: <years>, month :: <month>, day :: <days>, hours :: <hours>,
     minutes :: <minutes>, seconds :: <seconds>, day-of-week :: <day-of-week>,
     time-zone-offset :: <timezone>)
  values(date-year(d), date-month(d), date-day(d), date-hours(d),
	 date-minutes(d), date-seconds(d), date-day-of-week(d),
	 date-time-zone-offset(d));
end function decode-date;

define function date-year(d :: <date>) => (year :: <years>)
  d.time.year;
end function date-year;

define function date-month(d :: <date>) => (month :: <month>)
  d.time.month;
end function date-month;

define function date-day(d :: <date>) => (day :: <days>)
  d.time.day-of-month;
end function date-day;

define function date-day-of-week(d :: <date>) => (day :: <day-of-week>)
  *day*[d.time.day-of-week];
end function date-day-of-week;

define function date-hours(d :: <date>) => (hours :: <hours>)
  d.time.hours;
end function date-hours;

define function date-minutes(d :: <date>) => (minutes :: <minutes>)
  d.time.minutes;
end function date-minutes;

define function date-seconds(d :: <date>) => (seconds :: <seconds>)
  d.time.seconds;
end function date-seconds;

define function date-microseconds(d :: <date>) => (usec :: <microseconds>)
  d.microseconds;
end function date-microseconds;

define function date-time-zone-offset(d :: <date>) => (tzo :: <timezone>)
  floor/(false-to-0(d.time.timezone), 60);
end function date-time-zone-offset;

define function as-iso8601-string(d :: <date>, #key precision :: <integer> = 0)
 => (ans :: <string>)
  // let ans = make(<byte-string>, size: 256);
  let stream = make(<byte-string-stream>, direction: #"input-output");
  format-time(stream, "%Y%m%dT%H%M%S", d.time);
  if(precision > 0)
    let zero = as(<integer>, '0');
    write-element(stream, '.');
    do(method(digit) write-element(stream, as(<character>, digit + zero)) end,
       tabulate(min(precision, 6),
 	        method(idx) modulo(floor/(d.microseconds, 6 - idx), 10) end));
  end if;
  write-element(stream, 'Z'); // ignoring time zone for now
  stream.stream-contents;
end function as-iso8601-string;

// Strangely enough, the 0th element is #"Monday" (not Sunday)
// ... this comes from the Time library.
define variable *day* = vector(#"Monday", #"Tuesday", #"Wednesday",
			       #"Thursday", #"Friday", #"Saturday", #"Sunday");
