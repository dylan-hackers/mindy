module:    Dates-and-Times
synopsis:  Common-Dylan compliance for the Date module
author:    Douglas M. Auclair
copyright: LGPL, 2000

/// This implementation module defines the dates and times.
/// It conforms to section 7.2 of the Date Module description
/// of the "System and I/O" document from Functional Objects, Inc.

// The Fun-O doc does not specify any slots for <date>, so I'll
// let Gwydion Dylan's <decoded-time> to handle all the details.
//
// One thing to be mindful of:  <decoded-time> assumes year 0 is
// actually 1970, whereas <date> takes year as year (so year: 0 for
// <time> is 1970, year: 0 for <date> is an error as it only handles
// 1800 - 2199).

// Hm, testing doesn't bear out the 1970 problem ... I'll go with
// the year == actual-honest-to-God-year theory

define class <date> (<number>)
  slot time :: <decoded-time>;
  slot microseconds :: <microseconds>, init-value: 0;
end class <date>;

// these constants are for the Date module (not the Time module's ones)
define constant <years> = limited(<integer>, min: 1800, max: 2199);
define constant <timezone> = limited(<integer>, min: -720, max: 720);
define constant <microseconds> = limited(<integer>, min: 0, max: 999999);

// either iso8601-string or year,month,day must be filled in.
define method initialize(date :: <date>, 
			 #key iso8601-string,
			      year :: false-or(<years>),
			      month :: false-or(<month>),
			      day :: false-or(<days>),
			      hours :: <hours> = 0,
			      minutes :: <minutes> = 0,
			      seconds :: <seconds> = 0,
			      microseconds :: <microseconds> = 0,
			      time-zone-offset :: <timezone> = 0)
 => (date :: <date>)
  if(iso8601-string)
    initialize-date-from-iso8601(date, iso8601-string);
  else
    unless(year & month & day)
      error("You must specify the year, month and day "
	      "to create a <date> instance");
    end unless;
    date.time := make(<decoded-time>, 
		      seconds: seconds,
		      minutes: minutes,
		      hours: hours,
		      day-of-month: day,
		      month: month,
		      year: year,
		      timezone: time-zone-offset * 60);
    date.microseconds := microseconds;
    date;
  end if;
end method initialize;

// A limitation on this function (and therefore on initialize itself) is
// that I'm only supporting UST iso strings.  Other timezones will come
// much later.
define function initialize-date-from-iso8601(date :: <date>, str :: <string>)
 => (result :: <date>)
  // first, let's make the string parse-time friendly:
  let year = string-to-integer(copy-sequence(str, end: 4));
  let rest = choose(method(x) x ~= 'T' & x ~= 'Z' end,
		    copy-sequence(str, start: 4));
  let spacy = make(<deque>);
  for(x in rest, y from 1)
    push-last(spacy, x);
    if(modulo(y, 2) = 0) push-last(spacy, ' '); end if;
  end for;
  let new-str = concatenate(integer-to-string(year), " ", spacy);
  
  let stream = make(<byte-string-stream>, contents: new-str);
  date.time := make(<decoded-time>, 
		    default-from: parse-time(stream, "%Y %m %d %H %M %S"),
		    timezone: 0);
  date;
end function initialize-date-from-iso8601;

define constant <day-of-week> = one-of(#"Monday", #"Tuesday",
				       #"Wednesday", #"Thursday", #"Friday",
				       #"Saturday", #"Sunday");

define function encode-date(year :: <years>,
			    month :: <month>,
			    day :: <days>,
			    hours :: <hours>,
			    minutes :: <minutes>,
			    seconds :: <seconds>,
			    #key microseconds :: <microseconds> = 0,
			         time-zone-offset :: <timezone> = 0)
 => (date :: <date>)
  make(<date>, year: year, month: month, day: day, hours: hours, 
       minutes: minutes, seconds: seconds, microseconds: microseconds,
       time-zone-offset: time-zone-offset);
end function encode-date;

// just any ol' date so we can modify it for our own (nefarious) purposes
// [snip vile and wicked cackling here]
define constant $j-random-date = make(<date>, year: 1970, month: 1, day: 1);

define function current-date() => (date :: <date>)
  let today = $j-random-date;
  today.time := get-decoded-time();
  today;
end function current-date;

//-------------------------------------------------------
// Internal (to this library) fn
//-------------------------------------------------------

define function false-to-0(foo :: false-or(<integer>)) => (ans :: <integer>)
  if(foo)
    foo;
  else
    0;
  end if;
end function false-to-0;
