module:    Durations
synopsis:  Common-Dylan compliance for the Date module
author:    Douglas M. Auclair
copyright: LGPL, 2000

/// This implementation module defines the dates and times.
/// It conforms to section 7.3 of the Date Module description
/// of the "System and I/O" document from Functional Objects, Inc.

define abstract class <duration> (<number>)
  slot duration :: <integer>, required-init-keyword: duration:;
end class <duration>;

define class <year/month-duration> (<duration>)
end class <year/month-duration>;

// usec are just too big for encoding days, must create a
// new slot to handle them specially, and treat everything
// else as seconds
define class <day/time-duration> (<duration>)
  slot usec :: <integer>, required-init-keyword: microseconds:;
end class <day/time-duration>;

define function encode-year/month-duration(years :: <integer>,
					   months :: <integer>)
 => (ans :: <duration>)
  make(<year/month-duration>, duration: months + 12 * years);
end function encode-year/month-duration;

define function encode-day/time-duration(days :: <integer>,
					 hours :: <hours>,
					 minutes :: <minutes>,
					 seconds :: <seconds>,
					 microseconds :: <microseconds>)
 => (ans :: <duration>)
  let secs = seconds + 60 * minutes + 3600 * hours + 24 * 3600 * days;
  make(<day/time-duration>, duration: secs, microseconds: microseconds);
end function encode-day/time-duration;

define generic decode-duration(d :: <duration>);

define method decode-duration(d :: <year/month-duration>)
 => (years :: <integer>, months :: <integer>)
  floor/(d.duration, 12);
end method decode-duration;

define method decode-duration(d :: <day/time-duration>)
 => (days :: <integer>, hours :: <integer>, minutes :: <integer>, 
     seconds :: <integer>, microseconds :: <integer>)
  let(days, secs) = floor/(d.duration, 24 * 3600);
  let(hours, rem) = floor/(secs, 3600);
  let(minutes, seconds) = floor/(rem, 60);
  values(days, hours, minutes, seconds, d.usec);
end method decode-duration;
