module:    Arithmetic
synopsis:  Common-Dylan compliance for the Date module
author:    Douglas M. Auclair
copyright: LGPL, 2000

/// This implementation module defines the dates and times.
/// It conforms to section 7.4.3 of the Date Module description
/// of the "System and I/O" document from Functional Objects, Inc.

// Note: I intentionally forego implementing arithmetic on the <duration>
// type itself (meaning \+, \- and interactions with <date>).  I believe it is
// not type-safe to operate on an unspecified kind of <duration>

// no <duration> + <duration>

define method \+(dur1 :: <year/month-duration>, dur2 :: <year/month-duration>)
 => (dur3 :: <year/month-duration>)
  make(<year/month-duration>, duration: dur1.duration + dur2.duration);
end method \+;

define method \+(dur1 :: <day/time-duration>, dur2 :: <day/time-duration>)
 => (dur3 :: <day/time-duration>)
  let new-sec = dur1.duration + dur2.duration;
  let new-usec = modulo(dur1.usec + dur2.usec, 1000000);
  if(dur1.usec + dur2.usec > 999999) new-sec := new-sec + 1; end if;
  make(<day/time-duration>, duration: new-sec, microseconds: new-usec);
end method \+;

// neither <date> + <duration> nor vice versa, but I will do it for 
// implementation classes

define method \+(d1 :: <date>, dur :: <year/month-duration>) => (d2 :: <date>)
  let t = d1.time;
  let (years, months) = floor/(dur.duration, 12);
  if(months + t.month > 12) years := years + 1; end if;
  let new-months = modulo(months + t.month, 12);
  make(<date>, 
       year: t.year + years, 
       month: if(new-months = 0) 12 else new-months end if,
       day: t.day-of-month, 
       hours: false-to-0(t.hours), 
       minutes: false-to-0(t.minutes),
       seconds: false-to-0(t.seconds),
       microseconds: false-to-0(d1.microseconds),
       time-zone-offset: floor/(false-to-0(t.timezone), 60));
end method \+;

define method \+(d1 :: <date>, dur :: <day/time-duration>) => (d2 :: <date>)
  let sec = dur.duration;
  let microsec = dur.usec + d1.microseconds;
  if(microsec > 1000000) sec := sec + 1; end if;
  let u = sec + encode-time(d1.time);             // time in seconds from 1970
  let t = decode-time(u, timezone: d1.time.timezone);
  make(<date>, 
       year: t.year, month: t.month, day: t.day-of-month, 
       hours: false-to-0(t.hours), 
       minutes: false-to-0(t.minutes), 
       seconds: false-to-0(t.seconds),
       microseconds: modulo(microsec, 1000000),
       time-zone-offset: floor/(false-to-0(t.timezone), 60));
end method \+;

// no <duration> - <duration>

define method \-(dur1 :: <year/month-duration>, dur2 :: <year/month-duration>)
 => (dur3 :: <year/month-duration>)
  make(<year/month-duration>, duration: dur1.duration - dur2.duration);
end method \-;

define method \-(dur1 :: <day/time-duration>, dur2 :: <day/time-duration>)
 => (dur3 :: <day/time-duration>)
  let new-usec = dur1.usec - dur2.usec;
  let new-sec = dur1.duration - dur2.duration;
  if(new-usec < 0) 
    new-usec := new-usec + 1000000;
    new-sec := new-sec - 1;
  end if;
  make(<day/time-duration>, duration: new-sec, microseconds: new-usec);
end method \-;

// no <date> - <duration>, but I will do <date> - an implementation class

define method \-(d1 :: <date>, dur :: <year/month-duration>) => (d2 :: <date>)
  let t = d1.time;
  let (years, months) = floor/(dur.duration, 12);
  if(t.month - months < 1) 
    years := years + 1;
    months := months - 12;
  end if;
  make(<date>, 
       year: t.year - years, month: months - t.month,
       day: t.day-of-month, hours: t.hours, minutes: t.minutes,
       seconds: t.seconds, microseconds: d1.microseconds, 
       time-zone-offset: floor/(t.timezone, 60));
end method \-;  

define method \-(d1 :: <date>, dur :: <day/time-duration>) => (d2 :: <date>)
  let sec = dur.duration;
  let microsec = dur.usec;
  if(d1.microseconds - microsec < 0) 
    sec := sec + 1; 
    microsec := microsec - 1000000;
  end if;
  let u = encode-time(d1.time) - sec;             // time in seconds from 1970
  let t = decode-time(u, timezone: d1.time.timezone);
  make(<date>, 
       year: t.year, month: t.month, day: t.day-of-month, hours: t.hours, 
       minutes: t.minutes, seconds: t.seconds,
       microseconds: d1.microseconds - microsec,
       time-zone-offset: floor/(t.timezone, 60));
end method \-;

define method \-(d1 :: <date>, d2 :: <date>) => (d3 :: <day/time-duration>)
  let u1 = encode-time(d1.time);
  let u2 = encode-time(d2.time);
  let new-usec = d1.microseconds - d2.microseconds;
  let dur = u1 - u2;
  if(new-usec < 0)
    new-usec := new-usec + 1000000;
    dur := dur - 1;
  end if;
  make(<day/time-duration>, duration: dur, microseconds: new-usec);
end method \-;

define method \*(d1 :: <day/time-duration>, r :: <real>) => (d2 :: <duration>)
  let (add-sec, new-usec) = floor/(floor(d1.usec * r), 1000000);
  make(<day/time-duration>, 
       duration: floor(d1.duration * r) + add-sec,
       microseconds: new-usec);
end method \*;

define method \*(d1 :: <year/month-duration>, r :: <real>) 
 => (d2 :: <duration>)
  make(<year/month-duration>, duration: floor(d1.duration * r));
end method \*;

define method \*(r :: <real>, d1 :: <duration>) => (d2 :: <duration>)
  d1 * r;
end method \*;

define method \/(d1 :: <duration>, r :: <real>) => (d2 :: <duration>)
  d1 * (1.0 / r);
end method \/;

