Module:       system-internals
Author:       Jonathan Bachrach, Gary Palter, Peter Housel
Synopsis:     Native UNIX specific portion of the Date library API
Copyright:    Original Code is Copyright (c) 1998-2001 Functional Objects, Inc.
              All rights reserved.
	      Adaptations Copyright 2003 Gwydion Dylan Maintainers
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function read-clock () => (time :: <machine-word>)
  let (err?, timeval) = %gettimeofday($null-pointer);
  as(<machine-word>, timeval.tv-sec);
end function read-clock;

define generic native-clock-to-tm (time) => (tm :: <tm>);

define method native-clock-to-tm (time :: <integer>) => (tm :: <tm>)
  native-clock-to-tm(as(<machine-word>, time))
end method native-clock-to-tm;

define method native-clock-to-tm
    (time :: <machine-word>)
 => (tm :: <tm>)
  let (err?, tm, gmtoff, zone)
    = %system-localtime(as(<integer>, time));
  tm
end method native-clock-to-tm;

define function encode-native-clock-as-date (native-clock) => (date :: <date>)
  let (err?, tm, gmtoff, zone)
    = %system-localtime(as(<integer>, native-clock));
  make(<date>, year: tm-year(tm) + 1900,
               month: tm-mon(tm) + 1,
               day: tm-mday(tm),
               hours: tm-hour(tm),
               minutes: tm-min(tm),
               seconds: tm-sec(tm),
               time-zone-offset: truncate/(gmtoff, 60))
end function encode-native-clock-as-date;

define function current-date () => (now :: <date>)
  encode-native-clock-as-date(read-clock())
end function current-date;

define function current-timestamp () => (milliseconds :: <integer>, days :: <integer>)
  let (err?, timeval) = %gettimeofday($null-pointer);
  let (err?, tm, gmtoff, zone)
    = %system-localtime(as(<integer>, timeval.tv-sec));
  let (ud, ut) = compute-universal-time(tm-year(tm) + 1900, tm-mon(tm) + 1,
                                        tm-mday(tm), tm-hour(tm),
                                        tm-min(tm), tm-sec(tm),
					truncate/(gmtoff, 60));
  values(1000 * ut + timeval.tv-usec, ud)
end function current-timestamp;

define function local-time-zone-offset () => (zone-offset :: <integer>)
  let (err?, tm, gmtoff, zone)
    = %system-localtime(as(<integer>, read-clock()));
  truncate/(gmtoff, 60)
end function local-time-zone-offset;

define function local-time-zone-name () => (zone-name :: <string>)
  let (err?, tm, gmtoff, zone)
    = %system-localtime(as(<integer>, read-clock()));
  zone
end function local-time-zone-name;

define function local-daylight-savings-time? () => (is-dst? :: <boolean>)
  tm-isdst(native-clock-to-tm(read-clock())) ~= 0
end function local-daylight-savings-time?;
