module: time-internal
author: dwatson@cmu.edu
synopsis: The Time library supporting c functions (generated by Melange).
copyright: See below.
rcs-header: $Header: /home/housel/work/rcs/gd/src/common/time/time-intr.intr,v 1.2 1996/10/06 13:43:51 nkramer Exp $

//======================================================================
//
// Copyright (c) 1996  Carnegie Mellon University
// All rights reserved.
// 
// Use and copying of this software and preparation of derivative
// works based on this software are permitted, including commercial
// use, provided that the following conditions are observed:
// 
// 1. This copyright notice must be retained in full on any copies
//    and on appropriate parts of any derivative works.
// 2. Documentation (paper or online) accompanying any system that
//    incorporates this software, or any part of it, must acknowledge
//    the contribution of the Gwydion Project at Carnegie Mellon
//    University.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports, questions, comments, and suggestions should be sent by
// E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
//
//======================================================================


define interface
  #include "time.h",  // sometimes sys/time.h

    // This file is used when building the Mindy interpretter (I'm
    // trying hard not to think about the bootstrapping problems this
    // could cause...)
    mindy-include-file: "time.inc",

    import: {"strftime", "timezone", "daylight"},
    rename: {"timezone" => c-timezone},
    name-mapper: minimal-name-mapping,
    equate: {"char *" => <c-string>},
    map: {"char *" => <byte-string>,
	  "struct tm" => <decoded-time>};
  function "strftime",
    input-argument: 1,
    equate-argument: {1 => <c-string>},
    map-argument: {1 => <c-string>};
end interface;

define method import-value (cls == <decoded-time>, value :: <tm>)
 => (result :: <decoded-time>);
  let day-of-week = if (value.tm-wday = 0) 6 else value.tm-wday - 1 end if;

  make(<decoded-time>, seconds: value.tm-sec, minutes: value.tm-min,
       hours: value.tm-hour, day-of-week: day-of-week,
       day-of-month: value.tm-mday, month: value.tm-mon + 1,
       year: value.tm-year + 1900,
       daylight-savings-time?: value.tm-isdst > 0);
end method import-value;

define method export-value (cls == <tm>, value :: <decoded-time>)
 => (result :: <tm>);
  let tm = make(<tm>);

  tm.tm-sec := value.seconds;
  tm.tm-min := value.minutes;
  tm.tm-hour := value.hours;
  tm.tm-mday := value.day-of-month;
  tm.tm-mon := value.month - 1;
  tm.tm-year := value.year - 1900;
  tm.tm-wday := if (value.day-of-week = 6) 0 else value.day-of-week + 1 end if;
//  tm.tm-yday := days-before-month(value.month, value.year) + value.day-of-month;
  tm.tm-isdst := if (value.daylight-savings-time?) 1 else 0 end if;

  tm;
end method export-value;

define sealed inline method get-local-timezone ()
 => (timezone :: <integer>);
  c-timezone();
end method get-local-timezone;

define method local-daylight-savings-time? ()
 => (result :: <boolean>);
  (daylight() ~== 0);
end method local-daylight-savings-time?;
