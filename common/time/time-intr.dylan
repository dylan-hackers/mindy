module: Time-IO
author: dwatson@cmu.edu
synopsis: The Time-IO module glue code.
copyright: See below.
rcs-header: $Header: /home/housel/work/rcs/gd/src/common/time/time-intr.dylan,v 1.1 1996/07/23 16:03:59 dwatson Exp $
 
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


define constant <size-t> = <integer>;

define  class <tm> (<statically-typed-pointer>) end;

define sealed method tm-sec
    (ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 0);
end method tm-sec;

define sealed method tm-sec-setter
    (value :: <integer>, ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 0) := value;
  value;
end method tm-sec-setter;

define sealed method tm-min
    (ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 4);
end method tm-min;

define sealed method tm-min-setter
    (value :: <integer>, ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 4) := value;
  value;
end method tm-min-setter;

define sealed method tm-hour
    (ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 8);
end method tm-hour;

define sealed method tm-hour-setter
    (value :: <integer>, ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 8) := value;
  value;
end method tm-hour-setter;

define sealed method tm-mday
    (ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 12);
end method tm-mday;

define sealed method tm-mday-setter
    (value :: <integer>, ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 12) := value;
  value;
end method tm-mday-setter;

define sealed method tm-mon
    (ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 16);
end method tm-mon;

define sealed method tm-mon-setter
    (value :: <integer>, ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 16) := value;
  value;
end method tm-mon-setter;

define sealed method tm-year
    (ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 20);
end method tm-year;

define sealed method tm-year-setter
    (value :: <integer>, ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 20) := value;
  value;
end method tm-year-setter;

define sealed method tm-wday
    (ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 24);
end method tm-wday;

define sealed method tm-wday-setter
    (value :: <integer>, ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 24) := value;
  value;
end method tm-wday-setter;

define sealed method tm-yday
    (ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 28);
end method tm-yday;

define sealed method tm-yday-setter
    (value :: <integer>, ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 28) := value;
  value;
end method tm-yday-setter;

define sealed method tm-isdst
    (ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 32);
end method tm-isdst;

define sealed method tm-isdst-setter
    (value :: <integer>, ptr :: <tm>) => (result :: <integer>);
  signed-long-at(ptr, offset: 32) := value;
  value;
end method tm-isdst-setter;

define method pointer-value (value :: <tm>, #key index = 0) => (result :: <tm>);
  value + index * 36;
end method pointer-value;

define method content-size (value :: limited(<class>, subclass-of: <tm>)) => (result :: <integer>);
  36;
end method content-size;

define constant anonymous-29
  = constrain-c-function(find-c-function("strftime"), #(), #t, list(<size-t>));
define method strftime
    (arg1 :: <c-string>, arg2 :: <size-t>, arg3 :: <byte-string>, arg4 :: <decoded-time>)
 => (result :: <size-t>);
  let result-value
    = anonymous-29(export-value(<c-string>, arg1), arg2, export-value(<c-string>, arg3), export-value(<tm>, arg4));
  values(result-value);
end method strftime;

define constant anonymous-30 = find-c-function("strptime");
define method string-parse-time
    (arg1 :: <byte-string>, arg2 :: <byte-string>)
 => (arg3 :: <decoded-time>);
  let arg3-ptr = make(<tm>);
  anonymous-30(export-value(<c-string>, arg1), export-value(<c-string>, arg2), arg3-ptr);
  let arg3-value = import-value(<decoded-time>, pointer-value(arg3-ptr));
  values(arg3-value);
end method string-parse-time;

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

define method string-format-time (format :: <byte-string>, time :: <decoded-time>)
 => (string :: <object>);
  let buffer = make(<c-string>, size: 500);
  strftime(buffer, 500, format, time);
  let string = as(<byte-string>, buffer);
  string;
end method string-format-time;
