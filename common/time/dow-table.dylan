module: Test
author: David Watson, dwatson@cmu.edu
synopsis: Code to generate the day-of-week table in time.dylan
copyright: See below.
rcs-header: $Header: /scm/cvs/src/common/time/dow-table.dylan,v 1.1 1998/05/03 19:55:05 andreas Exp $
 
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

define library Test
  use Dylan;
  use Streams;
  use Format;
end;

define module Test
  use Dylan;
  use Streams;
  use Extensions;
  use Standard-IO;
  use System;
  use Format;
end;

define method leap-year? (year)
 => result :: <boolean>;
  let (high, low) = truncate/(year, 100);
  if (low = 0)
    modulo(year, 400) = 0;
  else
    modulo(year, 4) = 0;
  end if;
end method leap-year?;


define method main (blah :: <byte-string>, #rest noise)
  let out = *standard-output*;
  
  let jan-1-0 = 5;  // 5 = Saturday
  for (year from 0 to 399,
       day-of-week = jan-1-0 then day-of-week)

    format(out, "%D, ", day-of-week);

    // Each normal year has 365 days, modulo 7 = 1 so we add one day
    day-of-week := modulo(day-of-week + 1, 7);

    // If we have a leap year, we add an extra day
    if (leap-year?(year))
      day-of-week := modulo(day-of-week + 1, 7);
    end if;
  end for;
end method main;
