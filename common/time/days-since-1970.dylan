module: Test
author: David Watson, dwatson@cmu.edu
synopsis: Code to generate the days-since-1970 table in time.dylan
copyright: See below.
 
//======================================================================
//
// Copyright (c) 1996  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
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
//    University, and the Gwydion Dylan Maintainers.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
// comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
// Also, see http://www.gwydiondylan.org/ for updates and documentation. 
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
  
  // If we're using a 32 bit integer, we can only go up to the year 2106
  // If we're using extended integers, we need to do a 400 year span to 2369
  
  for (year from 1970 to 2369,
       total-days = 0 then total-days)

    format(out, "%D, ", total-days);

    // Each normal year has 365 days
    total-days := total-days;

    // Add an extra day if it's a year
    if (leap-year?(year))
      total-days := total-days + 1;
    end if;
  end for;
end method main;
