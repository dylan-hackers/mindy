module: Dylan-user
author: Ben Folk-Williams, bfw@cmu.edu and David Watson, dwatson@cmu.edu
synopsis: The Time library definitions.
copyright: See below.
rcs-header: $Header: /home/housel/work/rcs/gd/src/common/time/library.dylan,v 1.1 1996/07/23 16:03:09 dwatson Exp $
 
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


define library Time
  use Dylan;
  use String-extensions;
  export Time;
  export Time-IO;
end library Time;

define module Time
  use Dylan;
  use Extensions;
  use System;
  use Character-type;

  export
    // Constants
    $default-time,
    <universal-time>,
    
    // Time types
    <seconds>,
    <minutes>,
    <hours>,
    <day-of-week>,
    <day-of-month>,
    <month>,
    <year>,
    <timezone>,

    // The decoded-time class and slots
    <decoded-time>,
    seconds,
    minutes,
    hours,
    day-of-week,
    day-of-month,
    month,
    year,
    timezone,
    daylight-savings-time?,

    // Functions
    get-universal-time,
    get-decoded-time,
    decode-time,
    encode-time,
    encodable-time?;
end module Time;

define module Time-IO
  use Dylan;
  use Time;
  use Extern;

  export
    // Functions
    string-parse-time,
    string-format-time;
end module Time-IO
