module: Time-Internal
author: Ben Folk-Williams, bfw@cmu.edu and
        David Watson, dwatson@cmu.edu
synopsis: Basic Time functions.
copyright: See below.
rcs-header: $Header: /scm/cvs/src/common/time/time.dylan,v 1.1 1998/05/03 19:55:05 andreas Exp $
 
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

//=============================================================================
//
// The Time module of the Time library.
// Everything is exported except as noted.
//
//=============================================================================

// Using <extended-integer>s guarantees that you can represent any date after
// 1 Jan 1970.  Using <integer>s will break for dates after a certain
// date (depending on $maximum-integer) and can sometimes cause unpredictable
// behavior.
//
#if (EXTENDED-UNIVERSAL-TIME)
define constant <universal-time> = <extended-integer>;
#else
define constant <universal-time> = <integer>;
#endif

// Internal constants.
//
define constant <seconds> = limited(<integer>, min: 0, max: 59);
define constant <minutes> = limited(<integer>, min: 0, max: 59);
define constant <hours> = limited(<integer>, min: 0, max: 23);
define constant <day-of-week> = limited(<integer>, min: 0, max: 6); // 0 = Mon
define constant <day-of-month> = limited(<integer>, min: 1, max: 31);
define constant <month> = limited(<integer>, min: 1, max: 12);
define constant <year> = <integer>;
// <timezone> is in seconds west of UTC
define constant <timezone> = limited(<integer>, min: -86400, max: 86400);

// Internal constants.
//
define constant $days-before-month
  = #[0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365];
define constant $minutes-per-day = 24 * 60;
define constant $jan-1-1970 = 719527; // in days
define constant $weekday-jan-1-1970 = 3;
define constant $weekday-jan-1-0AD = 3;
define constant $quarter-days-per-year = 1 + 4 * 365;
define constant $seconds-per-minute = 60;
define constant $seconds-per-hour = 3600;
define constant $seconds-per-quarter-day = 3600 * 6;
define constant $seconds-per-day = 3600 * 24;
define constant $seconds-per-avg-year = 365.2425 * $seconds-per-day;

// The table $jan-1-dow gives the day-of-week for Jan-1 modulo(year,400)
// (The system of leap years has a 400 year cycle.)
// See the file dow-table.dylan in the source directory.
//
define constant $jan-1-dow
  = #[5, 0, 1, 2, 3, 5, 6, 0, 1, 3, 4, 5, 6, 1, 2, 3, 4, 6, 0, 1,
      2, 4, 5, 6, 0, 2, 3, 4, 5, 0, 1, 2, 3, 5, 6, 0, 1, 3, 4, 5,
      6, 1, 2, 3, 4, 6, 0, 1, 2, 4, 5, 6, 0, 2, 3, 4, 5, 0, 1, 2,
      3, 5, 6, 0, 1, 3, 4, 5, 6, 1, 2, 3, 4, 6, 0, 1, 2, 4, 5, 6,
      0, 2, 3, 4, 5, 0, 1, 2, 3, 5, 6, 0, 1, 3, 4, 5, 6, 1, 2, 3,
      4, 5, 6, 0, 1, 3, 4, 5, 6, 1, 2, 3, 4, 6, 0, 1, 2, 4, 5, 6,
      0, 2, 3, 4, 5, 0, 1, 2, 3, 5, 6, 0, 1, 3, 4, 5, 6, 1, 2, 3,
      4, 6, 0, 1, 2, 4, 5, 6, 0, 2, 3, 4, 5, 0, 1, 2, 3, 5, 6, 0,
      1, 3, 4, 5, 6, 1, 2, 3, 4, 6, 0, 1, 2, 4, 5, 6, 0, 2, 3, 4,
      5, 0, 1, 2, 3, 5, 6, 0, 1, 3, 4, 5, 6, 1, 2, 3, 4, 6, 0, 1,
      2, 3, 4, 5, 6, 1, 2, 3, 4, 6, 0, 1, 2, 4, 5, 6, 0, 2, 3, 4,
      5, 0, 1, 2, 3, 5, 6, 0, 1, 3, 4, 5, 6, 1, 2, 3, 4, 6, 0, 1,
      2, 4, 5, 6, 0, 2, 3, 4, 5, 0, 1, 2, 3, 5, 6, 0, 1, 3, 4, 5,
      6, 1, 2, 3, 4, 6, 0, 1, 2, 4, 5, 6, 0, 2, 3, 4, 5, 0, 1, 2,
      3, 5, 6, 0, 1, 3, 4, 5, 6, 1, 2, 3, 4, 6, 0, 1, 2, 4, 5, 6,
      0, 1, 2, 3, 4, 6, 0, 1, 2, 4, 5, 6, 0, 2, 3, 4, 5, 0, 1, 2,
      3, 5, 6, 0, 1, 3, 4, 5, 6, 1, 2, 3, 4, 6, 0, 1, 2, 4, 5, 6,
      0, 2, 3, 4, 5, 0, 1, 2, 3, 5, 6, 0, 1, 3, 4, 5, 6, 1, 2, 3,
      4, 6, 0, 1, 2, 4, 5, 6, 0, 2, 3, 4, 5, 0, 1, 2, 3, 5, 6, 0,
      1, 3, 4, 5, 6, 1, 2, 3, 4, 6, 0, 1, 2, 4, 5, 6, 0, 2, 3, 4];

// Table and constant for use with decode and encode time.
// If using extended-integers for <universal-time> this should permit encoding
// arbitrary dates beyond 1970.
// See the file days-since-1970.dylan in the source directory.
//
define constant $days-per-400-years = 146097;
define constant $days-since-1970
  = #[0, 365, 730, 1096, 1461, 1826, 2191, 2557, 2922, 3287,
      3652, 4018, 4383, 4748, 5113, 5479, 5844, 6209, 6574, 6940,
      7305, 7670, 8035, 8401, 8766, 9131, 9496, 9862, 10227, 10592,
      10957, 11323, 11688, 12053, 12418, 12784, 13149, 13514, 13879, 14245,
      14610, 14975, 15340, 15706, 16071, 16436, 16801, 17167, 17532, 17897,
      18262, 18628, 18993, 19358, 19723, 20089, 20454, 20819, 21184, 21550,
      21915, 22280, 22645, 23011, 23376, 23741, 24106, 24472, 24837, 25202,
      25567, 25933, 26298, 26663, 27028, 27394, 27759, 28124, 28489, 28855,
      29220, 29585, 29950, 30316, 30681, 31046, 31411, 31777, 32142, 32507,
      32872, 33238, 33603, 33968, 34333, 34699, 35064, 35429, 35794, 36160,
      36525, 36890, 37255, 37621, 37986, 38351, 38716, 39082, 39447, 39812,
      40177, 40543, 40908, 41273, 41638, 42004, 42369, 42734, 43099, 43465,
      43830, 44195, 44560, 44926, 45291, 45656, 46021, 46387, 46752, 47117,
      47482, 47847, 48212, 48577, 48942, 49308, 49673, 50038, 50403, 50769,
      51134, 51499, 51864, 52230, 52595, 52960, 53325, 53691, 54056, 54421,
      54786, 55152, 55517, 55882, 56247, 56613, 56978, 57343, 57708, 58074,
      58439, 58804, 59169, 59535, 59900, 60265, 60630, 60996, 61361, 61726,
      62091, 62457, 62822, 63187, 63552, 63918, 64283, 64648, 65013, 65379,
      65744, 66109, 66474, 66840, 67205, 67570, 67935, 68301, 68666, 69031,
      69396, 69762, 70127, 70492, 70857, 71223, 71588, 71953, 72318, 72684,
      73049, 73414, 73779, 74145, 74510, 74875, 75240, 75606, 75971, 76336,
      76701, 77067, 77432, 77797, 78162, 78528, 78893, 79258, 79623, 79989,
      80354, 80719, 81084, 81450, 81815, 82180, 82545, 82911, 83276, 83641,
      84006, 84371, 84736, 85101, 85466, 85832, 86197, 86562, 86927, 87293,
      87658, 88023, 88388, 88754, 89119, 89484, 89849, 90215, 90580, 90945,
      91310, 91676, 92041, 92406, 92771, 93137, 93502, 93867, 94232, 94598,
      94963, 95328, 95693, 96059, 96424, 96789, 97154, 97520, 97885, 98250,
      98615, 98981, 99346, 99711, 100076, 100442, 100807, 101172, 101537,
      101903, 102268, 102633, 102998, 103364, 103729, 104094, 104459, 104825,
      105190, 105555, 105920, 106286, 106651, 107016, 107381, 107747, 108112,
      108477, 108842, 109208, 109573, 109938, 110303, 110669, 111034, 111399,
      111764, 112130, 112495, 112860, 113225, 113591, 113956, 114321, 114686,
      115052, 115417, 115782, 116147, 116513, 116878, 117243, 117608, 117974,
      118339, 118704, 119069, 119435, 119800, 120165, 120530, 120895, 121260,
      121625, 121990, 122356, 122721, 123086, 123451, 123817, 124182, 124547,
      124912, 125278, 125643, 126008, 126373, 126739, 127104, 127469, 127834,
      128200, 128565, 128930, 129295, 129661, 130026, 130391, 130756, 131122,
      131487, 131852, 132217, 132583, 132948, 133313, 133678, 134044, 134409,
      134774, 135139, 135505, 135870, 136235, 136600, 136966, 137331, 137696,
      138061, 138427, 138792, 139157, 139522, 139888, 140253, 140618, 140983,
      141349, 141714, 142079, 142444, 142810, 143175, 143540, 143905, 144271,
      144636, 145001, 145366, 145732];

//=============================================================================

// The <decoded-time> class.
//
define class <decoded-time> (<object>)
  slot seconds :: false-or(<seconds>), init-keyword: seconds:;
  slot minutes :: false-or(<minutes>), init-keyword: minutes:;
  slot hours :: false-or(<hours>), init-keyword: hours:;
  slot day-of-week :: false-or(<day-of-week>), init-keyword: day-of-week:;
  slot day-of-month :: false-or(<day-of-month>), init-keyword: day-of-month:;
  slot month :: false-or(<month>), init-keyword: month:;
  slot year :: false-or(<integer>), init-keyword: year:;
  slot daylight-savings-time? :: <boolean>,
    init-keyword: daylight-savings-time?:;
  slot timezone :: false-or(<timezone>), //in seconds west of UTC
    init-keyword: timezone:;
end class <decoded-time>;

//=============================================================================

// Constants for use with default-from: keyword.

// This one is internal,
#if (mindy)
define constant $null-decoded-time
  = make(<decoded-time>, seconds: #f, minutes: #f, hours: #f, day-of-week: #f,
	 day-of-month: #f, month: #f, year: #f, daylight-savings-time?: #f,
	 timezone: #f);
#else
define constant $null-decoded-time
  = make(<decoded-time>, default-from: #f);
#endif

// This one is exported.
define constant $default-time
  = make(<decoded-time>, seconds: 0, minutes: 0, hours: 0, day-of-month: 1,
	 month: 1, year: 0, daylight-savings-time?: #f, timezone: 0);

//=============================================================================

// Internal helper function.
//
// As a special kluge, month can be 13, in which case the answer is
// the # of days in that year.
//
define method days-before-month (month :: <integer>, year :: <year>)
 => num-days :: <integer>;
  element($days-before-month, month - 1)
    + if (month > 2 & leap-year?(year)) 1 else 0 end if;
end method days-before-month;

// Day-of-week Algorithm as proposed by the sci.math FAQ
// see: http://www.cis.ohio-state.edu/hypertext/faq/usenet/sci-math-faq/dayWeek/faq.html
//
define method compute-day-of-week
    (year :: <year>, month :: <month>, day-of-month :: <day-of-month>)
 => day-of-week :: <day-of-week>;

  let jan-1-dow-this-year = $jan-1-dow[modulo(year, 400)];

  let days-this-year = days-before-month(month, year)
    + day-of-month
    - 1; // We've double counted Jan 1
  
  modulo(jan-1-dow-this-year + days-this-year, 7);
end method compute-day-of-week;

// Internal helper function.
//
define method leap-year? (year)
 => result :: <boolean>;
  let (high, low) = truncate/(year, 100);
  if (low = 0)
    modulo(year, 400) = 0;
  else
    modulo(year, 4) = 0;
  end if;
end method leap-year?;

// <decoded-time> initialization method.
//
// All slots are initialized to the value supplied with their
// corresponding keyword. If a default-from: is supplied, any
// unspecified slots are initialized to the corresponding value from
// the default-from <decoded-time>.  If a default-from: is not
// supplied, any unspecified slot are initialized to #f.
//
// If the day of week is computable, it is computed and used to
// initialize the day-of-week slot. If this value is different than
// the supplied day-of-week, a warning is signalled.
//
define method initialize
      (time :: <decoded-time>,
       #next next-method,
       #key default-from :: false-or(<decoded-time>) = $null-decoded-time,
       seconds: secs :: false-or(<seconds>)
	 = if (default-from) default-from.seconds else #f end if,
       minutes: mins :: false-or(<minutes>)
	 = if (default-from) default-from.minutes else #f end if,
       hours: hrs :: false-or(<hours>)
	 = if (default-from) default-from.hours else #f end if,
       day-of-week: weekday :: false-or(<day-of-week>)
	 = if (default-from) default-from.day-of-week else #f end if,
       day-of-month: date :: false-or(<day-of-month>)
	 = if (default-from) default-from.day-of-month else #f end if,
       month: mth :: false-or(<month>)
	 = if (default-from) default-from.month else #f end if,
       year: yr :: false-or(<year>)
	 = if (default-from) default-from.year else #f end if,
       daylight-savings-time?: dst? :: <boolean>
	 = if (default-from) default-from.daylight-savings-time?
	   else
	     #f
	   end if,
       timezone: tzone :: false-or(<timezone>)
	 = if (default-from) default-from.timezone else #f end if);
  next-method();

  time.seconds := secs;
  time.minutes := mins;
  time.hours := hrs;
  time.day-of-month := date;
  time.month := mth;
  time.year := yr;
  time.daylight-savings-time? := dst?;
  time.timezone := tzone;

  if (time.year & time.month & time.day-of-month) // day-of-week is computable
    let computed-day-of-week
      = compute-day-of-week(time.year, time.month, time.day-of-month);
    // if (weekday & weekday ~= computed-day-of-week)
    //   signal("Supplied day-of-week is incorrect, using corrected value.");
    // end if;
    time.day-of-week := computed-day-of-week;
  else
    time.day-of-week := weekday;
  end if;
end method initialize;

// There's no point in not defining a print-object method, since the
// parse-time stuff already uses streams.  Might as well use print and
// format, too.
//
// We don't write "hh:mm:ss mm/dd/yy" or anything clever like that
// because it might not look so hot with undefined slots.  (Besides,
// mm/dd/yy is rather ambiguous)
//
define method print-object (time :: <decoded-time>, stream :: <stream>) => ();
  write(stream, "{<decoded-time> ");
  
  format(stream, 
	 "seconds=%= minutes=%= hours=%= day-of-month=%= month=%= "
	   "year=%= day-of-week=%= daylight-savings-time?=%= timezone=%=", 
	 time.seconds, time.minutes, time.hours, time.day-of-month, 
	 time.month, time.year, time.day-of-week, time.daylight-savings-time?,
	 time.timezone);
  write(stream, "}");
end method print-object;

// Return the current time as a <universal-time>.
//
define method get-universal-time () => current-time :: <universal-time>;
  get-time-of-day();
end method get-universal-time;

// Return current time as a <decoded-time>.
//
define method get-decoded-time 
    (#key timezone :: false-or(<timezone>) = #f)
 => current-time :: <decoded-time>;
  if (~timezone)
    if (local-daylight-savings-time?())
      timezone := get-local-timezone() - $seconds-per-hour;
    else
      timezone := get-local-timezone();
    end if;
  end if;

  decode-time(get-universal-time(), timezone: timezone);
end method get-decoded-time;


// Decode a <universal-time> into a <decoded-time> using the supplied timezone.
//
define method decode-time 
    (universal-time :: <universal-time>,
     #key timezone :: false-or(<timezone>) = #f)
 => decoded-time :: <decoded-time>;

  let daylight-savings-time? = local-daylight-savings-time?();
  
  let zone = if (timezone)
	       timezone;
	     elseif (daylight-savings-time?)
	       get-local-timezone() - $seconds-per-hour;
	     else
	       get-local-timezone();
	     end if;
  let secs = universal-time - zone;

  // Guess the number of years and work from there
  let years = as(<integer>, round/(secs, $seconds-per-avg-year));

  let (leap-year-cycles-guess, year-this-cycle-guess) = truncate/(years, 400);

  let days-since-1970
    = as(<universal-time>, $days-since-1970[year-this-cycle-guess])
    + leap-year-cycles-guess * $days-per-400-years;

  // Assume that we're correct +/- one year
  if (secs < days-since-1970 * $seconds-per-day)
    years := years - 1;
  else
    let days-since-1970 = days-since-1970
      + if (leap-year?(years + 1970)) 366 else 365 end if;
    
    if (secs > days-since-1970 * $seconds-per-day)
      years := years + 1;
    end if;
  end if;

  let (leap-year-cycles, year-this-cycle) = truncate/(years, 400);

  let seconds-this-year
    = as(<integer>, secs - (as(<universal-time>,
			       $days-since-1970[year-this-cycle])
			      + leap-year-cycles * $days-per-400-years)
			      * $seconds-per-day);

  let year = 1970 + years;

  let (days-this-year, seconds-today)
    = truncate/(seconds-this-year, $seconds-per-day);

  // truncate/ yields *full* days.  Unless seconds-today is 0, that's
  // one less than what we'd expect.
  //
  // I intentionally used = rather than == because I'm not convinced
  // this is <integer> arithmetic rather than <extended-integer>
  // arithmetic
  let days-this-year = days-this-year + if (seconds-today = 0) 0 else 1 end;

  let (hours, seconds-this-hour) = truncate/(seconds-today, $seconds-per-hour);
  let (minutes, seconds) = truncate/(seconds-this-hour, $seconds-per-minute);

  let month = 1;
  while (days-this-year > days-before-month(month + 1, year))
    month := month + 1;
  end while;

  let day-of-month = days-this-year - days-before-month(month, year);

  let day-of-week = #f;

  make(<decoded-time>, seconds: seconds, minutes: minutes, hours: hours,
       day-of-month: day-of-month, month: month, year: year,
       day-of-week: day-of-week,
       daylight-savings-time?: daylight-savings-time?, timezone: zone);

end method decode-time;

define method as (cls == <decoded-time>, universal-time :: <universal-time>) 
 => decoded-time :: <decoded-time>;
  decode-time(universal-time);
end method as;

// Encode a <decoded-time> into the universal time format.
//
define method encode-time (decoded-time :: <decoded-time>)
 => universal-time :: <universal-time>;
  
  if (~encodable-time?(decoded-time)) 
    error("Time not encodable. Only completely specified times after 1969 are encodable");
  else
    let years = decoded-time.year - 1970;
    let (leap-year-cycles, year-this-cycle) = truncate/(years, 400);

    let days = as(<universal-time>, decoded-time.day-of-month - 1
		    + days-before-month(decoded-time.month, decoded-time.year)
		    + $days-since-1970[year-this-cycle]
		    + leap-year-cycles * $days-per-400-years);
    
    let hours = as(<universal-time>, decoded-time.hours + days * 24);

    decoded-time.seconds + (decoded-time.minutes + hours * 60) * 60
      + decoded-time.timezone; 
    
  end if;
end method encode-time;

// Returns #t if time may be encoded.
// A <decoded-time> is encodable if all of its slots are specified,
// except possibly day-of-week.
//
define method encodable-time? (time :: <decoded-time>) => result :: <boolean>;
  time.seconds
    & time.minutes
    & time.hours
    & time.day-of-month
    & time.month
    & time.timezone
    & time.year
    & time.year >= 1970;
end method encodable-time?;
