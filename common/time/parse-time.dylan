module: Time-Internal
author: Nick Kramer
synopsis: Parses a time (like the C function strptime)
copyright: See below.
rcs-header: $Header: /scm/cvs/src/common/time/parse-time.dylan,v 1.1 1998/05/03 19:55:06 andreas Exp $

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


// <time-parsing-error> -- exported
//
// The error we signal when we have trouble parsing something.  Note
// that an unknown format directive is *not* considered a
// <time-parsing-error>, but a normal, unnamed (and close to
// uncatchable) error.
//
define class <time-parsing-error> (<error>)

  // The parse-state when we hit the error
  constant slot error-parse-state :: <parse-state>,
    required-init-keyword: #"parse-state";

  // A format string for constructing an error message
  constant slot error-format-string :: <string>, 
    required-init-keyword: #"error-format-string";

  // Arguments to error-format-string
  constant slot error-format-args :: <sequence>,
    required-init-keyword: #"error-format-args";
end class <time-parsing-error>;

define sealed domain make (singleton(<time-parsing-error>));
define sealed domain initialize (<time-parsing-error>);

define sealed method report-condition (cond :: <time-parsing-error>, stream)
 => ();
  condition-format
    (stream, "Time parsing error at position %d of input stream %=:\n",
     cond.error-parse-state.input-stream.stream-position,
     cond.error-parse-state.input-stream);
  condition-format(stream, "While handling the ");
  if (cond.error-parse-state.current-directive ~== #f)
    condition-format(stream, "%%%c directive", 
		     cond.error-parse-state.current-directive);
  else
    condition-format(stream, "format-string character");
  end if;
  condition-format(stream, " at position %d in\nformat-string %=:\n",
		   cond.error-parse-state.format-stream.stream-position,
		   cond.error-parse-state.time-format-string);
  apply(condition-format, stream, cond.error-format-string,
	cond.error-format-args);
end method report-condition;

define function parse-error
    (state :: <parse-state>, format-string :: <string>, #rest format-args)
 => ();
  let err = make(<time-parsing-error>, parse-state: state,
		 error-format-string: format-string,
		 error-format-args: format-args);
  signal(err);
end function parse-error;


// Stream utility functions, all internal

define function expect (state :: <parse-state>, expected :: <character>) => ();
  let actual = read-element(state.input-stream);
  if (expected ~== actual)
    parse-error(state, "Expected %= but instead got %=", expected, actual);
  end if;
end function expect;

// Read-to consumes the character that sets off the test.  We don't
// want that.  (This function could probably be implemented as read-to
// followed by unread, but this should work too)
//
define inline function read-while (input :: <stream>, test :: <function>) 
 => read :: <string>;
  let res = make(<stretchy-vector>);
  block (break)
    for (c = peek(input) then peek(input))
      if (~test(c))
	break();
      end if;
      add!(res, read-element(input));
    end for;
  exception (<end-of-stream-error>)
    #f;  // ignore it
  end block;
  as(<string>, res);
end function read-while;


define constant $day-numbers
  = begin
      let table = make(<case-insensitive-string-table>);
      table["monday"] := table["mon"] := 0;
      table["tuesday"] := table["tue"] := 1;
      table["wednesday"] := table["wed"] := 2;
      table["thursday"] := table["thu"] := 3;
      table["friday"] := table["fri"] := 4;
      table["saturday"] := table["sat"] := 5;
      table["sunday"] := table["sun"] := 6;
      table;
    end;

define constant $month-numbers
  = begin
      let table = make(<case-insensitive-string-table>);
      table["january"] := table["jan"] := 1;
      table["febuary"] := table["feb"] := 2;
      table["march"] := table["mar"] := 3;
      table["april"] := table["apr"] := 4;
      table["may"] := table["may"] := 5;
      table["june"] := table["jun"] := 6;
      table["july"] := table["jul"] := 7;
      table["august"] := table["aug"] := 8;
      table["september"] := table["sep"] := 9;
      table["october"] := table["oct"] := 10;
      table["november"] := table["nov"] := 11;
      table["december"] := table["dec"] := 12;
      table;
    end;


define class <parse-state> (<object>)

  // The format-string we are trying to parse the time with
  constant slot time-format-string :: <string>, 
    required-init-keyword: #"format-string";

  // format-string, as a stream
  constant slot format-stream :: <stream>, 
    required-init-keyword: #"format-stream";

  // The directive we're currently trying to process, or #f if we're
  // not trying to process one.  Used by the error handling stuff to
  // give informative messages.
  slot current-directive :: false-or(<character>) = #f;

  // The stream we're trying to parse
  constant slot input-stream :: <stream>, 
    required-init-keyword: #"input-stream";

  // A sequence of keywords and values we can pass to make(<time>)
  constant slot time-init-args = make(<stretchy-vector>);

  // We change this if we're asked to read in a 12-hour time.  We need
  // this slot because we can't make sense of a 12 hour time without
  // knowing whether it's am or pm.  If we're asked to read a 24-hour
  // time, this slot isn't touched.
  slot parsed-hour :: false-or(limited(<integer>, min: 1, max: 12)) = #f;
    
  // The status of am/pm, so we can process parsed-hour.
  slot pm? :: <boolean> = #f;

  // We change this if we're asked to read in a day-of-year.  Since we
  // can't make sense of a day-of-year without knowning if it's a
  // leap-year (and thus, knowing the year), we delay processing of
  // this information.
  slot parsed-day-of-year :: false-or(<integer>) = #f;

  // Used for processing day-of-year.  The <decoded-time> doesn't
  // actually get it's year from this slot, it gets that from the
  // init-args.
  slot parsed-year :: false-or(<integer>) = #f;
end class <parse-state>;
    
define sealed domain make (singleton(<parse-state>));
define sealed domain initialize (<parse-state>);

define function add-init-arg 
    (state :: <parse-state>, keyword :: <symbol>, value :: <object>) 
 => ();
  add!(state.time-init-args, keyword);
  add!(state.time-init-args, value);
end function add-init-arg;


// parse-time -- exported
//
define function parse-time (input :: <stream>, format-string :: <string>)
 => res :: <decoded-time>;
  let state 
    = make(<parse-state>, input-stream: input, format-string: format-string,
	   format-stream: make(<string-stream>, contents: format-string));
  block ()  // catch eof on input stream
    for (format-char = read-element(state.format-stream, on-end-of-stream: #f)
	   then read-element(state.format-stream, on-end-of-stream: #f),
       until: format-char == #f)
      if (format-char.whitespace?)
	parse-whitespace(state);
      elseif (format-char ~== '%')
	expect(state, format-char);
      else
	let format-directive = read-element(state.format-stream, 
					    on-end-of-stream: #f);
	if (format-directive == #f)
	  error("Unexpected end-of-string in %=\n"
		  "-- wanted a format directive after final %%",
		format-string);
	end if;
	state.current-directive := format-directive;
	process-format-directive(state, format-directive);
	state.current-directive := #f;
      end if;
    end for;
  exception (<end-of-stream-error>)
    // We can only assume the stream in question is the input stream
    parse-error(state, "Unexpected end of stream");
  end block;

  do-post-processing(state);

  apply(make, <decoded-time>, state.time-init-args);
end function parse-time;

define function process-format-directive
    (state :: <parse-state>, directive :: <character>) => ();
  select (directive)
    '%' => expect(state, '%');
    'a', 'A' => parse-day-name(state);
    'b', 'B', 'h' => parse-month-name(state);
    'c' => parse-date-time(state);  // local date-time rep
    'd', 'e' => parse-day-of-month(state);
    'D' => parse-month-day-year(state);
    'H' => parse-24-hour(state);
    'I' => parse-12-hour(state);
    'j' => parse-day-of-year(state);
    'm' => parse-month-number(state);
    'M' => parse-minute(state);
    'n', 't' => parse-whitespace(state);
    'p' => parse-am-or-pm(state);
    'r' => parse-12-hour-time-with-am-pm(state); // local 12hr time 
                                                 // with %p
    'R' => parse-hour-and-minute(state);
    'S' => parse-seconds(state);
    'T' => parse-hour-minutes-seconds(state);
    'w' => parse-day-of-week(state);
    'x' => parse-month-day-year(state); // local date rep
    'X' => parse-hour-minutes-seconds(state);  // local time rep
    'y' => parse-year-implicit-1900(state);
    'Y' => parse-year-no-implicit-1900(state);

    otherwise => error("Unrecognized format directive %=", directive);
  end select;
end function process-format-directive;

// compute things we couldn't have figured out before we finished parsing
//
define function do-post-processing (state :: <parse-state>) => ();
  if (state.parsed-hour ~== #f)
    let hour = state.parsed-hour + if (state.pm?) 12 else 0 end;
    // Now, we convert 12am to 0, and 12pm (noon) to 12 (reversing the
    // +12 for pm before)
    let hour = if (hour == 12 | hour == 24) hour - 12 else hour end;
    add-init-arg(state, hours: hour);
  end if;
  if (state.parsed-day-of-year ~== #f)
    if (~state.parsed-year)
      error("Can't process a day-of-year without knowing the year");
    end if;
    let days-this-year = state.parsed-day-of-year;
    let year = state.parsed-year;
    let month :: <integer> = 1;
    while (days-this-year > days-before-month(month + 1, year))
      month := month + 1;
    end while;
    let day-of-month = days-this-year - days-before-month(month, year);
    add-init-arg(state, month: month);
    add-init-arg(state, day-of-month: day-of-month);
  end if;
end function do-post-processing;


define function parse-day-name (state :: <parse-state>) => ();
  let day-name = read-while(state.input-stream, alpha?);
  let day-number = element($day-numbers, day-name, default: #f);
  if (~day-number)
    parse-error(state, "Unrecognized day name %S", day-name);
  else
    add-init-arg(state, day-of-week: day-number);
  end if;
end function parse-day-name;

define function parse-month-name (state :: <parse-state>) => ();
  let month-name = read-while(state.input-stream, alpha?);
  let month-number = element($month-numbers, month-name, default: #f);
  if (~month-number)
    parse-error(state, "Unrecognized month name %S", month-name);
  else
    add-init-arg(state, month: month-number);
  end if;
end function parse-month-name;

define function parse-day-of-month (state :: <parse-state>) => ();
  let num-string = read-while(state.input-stream, digit?);
  let day-of-month = string-to-integer(num-string);
  if (day-of-month > 31 | day-of-month == 0)
    parse-error(state, "Invalid day-of month %d (wanted 1-31)", day-of-month);
  end if;
  add-init-arg(state, day-of-month: day-of-month);
end function parse-day-of-month;

define function parse-month-day-year (state :: <parse-state>) => ();
  parse-month-number(state);
  expect(state, '/');
  parse-day-of-month(state);
  expect(state, '/');
  parse-year-implicit-1900(state);
end function parse-month-day-year;

define function parse-24-hour (state :: <parse-state>) => ();
  let num-string = read-while(state.input-stream, digit?);
  let hour = string-to-integer(num-string);
  if (hour > 23)
    parse-error(state, "Invalid hour of day %d (wanted 0-23)", hour);
  end if;
  add-init-arg(state, hours: hour);
end function parse-24-hour;

define function parse-12-hour (state :: <parse-state>) => ();
  let num-string = read-while(state.input-stream, digit?);
  let hour = string-to-integer(num-string);
  if (hour > 12 | hour == 0)
    parse-error(state, "Invalid hour %d (wanted 1-12)", hour);
  end if;
  state.parsed-hour := hour;  // can't make sense of it until we know am or pm
end function parse-12-hour;

define function parse-day-of-year (state :: <parse-state>) => ();
  let num-string = read-while(state.input-stream, digit?);
  let day-of-year = string-to-integer(num-string);
  if (day-of-year > 366 | day-of-year == 0)
    parse-error(state, "Invalid day-of-year %d (wanted 0-366)", day-of-year);
  end if;
  // can't make sense of it until we know if this is a leap year or not
  state.parsed-day-of-year := day-of-year;
end function parse-day-of-year;

define function parse-month-number (state :: <parse-state>) => ();
  let num-string = read-while(state.input-stream, digit?);
  let month = string-to-integer(num-string);
  if (month > 12 | month == 0)
    parse-error(state, "Invalid month number %d (wanted 1-12)", month);
  end if;
  add-init-arg(state, month: month);
end function parse-month-number;

define function parse-whitespace (state :: <parse-state>) => ();
  read-while(state.input-stream, whitespace?);
end function parse-whitespace;

define function parse-am-or-pm (state :: <parse-state>) => ();
  let string = read-while(state.input-stream, alpha?);
  let string = as-lowercase(string);
  select (string by \=)
    "am" => state.pm? := #f;
    "pm" => state.pm? := #t;
    otherwise => parse-error(state, "Expected either AM or PM, not %=", 
			     string);
  end select;
end function parse-am-or-pm;

define function parse-seconds (state :: <parse-state>) => ();
  let num-string = read-while(state.input-stream, digit?);
  let seconds = string-to-integer(num-string);
  if (seconds > 59)
    parse-error(state, "Invalid number of seconds %d (wanted 0-59)", seconds);
  end if;
  add-init-arg(state, seconds: seconds);
end function parse-seconds;

define function parse-hour-and-minute (state :: <parse-state>) => ();
  parse-24-hour(state);
  expect(state, ':');
  parse-minute(state);
end function parse-hour-and-minute;

define function parse-hour-minutes-seconds (state :: <parse-state>) => ();
  parse-24-hour(state);
  expect(state, ':');
  parse-minute(state);
  expect(state, ':');
  parse-seconds(state);
end function parse-hour-minutes-seconds;

// C's day of week is Sunday=0, Dylan's is Monday=0, so it has to do
// some conversions
//
define function parse-day-of-week (state :: <parse-state>) => ();
  let num-string = read-while(state.input-stream, digit?);
  let day-number = string-to-integer(num-string);
  if (day-number > 6)
    parse-error(state, "Invalid day-of-week %d (wanted 0-6)", day-number);
  end if;
  let internal-day-number
    = if (day-number == 0) 6 else day-number - 1 end if;
  add-init-arg(state, day-of-week: internal-day-number);
end function parse-day-of-week;

define function parse-minute (state :: <parse-state>) => ();
  let num-string = read-while(state.input-stream, digit?);
  let minutes = string-to-integer(num-string);
  if (minutes > 59)
    parse-error(state, "Invalid minute %d (wanted 0-59)", minutes);
  end if;
  add-init-arg(state, minutes: minutes);
end function parse-minute;

define function parse-year-no-implicit-1900 (state :: <parse-state>) => ();
  let num-string = read-while(state.input-stream, digit?);
  let year = string-to-integer(num-string);
  add-init-arg(state, year: year);
end function parse-year-no-implicit-1900;

define function parse-year-implicit-1900 (state :: <parse-state>) => ();
  let num-string = read-while(state.input-stream, digit?);
  let year = string-to-integer(num-string);
  let year = if (year > 0 & year < 100)
	       year + 1900;
	     else 
	       year;
	     end if;
  add-init-arg(state, year: year);
end function parse-year-implicit-1900;

define function parse-date-time (state :: <parse-state>) => ();
  parse-month-day-year(state);
  read-while(state.input-stream, whitespace?);
  parse-hour-minutes-seconds(state);
end function parse-date-time;

define function parse-12-hour-time-with-am-pm (state :: <parse-state>) => ();
  parse-12-hour(state);
  expect(state, ':');
  parse-minute(state);
  expect(state, ':');
  parse-seconds(state);
  parse-whitespace(state);  // ### Seems like a good idea, but who knows...
  parse-am-or-pm(state);
end function parse-12-hour-time-with-am-pm;

