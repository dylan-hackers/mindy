module: dylan-user
author: Douglas M. Auclair, dauclair@hotmail.com

/// This library implements the Common-Dylan Date module as described
/// in the "System and I/O" document from Functional Objects, inc.

/// The Date module here provides the interface and the other
/// modules implement that interface according to the section
/// in the "System and I/O" doc.

define library date
  use dylan;
  use collection-extensions;
  use string-extensions;
  use streams;
  use time;

  export date;
end library;

define module date
  create <date>, <day-of-week>, encode-date, current-date;

  create <duration>, <year/month-duration>, <day/time-duration>,
    encode-year/month-duration, encode-day/time-duration,
    decode-duration;
  
  create local-time-zone-name, local-time-zone-offset;

  create decode-date, 
    date-year, date-month, date-day, date-day-of-week, date-hours,
    date-minutes, date-seconds, date-microseconds, date-time-zone-offset,
    as-iso8601-string;
end module date;

// Section 7.2:  Representing Dates and Times
define module dates-and-times
  use date;

  use dylan;
  use extensions;
  use string-conversions;
  use streams;
  use time-io;
  use time, 
    exclude: { <timezone>, <day-of-week> },
    rename: { <day-of-month> => <days> },
    export: all;

  export <years>, <timezone>, <microseconds>, time, microseconds, false-to-0;
end module dates-and-times;

// Section 7.3: Representing Durations
define module durations
  use dylan;
  use dates-and-times;

  use date;

  export duration, usec;
end module durations;

// Section 7.4.1-2: Comparing dates and durations
define module comparing
  use dylan;
  use dates-and-times;
  use durations;

  use date;
end module comparing;

// Section 7.4.3: Arithmetic for dates and durations
// still must do mixed date-duration implementation arithmetic
define module arithmetic
  use dylan;
  use dates-and-times;
  use durations;

  use date;
end module arithmetic;

// Section 7.4.4: Dealing with time-zones
define module time-zones
  use dylan;
  use dates-and-times;
  use time-io;
  use streams;

  use date;
end module time-zones;

// Section 7.5: Extracting information from dates
define module extracting
  use dylan;
  use dates-and-times;
  use time-io;
  use streams;
  use sequence-utilities;

  use date;
end module extracting;
