module:    Comparing
synopsis:  Common-Dylan compliance for the Date module
author:    Douglas M. Auclair
copyright: LGPL, 2000

/// This implementation module defines the dates and times.
/// It conforms to sections 7.4.1 and 7.4.2 of the Date Module description
/// of the "System and I/O" document from Functional Objects, Inc.

// 7.4.1 Comparing Dates
define method \=(d1 :: <date>, d2 :: <date>) => (equal? :: <boolean>)
  d1.time.encode-time = d2.time.encode-time;
end method \=;

define method \<(d1 :: <date>, d2 :: <date>) => (before? :: <boolean>)
  d1.time.encode-time < d2.time.encode-time;
end method \<;

// 7.4.2 Comparing Durations
define method \=(d1 :: <year/month-duration>, d2 :: <year/month-duration>) 
 => (equal? :: <boolean>)
  d1.duration = d2.duration;
end method \=;

define method \=(d1 :: <day/time-duration>, d2 :: <day/time-duration>) 
 => (equal? :: <boolean>)
  d1.duration = d2.duration;
end method \=;

define method \<(d1 :: <year/month-duration>, d2 :: <year/month-duration>) 
 => (before? :: <boolean>)
  d1.duration < d2.duration;
end method \<;

define method \<(d1 :: <day/time-duration>, d2 :: <day/time-duration>) 
 => (before? :: <boolean>)
  d1.duration < d2.duration;
end method \<;
