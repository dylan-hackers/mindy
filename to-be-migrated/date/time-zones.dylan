module:    Time-Zones
synopsis:  Common-Dylan compliance for the Date module
author:    Douglas M. Auclair
copyright: LGPL, 2000

/// This implementation module defines the dates and times.
/// It conforms to section 7.4.4 of the Date Module description
/// of the "System and I/O" document from Functional Objects, Inc.

define function local-time-zone-name() => (tzn :: <string>)
  let ans = "   ";
  let stream = make(<byte-string-stream>, contents: ans);
  let time = get-decoded-time(timezone: get-local-timezone() * 3600);
  format-time(stream, "%Z", time);
  ans;
end function local-time-zone-name;

define function local-time-zone-offset() => (tzo :: <integer>)
  get-local-timezone() * 60;
end function local-time-zone-offset;
