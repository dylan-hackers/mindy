module: standard-io
author: gabor@mac.com
synopsis: This file provides some standard I/O stuff for the CodeWarrior IDE.
rcs-header: $Header:
copyright: (c) Gabor Greif 2000

define constant *standard-input* :: <fd-stream>
  = make(<fd-stream>, fd: 0);

define constant *standard-output* :: <buffered-byte-string-output-stream>
  = make(<buffered-byte-string-output-stream>);

define constant *standard-error* :: <buffered-byte-string-output-stream>
  = make(<buffered-byte-string-output-stream>);

/*
// should come from format-out!

define sealed inline method format-out
    (control-string :: <byte-string>, #rest args)
 => ();
  apply(format, *standard-output*, control-string, args);
end method format-out;
*/