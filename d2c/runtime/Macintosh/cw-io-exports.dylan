module: dylan-user
author: gabor@mac.com
synopsis: This file provides exports for standard I/O stuff for the CodeWarrior IDE.
rcs-header: $Header:
copyright: (c) Gabor Greif 2000

define library standard-io
  use dylan;
  use streams;
/**/  use format;
  export standard-io;
end library;

define module standard-io
  use dylan;
  use streams,
    import: {<buffered-byte-string-output-stream>, <fd-stream>};
//**/  use format,
//    import: {format};
  export
    *standard-input*, *standard-output*, *standard-error* /* , format-out */;
end module;

