Module:       dylan-user
Synopsis:     IO library test suite
Author:       Andy Armstrong, et al...
Copyright:    Original Code is Copyright (c) 1996-2001 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library io-test-suite
  use common-dylan;
  use io;

  use testworks;
  use testworks-specs;

  use common-dylan-test-suite;	// For stream testing protocol

  export io-test-suite;
end library io-test-suite;

define module io-test-suite
  use common-dylan,
    exclude: { format-to-string };
  use simple-random;
  use threads;

  use streams;
  use streams-internals;
  use print;
  use print-internals;
  use format;

  use testworks;
  use testworks-specs;

  use common-dylan-test-suite;	// For stream testing protocol

  // IO test suite
  export io-test-suite;
end module io-test-suite;
