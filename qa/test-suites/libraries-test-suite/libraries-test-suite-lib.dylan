Module:    dylan-user
Author:    Shri Amit, Andy Armstrong
Synopsis:  A wrapper suite around the test-suites for various dylan libraries
Copyright: Copyright (c) 1996-2001 Functional Objects, Inc. All rights reserved.

define library libraries-test-suite
//  use functional-dylan;
  use testworks;
  use dylan-test-suite;
  use common-dylan-test-suite;
  use random;
  //use functional-dylan-test-suite;
  use collections-test-suite;
  //use system-test-suite;
  //use io-test-suite;
  use testworks-test-suite;

  export libraries-test-suite
end library libraries-test-suite;

define module libraries-test-suite
  //use functional-dylan;
  use testworks;
  use dylan-test-suite;
  use common-dylan-test-suite;
  use random;
  //use functional-dylan-test-suite;
  use collections-test-suite;
  //use system-test-suite;
  //use io-test-suite;
  use testworks-test-suite;

  export libraries-test-suite
end module libraries-test-suite;
