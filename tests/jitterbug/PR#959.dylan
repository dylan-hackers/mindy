module: PR-959
use-libraries: common-dylan,io
use-modules: common-dylan, format-out
synopsis: case1 and case2 cause run-time errors not caught by d2c.
author: Brent Fulgham <bfulgham@debian.org>

define variable case1 :: <double-float> = 0;
define constant case2 :: <double-float> = 0;

define function main(name, arguments)
  format-out("The value is %=\n", case1);
  format-out("The value is %=\n", case2);
  exit-application(0);
end function main;

main(application-name(), application-arguments());
