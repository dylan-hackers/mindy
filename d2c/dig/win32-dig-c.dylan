module: d2c-gnu

#if (mindy)
define constant anonymous-3 = find-c-function("ignore_interrupts");
define method ignore-interrupts
    ()
 => ();
  anonymous-3();
  values();
end method ignore-interrupts;

#else
c-include("c:/fulgham/projects/gd/src/d2c/dig/win32-dig-support.h");

define method ignore-interrupts
    ()
 => ();
  call-out("ignore_interrupts", void:);
  values();
end method ignore-interrupts;

#endif
