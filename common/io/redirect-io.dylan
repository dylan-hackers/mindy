module: redirect-io

#if (d2c)
*warning-output* := *standard-output*;
*gdb-output* := *standard-output*;
#endif

define function do-nothing() => ()
  // XXX - Allow Mindy to compile this file.
  values();
end function do-nothing;
