module: redirect-io

#if (~mindy)
*warning-output* := *standard-output*;
*gdb-output* := *standard-output*;
#else
// XXX - Allow Mindy to compile this file.
1;
#endif
