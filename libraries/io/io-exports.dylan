module: dylan-user

define library io
  use dylan;

  use streams, export: {streams};
  use print, export: {print, pprint};
  use format, export: {format};
  use standard-io, export: {standard-io};
  use format-out, export: {format-out};
end library;

// XXX - Redirecting *warning-ouput* and *gdb-output* in a strange place.
// We need to redirect these so that the runtime can use the real 'format'
// implementation when printing conditions. Ideally, we should redirect these
// somewhere in the standard-io library, but we can't--that library is needed
// to bootstrap d2c, and must therefore compile with the 2.2.0 runtime, which
// lacks *gdb-output*. But the io library isn't part of the bootstrap
// process (in fact, it's explicitly avoided), so this is a safe place for
// this code to live.

define module redirect-io
  use dylan;

#if (~mindy)
  use standard-io;
  use extensions,
     import: {*warning-output*};
  use system,
     import: {*gdb-output*};
#endif
end module;
