Module: Dylan-user
Copyright: Copyright 2001 Peter S. Housel
License: GNU Lesser General Public License

define library gdk
  use Dylan;
  use Common-Dylan;
  use Melange-support;
  use glib;
  export gdk;
end library;

define module gdk-extra
  use Common-Dylan;
  use Melange-support, exclude: { subclass };
  use System, import: { pointer-deref, pointer-deref-setter }, export: all;
  use glib;
  export
    <GdkAtom>,
    <GdkModifierType*>,
    <GdkWindow**>,
    <GdkGC**>;
end module;
