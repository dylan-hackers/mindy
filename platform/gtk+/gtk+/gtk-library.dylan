Module: Dylan-user
Copyright: Copyright 2001 Peter S. Housel
License: GNU Lesser General Public License

define library gtk
  use Dylan;
  use Common-Dylan;
  use Melange-support;
  use glib;
  use gdk;
  export gtk;
end library;

define module gtk-extra
  use Common-Dylan;
  use Melange-support, exclude: { subclass };
  use System, import: { pointer-deref, pointer-deref-setter }, export: all;
  use glib;
  export <GtkType>, <GtkType*>;
  export
    <GtkFunction>, <GtkSignalFunc>, <GtkCallbackMarshal>, <GtkDestroyNotify>;
end module;
