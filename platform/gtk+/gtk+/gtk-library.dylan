Module: Dylan-user
Copyright: Copyright 2001 Peter S. Housel
License: GNU Lesser General Public License

define library gtk
  use Common-Dylan;
  use Melange-support;
  use glib;
  use gdk;
  export gtk;
end library;

define module gtk-extra
  use Common-Dylan;
  use Melange-support, exclude: { subclass };
  use System, import: { <raw-pointer> };
  use System,
    import: { <raw-pointer>, pointer-deref, pointer-deref-setter },
    export: { pointer-deref, pointer-deref-setter };
  use glib;
  export <GtkType>, <GtkType*>;
  export
    <GtkFunction>, <GtkSignalFunc>, <GtkCallbackMarshal>, <GtkDestroyNotify>;
end module;
