module: dylan-user

define library gtk-2
  use common-dylan;
  use io;
  use dylan; // for extensions
  use melange-support;
  use gtk-2-internal;

  export gtk;
end library gtk-2;

define module gtk-support
  use dylan;
  use common-dylan;
  use extensions;
  use common-extensions;
  use introspection;
  use system;
  use format-out;
  use melange-support;
  use gtk-internal-all;

  export g-signal-connect;
end module gtk-support;

define module gtk
  use gtk-internal-all, export: all;
  use gtk-support, export: all;
end module gtk;
  