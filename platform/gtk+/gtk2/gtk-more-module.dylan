module: dylan-user

define module gtk-support
  use dylan;
  use extensions;
  use melange-support;
  use gtk-internal;

  export g-signal-connect, g-signal-connect-swapped, gtk-init;
end module gtk-support;

define module gtk
  use gtk-internal, export: all;
  use gtk-support, export: all;
end module gtk;
  