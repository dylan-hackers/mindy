module: dylan-user

define module gtk-internal-support
  use dylan;
  use extensions;
  use introspection;
  use system;
  use melange-support;
  use gtk-internal;

  export gtk-init, find-gtype, g-type-from-instance, g-value-type;
end module gtk-internal-support;

define module gtk-internal-all
  use gtk-internal, export: all;
  use gtk-internal-support, export: all;
end module gtk-internal-all;
  