Module: Dylan-user
Copyright: Copyright 2001 Peter S. Housel
License: GNU Lesser General Public License

define module gdk
  use Common-Dylan;
  use Melange-support, exclude: { subclass };
  use gdk-extra, export: all;
  use gdk-internal, export: all;
end module;
