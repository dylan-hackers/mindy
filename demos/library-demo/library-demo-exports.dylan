rcs-header: $Header: /home/housel/work/rcs/gd/src/demos/library-demo/library-demo-exports.dylan,v 1.3 1995/12/13 00:52:14 wlott Exp $
module: dylan-user

define library gobbledygook
  use dylan;
  use fact;
end library;

define module gobbledygook
  use dylan;
  use extensions;
  use cheap-io;
  use fact;
end module;
