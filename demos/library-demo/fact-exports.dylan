rcs-header: $Header: /home/housel/work/rcs/gd/src/demos/library-demo/fact-exports.dylan,v 1.1 1995/11/15 20:21:15 ram Exp $
module: dylan-user

define library fact
  use dylan;
  export fact;
end library;

define module fact
  use dylan;
  export fact;
end module;
