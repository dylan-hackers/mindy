module: dylan-user

define library gtk-2-internal
  use common-dylan;
  use io;
  use dylan; // for extensions
  use melange-support;
  
  export gtk-internal-all;
end library gtk-2-internal;

