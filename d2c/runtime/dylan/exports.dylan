rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/exports.dylan,v 1.2 1995/11/13 23:08:09 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera

define library dylan
  export
    dylan;
end;

define module dylan
  use dylan-viscera,
    import: all,
    export: {};
end;

