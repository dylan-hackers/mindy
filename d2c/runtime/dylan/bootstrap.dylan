rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/bootstrap.dylan,v 1.1 1995/11/13 23:07:39 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera

// This file initializes the module system sufficiently so that we can
// start compiling real definitions.  Like the define module macro.

// Have to use the define %%module internal form because define module
// hasn't been defined yet.
//
define %%module dylan-viscera
end;
