module: dylan-user
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/main/bootstrap-exports.dylan,v 1.1 1995/11/14 13:48:02 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.

// Have to use the define %%module internal form because define module
// hasn't been defined yet.
//
define %%module dylan-viscera
  %%export
    module-definer, library-definer;
end;

