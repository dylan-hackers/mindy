module: params
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/Attic/params.dylan,v 1.4 1996/01/12 00:58:19 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.

define constant $fixed-integer-bits = 32;

define constant $minimum-integer
  = ash(as(<extended-integer>, -1),
	$fixed-integer-bits - 1);

define constant $maximum-integer
  = lognot($minimum-integer);

