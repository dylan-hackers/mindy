module: params
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/Attic/params.dylan,v 1.3 1995/04/25 02:49:45 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.

define constant $fixed-integer-bits = 32;

define constant $minimum-fixed-integer
  = ash(as(<extended-integer>, -1),
	$fixed-integer-bits - 1);

define constant $maximum-fixed-integer
  = lognot($minimum-fixed-integer);

