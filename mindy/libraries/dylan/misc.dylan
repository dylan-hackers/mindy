module: Dylan

//////////////////////////////////////////////////////////////////////
//
//  Copyright (C) 1994, Carnegie Mellon University
//  All rights reserved.
//
//  This code was produced by the Gwydion Project at Carnegie Mellon
//  University.  If you are interested in using this code, contact
//  "Scott.Fahlman@cs.cmu.edu" (Internet).
//
//////////////////////////////////////////////////////////////////////
//
//  $Header: /home/housel/work/rcs/gd/src/mindy/libraries/dylan/misc.dylan,v 1.3 1994/06/01 20:33:17 rgs Exp $
//
//  This file does whatever.
//

define method identity (x)
  x;
end;

// The built-in "as(<symbol>...)" method only handles byte-strings.
define method as(cls == <symbol>, str :: <string>)
  as(<symbol>, as(<byte-string>, str));
end method as;
