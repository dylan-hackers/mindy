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
//  $Header: /home/housel/work/rcs/gd/src/mindy/libraries/dylan/char.dylan,v 1.2 1994/03/30 06:07:19 wlott Exp $
//
//  This file does whatever.
//

define method \< (c1 :: <character>, c2 :: <character>)
  as(<integer>, c1) < as(<integer>, c2);
end;

define method as-uppercase (c :: <character>)
  if ('a' <= c & c <= 'z')
    as(<character>, as(<integer>, c) - 32);
  else
    c;
  end;
end;

define method as-lowercase (c :: <character>)
  if ('A' <= c & c <= 'Z')
    as(<character>, as(<integer>, c) + 32);
  else
    c;
  end;
end;
