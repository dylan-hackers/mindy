module: representation
rcs-header: $Header: /scm/cvs/src/d2c/compiler/base/rep.dylan,v 1.1 1998/05/03 19:55:30 andreas Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// All rights reserved.
// 
// Use and copying of this software and preparation of derivative
// works based on this software are permitted, including commercial
// use, provided that the following conditions are observed:
// 
// 1. This copyright notice must be retained in full on any copies
//    and on appropriate parts of any derivative works.
// 2. Documentation (paper or online) accompanying any system that
//    incorporates this software, or any part of it, must acknowledge
//    the contribution of the Gwydion Project at Carnegie Mellon
//    University.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports, questions, comments, and suggestions should be sent by
// E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
//
//======================================================================

define abstract class <representation> (<object>)
end;

define abstract class <data-word-representation> (<representation>)
end;

define generic pick-representation
    (type :: <ctype>, optimize-for :: one-of(#"speed", #"space"))
    => res :: <representation>;

define generic representation-alignment (rep :: <representation>)
    => alignment :: <integer>;

define generic representation-size (rep :: <representation>)
    => size :: <integer>;

define generic representation-has-bottom-value? (rep :: <representation>)
    => res :: <boolean>;

define generic use-data-word-representation
    (class :: <cclass>, data-word-type :: <ctype>) => ();

define generic use-general-representation
    (class :: <cclass>) => ();

