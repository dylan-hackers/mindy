module: representation
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/rep.dylan,v 1.4 1996/04/13 21:11:07 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.

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

