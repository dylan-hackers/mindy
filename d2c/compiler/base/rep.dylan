module: representation
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/rep.dylan,v 1.3 1996/01/12 00:58:19 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.

define abstract class <representation> (<object>)
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
