module: representation
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/rep.dylan,v 1.2 1995/04/25 02:49:45 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.

define abstract class <representation> (<object>)
end;

define generic pick-representation
    (type :: <ctype>, optimize-for :: one-of(#"speed", #"space"))
    => res :: <representation>;

define generic representation-alignment (rep :: <representation>)
    => alignment :: <fixed-integer>;

define generic representation-size (rep :: <representation>)
    => size :: <fixed-integer>;

define generic representation-has-bottom-value? (rep :: <representation>)
    => res :: <boolean>;
