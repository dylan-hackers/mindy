module: tk
author: Robert Stockton (rgs@cs.cmu.edu)

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
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
//
// This file contains support for <canvas>s and <canvas-item>s.
//
//======================================================================

define class <canvas> (<window>)
  slot items :: <mutable-explicit-key-collection> = make(<equal-table>);
end class;

define-widget(<canvas>, "canvas",
	      #"closeenough", #"confine", #"height", #"insertbackground",
	      #"insertborderwidth", #"insertofftime", #"insertontime",
	      #"insertwidth", #"xscrollincrement", #"yscrollincrement",
	      #"scrollregion", #"selectbackground", #"selectborderwidth",
	      #"selectforeground", #"width", #"xscrollcommand",
	      #"yscrollcommand");

define class <canvas-item> (<object>)
  slot window :: <window>, required-init-keyword: #"in";
  slot name :: <string>, required-init-keyword: #"name";
end class <canvas-item>;

define class <canvas-tag> (<object>)
  slot window :: <window>, required-init-keyword: #"in";
  slot name :: <string>, required-init-keyword: #"name";
end class <canvas-tag>;

define constant <tag-or-item> = type-union(<canvas-tag>, <canvas-item>);

define method initialize (object :: <canvas-item>, #next next, #key, #all-keys)
  next();
  object.window.items[object.name] := object;
end method initialize;

define method initialize (object :: <canvas-tag>, #next next, #key, #all-keys)
  next();
end method initialize;

define method as (cls == <string>, value :: <canvas-item>)
 => string :: <string>;
  value.name;
end method as;

define method as (cls == <string>, value :: <canvas-tag>)
 => string :: <string>;
  value.name;
end method as;

define method configure
    (item :: <tag-or-item>, #rest options, #all-keys)
 => (item :: <tag-or-item>);
  apply(put-tk-line, item.window, " itemconfigure ", item.name,
	std-options(#[#"anchor", #"arrow", #"arrowshape", #"capstyle",
		      #"joinstyle", #"smooth", #"splinesteps", #"font",
		      #"justify", #"text", #"bitmap", #"extent", #"fill",
		      #"outline", #"start", #"stipple", #"style", #"width",
		      #"outlinestipple", #"tags", #"background", #"foreground",
		      #"image", #"height", #"window"],
		    #t, options));
  item;
end method configure;

define method configuration
    (item :: <tag-or-item>) => (result :: <sequence>);
  let string = call-tk-function(item.window, " itemconfigure ", item.name);
  parse-tk-list(string, depth: 2);
end method configuration;

define method bind
    (item :: <tag-or-item>, event :: <string>, command)
 => (item :: <tag-or-item>);
  put-tk-line(item.window, " bind ", item.name, " ", event,
	       " {" , command, "}");
  item;
end method bind;

define method delete-item (item :: <tag-or-item>, #rest rest) => ();
  put-tk-line(item.window, " delete ",
	      apply(join-tk-args, item.name, map(name, rest)));
end method delete-item; 

define method raise-item
    (item :: <tag-or-item>, #key past :: false-or(<tag-or-item>)) => ();
  put-tk-line(item.window, " raise ", item.name, " ",
	       if (past) past else "" end if);
end method raise-item;

define method lower-item
    (item :: <tag-or-item>, #key past :: false-or(<tag-or-item>)) => ();
  put-tk-line(item.window, " lower ", item.name, " ",
	       if (past) past else "" end if);
end method lower-item;

define method move-item
    (item :: <tag-or-item>, x :: <object>, y :: <object>) => ();
  put-tk-line(item.window, " move ", item.name, " ", x, " ", y);
end method move-item;

define method scale-item
    (item :: <tag-or-item>, x-origin :: <object>, y-origin :: <object>,
     x-scale :: <object>, y-scale :: <object>)
 => ();
  put-tk-line(item.window, " scale ", item.name, " ", x-origin, " ",
	       y-origin, " ", x-scale, " ", y-scale);
end method scale-item;

define method item-coords (item :: <tag-or-item>) => (result :: <sequence>);
  map(curry(tk-as, <integer>),
      parse-tk-list(call-tk-function(item.window, " coords ", item.name)));
end method item-coords;

define method item-coords-setter
    (value :: <sequence>, item :: <tag-or-item>) => ();
  put-tk-line(item.window, " coords ", item.name, " ",
	      apply(join-tk-args, value));
end method item-coords-setter;

define method item-type (item :: <tag-or-item>) => (result :: <string>);
  call-tk-function(item.window, " type ", item.name);
end method item-type;

define method xview (canvas :: <canvas>, index) => canvas :: <canvas>;
  put-tk-line(canvas, " xview ", tk-as(<string>, index));
  canvas;
end method xview;

define method yview (canvas :: <canvas>, index) => canvas :: <canvas>;
  put-tk-line(canvas, " yview ", tk-as(<string>, index));
  canvas;
end method yview;

define method focus (canvas :: <canvas>) => (result :: <canvas-item>);
  canvas.items[call-tk-function(canvas, " focus")];
end method focus;

define method focus-setter (value :: <tag-or-item>, canvas :: <canvas>) => ();
  if (value.window ~= canvas)
    error("Can't focus on item not in canvas %=: %=", canvas, value);
  end if;
  put-tk-line(canvas, "focus ", value);
end method focus-setter;

define method scan-mark
    (window :: <canvas>, #rest coords) => (window :: <canvas>);
  put-tk-line(window, " scan mark ", apply(join-tk-args, coords));
  window;
end method scan-mark;

define method scan-dragto
    (window :: <canvas>, #rest coords) => window :: <canvas>;
  put-tk-line(window, " scan dragto ", apply(join-tk-args, coords));
  window;
end method scan-dragto;

define method select-item
    (window :: <canvas>, index) => (window :: <canvas>);
  put-tk-line(window, " select item ", index);
  window;
end method select-item;

define method add-canvas-tag (tag :: <tag-or-item>, spec :: <string>,
			      #rest spec-args)
  put-tk-line(tag.window, " addtag ",
	      apply(join-tk-args, tag.name, spec, spec-args));
end method add-canvas-tag;

define method find-items (canvas :: <canvas>, spec :: <string>, #rest specargs)
 => found :: <sequence>;
  parse-tk-list(call-tk-function(canvas, " find ",
				 apply(join-tk-args, spec, specargs)));
end method find-items;

define method get-canvas-tags (tag :: <tag-or-item>)
 => tags :: <sequence>;
  map(method (x) make(<canvas-tag>, in: tag.window, name: x) end, 
      parse-tk-list(call-tk-function(tag.window, " gettags ", tag.name)));
end method get-canvas-tags;

define method bounding-box (item :: <tag-or-item>, #rest items)
 => box-coords :: <sequence>;
  parse-tk-list(call-tk-function(item.window, " bbox ",
				 apply(join-tk-args, item.name
					 (apply(name, items)))));
end method bounding-box;

define method canvas-x (canvas :: <canvas>, screen-x :: <number>,
			#key grid-spacing = 1) => x :: <integer>;
  tk-as(<integer>, call-tk-function(canvas, " canvasx ", screen-x, " ",
				    grid-spacing));
end method canvas-x;

define method canvas-y (canvas :: <canvas>, screen-y :: <number>,
			#key grid-spacing = 1) => y :: <integer>;
  tk-as(<integer>, call-tk-function(canvas, " canvasy ", screen-y, " ",
				    grid-spacing));
end method canvas-y;

define method delete-canvas-tag (item :: <tag-or-item>,
			  tag-to-delete :: <canvas-tag>)
  put-tk-line(item.window, " dtag ", item.name, " ", tag-to-delete.name);
end method delete-canvas-tag;

define method create-arc
    (canvas :: <canvas>, x1 :: <object>, y1 :: <object>,
     x2 :: <object>, y2 :: <object>, #rest rest)
 => (result :: <canvas-item>);
  let str
    = apply(call-tk-function, canvas, " create arc ",
	    join-tk-args(x1, y1, x2, y2),
	    std-options(#[#"extent", #"fill", #"outline", #"start", #"tags",
			    #"stipple", #"style", #"width", #"outlinestipple"],
			#f, rest));
  make(<canvas-item>, in: canvas, name: str);
end method create-arc;

define method create-bitmap
    (canvas :: <canvas>, x1 :: <object>, y1 :: <object>, #rest rest)
 => (result :: <canvas-item>);
  let str
    = apply(call-tk-function, canvas, " create bitmap ",
	    join-tk-args(x1, y1),
	    std-options(#[#"anchor", #"bitmap", #"background", "foreground",
			  #"tags"],
			#f, rest));
  make(<canvas-item>, in: canvas, name: str);
end method create-bitmap;

define method create-image
    (canvas :: <canvas>, x1 :: <object>, y1 :: <object>, #rest rest)
 => (result :: <canvas-item>);
  let str
    = apply(call-tk-function, canvas, " create bitmap ",
	    join-tk-args(x1, y1),
	    std-options(#[#"anchor", #"image", #"tags"],
			#f, rest));
  make(<canvas-item>, in: canvas, name: str);
end method create-image;

define method create-line
    (canvas :: <canvas>, points :: <sequence>, #rest rest)
 => (result :: <canvas-item>);
  let str
    = apply(call-tk-function, canvas, " create line ",
	    apply(join-tk-args, points),
	    std-options(#[#"arrow", #"arrowshape", #"capstyle", #"fill",
			    #"joinstyle", #"smooth", #"splinesteps",
			    #"stipple", #"width", #"tags"],
			#f, rest));
  make(<canvas-item>, in: canvas, name: str);
end method create-line;

define method create-oval
    (canvas :: <canvas>, x1 :: <object>, y1 :: <object>,
     x2 :: <object>, y2 :: <object>, #rest rest)
 => (result :: <canvas-item>);
  let str
    = apply(call-tk-function, canvas, " create oval ",
	    join-tk-args(x1, y1, x2, y2),
	    std-options(#[#"fill", #"outline", #"stipple", #"width", #"tags"],
			#f, rest));
  make(<canvas-item>, in: canvas, name: str);
end method create-oval;

define method create-polygon
    (canvas :: <canvas>, points :: <sequence>, #rest rest)
 => (result :: <canvas-item>);
  let str
    = apply(call-tk-function, canvas, " create polygon ",
	    apply(join-tk-args, points),
	    std-options(#[#"fill", #"outline", #"smooth", #"splinesteps",
			  #"stipple", #"tags", #"width"],
			#f, rest));
  make(<canvas-item>, in: canvas, name: str);
end method create-polygon;

define method create-rectangle
    (canvas :: <canvas>, x1 :: <object>, y1 :: <object>,
     x2 :: <object>, y2 :: <object>, #rest rest)
 => (result :: <canvas-item>);
  let str
    = apply(call-tk-function, canvas, " create rectangle ",
	    join-tk-args(x1, y1, x2, y2),
	    std-options(#[#"fill", #"outline", #"stipple", #"width", #"tags"],
			#f, rest));
  make(<canvas-item>, in: canvas, name: str);
end method create-rectangle;

define method create-text
    (canvas :: <canvas>, x1 :: <object>, y1 :: <object>, #rest rest)
 => (result :: <canvas-item>);
  let str
    = apply(call-tk-function, canvas, " create text ",
	    join-tk-args(x1, y1),
	    std-options(#[#"anchor", #"fill", #"font", #"justify", #"stipple",
			    #"text", #"width", #"tags"],
			#f, rest));
  make(<canvas-item>, in: canvas, name: str);
end method create-text;

define method create-window
    (canvas :: <canvas>, x1 :: <object>, y1 :: <object>, #rest rest)
 => (result :: <canvas-item>);
  let str
    = apply(call-tk-function, canvas, " create window ",
	    join-tk-args(x1, y1),
	    std-options(#[#"anchor", #"height", #"tags", #"width", #"window"],
			#f, rest));
  make(<canvas-item>, in: canvas, name: str);
end method create-window;

define method postscript
    (canvas :: <canvas>, #rest rest) => (result :: <string>);
  let str
    = apply(call-tk-function, canvas, " postscript ",
	    std-options(#[#"colormap", #"colormode", #"fontmap", #"height",
			    #"pageanchor", #"pageheight", #"pagewidth",
			    #"pagex", #"pagey", #"rotate", #"width", #"x",
			    #"y"],
			#f, rest));
  tk-unquote(str);
end method postscript;
