module: tk
author: Russ Schaaf (rsbe@cmu.edu)

//======================================================================
//
// Copyright (c) 1996  Carnegie Mellon University
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
// This file contains bindings for the "winfo" command.
//
//======================================================================

// From the string-extensions library (wasn't exported)
define method string-to-number (string :: <sequence>, #key base: base = 10)
 => num :: <number>;
  let number = 0;
  let negate = 1;
  let seen-decimal = #f;
  let decimal-divisor = 1;
  for (c in string)
    select (c)
      '-' =>      negate := -1;
      '+' =>      negate := 1;
      '.' =>      seen-decimal := #t;
      otherwise =>
	let digit = digit-to-integer(c);
	if (digit >= base) 
	  error("\"%s\" isn't in base %d\n", string, base);
	elseif (seen-decimal)  
	  decimal-divisor := decimal-divisor * base;
	  number := number + as(<float>, digit) / as(<float>, decimal-divisor);
	else 
	  number := number * base  + digit;
	end if;
    end select;
  end for;

  number * negate;
end method string-to-number;

define class <point> (<object>)
  slot x :: <integer>, init-keyword: #"x";
  slot y :: <integer>, init-keyword: #"y";
end class <point>;

define class <color> (<object>)
end class <color>;

define class <rgb-color> (<color>)
  slot red :: <integer>, init-keyword: #"red";
  slot green :: <integer>, init-keyword: #"green";
  slot blue :: <integer>, init-keyword: #"blue";
end class <rgb-color>;

define method colormap-cells (widget :: <window>) => (c-m-cells :: <integer>);
  tk-as(<integer>, call-tk-function("winfo cells ", widget));
end method colormap-cells;

define method colormap-full? (widget :: <window>) => (c-m-full? :: <boolean>);
  tk-as(<boolean>, call-tk-function("winfo colormapfull ", widget));
end method colormap-full?;

define method children (widget :: <window>) => (childs :: <sequence>);
  let child-seq = parse-tk-list(call-tk-function("winfo children ", widget));
  let child-objs = make(<sequence>, size: child-seq.size);
  for (child in child-seq)
    child-objs := add!(child-objs,
		       make(tk-class(child), parent: widget, abs-path: child));
  end for;
  child-objs;
end method children;

define method window-containing-x-y (x :: <integer>, y :: <integer>,
				     #key on-display-of = #f)
 => (window :: <window>);
  let win-name = if (on-display-of)
		   call-tk-function("winfo containing -displayof ",
		     on-display-of, x, y);
		 else
		   call-tk-function("winfo containing ", x, y);
		 end if;

  make(tk-class(win-name), parent: call-tk-function("winfo parent ", win-name),
	 abs-path: win-name);
end method window-containing-x-y;

define method window-containing-point (point :: <point>,
				       #key on-display-of = #f)
 => window :: <window>;
  window-containing-x-y(point.x, point.y, on-display-of: on-display-of);
end method window-containing-point;

define method depth (window :: <window>) => colors :: <integer>;
  tk-as(<integer>, call-tk-function("winfo depth ", window));
end method depth;

define method exists? (window :: <window>) => exist :: <boolean>;
  tk-as(<boolean>, call-tk-function("winfo exists ", window));
end method exists?;

/*define method number-to-string (number :: <number>, #key base = 10, digits = 4)
 => (string :: <string>);
  let frac-string = make(<string>, size: digits);
  let (ipart, fpart) = truncate(number);
  let int-string = tk-as(<integer>, ipart);
  if (fpart ~= 0)
    let (new-dig, fpart) = truncate(fpart * base);
    let digits-converted = 0;
    let zero-byte = as(<integer>, '0');
    while (new-dig ~= 0 & fpart ~= 0 & digits-converted < digits)
      frac-string[digits-converted] = tk-as(<character>, new-dig + zero-byte);
      let (new-dig, fpart) = truncate(fpart * base);
    end while;
    concatenate(int-string, ".", frac-string);
  else
    int-string;
  end if;
end method number-to-string;*/

define method distance-to-float-pixels (dist :: <number>, #rest units)
 => (pixels :: <number>);
  let dist-str = format-to-string("%=", dist);
  let input-string = make(<string>);
  if (units)
    input-string := select (units[0])
		      #"centimeters", "c" => concatenate(dist-str, "c");
		      #"inches", "i" => concatenate(dist-str, "i");
		      #"millimeters", "m" => concatenate(dist-str, "m");
		      #"points", "p" => concatenate(dist-str, "p");
		      otherwise => dist-str;
		    end select;
  end if;
  string-to-number(call-tk-function("winfo fpixels ",input-string));
end method distance-to-float-pixels;

define method geometry (window :: <window>) => geom :: <string>;
  call-tk-function("winfo geometry ",window);
end method geometry;

define method height (window :: <window>) => h :: <integer>;
  tk-as(<integer>, call-tk-function("winfo height ", window));
end method height;

define method X-id (window :: <window>) => id :: <integer>;
  tk-as(<integer>, call-tk-function("winfo height ", window));
end method X-id;

define method mapped? (window :: <window>) => map? :: <boolean>;
  tk-as(<boolean>, call-tk-function("winfo ismapped ", window));
end method mapped?;

define method geometry-manager (window :: <window>) => manager :: <string>;
  call-tk-function("winfo manager ", window);
end method geometry-manager;

define method window-with-X-id (id :: <integer>, #key on-display-of = #f)
 => window :: <window>;
  let path = if (on-display-of)
	       call-tk-function("winfo pathname -display ", on-display-of, id);
	     else
	       call-tk-function("winfo pathname ", id);
	     end if;
  make(tk-class(path), parent: call-tk-function("winfo parent ", path),
       abs-path: path);
end method window-with-X-id;

define method distance-to-pixels (dist :: <number>, #rest units)
 => (pixels :: <integer>);
  let dist-str = format-to-string("%=", dist);
  let input-string = make(<string>);
  if (units)
    input-string := select (units[0])
		      #"centimeters", "c" => concatenate(dist-str, "c");
		      #"inches", "i" => concatenate(dist-str, "i");
		      #"millimeters", "m" => concatenate(dist-str, "m");
		      #"points", "p" => concatenate(dist-str, "p");
		      otherwise => dist-str;
		    end select;
  end if;
  tk-as(<integer>, call-tk-function("winfo fpixels ",input-string));
end method distance-to-pixels;

define method mouse-x-y (window :: <window>)
 => (x :: <integer>, y :: <integer>);
  let my-x = tk-as(<integer>, call-tk-function("expr [winfo rootx ",
					       window,
					       " ] - [winfo pointerx ",
					       window, " ]"));
  let my-y = tk-as(<integer>, call-tk-function("expr [winfo rooty ",
					       window,
					       " ] - [winfo pointery ",
					       window, " ]"));
  values(my-x, my-y);
end method mouse-x-y;

define method mouse-point (window :: <window>) => (point :: <point>);
  let (x,y) = window.mouse-x-y;
  make(<point>, x: x, y: y);
end method mouse-point;

define method requested-height (window :: <window>) => (h :: <integer>);
  tk-as(<integer>, call-tk-function("winfo reqheight ", window));
end method requested-height;

define method requested-width (window :: <window>) => (w :: <integer>);
  tk-as(<integer>, call-tk-function("winfo reqwidth ", window));
end method requested-width;

define method width (window :: <window>) => (w :: <integer>);
  tk-as(<integer>, call-tk-function("winfo width ", window));
end method width;

define method color-to-r-g-b (window :: <window>, color :: <string>)
 => (red :: <integer>, green :: <integer>, blue :: <integer>);
  apply(values, map(curry(tk-as, <integer>),
		    parse-tk-list(call-tk-function("winfo rgb ",
						   window, " ", color))));
end method color-to-r-g-b;

define method color-to-rgb-color (window :: <window>, color :: <string>)
  => (rgb-color :: <rgb-color>);
  let (r,g,b) = color-to-r-g-b(window, color);
  make(<rgb-color>, red: r, green: g, blue: b);
end method color-to-rgb-color;

define method abs-position-x-y (window :: <window>)
 => (x :: <integer>, y :: <integer>);
  let x = tk-as(<integer>, call-tk-function("winfo rootx ", window));
  let y = tk-as(<integer>, call-tk-function("winfo rooty ", window));
  values(x,y);
end method abs-position-x-y;

define method abs-position-point (window :: <window>) => (point :: <point>);
  let (x,y) = window.abs-position-x-y;
  make(<point>, x: x, y: y);
end method abs-position-point;

define method screen-name (window :: <window>) => (name :: <string>);
  call-tk-function("winfo screen ", window);
end method screen-name;

define method screen-colormap-cells (window :: <window>)
 => (num-cells :: <integer>);
  tk-as(<integer>, call-tk-function("winfo screencells ", window));
end method screen-colormap-cells;

define method screen-depth (window :: <window>) => (bit-depth :: <integer>);
 tk-as(<integer>, call-tk-function("winfo screendepth ", window));
end method screen-depth;

define method screen-height (window :: <window>) => (h :: <integer>);
  tk-as(<integer>, call-tk-function("winfo screenheight ", window));
end method screen-height;

define method screen-width (window :: <window>) => (w :: <integer>);
  tk-as(<integer>, call-tk-function("winfo screendepth ", window));
end method screen-width;

define method server (window :: <window>) => (srv :: <integer>);
  call-tk-function("winfo server ", window);
end method server;

define method toplevel (window :: <window>) => (top :: <window>);
  let path = call-tk-function("winfo toplevel ", window);
  make(tk-class(path), parent: call-tk-function("winfo parent ", path),
       abs-path: path);
end method toplevel;

define method viewable? (window :: <window>) => (view? :: <boolean>);
  tk-as(<boolean>, call-tk-function("winfo viewable ", window));
end method viewable?;

define method visual-class (window :: <window>) => (vis-class :: <string>);
  call-tk-function("winfo visual ", window);
end method visual-class;

define method available-visuals (window :: <window>)
 => (avail-vis :: <sequence>); 
  map(method (x) x[1] := tk-as(<integer>, x[1]); end,
      parse-tk-list(call-tk-function("winfo visualsavailable ", window)));
end method available-visuals;
  
define method virtual-root-height (window :: <window>) => (h :: <integer>);
  tk-as(<integer>, call-tk-function("winfo vrootheight ", window));
end method virtual-root-height;

define method virtual-root-width (window :: <window>) => (w :: <integer>);
  tk-as(<integer>, call-tk-function("winfo vrootwidth ", window));
end method virtual-root-width;

define method virtual-root-position-x-y (window :: <window>)
 => (x :: <integer>, y :: <integer>);
  let x = tk-as(<integer>, call-tk-function("winfo vrootx ", window));
  let y = tk-as(<integer>, call-tk-function("winfo vrooty ", window));
  values(x,y);
end method virtual-root-position-x-y;

define method virtual-root-position-point (window :: <window>)
 => (point :: <point>);
  let (x,y) = window.virtual-root-position-x-y;
  make(<point>, x: x, y: y);
end method virtual-root-position-point;

define method x-y-in-parent (window :: <window>)
 => (x :: <integer>, y :: <integer>);
  let x = tk-as(<integer>, call-tk-function("winfo x ", window));
  let y = tk-as(<integer>, call-tk-function("winfo y ", window));
  values(x,y);
end method x-y-in-parent;

define method point-in-parent (window :: <window>)
 => (point :: <point>);
  let (x,y) = window.x-y-in-parent;
  make(<point>, x: x, y: y);
end method point-in-parent;