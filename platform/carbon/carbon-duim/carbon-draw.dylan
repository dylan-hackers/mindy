Module:       carbon-duim
Synopsis:     Carbon back-end
Author:	   		Scott McKay, Andy Armstrong, Peter Housel
Copyright:    Original Code is Copyright (c) 1995-2000 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// TODO: Fill vs. Paint xxx


/// Figure graphics

define method draw-point
    (medium :: <carbon-medium>, x, y) => (record)
  let transform = sheet-device-transform(medium-sheet(medium));
  let grafport :: <CGrafPtr> = update-drawing-state(medium);
  with-device-coordinates (transform, x, y)
    let thickness = pen-width(medium-pen(medium));
    if (thickness < 2)
      SetCPixel(x, y, medium.%foreground-color)
    else 
      let half-thickness = truncate/(thickness, 2);
      with-stack-structure(rect :: <Rect*>)
      	rect.left-value := x - half-thickness;
      	rect.right-value := x + half-thickness;
      	rect.top-value := y - half-thickness;
      	rect.bottom-value := y + half-thickness;
				PaintOval(rect);
	    end;
    end;
  end;
  #f;
end method draw-point;

// XXX - HERE!!!

define method draw-points
    (medium :: <carbon-medium>, coord-seq :: <coordinate-sequence>) => (record)
  let transform = sheet-device-transform(medium-sheet(medium));
  let grafport :: <CGrafPtr> = update-drawing-state(medium);
  let thickness = pen-width(medium-pen(medium));
  if (thickness < 2)
    do-coordinates
      (method (x, y)
        with-device-coordinates (transform, x, y)
          SetCPixel(x, y, medium.%foreground-color)
        end
       end,
       coord-seq);
  else
    let half-thickness = truncate/(thickness, 2);
    with-stack-structure(rect :: <Rect*>)
      do-coordinates
        (method (x, y)
          with-device-coordinates (transform, x, y)
							rect.left-value := x - half-thickness;
							rect.right-value := x + half-thickness;
							rect.top-value := y - half-thickness;
							rect.bottom-value := y + half-thickness;
							PaintOval(rect);
          end
        end,
        coord-seq);
    end;
  end;
  #f;
end method draw-points;

define method draw-line
    (medium :: <carbon-medium>, x1, y1, x2, y2) => (record)
  let transform = sheet-device-transform(medium-sheet(medium));
  let grafport :: <CGrafPtr> = update-drawing-state(medium);
  with-device-coordinates (transform, x1, y1, x2, y2)
    MoveTo(x1, y1);
    LineTo(x2, y2);
  end;
  #f;
end method draw-line;

define method draw-lines
    (medium :: <carbon-medium>, coord-seq :: <coordinate-sequence>) => (record)
  let transform = sheet-device-transform(medium-sheet(medium));
  let grafport :: <CGrafPtr> = update-drawing-state(medium);
  do-endpoint-coordinates
    (method (x1, y1, x2, y2)
      with-device-coordinates (transform, x1, y1, x2, y2)
        MoveTo(x1, y1);
        LineTo(x2, y2)
      end
     end,
     coord-seq);
  #f;
end method draw-lines;

define method draw-rectangle
    (medium :: <carbon-medium>, left, top, right, bottom,
     #key filled? = #t) => (record)
  let transform = sheet-device-transform(medium-sheet(medium));
  if (~rectilinear-transform?(transform))
    with-stack-vector (coords = left, top, right, top, right, bottom, left, right)
      draw-polygon(medium, coords, filled?: filled?, closed?: #t);
    end;
  else
    let grafport :: <CGrafPtr> = update-drawing-state(medium, pen: ~filled? & medium-pen(medium));
    //with-fill-selected (grafport, filled?)
      with-device-coordinates (transform, left, top, right, bottom)
        with-stack-structure(rect :: <Rect*>)
          rect.left-value := left;
          rect.right-value := right;
          rect.top-value := top;
          rect.bottom-value := bottom;
          if(filled?)
            PaintRect(rect);
          else
            FrameRect(rect);
          end if;
        end;
      end;
    //end;
  end;
  #f
end method draw-rectangle;

define method draw-rectangles
    (medium :: <carbon-medium>, coord-seq :: <coordinate-sequence>,
     #key filled? = #t) => (record)
  let transform = sheet-device-transform(medium-sheet(medium));
  if(~rectilinear-transform?(transform))
    draw-transformed-rectangles(medium, coord-seq, filled?: filled?);
  else
    let grafport :: <CGrafPtr> = update-drawing-state(medium, pen: ~filled? & medium-pen(medium));
    let transform = medium-device-transform(medium);
    //with-fill-selected (grafport, filled?)
    //---*** Use PolyPolyLine
    with-stack-structure(rect :: <Rect*>)
      do-endpoint-coordinates
      (method (x1, y1, x2, y2)
          with-device-coordinates (transform, x1, y1, x2, y2)
            rect.left-value := x1;
            rect.right-value := x2;
            rect.top-value := y1;
            rect.bottom-value := y2;
            if(filled?)
              PaintRect(rect);
            else
              FrameRect(rect);
            end if;
          end;
        end,
        coord-seq)
      end;
    //end;
  end;
  #f
end method draw-rectangles;

define sealed method draw-transformed-rectangles
    (medium :: <carbon-medium>, coord-seq :: <coordinate-sequence>,
     #rest keys, #key filled? = #t) => (record)
  dynamic-extent(keys);
  ignore(filled?);
  let ncoords :: <integer> = size(coord-seq);
  assert(zero?(modulo(ncoords, 4)),
	 "The coordinate sequence has the wrong number of elements");
  local method draw-one (x1, y1, x2, y2) => ()
	  with-stack-vector (coords = x1, y1, x2, y1, x2, y2, x1, y2)
	    apply(draw-polygon, medium, coords, closed?: #t, keys)
	  end
        end method;
  dynamic-extent(draw-one);
  without-bounds-checks
    for (i :: <integer> = 0 then i + 4, until: i = ncoords)
      draw-one(coord-seq[i + 0], coord-seq[i + 1],
	       coord-seq[i + 2], coord-seq[i + 3])
    end
  end;
  #f
end method draw-transformed-rectangles;

define method draw-rounded-rectangle
    (medium :: <carbon-medium>, x1, y1, x2, y2,
     #key filled? = #t, radius) => (record)
  let transform = sheet-device-transform(medium-sheet(medium));
  let grafport :: <CGrafPtr> = update-drawing-state(medium, pen: ~filled? & medium-pen(medium));
  with-device-coordinates (transform, x1, y1, x2, y2)
    let grafport :: <CGrafPtr> = update-drawing-state(medium, pen: ~filled? & medium-pen(medium));
    let transform = medium-device-transform(medium);
    unless (radius)
      let width  = x2 - x1;
      let height = y2 - y1;
      radius := max(truncate/(min(width, height), 3), 2);
    end;
    //with-fill-selected (grafport, filled?)
      with-stack-structure(rect :: <Rect*>)
        rect.left-value := x1;
        rect.right-value := x2;
        rect.top-value := y1;
        rect.bottom-value := y2;
        if(filled?)
          PaintRoundRect(rect, radius, radius);
        else
          FrameRoundRect(rect, radius, radius);
        end if;
      end;
    //end;
  end;
  #f;
end method draw-rounded-rectangle;

// XXX - We should check for errors/NULL after open & close, 
//			 and put the KillPoly in a finally block

// XXX - Also, can Mac polys be open? If not, use multiple LineTos

define sealed method draw-polygon
    (medium :: <carbon-medium>, coord-seq :: <coordinate-sequence>,
     #key closed? = #t, filled? = #t) => (record)
  let grafport :: <CGrafPtr> = update-drawing-state(medium, pen: ~filled? & medium-pen(medium));
  let transform = medium-device-transform(medium);
  let seqsize :: <integer> = size(coord-seq);
	let poly :: <PolyHandle> = OpenPoly();
	let h = coord-seq[0];
	let v = coord-seq[1];
	with-device-coordinates (transform, h, v)
		MoveTo(h, v);
	end;
	for(i :: <integer> from 2 below seqsize by 2)
		let h = coord-seq[i + 0];
		let v = coord-seq[i + 1];
		with-device-coordinates (transform, h, v)
			LineTo(h, v);
		end;
	end;
	when(closed?)
		let h = coord-seq[0];
		let v = coord-seq[1];
		with-device-coordinates (transform, h, v)
			LineTo(h, v);
		end;
	end;
	ClosePoly();
	if(filled?)
		PaintPoly(poly);
	else
		FramePoly(poly);
	end if;
	KillPoly(poly);
  #f
end method draw-polygon;

define sealed method draw-triangle
    (medium :: <carbon-medium>, x1, y1, x2, y2, x3, y3,
     #key filled? = #t) => (record)
  let grafport :: <CGrafPtr> = update-drawing-state(medium, pen: ~filled? & medium-pen(medium));
  let transform = medium-device-transform(medium);
	let poly :: <PolyHandle> = OpenPoly();
	with-device-coordinates (transform, x1, y1)
		MoveTo(x1, y1);
	end;
	with-device-coordinates (transform, x2, y2)
		LineTo(x2, y2);
	end;
	with-device-coordinates (transform, x3, y3)
		LineTo(x3, y3);
	end;
	with-device-coordinates (transform, x1, y1)
		LineTo(x1, y1);
	end;
	ClosePoly();
	if(filled?)
		PaintPoly(poly);
	else
		FramePoly(poly);
	end if;
	KillPoly(poly);
  #f
end method draw-triangle;

define sealed method draw-ellipse
    (medium :: <carbon-medium>, center-x, center-y,
     radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy,
     #key start-angle, end-angle, filled? = #t) => (record)
  let grafport :: <CGrafPtr> = update-drawing-state(medium, pen: ~filled? & medium-pen(medium));
  let transform = medium-device-transform(medium);
  with-device-coordinates (transform, center-x, center-y)
    with-device-distances (transform, radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy)
      let (angle-2, x-radius, y-radius, angle-1)
      = singular-value-decomposition-2x2(radius-1-dx, radius-2-dx, radius-1-dy, radius-2-dy);
      //with-fill-selected (grafport, filled?)
        if (#t					//---*** remove when tilted ellipses work
            | x-radius = abs(y-radius)		// a circle - rotations are irrelevant
            | zero?(angle-1))			// axis-aligned ellipse
          x-radius := abs(x-radius);
          y-radius := abs(y-radius);
          
          // XXX - Must the line be centred with pen-width(medium-pen(medium) ???
              
          with-stack-structure(rect :: <Rect*>)
            rect.top-value := center-y -  y-radius;
            rect.bottom-value := center-y +  y-radius;
            rect.left-value := center-x -  x-radius;
            rect.right-value := center-x +  x-radius;
            if (start-angle & end-angle)
              if (filled?)
                PaintArc(rect, start-angle, end-angle)
              else
                FrameArc(rect, start-angle, end-angle)
              end
            else
              if (filled?)
                PaintOval(rect);
              else
                FrameOval(rect);
              end;
            end;
          end;
        else
          #f					//---*** do tilted ellipses here
      end;
    //end;
    end;
  end;
  #f
end method draw-ellipse;


/// Pixel graphics

// XXX - SetCPixel is slow, but robust under Carbon

define sealed method set-pixel
    (medium :: <carbon-medium>, color :: <rgb-color>, x, y) => (record)
  let grafport :: <CGrafPtr> = medium-grafport(medium);
  let transform = medium-device-transform(medium);
  let color     = %color->native-color(color);
  with-device-coordinates (transform, x, y)
    SetCPixel(x, y, color)
  end;
  #f
end method set-pixel;

define sealed method set-pixels
    (medium :: <carbon-medium>, color :: <rgb-color>, coord-seq :: <coordinate-sequence>)
 => (record)
  let grafport :: <CGrafPtr> = update-drawing-state(medium);
  let transform = medium-device-transform(medium);
  let color     = %color->native-color(color);
  do-coordinates
    (method (x, y)
       with-device-coordinates (transform, x, y)
	 SetCPixel(x, y, color)
       end
     end,
     coord-seq);
  #f
end method set-pixels;


/// Pixmap graphics

// carbon bitmaps and icons are handled separately
define sealed method draw-image
    (medium :: <carbon-medium>, image :: <stencil>, x, y) => (record)
  let grafport :: <CGrafPtr> = update-drawing-state(medium);
  let width  = image-width(image);
  let height = image-height(image);
  let grafport :: <CGrafPtr> = medium-grafport(medium);
  let cache  = medium.%ink-cache;
  let pixmap
    = gethash(cache, image)
      | begin
	  // Decode the pattern into a pixmap...
	  let (array, colors) = decode-pattern(image);
	  let ncolors :: <integer> = size(colors);
	  let pixels  :: <simple-object-vector> = make(<simple-vector>, size: ncolors);
	  without-bounds-checks
	    for (n :: <integer> from 0 below ncolors)
	      let pixel = convert-ink-to-drawable-components(medium, grafport, colors[n]);
	      pixels[n] := pixel
	    end;
	    let pixmap
	      = with-output-to-pixmap(medium, width: width, height: height)	
      // get the port for the pixmap medium
		  let grafport :: <CGrafPtr> = medium-grafport(medium);
		  for (y :: <integer> from 0 below height)
		    for (x :: <integer> from 0 below width)
		      SetCPixel(x, y, pixels[array[y,x]])	// XXX - Slooooow!!!
		    end
		  end
		end;
	    gethash(cache, image) := pixmap;
	    pixmap
	  end
	end;
  do-copy-area(pixmap, 0, 0, width, height,
	       medium, x, y)
end method draw-image;


/// Path graphics

define sealed method start-path
    (medium :: <carbon-medium>) => (record)
  if(medium.%current-path)
  	DisposeRgn(medium.%current-path);
		medium.%current-path := NewRgn();
		OpenRgn();
  else
  	SetEmptyRgn(medium.%current-path);
  end if;
  
  #f
end method start-path;

define sealed method end-path
    (medium :: <carbon-medium>) => (record)
  let grafport :: <CGrafPtr> = medium-grafport(medium);
  if(medium.%current-path)
    CloseRgn(medium.%current-path);
  end if;
  #f
end method end-path;

define sealed method abort-path
    (medium :: <carbon-medium>) => (record)
  if(medium.%current-path)
    SetEmptyRgn(medium.%current-path);
  end if;
  #f
end method abort-path;

define sealed method close-path
    (medium :: <carbon-medium>) => (record)
    
  if(medium.%current-path)
    CloseRgn(medium.%current-path);
  end if;
  #f
end method close-path;

define sealed method stroke-path
    (medium :: <carbon-medium>, #key filled?) => (record)
  let grafport :: <CGrafPtr> = update-drawing-state(medium, pen: medium-pen(medium));
  if (filled?)
  	// XXX - Fill *and stroke*?
    PaintRgn(medium.%current-path)
  else
    FrameRgn(medium.%current-path)
  end;
  #f
end method stroke-path;

define sealed method fill-path
    (medium :: <carbon-medium>) => (record)
  let grafport :: <CGrafPtr> = update-drawing-state(medium);
  PaintRgn(medium.%current-path);
  #f;
end method fill-path;

define sealed method clip-from-path
    (medium :: <carbon-medium>, #key function = $boole-and) => (record)
  let grafport :: <CGrafPtr> = medium-grafport(medium);
  let new-rgn :: <RgnHandle> = NewRgn();
  let old-clip :: <RgnHandle> = NewRgn();
  GetClip(old-clip);
  select (function)
		 $boole-and   => SectRgn(old-clip, medium.%current-path, new-rgn);
		 $boole-set   => CopyRgn(medium.%current-path, new-rgn);
		 $boole-ior   => UnionRgn(old-clip, medium.%current-path, new-rgn);
		 $boole-xor   => XorRgn(old-clip, medium.%current-path, new-rgn);
		 $boole-andc2 => DiffRgn(old-clip, medium.%current-path, new-rgn);
	 end;
  SetClip(new-rgn);
  DisposeRgn(new-rgn);
  DisposeRgn(old-clip);
  #f
end method clip-from-path;

define sealed method save-clipping-region
    (medium :: <carbon-medium>) => (record)
  //---*** Push the clipping region
  #f
end method save-clipping-region;

define sealed method restore-clipping-region
    (medium :: <carbon-medium>) => (record)
  //---*** Pop the clipping region
  #f
end method restore-clipping-region;

define sealed method move-to
    (medium :: <carbon-medium>, x, y) => (record)
  let grafport :: <CGrafPtr> = medium-grafport(medium);
  let transform = medium-device-transform(medium);
  with-device-coordinates (transform, x, y)
    MoveTo(x, y)
  end;
  #f
end method move-to;

define sealed method line-to
    (medium :: <carbon-medium>, x, y) => (record)
  let grafport :: <CGrafPtr> = medium-grafport(medium);
  let transform = medium-device-transform(medium);
  with-device-coordinates (transform, x, y)
    LineTo(x, y)
  end;
  #f
end method line-to;

define sealed method arc-to
    (medium :: <carbon-medium>, center-x, center-y,
     radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy,
     #key start-angle, end-angle) => (record)
  let grafport :: <CGrafPtr> = medium-grafport(medium);
  let transform = medium-device-transform(medium);
  with-device-coordinates (transform, center-x, center-y)
    with-device-distances (transform, radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy)
      let (x-radius, y-radius)
	= case
	    radius-1-dx = 0 & radius-2-dy = 0 =>
	      values(abs(radius-2-dx), abs(radius-1-dy));
	    radius-2-dx = 0 & radius-1-dy = 0 =>
	      values(abs(radius-1-dx), abs(radius-2-dy));
	    otherwise =>
	      not-yet-implemented("Tilted ellipses");
	  end;
      let (left, top, right, bottom)
	= elliptical-arc-box(center-x, center-y,
			     radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy);
      //---*** What angle conventions does Windows use?
      start-angle := start-angle | 0.0;
      end-angle   := end-angle | $2pi;
      when (end-angle < start-angle)
	end-angle := end-angle + $2pi
      end;
      /*let (rx1, ry1) = values(cos(start-angle), sin(start-angle));
      let (rx2, ry2) = values(cos(end-angle),   sin(end-angle));*/
      let r :: <Rect*> = make(<Rect*>, top: top, left: left, right: right, bottom: bottom);
      FrameArc(r, start-angle, end-angle - start-angle);
    end
  end;
  #f
end method arc-to;

define sealed method curve-to
    (medium :: <carbon-medium>, x1, y1, x2, y2, x3, y3) => (record)
  ignoring("curve-to");
  /*let grafport :: <CGrafPtr> = medium-grafport(medium);
  let transform = medium-device-transform(medium);
  with-device-coordinates (transform, x1, y1, x2, y2, x3, y3)
    with-stack-structure (c-points :: <Point*>, element-count: 3)
      //---*** Should be (in some later version of Webster)...
      //---*** c-points[i].h-value := x;
      //---*** c-points[i].v-value := y;
      pointer-value-address(c-points, index: 0).h-value := x1;
      pointer-value-address(c-points, index: 0).v-value := y1;
      pointer-value-address(c-points, index: 1).h-value := x2;
      pointer-value-address(c-points, index: 1).v-value := y2;
      pointer-value-address(c-points, index: 2).h-value := x3;
      pointer-value-address(c-points, index: 2).v-value := y3;
      PolyBezierTo(grafport, c-points, 3)
    end
  end;*/
  #f
end method curve-to;


/// 'clear-box'

/*---*** This doesn't work... let's just use the default method for now,
//---*** which uses draw-rectangle using the background brush.
define sealed method clear-box
    (medium :: <carbon-medium>, left,  top, right, bottom) => ()
  let grafport = get-DC(medium);
  let sheet = medium-sheet(medium);
  let transform = sheet-device-transform(sheet);
  with-device-coordinates (transform, left, top, right, bottom)
    EraseRect(left, top, right, bottom);
  end
end method clear-box;
*/


/// Text drawing

define sealed method draw-text
    (medium :: <carbon-medium>, character :: <character>, x, y,
     #key start: _start, end: _end,
          align-x = #"left", align-y = #"baseline", do-tabs? = #f,
          towards-x, towards-y, transform-glyphs?) => (record)
  ignore(_start, _end);
  let text-style :: <text-style> = medium-merged-text-style(medium);
  let font :: <carbon-font> = text-style-mapping(port(medium), text-style);
  let grafport :: <CGrafPtr> = update-drawing-state(medium, font: font);
  let transform = medium-device-transform(medium);
  with-device-coordinates (transform, x, y)
    when (towards-x & towards-y)
      convert-to-device-coordinates!(transform, towards-x, towards-y)
    end;
    let (font, width, height, ascent, descent)
      = font-metrics(text-style, port(medium));
    let c-string :: <c-string> = make(<c-string>);
    c-string[0] := character;  
    move-to-text-aligned(character, 0, 0, x, y, align-x, align-y, height, ascent);
    //---*** Doesn't handle UNICODE strings!
    DrawChar(character)
  end;
  #f
end method draw-text;

//---*** What do we do about Unicode strings?
define sealed method draw-text
    (medium :: <carbon-medium>, string :: <string>, x, y,
     #key start: _start, end: _end,
          align-x = #"left", align-y = #"baseline", do-tabs? = #f,
          towards-x, towards-y, transform-glyphs?) => (record)
  let text-style :: <text-style> = medium-merged-text-style(medium);
  let font :: <carbon-font> = text-style-mapping(port(medium), text-style);
  let grafport :: <CGrafPtr> = update-drawing-state(medium, font: font);
  let transform = medium-device-transform(medium);
  let length :: <integer> = size(string);
  let _start :: <integer> = _start | 0;
  let _end   :: <integer> = _end   | length;
 /* XXX - assert(_end - _start < 32000,
	 "'draw-text' cannot draw text strings longer than 32000 characters");
  with-device-coordinates (transform, x, y)
    when (towards-x & towards-y)
      convert-to-device-coordinates!(transform, towards-x, towards-y)
    end;
    //---*** It would be great if 'with-c-string' took start & end!
    let substring
      = if (_start = 0 & _end = length) string
	else copy-sequence(string, start: _start, end: _end) end;
    with-c-string (c-string = substring)
	let (font, width, height, ascent, descent)
	  = font-metrics(font, port(medium));
      if (do-tabs?)
      	// XXX -  How do we do tabs? The text editor needs this!
      	#f; 
        /*let tab-origin :: <integer> = if (do-tabs? == #t) x else do-tabs? end;
        TabbedTextOut(grafport, x, y, c-string, _end - _start,
		      0, null-pointer(<Point*>), tab-origin)*/
      else
      	move-to-text-aligned(c-string, _start, _end, x, y, align-x, align-y, height, ascent);
				DrawText(grafport, x, y, c-string, _end - _start)
      end;
    end
  end;*/
  #f
end method draw-text;


// Ugly but effective. It's this or make style runs

define inline function move-to-text-aligned
    (text, _start, _end, h :: <integer>, v :: <integer>, align-x, align-y, 
     height :: <integer>, ascent :: <integer>) => ()
  h := h + select (align-x)
						 #"left"     => 0;
						 #"right"    => - TextWidth(text, _start, _end);
						 #"center"   => - floor/(TextWidth(text, _start, _end), 2);
					 end;
	v := v + select (align-y)
						 #"top"      => height;
						 #"bottom"   => 0;
						 #"center"   => floor/(height, 2);
						 #"baseline" => ascent;
					 end;
end function move-to-text-aligned;

//--- Provide this method if you can do faster than the default
/*define method glyph-for-character
    (_port :: <carbon-port>, char :: <character>, text-style :: <text-style>, #key font)
 => (index :: <integer>, font,
     escapement-x :: <real>, escapement-y :: <real>,
     origin-x :: <real>, origin-y :: <real>, bb-x :: <real>, bb-y :: <real>);
  let (index, char-set) = index-and-character-set(char);
  let font = font | text-style-mapping(_port, text-style, character-set: char-set);
  //--- Do it
end method glyph-for-character;

//--- Provide this method if you can do faster than the default
define method text-size
    (_port :: <carbon-port>, char :: <character>,
     #key text-style :: <text-style> = $default-text-style,
          start: _start, end: _end, do-newlines? = #f, do-tabs? = #f)
 => (largest-x :: <real>, total-height :: <real>,
     last-x :: <real>, last-y :: <real>, baseline :: <real>)
  ignore(_start, _end, do-newlines?);
  //--- Do it
end method text-size;

//--- Provide this method if you can do faster than the default,
//--- for instance, if there's a function to measure a whole string
define method text-size
    (_port :: <carbon-port>, string :: <string>,
     #key text-style :: <text-style> = $default-text-style,
          start: _start = 0, end: _end = size(string), do-newlines? = #f, do-tabs? = #f)
 => (largest-x :: <real>, total-height :: <real>,
     last-x :: <real>, last-y :: <real>, baseline :: <real>)
  //--- Do it
end method text-size;*/


////////////////////////////////////////////////////////////////////////////////
/// Methods required but not implemented by Vanilla medium
////////////////////////////////////////////////////////////////////////////////

define method convert-color( rep, color == #"white" )
=>( color )
	$native-white;
end method convert-color;

define method establish-clipping-region( medium :: <carbon-medium> )
=> ()
    values();
end method establish-clipping-region;

define method make-native-pixarray( drawable :: <drawable>, width :: <integer>, height :: <integer> )
=> (  result :: <array>  )
    make(<array>, dimensions: pair(width, height)); //* dummy return
end method make-native-pixarray;


define method make-native-image(drawable :: <drawable>, image)
=> ()
    values();
end method make-native-image;