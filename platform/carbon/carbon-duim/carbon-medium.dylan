Module:       carbon-duim
Synopsis:     Carbon back-end
Author:	   		Scott McKay, Andy Armstrong, Peter Housel, Rob Myers
Copyright:    Original Code is Copyright (c) 1995-2000 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Carbon medium

define sealed class <carbon-medium> (<basic-medium>)
	// Ink
  sealed slot %ink-cache :: <object-table> = make(<table>);
  // Graphics
  sealed slot %foreground-color :: false-or(<RGBColor*>) = #f;
  sealed slot %background-color :: false-or(<RGBColor*>) = #f;
  sealed slot %transfer-mode :: false-or(<RGBColor*>) = #f;
  sealed slot %pen-width :: <integer> = 1;
  // XXX - And the texture brush???
  // XXX - And the background pattern???
  sealed slot %pen-dashes /*:: false-or(<Pattern>)*/ = #f;
  // Text
  sealed slot %font :: false-or(<carbon-font>) = #f;
  // Cached clipping region
  sealed slot %clip-mask = #f;		// #f, #"none", or a mac region
  // Path in progress
	sealed slot %current-path :: false-or(<RgnHandle>);
end class <carbon-medium>;

define sealed domain make (singleton(<carbon-medium>));
define sealed domain initialize (<carbon-medium>);

define method clear-ink-cache (medium :: <carbon-medium>)
  //--- This should really deallocate any cached pixmaps, etc.
  remove-all-keys!(medium.%ink-cache)
end method clear-ink-cache;

define sealed method initialize
    (medium :: <carbon-medium>, #key) => ()
  next-method();
  let cache = medium.%ink-cache;
  gethash(cache, $native-black) := $black;
  gethash(cache, $native-white) := $white;
end method initialize;


define method make-medium
    (_port :: <carbon-port>, sheet :: <sheet>)
 => (medium :: <carbon-medium>)
  make(<carbon-medium>,
       port: _port, sheet: sheet)
end method make-medium;

define sealed method destroy-medium
    (medium :: <carbon-medium>) => ()
  clear-ink-cache(medium);
  next-method();
  medium-drawable(medium) := #f
end method destroy-medium;

define sealed method medium-grafport
    (medium :: <carbon-medium>) => (grafptr :: <CGrafPtr>)
  debug-message("medium-grafport <carbon-medium>");
  mirror-grafport(medium-drawable(medium));
end method medium-grafport;

define sealed method do-attach-medium
    (sheet :: <sheet>, medium :: <carbon-medium>) => ()
  debug-message("do-attach-medium");
  let mirror = sheet-mirror(sheet);
  assert(mirror,
	 "Unexpected failure: no mirror when attaching medium for %=",
	 sheet);
  clear-ink-cache(medium);
  medium-drawable(medium) := mirror
end method do-attach-medium;

define sealed method do-detach-medium
    (sheet :: <sheet>, medium :: <carbon-medium>) => ()
  clear-ink-cache(medium);
  medium-drawable(medium) := #f
end method do-detach-medium;

define sealed method deallocate-medium
    (_port :: <carbon-port>, medium :: <carbon-medium>) => ()
  next-method();
  medium-drawable(medium) := #f
end method deallocate-medium;


define sealed method medium-foreground-setter
    (fg :: <ink>, medium :: <carbon-medium>) => (foreground :: <ink>)
  next-method();	// also sets 'medium-drawing-state-cache' appropriately
  clear-ink-cache(medium);
  // Force repaint to be done later
  unless (instance?(medium, <pixmap-medium>))
    InvalWindowRect(mirror-window(medium-drawable(medium)), $NULL)
  end;
  fg
end method medium-foreground-setter;

define sealed method medium-background-setter
    (bg :: <ink>, medium :: <carbon-medium>) => (background :: <ink>)
  next-method();	// also sets 'medium-drawing-state-cache' appropriately
  clear-ink-cache(medium);
  // Force repaint to be done later
  unless (instance?(medium, <pixmap-medium>))
    InvalWindowRect(mirror-window(medium-drawable(medium)), $NULL)
  end;
  bg
end method medium-background-setter;

define sealed method medium-clipping-region-setter
    (region :: <region>, medium :: <carbon-medium>) => (region :: <region>)
  next-method();
  // Don't flush the cache if the region isn't really changing.
  // This situation comes up all the time during repainting, when we set
  // the clipping region for every output record, but we almost always
  // just set it to $everywhere.
  unless (region == medium-clipping-region(medium))
    medium.%clip-mask := #f
  end;
  region
end method medium-clipping-region-setter;

define sealed method invalidate-cached-region
    (medium :: <carbon-medium>) => ()
  medium.%clip-mask := #f;
  next-method()
end method invalidate-cached-region;

define sealed method invalidate-cached-transform
    (medium :: <carbon-medium>) => ()
  medium.%clip-mask := #f;
  next-method()
end method invalidate-cached-transform;

define sealed method invalidate-cached-drawing-state
    (medium :: <carbon-medium>, cached-state :: <integer>) => ()
  ignore(cached-state);
end method invalidate-cached-drawing-state;


/// Display forcing

define sealed method force-display
    (medium :: <carbon-medium>) => ()
  debug-message("force-display()");
  let port :: <CGrafPtr>  = medium-grafport(medium);
  if(QDIsPortBuffered(port))
  	QDFlushPortBuffer(port, as(<RgnHandle>, 0));
  end if;
end method force-display;

define sealed method synchronize-display
    (medium :: <carbon-medium>) => ()
  force-display( medium );
end method synchronize-display;

/// Pen and brush support

// 'update-drawing-state' is a useful utility to be used by all of the
// drawing routines
define method update-drawing-state
    (medium :: <carbon-medium>, #key font, pen) => (drawable)
  debug-message("update-drawing-state ignoring pen: keyword");
  let drawable = medium-drawable(medium);
  when (drawable)
    let old-cache :: <integer> = medium-drawing-state-cache(medium);
    let new-cache :: <integer> = 0;
    when (old-cache ~= $medium-fully-cached)
      // Establish a brush, unless it's already cached
      when (zero?(logand(old-cache, $medium-brush-cached)))
	let brush = medium-brush(medium);
	establish-brush(medium, brush, drawable);
	new-cache := logior(new-cache, $medium-brush-cached)
      end;
      // Establish a pen, unless it's already cached
      //--- Note that you may have to establish a new pen if the brush changed
      //--- on platforms in which the pen contains color information
      when (zero?(logand(old-cache, $medium-pen-cached)))
	let pen = medium-pen(medium);
	establish-pen(medium, pen, drawable);
	new-cache := logior(new-cache, $medium-pen-cached)
      end;
      // Establish a font only if requested, unless it's already cached
      //--- Note that on some platforms, you may still have to set the text color
      //--- if the brush changed, even if the font didn't change
  // XXX - This crashes for no reason! Font method dispatch seems cursed!
     when (zero?(logand(old-cache, $medium-font-cached)))
	let text-style :: <text-style> = medium-merged-text-style(medium);
  let port :: <port> = port(medium);
	let font  = text-style-mapping(port, text-style);
	establish-font(medium, font, drawable);
	new-cache := logior(new-cache, $medium-font-cached)
      end;
      when (zero?(logand(old-cache, $medium-region-cached)))
	establish-clipping-region(medium);
	new-cache := logior(new-cache, $medium-region-cached)
      end;
      medium-drawing-state-cache(medium) := logior(old-cache, new-cache)
    end
  end;
  medium-grafport(medium);
end method update-drawing-state;

define method establish-brush
    (medium :: <carbon-medium>, brush :: type-union(<standard-brush>, <ink>), drawable) => ()
  let (pixel, fill-style, operation, image)
    = convert-ink-to-drawable-components(medium, drawable, brush);
  ignoring("establish-brush");
  // XXX - compute-clip-mask crashes at present with bounding box <simple-box>
  //let clip-mask = compute-clip-mask(medium);
  //--- Update drawable state
  //--- This should set the tile-x/y to (0,0) if the brush is a stipple, or it
  //--- should align it to the left/top of the figure if the brush is a pattern
end method establish-brush;

define method establish-pen 
    (medium :: <carbon-medium>, pen :: <standard-pen>, drawable) => ()
  //--- Update drawable state
end method establish-pen;

define method establish-font
    (medium :: <carbon-medium>, font, drawable) => ()
  //--- Update drawable state
end method establish-font;


// Compute the clip mask. This suggested implementation keeps a cached
// version around for efficiency
define method compute-clip-mask (medium :: <carbon-medium>) => (mask)
  let mask = medium.%clip-mask 
  | (medium.%clip-mask
       := begin
	    let sheet = medium-sheet(medium);
	    let sregion = sheet-device-region(sheet);
      debug-message(format-to-string("sregion: %=", sregion));
	    let mregion = medium-clipping-region(medium);
	    let valid? = #t;
	    if (sregion == $nowhere | mregion == $nowhere)
	      #"none"
	    else
	      let (sleft, stop, sright, sbottom) = box-edges(sregion);
	      unless (mregion == $everywhere)
		let (mleft, mtop, mright, mbottom) = box-edges(mregion);
		let (mleft, mtop, mright, mbottom)
		  = transform-box(sheet-device-transform(sheet), 
				  mleft, mtop, mright, mbottom);
		let (v, left, top, right, bottom)
		  = ltrb-intersects-ltrb?(sleft, stop, sright, sbottom,
					  mleft, mtop, mright, mbottom);
		sleft := left;
		stop  := top;
		sright  := right;
		sbottom := bottom;
		valid? := v
	      end;
	      if (valid?)
		vector(sleft, stop, sright, sbottom)
	      else
		#"none"
	      end
	    end
	  end);
  let (left, top, right, bottom) = box-edges(mask);
  debug-message("compute-clip-mask=> left: %d top: %d right: %d bottom: %d",
    left, top, right, bottom);
  mask;
end method compute-clip-mask;


define generic convert-ink-to-drawable-components
    (medium :: <carbon-medium>, drawable, brush)
 => (pixel, fill-style, operation, image :: false-or(<image>));

// Given a color, returns a native pixel value
define function color->native-color (color :: <color>, mirror) => (native-color)
  let cache = mirror.%ink-cache;
  gethash(cache, color)
  | begin
      let (r, g, b) = color-rgb(color);
      let value = convert-color-to-native(mirror, r, g, b);
      gethash(cache, color) := value;
      value
    end
end function color->native-color;

// Given a native color, returns a color
define function native-color->color (native-color, mirror) => (color :: <color>)
  let cache = mirror.%ink-cache;
  gethash(cache, native-color)
  | begin
      let (r, g, b) = native-color-rgb(native-color);
      let value = make-rgb-color(r, g, b);
      gethash(cache, native-color) := value;
      value
    end
end function native-color->color;

define method convert-ink-to-drawable-components 
    (medium :: <carbon-medium>, drawable, brush :: <foreground>)
 => (pixel, fill-style, operation, image :: false-or(<image>))
  convert-ink-to-drawable-components(medium, drawable, medium-foreground(medium))
end method convert-ink-to-drawable-components;

define method convert-ink-to-drawable-components 
    (medium :: <carbon-medium>, drawable, brush :: <background>)
 => (pixel, fill-style, operation, image :: false-or(<image>))
  convert-ink-to-drawable-components(medium, drawable, medium-background(medium))
end method convert-ink-to-drawable-components;

define method convert-ink-to-drawable-components 
    (medium :: <carbon-medium>, drawable, brush :: <color>)
 => (pixel, fill-style, operation, image :: false-or(<image>))
  values(color->native-color(brush, drawable), #"solid", $boole-1, #f)
end method convert-ink-to-drawable-components;

define method convert-ink-to-drawable-components
    (medium :: <carbon-medium>, drawable, brush :: <contrasting-color>)
 => (pixel, fill-style, operation, image :: false-or(<image>))
  convert-ink-to-drawable-components(medium, drawable, contrasting-color->color(brush))
end method convert-ink-to-drawable-components;

//--- You might want to handle general <image> objects, too
define method convert-ink-to-drawable-components
    (medium :: <carbon-medium>, drawable, brush :: <stencil>)
 => (pixel, fill-style, operation, image :: false-or(<image>))
  debug-message("convert-ink-to-drawable-components");
  let cache = medium-drawable(medium).%ink-cache;    // mirror.%ink-cache;
  let pattern
    = gethash(cache, brush)
      | begin
	  let (array, colors) = decode-pattern(brush);
	  let width  = image-width(brush);
	  let height = image-height(brush);
	  let ncolors :: <integer> = size(colors);
	  let pixels :: <simple-object-vector> = make(<simple-vector>, size: ncolors);
	  let image = make-native-pixarray(drawable, width, height);
	  without-bounds-checks
	    for (n :: <integer> from 0 below ncolors)
	      let pixel = convert-ink-to-drawable-components(medium, drawable, colors[n]);
	      pixels[n] := pixel
	    end;
	    for (y :: <integer> from 0 below height)
	      for (x :: <integer> from 0 below width)
		image[y,x] := pixels[array[y,x]]
	      end
	    end
	  end;
	  let value = make-native-image(drawable, image);
	  gethash(cache, brush) := value;
	  value
	end;
  values(convert-color(medium, #"white"), #"solid", $boole-1, pattern)
end method convert-ink-to-drawable-components;

define method convert-ink-to-drawable-components
    (medium :: <carbon-medium>, drawable, brush :: <pixmap>)
 => (pixel, fill-style, operation, image :: false-or(<image>))
  debug-message("convert-ink-to-drawable-components");
  //--- You might be able to draw directly with a pixmap...
  values(convert-color(medium, #"white"), #"solid", $boole-1, brush)
end method convert-ink-to-drawable-components;

define method convert-ink-to-drawable-components
    (medium :: <carbon-medium>, drawable, brush :: <standard-brush>)
 => (pixel, fill-style, operation, image :: false-or(<image>))
  debug-message("convert-ink-to-drawable-components");
  let (pixel, fill-style, operation, pattern)
    = case
	brush-tile(brush) =>
	  convert-ink-to-drawable-components(medium, drawable, brush-tile(brush));
	brush-stipple(brush) =>
	  convert-ink-to-drawable-components(medium, drawable, brush-stipple(brush));
	otherwise =>
	  convert-ink-to-drawable-components(medium, drawable, brush-foreground(brush));
      end;
  ignore(operation);
  values(pixel, fill-style, brush-mode(brush), pattern);
end method convert-ink-to-drawable-components;