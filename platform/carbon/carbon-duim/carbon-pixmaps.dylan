Module:       carbon-duim
Synopsis:     Carbon back-end
Author:	   Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2000 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// XXX - Actually a GWorld rather than a PixMap so it can be drawn into!

define sealed class <carbon-pixmap> (<mirror>, <pixmap>)
  sealed slot %pixmap :: <GWorldPtr>,
    init-keyword: pixmap:;
  sealed slot %medium :: <medium>, 
    init-keyword: medium:;
end class <carbon-pixmap>;

//--- A little wierd that this is called 'medium-drawable', but what the heck
/*define method medium-drawable
    (pixmap :: <carbon-pixmap>) => (drawable :: <GWorldPtr>)
  // XXX - Return its grafptr?
  pixmap.%pixmap
end method medium-drawable;*/

define method image-width 
    (pixmap :: <carbon-pixmap>) => (width :: <integer>)
  //--- Do it
end method image-width;

define method image-height 
    (pixmap :: <carbon-pixmap>) => (width :: <integer>)
  //--- Do it
end method image-height;

define method image-depth 
    (pixmap :: <carbon-pixmap>) => (width :: <integer>)
  //--- Do it
end method image-depth;

//---*** Should all pixmaps just keep a track of their medium anyway?
define method port 
    (pixmap :: <carbon-pixmap>) => (port :: <port>)
  port(pixmap.%medium)
end method port;


define method do-make-mirror
    (_port :: <carbon-port>, sheet :: <pixmap-sheet>)
 => (mirror :: <carbon-mirror>)
  //--- Do it, or maybe you don't need to do anything
end method do-make-mirror;


define method do-make-pixmap
    (_port :: <carbon-port>, medium :: <carbon-medium>, 
     width :: <integer>, height :: <integer>)
 => (pixmap :: <carbon-pixmap>)
  //--- Do it
end method do-make-pixmap;

define method destroy-pixmap
    (pixmap :: <carbon-pixmap>) => ()
  //--- Do it
end method destroy-pixmap;


define sealed class <carbon-pixmap-medium>
    (<carbon-medium>, 
     <basic-pixmap-medium>)
  sealed slot %pixmap, init-keyword: pixmap:;
  sealed slot %medium, init-keyword: medium:;
end class <carbon-pixmap-medium>;

define method make-pixmap-medium
    (_port :: <carbon-port>, sheet :: <sheet>, #key width, height)
 => (medium :: <carbon-pixmap-medium>)
  //--- Do it
end method make-pixmap-medium;


define method do-copy-area
    (from :: <carbon-pixmap>, from-x, from-y, width, height,
     to :: <carbon-pixmap>, to-x, to-y,
     #key function = $boole-1) => ()
  //--- Do it
end method do-copy-area;

define method do-copy-area
    (from :: <carbon-pixmap>, from-x, from-y, width, height,
     to :: <carbon-medium>, to-x, to-y,
     #key function = $boole-1) => ()
  //--- Do it
end method do-copy-area;

define method do-copy-area
    (from :: <carbon-medium>, from-x, from-y, width, height,
     to :: <carbon-pixmap>, to-x, to-y,
     #key function = $boole-1) => ()
  //--- Do it
end method do-copy-area;

define method do-copy-area
    (from :: <carbon-medium>, from-x, from-y, width, height,
     to :: <carbon-medium>, to-x, to-y,
     #key function = $boole-1) => ()
  //--- Do it
end method do-copy-area;
