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
// This file contains the library declarations for the "tk" library.  It
// exports two modules:  "tk-internal" provides an informal "extension
// protocol" for outside implementors, while "tk" provides all user level
// functions. 
//
//======================================================================

define library tk
  use dylan;
  use streams;
  use standard-io;
  use string-extensions;
  use collection-extensions;
  use table-extensions;
  use regular-expressions;
  use format;
  export tk;
  export tk-extension;
end library tk;

define module tk-internal
  use dylan;
  use extensions;
  use table-extensions;

  use file-descriptors;
  use system;
  use threads;

  use streams;
  use standard-io;
  
  use vector-search;
  use self-organizing-list;

  use format;

  export
    // from tk-io.dylan
    tk-as, tk-unquote, tk-quote, put-tk-line, 
    // from tk-util.dylan
    anonymous-name, make-option, join-tk-args, parse-tk-list,
    get-name-from-class, get-class-from-name,
    // from tk-call.dylan
    <active-variable>, value, value-setter, call-tk-function;
end module tk-internal;

define module tk
  use dylan;
  use extensions;
  use table-extensions;

  use file-descriptors;
  use system;
  use threads;

  use streams;
  use standard-io;

  use regular-expressions;
  use string-conversions;
  
  use format;
  
  use tk-internal,
    export: {<active-variable>, value, value-setter, tk-as, tk-unquote};

  export // widget classes
    <window>, <button>, <canvas>, <checkbutton>, <radiobutton>, <entry>,
    <frame>, <label>, <listbox>, <menu>, <menubutton>, <message>, <scale>,
    <scrollbar>, <text>, <toplevel>;
  export // support classes
    <text-index>, <text-mark>, <text-tag>, <canvas-item>, <canvas-tag>,
    <point>, <color>, <rgb-color>;
  export // variables
    *root-window*;
  export // user functions
    configure, configuration, map-window, unmap-window, destroy-window,
    pack, unpack;
  export // bindings
    bind, get-binding, get-bindings, tk-break, tk-continue;
  export // <button>s
    flash, invoke, select-value, deselect-value,
    toggle-value;
  export // <canvas>s
    xview, yview, focus, focus-setter, scan-mark, scan-dragto, select-item,
    create-arc, create-bitmap, create-line, create-oval, create-polygon,
    create-rectangle, create-text, create-window, postscript, items,
    canvas-x, canvas-y;
  export // <entry>s, <listbox>s, and <text>s
    delete, insert, get-all, get-elements, scan-mark, scan-dragto,
    select-adjust, select-clear, select-from, select-to;
  export // <entry>s
    icursor, xview;
  export // <listbox>s
    // size, 
    current-selection, nearest, xview, yview, set-selection,
    selection-anchor-setter, clear-selection, selection-includes?;
  export // <menu>s
    add-command, add-checkbutton, add-radiobutton, add-cascade, add-separator,
    activate-entry, delete, configure-entry, entry-configuration, invoke-entry,
    post, unpost, yposition-entry, post-cascade;
  export // <scale>s
    get-value, set-value;
  export // <scrollbar>s
    scroll, get-units, set-units;
  export // <text>s
    xview, yview, line-end, tags, marks;
  export // <toplevel>s
    tk-dialog;
  export // <text-index>s
    text-at, line, character;
  export // <text-mark>s
    name; // value, value-setter
  export // <text-tag>s
    name, configure, configuration, bind, delete-tag, raise-tag,
    lower-tag, next-range, add-tag, remove-tag;
  export // <canvas-item>s and <canvas-tag>s
    configure, configuration, bind, delete-item, raise-item,
    lower-item, move-item, scale-item, item-type, item-coords,
    item-coords-setter, find-items, add-canvas-tag, get-canvas-tags,
    delete-canvas-tag, bounding-box;
  export // window information functions
    tk-class, children, window-containing-x-y, window-containing-point,
    depth, exists?, distance-to-float-pixels, geometry, height, X-id,
    mapped?, geometry-manager, window-with-X-id, distance-to-pixels,
    mouse-x-y, mouse-point, requested-height, requested-width, width,
    color-to-r-g-b, color-to-rgb-color, abs-position-x-y, abs-position-point,
    screen-name, screen-colormap-cells, screen-depth, screen-height,
    screen-width, toplevel, viewable?, visual-class, available-visuals,
    virtual-root-height, virtual-root-width, virtual-root-position-x-y,
    virtual-root-position-point, x-y-in-parent, point-in-parent,
    colormap-cells, colormap-full?;
end module tk;

define module tk-extension
  use tk-internal,
    export: {tk-as, put-tk-line, call-tk-function, join-tk-args, make-option,
	       tk-quote, tk-unquote, anonymous-name, parse-tk-list};
end module tk-extension;
