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
  use string-extensions;
  use collection-extensions;
  export tk;
  export tk-extension;
end library tk;

define module tk-internal
  use dylan;
  use extensions;

  use file-descriptors;
  use system;
  use threads;

  use streams;
  use standard-io;

  use vector-search;
  use self-organizing-list;

  export
    // from tk-io.dylan
    tk-as, tk-unquote, tk-quote, put-tk-line, 
    // from tk-util.dylan
    anonymous-name, make-option, join-tk-args, parse-tk-list,
    // from tk-call.dylan
    <active-variable>, value, value-setter, call-tk-function;
end module tk-internal;

define module tk
  use dylan;
  use extensions;

  use file-descriptors;
  use system;
  use threads;

  use streams;
  use standard-io;

  use regular-expressions;
  
  use tk-internal,
    export: {<active-variable>, value, value-setter, tk-as, tk-unquote};

  export // widget classes
    <window>, <button>, <canvas>, <checkbutton>, <radiobutton>, <entry>,
    <frame>, <label>, <listbox>, <menu>, <menubutton>, <message>, <scale>,
    <scrollbar>, <text>, <toplevel>;
  export // support classes
    <text-index>, <text-mark>, <text-tag>, <canvas-item>;
  export // variables
    *root-window*;
  export // user functions
    configure, configuration, map-window, unmap-window, destroy-window,
    pack, unpack;
  export // bindings
    bind, get-binding, get-bindings;
  export // <button>s
    flash, invoke, activate, deactivate, select-value, deselect-value,
    toggle-value;
  export // <canvas>s
    xview, yview, focus, focus-setter, scan-mark, scan-dragto, select-item,
    create-arc, create-bitmap, create-line, create-oval, create-polygon,
    create-rectangle, create-text, create-window, postscript, items;
  export // <entry>s, <listbox>s, and <text>s
    delete, insert, get-all, get-elements, scan-mark, scan-dragto,
    select-adjust, select-clear, select-from, select-to;
  export // <entry>s
    icursor;
  export // <listbox>s
    // size, 
    current-selection, nearest, xview, yview;
  export // <menu>s
    add-command, add-checkbutton, add-radiobutton, add-cascade, add-separator;
  export // <scale>s
    get-value, set-value;
  export // <scrollbar>s
    scroll, get-units, set-units;
  export // <text>s
    xview, yview, line-end, tags, marks;
  export // <text-index>s
    text-at, line, character;
  export // <text-mark>s
    name; // value, value-setter
  export // <text-tag>s
    name, configure, configuration, bind, delete-tag, raise-tag,
    lower-tag, next-range, add-tag, remove-tag;
  export // <canvas-item>s
    configure, configuration, bind, delete-item, raise-item,
    lower-item, move-item, scale-item, item-type, item-coords,
    item-coords-setter;
end module tk;

define module tk-extension
  use tk-internal,
    export: {tk-as, put-tk-line, call-tk-function, join-tk-args, make-option,
	       tk-quote, tk-unquote, anonymous-name, parse-tk-list};
end module tk-extension;
