module: tk

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
    pack;
  export // bindings
    bind, get-binding, get-bindings;
  export // <button>s
    flash, invoke, activate, deactivate, select-value, deselect-value,
    toggle-value;
  export // <canvas>s
    xview, yview, focus, focus-setter, scan-mark, scan-dragto, select-item,
    create-arc, create-bitmap, create-line, create-oval, create-polygon,
    create-rectangle, create-window, postscript;
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
    get-units, set-units;
  export // <scrollbar>s
    scroll, get-units, set-units;
  export // <text>s
    xview, yview, line-end, tags, marks;
  export // <text-index>s
    text-at, line, character;
  export // <text-mark>s
    name; // value, value-setter
  export // <text-tag>s
    name, configure-tag, tag-configuration, bind-tag, delete-tag, raise-tag,
    lower-tag, next-range, add-tag, remove-tag;
  export // <canvas-item>s
    configure-item, item-configuration, bind-item, delete-item, raise-item,
    lower-item, move-item, scale-item, item-type, item-coords,
    item-coords-setter;
end module tk;

define module tk-extension
  use tk-internal,
    export: {tk-as, put-tk-line, call-tk-function, join-tk-args, make-option,
	       tk-quote, tk-unquote, anonymous-name};
end module tk-extension;
