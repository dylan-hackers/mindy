Module: Dylan-user
Copyright: Copyright 2001 Peter S. Housel
License: GNU Lesser General Public License

define module gtk
  use Dylan;
  use Melange-support,
    exclude: { null-pointer },
    export: { <C-string>,
	      destroy,
              pointer-value,
              pointer-value-setter,
              callback-method };

  use System, 
    import: { pointer-deref, pointer-deref-setter, <raw-pointer> }, 
    export: all;
  use glib;
  use gdk, export: all;
  use gtk-extra, export: all;
  use gtk-internal, export: all;
  
  export
    null-pointer, null-pointer?, pointer-address,
    // pointer-value, pointer-value-setter, size-of,
    // <C-void*>
    <C-int*>,
    <C-pointer>,
    // <C-string>, <C-unicode-string>,
    <C-string*>,
    // <C-unicode-string*>,
    pointer-cast, // destroy,
    pointer-value-address, c-type-cast,
    // c-callable-wrapper-definer, 
    \with-c-string,
    \with-stack-structure,
    \C-callable-wrapper-definer,
    \%wrapper-callback, \%wrapper-aux;

  export gtk-signal-connect*,
         gtk-signal-connect-object*,
         \gtk-callback-definer,
         \delete-event-callback-definer,
         \widget-callback-definer,
         \configure-callback-definer,
         \motion-callback-definer,
         \expose-callback-definer,
         \button-press-callback-definer;

  export initialize-gtk;

  export \gtk-type-cast-function-definer,
         \GTK-OBJECT,
         \GTK-ADJUSTMENT,
         \GTK-CONTAINER,
         \GTK-BIN,
         \GTK-FIXED,
         \GTK-FRAME,
         \GTK-BOX,
         \GTK-WIDGET,
         \GTK-WINDOW,
         \GTK-LABEL,
         \GTK-BUTTON,
         \GTK-TOGGLE-BUTTON,
         \GTK-CHECK-BUTTON,
         \GTK-RADIO-BUTTON,
         \GTK-ENTRY,
         \GTK-TEXT,
         \GTK-CLIST,
         \GTK-DRAWING-AREA,
         \GTK-HSCALE,
         \GTK-VSCALE,
         \GTK-HSCROLLBAR,
         \GTK-VSCROLLBAR,
         \GTK-MENU,
         \GTK-MENU-BAR,
         \GTK-MENU-ITEM,
         \GTK-FILE-SELECTION;
end module;
