Module: gtk
Copyright: Copyright 2001 Peter S. Housel
License: GNU Lesser General Public License

c-include("argv.h");
c-include("gdk/gdk.h");
c-include("gtk/gtk.h");

// C-FFI emulation

define constant <C-pointer> = <statically-typed-pointer>;

define method null-pointer
    (pointer-designator-class :: subclass(<statically-typed-pointer>))
 => (null :: <statically-typed-pointer>);
  as(pointer-designator-class, 0);
end method;

define method null-pointer?
    (null :: <statically-typed-pointer>)
 => (null? :: <boolean>);
  as(<integer>, null) == 0;
end method;

define method pointer-address
    (c-pointer :: <statically-typed-pointer>)
 => (address :: <integer>)
  as(<integer>, c-pointer.raw-value);
end method;

define method pointer-cast
    (pointer-designator-class :: subclass(<statically-typed-pointer>),
     c-pointer :: <statically-typed-pointer>)
 => (new-c-pointer :: <statically-typed-pointer>);
  make(pointer-designator-class, pointer: c-pointer.raw-value);
end method;

define method pointer-value-address
    (pointer :: <statically-typed-pointer>, #key index)
 => (offset-pointer :: <statically-typed-pointer>);
  make(object-class(pointer),
       pointer: pointer.raw-value
         + index * content-size(object-class(pointer)));
end method;

define sealed method element
    (pointer :: <indexable-statically-typed-pointer>, index :: <integer>,
     #key default)
 => (object :: <object>);
  pointer-value(pointer, index: index);
end method;

define sealed method element-setter
    (object :: <object>, pointer :: <indexable-statically-typed-pointer>,
     index :: <integer>)
 => (object :: <object>);
  pointer-value(pointer, index: index) := object;
end method;

define open generic c-type-cast
    (type :: <class>, value :: <object>)
 => (value :: <object>);

define method c-type-cast
    (type == <integer>, value :: <object>)
 => (value :: <object>);
  as(<integer>, value);
end method;

define macro with-c-string
  { with-c-string ( ?:name = ?:expression ) ?:body end }
    => { let ?name = as(<c-string>, ?expression);
         ?body; }
end macro;

define macro with-stack-structure
  { with-stack-structure (?:name :: ?type:name,
			  #key ?element-count:expression = 1,
                               ?extra-bytes:expression = 0)
      ?:body
    end }
    => { let ?name = make(?type,
			  element-count: ?element-count,
			  extra-bytes: ?extra-bytes);
	 block ()
	   ?body
	 cleanup
	   destroy(?name);
	 end }
end macro;

define functional class <C-int*> (<statically-typed-pointer>) end;

define sealed method pointer-value
    (c-pointer :: <C-int*>, #key index = 0)
 => (result :: <object>);
  pointer-deref(int:, c-pointer.raw-value,
                index * c-expr(int:, "sizeof(int)"));
end method;

define sealed method pointer-value-setter
    (value :: <object>, c-pointer :: <C-int*>, #key index = 0)
 => (value :: <object>);
  pointer-deref(int:, c-pointer.raw-value,
                index * c-expr(int:, "sizeof(int)")) := value;
end method;

define sealed method content-size
    (class == <C-int*>)
 => (size :: <integer>);
  c-expr(int:, "sizeof(int)");
end method;

define functional class <C-string*> (<indexable-statically-typed-pointer>) end;

define sealed method pointer-value
    (c-pointer :: <C-string*>, #key index = 0)
 => (result :: <object>);
  make(<c-string>,
       pointer: pointer-deref(ptr:, c-pointer.raw-value,
                              index * c-expr(int:, "sizeof(char *)")));
end method;

define sealed method pointer-value-setter
    (value :: <object>, c-pointer :: <C-string*>, #key index = 0)
 => (value :: <object>);
  pointer-deref(ptr:, c-pointer.raw-value,
                index * c-expr(int:, "sizeof(char *)")) := value.raw-value;
end method;

define sealed method content-size
    (class == <C-string*>)
 => (size :: <integer>);
  c-expr(int:, "sizeof(char *)");
end method;

define sealed method pointer-value
    (c-pointer :: <GdkGC**>, #key index = 0)
 => (result :: <object>);
  make(<GdkGC*>,
       pointer: pointer-deref(ptr:, c-pointer.raw-value,
                              index * c-expr(int:, "sizeof(GdkGC *)")));
end method;

define sealed method content-size
    (class == <GdkGC**>)
 => (size :: <integer>);
  c-expr(int:, "sizeof(GdkGC *)");
end method;

// This only works if all of the parameters are pointers

define macro C-callable-wrapper-definer
  { define C-callable-wrapper ?wrapper:name of ?wrapped:name
      ?params:*
    end }
    => { define constant ?wrapper
           = make(<function-pointer>, pointer:
                    callback-entry(%wrapper-callback(?params)
                                     %wrapper-aux(?wrapped(?params))
                                   end)) }
end macro;

define macro %wrapper-callback
  { %wrapper-callback ( ?params ) ?:body end }
    => { callback-method(?params) => (value :: <integer>); ?body end }
params:
  { } => { }
  { parameter ?:name :: ?type:expression ; ... }
    => { ?name :: <raw-pointer>, ... }
end macro;

define macro %wrapper-aux
  { %wrapper-aux ( ?wrapped:name(?params) ) }
    => { ?wrapped(?params) }
params:
  { } => { }
  { parameter ?:name :: ?type:expression ; ... }
    => { make(?type, pointer: ?name), ... }
end macro;

// Callback functions

define macro delete-event-callback-definer
  { define delete-event-callback ?wrapper:name = ?wrapped:name }
    => { define constant ?wrapper
           = make(<function-pointer>, pointer:
                    callback-entry(callback-method(widget :: <raw-pointer>,
                                                   event :: <raw-pointer>,
                                                   data :: <raw-pointer>)
                                     => (value :: <integer>);
                                     ?wrapped(make(<GtkWidget*>,
                                                   pointer: widget),
                                              make(<GdkEvent*>,
                                                   pointer: event),
                                              data);
                                   end));
       }
end macro;

define macro gtk-callback-definer
  { define gtk-callback(?new:name, ?data-type:expression) ?eq:token ?old:name }
    => { define constant ?new
           ?eq make(<function-pointer>, pointer:
                      callback-entry(callback-method(widget :: <raw-pointer>,
                                                     data :: <raw-pointer>)
                                       => ();
                                     ?old(make(<GtkWidget*>, pointer: widget),
                                          make(?data-type, pointer: data));
                                    end));
       }
end macro;

define macro widget-callback-definer
  { define widget-callback ?wrapper:name = ?wrapped:name }
    => { define gtk-callback(?wrapper, <gpointer>) = ?wrapped }
end macro;

define macro configure-callback-definer
  { define configure-callback ?wrapper:name = ?wrapped:name }
    => { define gtk-callback(?wrapper, <GdkEventConfigure*>) = ?wrapped }
end macro;

define macro motion-callback-definer
  { define motion-callback ?wrapper:name = ?wrapped:name }
    => { define gtk-callback(?wrapper, <GdkEventMotion*>) = ?wrapped }
end macro;

define macro expose-callback-definer
  { define expose-callback ?wrapper:name = ?wrapped:name }
    => { define gtk-callback(?wrapper, <GdkEventExpose*>) = ?wrapped }
end macro;

define macro button-press-callback-definer
  { define button-press-callback ?wrapper:name = ?wrapped:name }
    => { define gtk-callback(?wrapper, <GdkEventButton*>) = ?wrapped }
end macro;

// Signal callback utilities

define method gtk-signal-connect*
    (object :: <GtkObject*>,
     name :: <byte-string>,
     func :: <GtkFunction>,
     data :: <gpointer>)
 => (result :: <guint>);
  with-c-string(c-name = name)
    gtk-signal-connect(object, c-name, func, data);
  end;
end method;
     
define method gtk-signal-connect-object*
    (object :: <GtkObject*>,
     name :: <byte-string>,
     func :: <GtkFunction>,
     slot-object :: <GtkObject*>)
 => (result :: <guint>);
  with-c-string(c-name = name)
    gtk-signal-connect-object(object, c-name, func, slot-object);
  end;
end method;
     
// Cast functions

define macro gtk-type-cast-function-definer
  { define gtk-type-cast-function ?:name => ?type:expression }
    => { define inline function ?name
             (ptr :: <statically-typed-pointer>)
          => (new :: ?type);
           pointer-cast(?type, ptr);
         end function }
end macro;

define gtk-type-cast-function GTK-OBJECT => <GtkObject*>;
define gtk-type-cast-function GTK-ADJUSTMENT => <GtkAdjustment*>;
define gtk-type-cast-function GTK-CONTAINER => <GtkContainer*>;
define gtk-type-cast-function GTK-BIN => <GtkBin*>;
define gtk-type-cast-function GTK-FIXED => <GtkFixed*>;
define gtk-type-cast-function GTK-FRAME => <GtkFrame*>;
define gtk-type-cast-function GTK-BOX => <GtkBox*>;
define gtk-type-cast-function GTK-WIDGET => <GtkWidget*>;
define gtk-type-cast-function GTK-WINDOW => <GtkWindow*>;
define gtk-type-cast-function GTK-LABEL => <GtkLabel*>;
define gtk-type-cast-function GTK-BUTTON => <GtkButton*>;
define gtk-type-cast-function GTK-TOGGLE-BUTTON => <GtkToggleButton*>;
define gtk-type-cast-function GTK-CHECK-BUTTON => <GtkCheckButton*>;
define gtk-type-cast-function GTK-RADIO-BUTTON => <GtkRadioButton*>;
define gtk-type-cast-function GTK-ENTRY => <GtkEntry*>;
define gtk-type-cast-function GTK-TEXT => <GtkText*>;
define gtk-type-cast-function GTK-CLIST => <GtkCList*>;
define gtk-type-cast-function GTK-DRAWING-AREA => <GtkDrawingArea*>;
define gtk-type-cast-function GTK-HSCALE => <GtkHScale*>;
define gtk-type-cast-function GTK-VSCALE => <GtkVScale*>;
define gtk-type-cast-function GTK-HSCROLLBAR => <GtkHScrollbar*>;
define gtk-type-cast-function GTK-VSCROLLBAR => <GtkVScrollbar*>;
define gtk-type-cast-function GTK-MENU => <GtkMenu*>;
define gtk-type-cast-function GTK-MENU-BAR => <GtkMenuBar*>;
define gtk-type-cast-function GTK-MENU-ITEM => <GtkMenuItem*>;
define gtk-type-cast-function GTK-FILE-SELECTION => <GtkFileSelection*>;

// Initialize

define method initialize-gtk() => ();
  call-out("gtk_init", void:,
           ptr: c-expr(ptr: "&application_argc"),
           ptr: c-expr(ptr: "&application_argv"));
end method;
