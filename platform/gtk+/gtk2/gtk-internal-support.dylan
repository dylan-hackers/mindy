module: gtk-internal-support

c-include("gtk/gtk.h");

define class <value-cell> (<object>)
  slot value-cell-value, required-init-keyword: value:
end;

define sealed domain make (singleton(<value-cell>));
define sealed domain initialize (<value-cell>);

define method export-value(cls == <GCallback>, value :: <function>) => (result :: <function-pointer>);
  make(<function-pointer>, pointer: value.callback-entry); 
end method export-value;

define method import-value(cls == <function>, value :: <GCallback>) => (result :: <function>);
  error("Is this possible?");
end method import-value;

define sealed method export-value(cls == <gpointer>, the-value :: <object>) 
 => (result :: <gpointer>);
  make(<gpointer>, 
       pointer: object-address(make(<value-cell>, value: the-value)));
end method export-value;

define sealed method import-value(cls == <object>, the-value :: <gpointer>) 
 => (result :: <object>);
  value-cell-value(heap-object-at(the-value.raw-value));
end method import-value;

define method make(type :: subclass(<GTypeInstance>), #rest args, 
                   #key pointer, #all-keys)
 => (result :: <GTypeInstance>)
  if(pointer & (as(<integer>, pointer) ~= 0))
    let instance = next-method(<GTypeInstance>, pointer: pointer);
    let g-type = g-type-from-instance(instance);
    let dylan-type = find-gtype(g-type);
    next-method(dylan-type, pointer: pointer);
  else
    next-method();
  end if;
end method make;

define open generic find-gtype(type :: <GType>) => (dylan-type :: <type>);

define function g-type-from-instance(instance :: <GTypeInstance>)
 => (type :: <GType>);
  c-decl("GType g_type_from_instance(gpointer instance) { return G_TYPE_FROM_INSTANCE(instance); }");
  call-out("g_type_from_instance", int:, ptr: instance.raw-value);
end function g-type-from-instance;

define function g-value-type(instance :: <GValue>)
 => (type :: <GType>);
  c-decl("GType g_value_type(gpointer instance) { return G_VALUE_TYPE(instance); }");
  call-out("g_value_type", int:, ptr: instance.raw-value);
end function g-value-type;

// Another stupid workaround. Sometimes we need to access mapped types
// as pointers, and Melange doesn't provide any way to do so. Or does it?
define sealed functional class <c-pointer-vector> (<c-vector>) end;

define sealed domain make (singleton(<c-pointer-vector>));

define constant $pointer-size = 4;

define sealed method pointer-value
    (ptr :: <c-pointer-vector>, #key index = 0)
 => (result :: <statically-typed-pointer>);
  pointer-at(ptr,
	     offset: index * $pointer-size,
	     class: <statically-typed-pointer>);
end method pointer-value;

define sealed method pointer-value-setter
    (value :: <statically-typed-pointer>,
     ptr :: <c-pointer-vector>, #key index = 0)
 => (result :: <statically-typed-pointer>);
  pointer-at(ptr,
	     offset: index * $pointer-size,
	     class: <statically-typed-pointer>) := value;
  value;
end method pointer-value-setter;

define sealed method content-size (value :: subclass(<c-pointer-vector>))
 => (result :: <integer>);
  $pointer-size;
end method content-size;

define function gtk-init(progname, arguments)
  let (argc, argv) = c-arguments(progname, arguments);
  %gtk-init(argc, argv);
end function gtk-init;

define method c-arguments(progname :: <string>, arguments)
 => (<int*>, <char***>)
  let argc = 1 + arguments.size;
  let argv :: <c-string-vector> =
    make(<c-string-vector>, element-count: argc + 1);
  // XXX - We'd need to delete these if we weren't using
  // a garbage collector which handles the C heap.
  argv[0] := progname;
  for (i from 1 below argc,
       arg in arguments)
    argv[i] := arg;
  end for;
  as(<c-pointer-vector>, argv)[argc] := null-pointer;
  let pargc = make(<int*>);
  pointer-value(pargc) := argc;
  let pargv = make(<char***>);
  pointer-value(pargv) := argv;
  values(pargc, pargv);
end method c-arguments;
