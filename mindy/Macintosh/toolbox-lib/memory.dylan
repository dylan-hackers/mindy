module: Toolbox
author: Patrick Beard, maintenance by Rob Myers (yarrel@netscape.net)
copyright: (c)1999 Patrick Beard

// Memory Manager.

define constant $nil = as(<statically-typed-pointer>, 0);

// <Ptr>

define class <Ptr> (<statically-typed-pointer>) end class;

define constant NewPtr = get-c-function("NewPtr", args: list(<integer>),
											result: <Ptr>, file: *InterfaceLib*);
define constant DisposePtr = get-c-function("DisposePtr", args: list(<Ptr>),
											result: #(), file: *InterfaceLib*);

define method destroy (pointer :: <Ptr>) => ();
  DisposePtr(pointer);
end method destroy;

define class <Handle> (<statically-typed-pointer>) end class;

define constant NewHandle = get-c-function("NewHandle", args: list(<integer>),
											result: <Handle>, file: *InterfaceLib*);
define constant DisposeHandle = get-c-function("DisposeHandle", args: list(<Handle>),
											result: #(), file: *InterfaceLib*);

define method destroy (handle :: <Handle>) => ();
  DisposeHandle(handle);
end method destroy;