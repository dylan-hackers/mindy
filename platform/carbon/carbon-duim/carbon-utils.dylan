Module:       carbon-duim
Synopsis:     Carbon back-end
Author:	      Rob Myers, Peter Housel, Erik Kidd, XXX - Fun-O credits!
Copyright:    (c) 2001 Gwydion Dylan Maintainers
License:      GNU Lesser General Public License
Dual-license: Functional Objects Library Public License Version 1.0
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/*	
  // TODO
	// Display an error dialog
	display-error-dialog [ignore, debugger, kill]
*/


////////////////////////////////////////////////////////////////////////////////
/// DUMMIES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
////////////////////////////////////////////////////////////////////////////////

define method sheet-screen-position(a, b)
  values(0,0); 
end method sheet-screen-position;

define method carbon-dialog-x-pixels(a, b)
  b * 5;
end method carbon-dialog-x-pixels;

define method tool-bar-decoration(a)
  #f;
end method tool-bar-decoration;


////////////////////////////////////////////////////////////////////////////////
/// Useful constants
////////////////////////////////////////////////////////////////////////////////

//---*** Can we get these from somewhere?
define constant $TRUE    :: <integer> = 1;
define constant $FALSE     :: <integer> = 0;

define constant $empty-c-string = as(<C-string>, "");
define constant $empty-pascal-string = as(<pascal-string>, "");


////////////////////////////////////////////////////////////////////////////////
// Sealing
////////////////////////////////////////////////////////////////////////////////

define sealed class <sealed-constructor-mixin> (<object>) end;

define sealed domain make (subclass(<sealed-constructor-mixin>));
define sealed domain initialize (<sealed-constructor-mixin>);


////////////////////////////////////////////////////////////////////////////////
// Error-checking 
////////////////////////////////////////////////////////////////////////////////

// The following function can be used to check the returned value 
// of a Carbon function and signal an error in case of failure.
// Note that 0 == noErr, so non-zero values are errors for most functions.

// Note we check for memory and resource errors separately
define method check-result
    (name :: <string>, handle :: <C-pointer>) => (value :: <C-pointer>)
  when (handle = $NULL)
    error(name)
  end;
  handle
end method check-result;

define method check-result
    (name :: <string>, ok? :: <boolean>) => (value :: <boolean>)
  unless (ok?)
    error(name)
  end;
  ok?
end method check-result;

define method check-result
    (name :: <string>, code :: type-union(<OSErr>, <OSStatus>))
 => (value :: type-union(<OSErr>, <OSStatus>))
  unless (code = $noErr)
    let err = format-to-string("%s: error %d", name, code);
    error(err, error: code)
  end;
  code
end method check-result;

define function ensure-no-qd-error (name :: <string>) => ()
  let error = QDError();
  unless (error = $noErr)
    let err = format-to-string("%s: error %d", name, error);
    error(err, error: error)
  end
end function ensure-no-qd-error;

define function ensure-no-resource-error (name :: <string>) => ()
  let error = ResError();
  unless (error = $noErr)
    let err = format-to-string("%s: error %d", name, error);
    error(err, error: error)
  end
end function ensure-no-resource-error;

define function ensure-no-memory-error (name :: <string>) => ()
  let error = MemError();
  unless (error = $noErr)
    let err = format-to-string("%s: error %d", name, error);
    error(err, error: error)
  end
end function ensure-no-memory-error;

define function check-new-resource-handle(name :: <string>, handle :: <Handle>) => ()
	check-result(name, handle);
	ensure-no-resource-error(name);
end function check-new-resource-handle;

define function check-new-memory-handle(name :: <string>, handle :: <Handle>) => ()
	check-result(name, handle);
	ensure-no-resource-error(name);
end function check-new-memory-handle;

define function ensure-rect-valid(name :: <string>, r :: <Rect*>)
 => (result :: <boolean>)
  unless((r.left-value <= r.right-value) & (r.top-value <= r.bottom-value))
    error(format-to-string("Invalid <Rect*> %s ltrb: %d %d %d %d",
                           name, r.left-value, r.top-value, 
                           r.right-value, r.bottom-value));
  end unless;
end function ensure-rect-valid;

////////////////////////////////////////////////////////////////////////////////
// Macros for unimplemented/ignorable methods and other notes
////////////////////////////////////////////////////////////////////////////////

define function not-yet-implemented
    (format-message :: <string>, #rest format-args)
  apply(error, 
	concatenate(format-message, " not yet implemented!"),
	format-args)
end function not-yet-implemented;

define function ignoring
    (format-message :: <string>, #rest format-args)
  apply(debug-message, 
	concatenate("Ignoring ", format-message),
	format-args)
end function ignoring;

////////////////////////////////////////////////////////////////////////////////
// Resource Management
////////////////////////////////////////////////////////////////////////////////

define macro with-pascal-string
  { with-pascal-string (?:name = ?:expression) ?:body end }
    => {	let ?name :: <pascal-string> = as(<pascal-string>, ?expression);
          ?body }
end macro with-pascal-string;

define macro with-c-string
  { with-c-string (?:name = ?:expression) ?:body end }
    => {	let ?name :: <c-string> = as(<c-string>, ?expression);
          ?body }
end macro with-c-string;

define method as 
    (cls == <CFStringRef>, str :: <string>)
 => (result :: <CFStringRef>)
  // XXX - This could be more efficient if we could get byte-string buffers in a common way
  with-c-string(cs = str)
    CFStringCreateWithBytes(CFAllocatorGetDefault(), cs, cs.size, 
      $kCFStringEncodingASCII, #f);
  end;
end method as;

define macro with-CFString
  { with-CFString (?:name = ?:expression) ?:body end }
    => {	let ?name :: <CFStringRef> = as(<CFStringRef>, ?expression);
          ?body; 
          CFRelease(?name);}
end macro with-CFString;

define macro with-window-port
  { with-window-port (?window:expression) ?:body end }
    => { 	let old-port :: false-or(<CGrafPtr>) = #f;
          block()
            old-port := GetPort();
            SetPortWindowPort(?window);
    				?body
          cleanup
            SetPort(old-port); 
	 				end;}
end macro with-window-port;

define macro with-GWorld-port
  { with-GWorld-port (?gworld:expression) ?:body end }
    => { 	let (old-port, old-device) = GetCurrentGWorld()
          block()
            let new-world :: <GWorldPtr> = ?gworld;
            let newdevice :: <GDHandle> = GetGWorldDevice(new-world);
            SetGWorld(new-world, new-device);
    				?body
          cleanup
            SetGWorld(old-port, old-device); 
	 				end;}
end macro with-GWorld-port;

define macro lock-Handle
  { lock-Handle (?the-handle:expression) ?body:body end }
    => {	let handle :: <handle> = ?the-handle;
          let handle-state :: <integer> = HGetState(handle);
          block()
    				HLock(handle);
	   				?body  
	 				cleanup
	 					HSetState(handle, handle-state);
	 				end }
end macro lock-Handle;

define macro with-Handle
  { with-Handle (?handle:name, #key ?size:expression) ?body:body end }
    => {  let ?handle :: <Handle> := NewHandle(?size);
          block()
            check-new-memory-handle("Handle from with-Handle macro", ?handle);
	   				?body  
	 				cleanup
	 					KillHandle(?handle);
	 				end }
end macro with-Handle;

define macro with-Resource
  { with-Resource (?resource-handle:name, #key ?kind:expression, ?id:expression) ?body:body end }
    => { 	let ?resource-handle :: <Handle> := GetResource(?kind, ?id);
            block()
            check-new-resource-handle("Resource Handle from with-Resource macro", ?resource-handle);
	   				?body  
	 				cleanup
	 					ReleaseResource(?resource-handle);
	 				end }
end macro with-Resource;

define macro lock-PixmapHandle
  { lock-PixmapHandle (?PixmapHandle:expression) ?body:body end }
    => { 	let pixmap :: <PixMapHandle> = ?PixmapHandle;
          block()
            LockPixels(pixmap);
	   				?body  
	 				cleanup
	 					UnlockPixels(pixmap);
	 				end }
end macro lock-PixmapHandle;

/////////////////////////////////////////////////////////////////////////////////// Window size functions
////////////////////////////////////////////////////////////////////////////////

// Note that the return values are in _screen_ coordinates
define sealed method get-window-edges
    (reference :: <WindowRef>)
 => (left :: <integer>, top :: <integer>,
     right :: <integer>, bottom :: <integer>)   
  let rect :: <Rect*> = GetWindowPortBounds(reference);
  values(rect.left-value, rect.top-value,
    rect.right-value, rect.bottom-value);
end method get-window-edges;

define sealed method set-window-edges
    (reference :: <WindowRef>, 
     left :: <integer>, top :: <integer>,
     right :: <integer>, bottom :: <integer>) 
=> ()
  let menu-bar-height :: <integer> = GetMBarHeight();
  MoveWindowStructure(reference, left, top + menu-bar-height);
  SizeWindow(reference, right - left, bottom - top, #f);
end method set-window-edges;

define method get-window-size
    (reference :: <WindowRef>)
 => (width :: <integer>, height :: <integer>)
  let (left, top, right, bottom) = get-window-edges(reference);
  values(right - left, bottom - top)
end method get-window-size;

////////////////////////////////////////////////////////////////////////////////
// D2C COMPATIBILITY
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
// c-ffi compatibility
/// From Peter Housel's gtk ffi compatibility in gtk.dylan
////////////////////////////////////////////////////////////////////////////////

define constant <C-void*> :: <type> = <statically-typed-pointer>;

define constant <C-pointer> = <statically-typed-pointer>;

define method pointer-address
    (c-pointer :: <statically-typed-pointer>)
 => (address :: <integer>)
  as(<integer>, c-pointer.raw-value);
end method;

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

// My copy&paste antipattern implementation for shorts

define functional class <C-short*> (<statically-typed-pointer>) end;

define sealed method pointer-value
    (c-pointer :: <C-short*>, #key index = 0)
 => (result :: <object>);
  pointer-deref(short:, c-pointer.raw-value,
                index * c-expr(int:, "sizeof(short)"));
end method;

define sealed method pointer-value-setter
    (value :: <object>, c-pointer :: <C-short*>, #key index = 0)
 => (value :: <object>);
  pointer-deref(short:, c-pointer.raw-value,
                index * c-expr(int:, "sizeof(short)")) := value;
end method;

define sealed method content-size
    (class == <C-short*>)
 => (size :: <integer>);
  c-expr(int:, "sizeof(short)");
end method;


// And from Erik Kidd's C-FFI implementation

define sealed inline method C-char-at
    (ptr :: <C-pointer>,
     #key byte-index :: <integer> = 0, scaled-index :: <integer> = 0)
 => (result :: <character>);
  pointer-deref(char:, ptr.pointer-address, byte-index + scaled-index);
end method C-char-at;

define sealed inline method C-char-at-setter
    (new :: <character>, ptr :: <C-pointer>,
     #key byte-index :: <integer> = 0, scaled-index :: <integer> = 0)
 => (result :: <character>);
  pointer-deref(char:, ptr.pointer-address, byte-index + scaled-index) := new;
end method C-char-at-setter;
