module: carbon

c-include( "Carbon.h" );

// CFTypeRef
// Polymorphic base type for all CF types

define functional class <CFTypeRef>
  (<statically-typed-pointer>)
end class <CFTypeRef>;  
  
define method CFRetain
    (cf :: <CFTypeRef>)  
 => (cf :: <CFTypeRef>)
  call-out("CFRetain", ptr:, ptr: cf.raw-value);
  cf;
end method CFRetain;
  
define method CFRelease
    (cf :: <CFTypeRef>)  
 => (cf :: <CFTypeRef>)
  call-out("CFRelease", ptr:, ptr: cf.raw-value);
  cf;
end method CFRelease;

// CFAllocator
// Memory management

define functional class <CFAllocatorRef>
  (<CFTypeRef>)
end class <CFAllocatorRef>;  

define constant $kCFAllocatorNull :: <CFAllocatorRef> = 
  make(<CFAllocatorRef>, pointer: c-expr(ptr: "kCFAllocatorNull"));
  
define method CFAllocatorGetDefault
    () => (default :: <CFAllocatorRef>)
  make(<CFAllocatorRef>, pointer: call-out("CFAllocatorGetDefault", ptr:));
end method CFAllocatorGetDefault;

// CFString
// Immutable Unicode Strings

define constant $kCFStringEncodingMacRoman :: <integer> = c-expr(int: "kCFStringEncodingMacRoman");
define constant $kCFStringEncodingASCII :: <integer> = c-expr(int: "kCFStringEncodingASCII");
define constant $kCFStringEncodingUnicode :: <integer> = c-expr(int: "kCFStringEncodingUnicode");
define constant $kCFStringEncodingUTF8 :: <integer> = c-expr(int: "kCFStringEncodingUnicode");

define functional class <CFStringRef>
  (<CFTypeRef>)
end class <CFStringRef>; 

define method CFStringCreateWithBytes
    (alloc :: <CFAllocatorRef>, bytes, numBytes :: <integer>, encoding :: <integer>, 
     isExternalRepresentation :: <boolean>)
 => (cfs :: <CFStringRef>)
  let is-external :: <integer> = if(isExternalRepresentation) 1 else 0 end if;
  make(<CFStringRef>, pointer: call-out("CFStringCreateWithBytes", ptr:, ptr: alloc.raw-value,
    ptr: bytes.raw-value, int: numBytes, int: encoding, int: is-external));
end method CFStringCreateWithBytes;

define method CFStringCreateWithCStringNoCopy
    (alloc :: <CFAllocatorRef>, bytes, numBytes :: <integer>, encoding :: <integer>, 
     dealloc :: <CFAllocatorRef>)
 => (cfs :: <CFStringRef>)
  make(<CFStringRef>, pointer: call-out("CFStringCreateWithCStringNoCopy", ptr:, ptr: alloc.raw-value,
    ptr: bytes.raw-value, int: numBytes, int: encoding, ptr: dealloc));
end method CFStringCreateWithCStringNoCopy;