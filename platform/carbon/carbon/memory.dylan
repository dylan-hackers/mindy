module: carbon

// Memory Manager Routines.

// Include The Mac's Memory manager header

c-include("Carbon.h");

/*
	<Ptr>
	A Macintosh Memory Manager Ptr.
*/

define functional class <Ptr> (<statically-typed-pointer>) 
end class <Ptr>;


/*
	make <Ptr>
*/

define sealed domain make( singleton(<Ptr>) );


/*
	pointer-value <Ptr>
	Returns the value of a memory location accessed through the <Ptr>
*/

define method pointer-value( ptr :: <Ptr>, #key index = 0 )
 => (result :: <integer>)
  signed-byte-at( ptr, offset: index * 1 );
end method pointer-value;


/*
	pointer-value-setter <Ptr>
	Sets the value of a memory location accessed through the <Ptr>
*/
 
define method pointer-value-setter( value :: <integer>, ptr :: <Ptr>, #key index = 0 )
 => (result :: <integer>);
  signed-byte-at(ptr, offset: index * 1) := value;
  value;
end method pointer-value-setter;


/*
	content-size subclass(<Ptr>)
	Returns the size of the content accessed through a <Ptr>
*/

define method content-size (value :: subclass(<Ptr>))
=> (result :: <integer>);
  1;
end method content-size;


/*
	$nil
	$NULL
	Location Zero in memory.
	The value of an invalid <Ptr>
	Used as a flag value by some routines.
*/

define constant $nil :: <Ptr>	= as(<Ptr>, 0);
define constant $NULL :: <Ptr>	= $nil;


/*
	NewPtr
	The Macintosh Memory Manager's Pointer creation function.
	Pointers allocated using this must be deallocated using DisposePtr.
*/

define method NewPtr( size :: <integer> )
=> ( result :: <Ptr> )
	let result = call-out( "NewPtr", ptr:, int: size );
	make( <Ptr>, pointer: result );
end method NewPtr;


/*
	DisposePtr <Ptr>
	The Macintosh Memory Manager's pointer deallocation function.
*/

define method DisposePtr( ptr :: <Ptr> )
=> ()
	call-out( "DisposePtr", void:, ptr: ptr.raw-value );	
end method DisposePtr;

/*
define method finalize( pointer :: <Ptr> ) => ();
  DisposePtr( pointer );
end method finalize;
*/


/*
	Handles
*/


/*
	<Handle>
	A macintosh Memory Manager Handle or Resource Handle.
*/

define functional class <Handle> ( <Ptr> ) 
end;

/*
	pointer-value <Handle>
	Gets the pointer that a handle contains.
	This will need dereferencing to get the value the Handle points to.
*/

define method pointer-value( ptr :: <Handle>, #key index = 0 )
 => ( result :: <Ptr> )
  pointer-at(ptr, offset: index * 4, class: <Ptr>);
end method pointer-value;



define method pointer-value-setter( value :: <Ptr>, ptr :: <Handle>, #key index = 0 )
 => (result :: <Ptr>)
  pointer-at(ptr, offset: index * 4, class: <Ptr> ) := value;
  value;
end method pointer-value-setter;


/*
	content-size <Handle>
	Returns the size of the data a Handle contains.
*/

define method content-size (value :: subclass(<Handle>))
 => (result :: <integer>)
  4;
end method content-size;


/*
	make <Handle>
*/

define sealed domain make (singleton(<Handle>));


/*
	NewHandle
	Returns a newly allocated Macintosh Memory Manager Handle.
*/

define method NewHandle( size :: <integer> )
=> ( result :: <Handle> )
	let ptr = call-out( "NewHandle", ptr:, int: size );
	make( <Handle>, pointer: ptr );
end method NewHandle;


/*
	NewHandle
	Deallocates a Macintosh Memory Manager Handle.
*/

define method DisposeHandle( handle :: <Handle> )
=> ()
	call-out( "DisposeHandle", void:, ptr: handle.raw-value );
end method DisposeHandle;

/*
define method finalize( handle :: <Handle> )
=> ()
  DisposeHandle( handle );
end method finalize;
*/

// MemError

define method MemError()
=> ( result :: <OSErr> )
	as( <OSErr>, call-out( "MemError", int: ) );
end method MemError;