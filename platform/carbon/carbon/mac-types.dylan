module: mac-types


/*
	Basic types
*/

define constant <UInt8> = <integer>;

define constant <SInt8> = <integer>;

define constant <UInt16> = <integer>;

define constant <SInt16> = <integer>;

define constant <UInt32> = <integer>;

define constant <SInt32> = <integer>;

define constant <OSErr> = <SInt16>;

define constant <OSStatus> = <SInt32>;

define constant <FourCharCode> = <integer>;

define constant <OSType> = <FourCharCode>;

define constant <ResType> = <FourCharCode>;

/*
        noErr
*/

define constant $noErr :: <OSErr> = 0;


/*
	os-type <string>
	converts a 4 char string to an OSType
*/

define constant os-type = method (typestr :: <string>)
=> (result :: <OSType>);
	let type = as(<OSType>, as(<integer>, typestr[0]));
	for (i from 1 below 4)
		type := type * 256 + as(<integer>, typestr[i]);
	finally
		type;
	end for;
end method;


/*
	as <OSType
*/

define method as( cls == <OSType>, string :: <string> )
=> ( result :: <OSType> )

	os-type( string );

end method as;


/*
	Debugger
	Breaks into the low-level debugger.
*/

define method Debugger()
=> ()
	call-out( "Debugger", void: );
	values();
end method Debugger;
	
	
/* 
	DebugStr
	Logs or executes a string in the low-level debugger.
*/											

define method DebugStr( message :: <pascal-string> )
 => ()
  call-out( "DebugStr", void: , ptr: message.raw-value );
  values();
end method DebugStr;
