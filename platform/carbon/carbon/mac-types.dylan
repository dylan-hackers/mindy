module: mac-types


/*
	OSErr.
*/

define constant <OSErr> = <integer>;


/*
    OSStatus
*/

define constant <OSStatus> = <integer>;


/*
	OSType.
*/

define constant <OSType> = <integer>;


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
