module: carbon

c-include("Carbon.h");

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

// Fonts

define constant $normal :: <integer> = c-expr(int: "normal");
define constant $bold :: <integer> = c-expr(int: "bold");
define constant $italic :: <integer> = c-expr(int: "italic");
define constant $underline :: <integer> = c-expr(int: "underline");
define constant $outline :: <integer> = c-expr(int: "outline");
define constant $shadow :: <integer> = c-expr(int: "shadow");
define constant $condense :: <integer> = c-expr(int: "condense");
define constant $extend :: <integer> = c-expr(int: "extend");


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
