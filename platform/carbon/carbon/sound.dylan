module: carbon

/*
	Sound Manager
*/


/*
	includes
*/

c-include( "Carbon.h" );


/*
	SysBeep
*/

define method SysBeep( pointless :: <integer> )
=> ()
	call-out( "SysBeep", void:, int: pointless );
	values();
end method SysBeep;


/*
	<SndChannel>
*/
/*
define functional class <SndChannel> ( <statically-typed-pointer> ) 
end class <SndChannel>;
*/

/*
	content-size
	The size of object a <SndChannel> contains
*/
/*
define method content-size( cls == <SndChannel> )
=>( result :: <integer> )
	c-expr( int: "sizeof(SndChannel)" );
end method content-size;
*/

/*
	SndPlay
*/
/*
define method SndPlay( channel :: <SndChannel>, sound :: <Handle>, synchronous :: <boolean> )
=> ( result :: <OSErr> )
	let synch = if( synchronous ) 1 else 0  end if;
	call-out( "SndPlay", int:, ptr: channel.raw-value, ptr: sound.raw-value, unsigned-char: synch );
end method SndPlay;
*/
