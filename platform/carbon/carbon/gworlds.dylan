module: carbon


/*
	c-includes
*/

c-include( "Carbon.h" );


/*
	classes
*/

define functional class <GDHandle> (<Handle>)
end class <GDHandle>;

define functional class <GWorldPtr> (<Ptr>)
end class <GWorldPtr>;


/*
	Methods
*/


/*
	NewGWorld
*/

define method NewGWorld( PixelDepth :: <integer>, bounds-rect :: <Rect*>, cTable :: <Handle>, /* can be NULL */
                         aGDevice :: <GDHandle>, /* can be NULL */ flags :: <integer> /* GWorldFlags */ )
=> ( offscreenGWorld :: <GWorldPtr>, err :: <OSErr> )

	let worldPtr = make( <Handle> );
	
	let result = call-out(	"NewGWorld", int:, ptr: worldPtr.raw-value, short: PixelDepth,
							ptr: bounds-rect.raw-value, ptr: cTable.raw-value, 
							ptr: aGDevice.raw-value, int: flags );

	values( pointer-at( worldPtr, offset: 0, class: <GWorldPtr> ), as( <OSErr>, result ) );

end method NewGWorld;


/*
	DisposeGWorld
*/

define method DisposeGWorld( offscreenGWorld :: <GWorldPtr> )
=> ()

	let ptr = call-out( "DisposeGWorld", ptr:, ptr: offscreenGWorld.raw-value );
	
	values();

end method DisposeGWorld;


/*
	GetGWorld
*/

define method GetGWorld()
=> ( port :: <CGrafPtr>, gdh :: <GDHandle> )  

	let port = make( <Handle> );
	let gdh = make( <Handle> );

	call-out( "GetGWorld", void:, ptr: port.raw-value, ptr: gdh.raw-value );
	
	values( pointer-at( port, offset: 0, class: <CGrafPtr> ),
			pointer-at( gdh, offset: 0, class: <GDHandle> ) );

end method GetGWorld;


/*
	SetGWorld
*/

define method SetGWorld( port :: <CGrafPtr>, gdh :: <GDHandle> )    
=> ()

	call-out( "SetGWorld", void:, ptr: port.raw-value, ptr: gdh.raw-value );

	values();

end method SetGWorld;


/*
	GetGWorldPixMap
*/

define method GetGWorldPixMap( offscreenGWorld :: <GWorldPtr> )
=> ( result :: <PixMapHandle> )

	let result = call-out( "GetGWorldPixMap", ptr:, ptr: offscreenGWorld.raw-value );

	make( <PixMapHandle>, pointer: result );

end method GetGWorldPixMap;


/*
	LockPixels
*/

define method LockPixels( pm :: <PixMapHandle> )
=> ( result :: <boolean> )

	let result = call-out( "LockPixels", unsigned-char:, ptr: pm.raw-value );

	if( result = 1 )
		#t
	else
		#f
	end if;

end method LockPixels;


/*
	UnlockPixels	
*/

define method UnlockPixels( pm :: <PixMapHandle> ) 
=> ()

	call-out( "UnlockPixels", void:, ptr: pm.raw-value );

	values();

end method UnlockPixels;


/*
	GetGWorldDevice
*/

define method GetGWorldDevice( offscreenGWorld /*:: type-union(<GWorldPtr>, <CGrafPtr>, <WindowRef>)*/ )
=> ( result :: <GDHandle> )

	let ptr = call-out( "GetGWorldDevice", ptr:, ptr: offscreenGWorld.raw-value );
	
	make( <GDHandle>, pointer: ptr );

end method GetGWorldDevice;


/*
	PixMap32Bit
*/

define method PixMap32Bit( pmHandle :: <PixMapHandle> )
=> ( result :: <boolean> )

	let result = call-out( "PixMap32Bit", unsigned-char:, ptr: pmHandle.raw-value );
	
	if( result = 1 )
		#t
	else
		#f
	end if;

end method PixMap32Bit;
