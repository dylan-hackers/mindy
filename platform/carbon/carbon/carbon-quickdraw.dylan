module: quickdraw

/*
	Includes.
*/

c-include("Carbon.h");


/*
    MacOS X Buffering Routines
*/

/*
    QDIsPortBuffered
*/

define method QDIsPortBuffered( port :: <CGrafPtr> )
=> ( result :: <boolean> )
   let result = call-out( "QDIsPortBuffered", int:, ptr: port.raw-value );
    if( result = 0 )
        #f;
    else
        #t;
    end if;
end method QDIsPortBuffered;

/*
    QDIsPortBufferDirty
*/

define method QDIsPortBufferDirty( port :: <CGrafPtr> )
=> ( result :: <boolean> )
   let result = call-out( "QDIsPortBufferDirty", int:, ptr: port.raw-value );
    if( result = 0 )
        #f;
    else
        #t;
    end if;
end method QDIsPortBufferDirty;

/*
    QDFlushPortBuffer
*/

define method QDFlushPortBuffer( port :: <CGrafPtr>, region :: <RgnHandle> )
=> ()
   call-out( "QDFlushPortBuffer", void:, ptr: port.raw-value, ptr: region.raw-value );
   values();
end method QDFlushPortBuffer;

/*
    QDGetDirtyRegion
*/

define method QDGetDirtyRegion( port :: <CGrafPtr>, region :: <RgnHandle> )
=> ( result :: <OSStatus> )
   let status :: <integer> = call-out( "QDGetDirtyRegion", void:, ptr: port.raw-value, ptr: region.raw-value );
   as( <OSStatus>, status );
end method QDGetDirtyRegion;

/*
    QDSetDirtyRegion
*/

define method QDSetDirtyRegion( port :: <CGrafPtr>, region :: <RgnHandle> )
=> ( result :: <OSStatus> )
   let status :: <integer> = call-out( "QDSetDirtyRegion", void:, ptr: port.raw-value, ptr: region.raw-value );
   as( <OSStatus>, status );
end method QDSetDirtyRegion;
