module: carbon

c-include("Carbon.h");

// Resource Manager.

define method ResError()
=> ( result :: <OSErr> )
	as( <OSErr>, call-out( "ResError", int: ) );
end method ResError;

define method GetResource( type :: <OSType>, id :: <integer> )
=> ( result :: <Handle> )
	call-out( "GetResource", ptr:, int: type, short: id );
end method GetResource;


define method ReleaseResource( res :: <Handle> )
	call-out( "ReleaseResource", void:, ptr: res );
end method ReleaseResource;

