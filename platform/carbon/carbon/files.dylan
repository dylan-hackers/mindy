module: carbon

/*
	c-includes
*/

c-include( "Carbon.h" );
  
  
/*
	Constants
*/                                 

define constant $fsCurPerm                   = #x00;                         /* open access permissions in ioPermssn */
define constant $fsRdPerm                    = #x01;
define constant $fsWrPerm                    = #x02;
define constant $fsRdWrPerm                  = #x03;
define constant $fsRdWrShPerm                = #x04;
define constant $fsRdDenyPerm                = #x10;                         /* for use with OpenDeny and OpenRFDeny */
define constant $fsWrDenyPerm                = #x20;
  
  
/*
	<FSSpec*>
*/                                 

define functional class <FSSpec*> (<statically-typed-pointer>)
end class <FSSpec*>;
  
  
/*
	content-size <FSSpec*>
*/                                 

define method content-size( class == <FSSpec*> )
=> ( result :: <integer> )
	70; 	// short, long, Str63
end method content-size;
  
  
/*
	initialize <FSSpec*>
*/                                 

define method initialize( spec :: <FSSpec*>, #key parID :: <integer> = 0, 
							vRefNum :: <integer> = 0, name :: <pascal-string> = make( <pascal-string> ) )
=> ( result :: <FSSpec*> )

	spec.parID-value := parID;
	spec.vRefNum-value := vRefNum;
	spec.name-value := name;

	spec;

end method initialize;
  
  
/*
	vRefNum
*/                                 

define method vRefNum-value( spec :: <FSSpec*> )
=> ( result :: <integer> )

	signed-short-at( spec, offset: 0 );
	
end method vRefNum-value;
  
  
/*
	vRefNum-setter
*/                                 

define method vRefNum-value-setter( num :: <integer>, spec :: <FSSpec*> )
=> ()

	signed-short-at( spec, offset: 0 ) := num;
	
end method vRefNum-value-setter;
  
  
/*
	parID
*/                                 

define method parID-value( spec :: <FSSpec*> )
=> ( result :: <integer> )

	signed-long-at( spec, offset: 2 );
	
end method parID-value;
  
  
/*
	parID-setter
*/                                 

define method parID-value-setter( id :: <integer>, spec :: <FSSpec*> )
=> ( result :: <integer> )

	signed-long-at( spec, offset: 2 ) := id;
	
end method parID-value-setter;
  
  
/*
	name
*/                                 

define method name-value( spec :: <FSSpec*> )
=> ( result :: <string> )

	let sz :: <integer> = unsigned-byte-at( spec.name-value, offset: 0 );
	let result :: <byte-string> = make( <byte-string>, size: sz );
	
	for( i from 0 below sz)
		result[i] := as( <character>, unsigned-byte-at( spec.name-value, offset: i + 1 ) );
	end for;
	
	result;

end method name-value;
  
  
/*
	name-value-setter
*/                                 

define method name-value-setter(str :: <string>, spec :: <FSSpec*> )
=> ()

	let sz =  case 
				str.size < 63	=>	str.size;
				otherwise => 63;
			end case;
	for ( i from 1 to sz )
		unsigned-byte-at( spec.name-value, offset: i ) := as( <integer>, str[i - 1] );
	end for;
	unsigned-byte-at( spec.name-value, offset: 0 ) := sz;


end method name-value-setter;
  
/*
	<FSRef>
	Minimal support for now....
*/                                 

define functional class <FSRef> (<statically-typed-pointer>)
end class <FSRef>;
  
  
/*
	content-size <FSRef>
*/                                 

define method content-size( class == <FSRef> )
=> ( result :: <integer> )
	80; 	// private
end method content-size;
  
  
/*
	FSClose
*/                                 

define method FSClose( ref :: <integer> )
=> ( result :: <OSErr> )

	let result = call-out( "FSClose", short:, short: ref );
	
	as( <OSErr>, result );

end method FSClose;
  
  
/*
	FSRead
*/                                 

define method FSRead( refNum :: <integer>, count :: <integer>, buffer :: <machine-pointer> )
=> ( result :: <OSErr>, count :: <integer> )

	let countPtr = make( <Handle> );
	signed-long-at( countPtr ) := count;

	let result = call-out( "FSRead", short:, short: refNum, ptr: countPtr.raw-value, ptr: buffer.raw-value );
	
	values( as( <OSErr>, result ), signed-long-at( countPtr ) );

end method FSRead;
  
  
/*
	FSWrite
*/                                 

define method FSWrite( refNum :: <integer>, count :: <integer>, buffer :: <machine-pointer> )
=> ( result :: <OSErr>, count :: <integer> )

	let countPtr = make( <Handle> );
	signed-long-at( countPtr ) := count;

	let result = call-out( "FSWrite", short:, short: refNum, ptr: countPtr.raw-value, ptr: buffer.raw-value );
	
	values( as( <OSErr>, result ), signed-long-at( countPtr ) );

end method FSWrite;
  
  
/*
	Allocate
*/                                 

define method Allocate( refNum :: <integer>, count :: <integer> )
=> ( result :: <OSErr>, count :: <integer> )

	let countPtr = make( <Handle> );
	signed-long-at( countPtr ) := count;

	let result = call-out( "Allocate", short:, short: refNum, ptr: countPtr.raw-value );
	
	values( as( <OSErr>, result ), signed-long-at( countPtr ) );

end method Allocate;
  
  
/*
	GetEOF
*/                                 

define method GetEOF( refNum :: <integer> )
=> ( result :: <OSErr>, logEOF :: <integer> )

	let eofPtr = make( <Handle> );

	let result = call-out( "GetEOF", short:, short: refNum, ptr: eofPtr.raw-value );
	
	values( as( <OSErr>, result ), signed-long-at( eofPtr ) );

end method GetEOF;
  
  
/*
	SetEOF
*/                                 

define method SetEOF( refNum :: <integer>, logEOF :: <integer>)
=> ( result :: <OSErr> )

	let result = call-out( "SetEOF", short:, short: refNum, short: logEOF );
	
	as( <OSErr>, result );

end method SetEOF;
  
  
/*
	GetFPos
*/                                 

define method GetFPos( refNum :: <integer> )
=> ( result :: <OSErr>, filePos :: <integer> )

	let posPtr = make( <Handle> );

	let result = call-out( "GetFPos", short:, short: refNum, ptr: posPtr.raw-value );
	
	values( as( <OSErr>, result ), signed-long-at( posPtr ) );

end method GetFPos;
  
  
/*
	SetFPos
*/                                 

define method SetFPos( refNum :: <integer>, posMode :: <integer>, posOff :: <integer> )
=> ( result :: <OSErr> )

	let result = call-out( "SetFPos", short:, short: refNum, short: posMode, long: posOff );
	
	as( <OSErr>, result );

end method SetFPos;
  
  
/*
	GetVRefNum
*/                                 

define method GetVRefNum( fileRefNum :: <integer> )
=> ( result :: <OSErr>, vRefNum :: <integer> )

	let numPtr = make( <Handle> );

	let result = call-out( "Allocate", short:, short: filerefNum,  ptr: numPtr.raw-value );
	
	values( as( <OSErr>, result ), signed-long-at( numPtr ) );

end method GetVRefNum; 
                                 
/*EXTERN_API( OSErr )
FSMakeFSSpec                    (short                  vRefNum,
                                 long                   dirID,
                                 ConstStr255Param       fileName,
                                 FSSpec *               spec)                               TWOWORDINLINE(0x7001, 0xAA52);
*/
  
  
/*
	FSpOpenDF
*/                                 

define method FSpOpenDF( spec :: <FSSpec*>, permission :: <integer> )
=> ( result :: <OSErr>, refNum :: <integer> )

	let refPtr = make( <Handle> );

	let result = call-out( "FSpOpenDF", short:, ptr: spec.raw-value, char: permission, ptr: refPtr.raw-value );
	
	values( as( <OSErr>, result ), signed-long-at( refPtr ) );

end method FSpOpenDF;
  
  
/*
	FSpOpenRF
*/                                 

define method FSpOpenRF( spec :: <FSSpec*>, permission :: <integer> )
=> ( result :: <OSErr>, refNum :: <integer> )

	let refPtr = make( <Handle> );

	let result = call-out( "FSpOpenRF", short:, ptr: spec.raw-value, char: permission, ptr: refPtr.raw-value );
	
	values( as( <OSErr>, result ), signed-long-at( refPtr ) );

end method FSpOpenRF;
  
  
/*
	FSpCreate
*/                                 

define method FSpCreate( spec :: <FSSpec*>, creator :: <OSType>, fileType :: <OSType>, scriptTag :: <integer> )
=> ( result :: <OSErr>, vRefNum :: <integer> )

	let dirPtr = make( <Handle> );

	let result = call-out( "FSpCreate", short:, ptr: spec.raw-value, int: creator, int: filetype, long: scriptTag  );
	
	values( as( <OSErr>, result ), signed-long-at( dirPtr ) );

end method FSpCreate;
  
  
/*
	FSpDirCreate
*/                                 

define method FSpDirCreate( spec :: <FSSpec*>, scriptTag :: <integer> )
=> ( result :: <OSErr>, vRefNum :: <integer> )

	let dirPtr = make( <Handle> );

	let result = call-out( "FSpDirCreate", short:, ptr: spec.raw-value, long: scriptTag, ptr: dirPtr.raw-value );
	
	values( as( <OSErr>, result ), signed-long-at( dirPtr ) );

end method FSpDirCreate;
  
  
/*
	FSpDelete
*/                                 

define method FSpDelete( spec :: <FSSpec*> )
=> ( result :: <OSErr> )

	let result = call-out( "FSpDelete", short:, ptr: spec.raw-value );
	
	as( <OSErr>, result );

end method FSpDelete;
  
  
/*
	FSpRename
*/                                 

define method FSpRename( spec :: <FSSpec*>, newName :: <pascal-string> )
=> ( result :: <OSErr> )

	let result = call-out( "FSpRename", short:, ptr: spec.raw-value, ptr: newName.raw-value );
	
	as( <OSErr>, result );

end method FSpRename;
  
  
/*
	FSpCatMove
*/                                 

define method FSpCatMove( fileRefNum :: <integer>, source :: <FSSpec*>, dest :: <FSSpec*> )
=> ( result :: <OSErr> )

	let result = call-out( "FSpCatMove", short:, ptr: source.raw-value, ptr: dest.raw-value );
	
	as( <OSErr>, result );

end method FSpCatMove;
  
  
/*
	FSpExchangeFiles
*/                                 

define method FSpExchangeFiles( fileRefNum :: <integer>, source :: <FSSpec*>, dest :: <FSSpec*> )
=> ( result :: <OSErr> )

	let result = call-out( "FSpExchangeFiles", short:, ptr: source.raw-value, ptr: dest.raw-value );
	
	as( <OSErr>, result );

end method FSpExchangeFiles;
  
  
/*
	FSpMakeFSRef
*/                                 

define method FSpMakeFSRef( fileRefNum :: <integer>, source :: <FSSpec*> )
=> ( result :: <OSErr>, newRef :: <FSRef> )

	let ref = make( <FSRef> );

	let result = call-out( "FSpMakeFSRef", short:, ptr: source.raw-value, ptr: ref.raw-value );
	
	values( as( <OSErr>, result ), ref );

end method FSpMakeFSRef; 
