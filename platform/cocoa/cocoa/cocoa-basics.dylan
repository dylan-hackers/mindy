module: cocoa-wrappers

c-expr( void: "#undef true" );
c-expr( void: "#undef false" );


c-system-include( "AppKit/AppKit.h" );

// Support

// Move these to the top of cocoa.dylan

// short?

define method as( cls == <boolean>, obj :: <raw-pointer> ) => ( res :: <boolean> )
	c-expr( void: "#define PTR2BOOL( a ) *(bool*)&a" );
	if( call-out( "PTR2BOOL", char:, ptr: obj ) == 0 )
		#f;
	else
		#t;
	end if;
end method as;

define method as( cls == <character>, obj :: <raw-pointer> ) => ( res :: <character> )
	c-expr( void: "#define PTR2CHAR( a ) *(char*)&a" );
	call-out( "PTR2CHAR", float:, ptr: obj );
end method as;

define method as( cls == <float>, obj :: <raw-pointer> ) => ( res :: <float> )
	c-expr( void: "#define PTR2FLOAT( a ) *(float*)&a" );
	call-out( "PTR2FLOAT", float:, ptr: obj );
end method as;

define method as( cls == <double-float>, obj :: <raw-pointer> ) => ( res :: <double-float> )
	c-expr( void: "#define PTR2DOUBLE( a ) *(double*)&a" );
	call-out( "PTR2DOUBLE", double:, ptr: obj );
end method as;

define method as( cls == <SEL>, obj :: <raw-pointer> ) => ( res :: <SEL> )
	make( <SEL>, pointer: obj );
end method as;


// Utilities

// with-autorelease-pool

define macro with-autorelease-pool
  { with-autorelease () ?:body end }
    => {let pool :: <NSAutoreleasePool> = init( alloc( <NSAutoreleasePool> ) );
			?body;
		release( pool );}
end macro with-autorelease-pool;


// Typedefs


// Cocoa

define /*exported*/ constant <unichar> :: <class> = <integer>; 			// unsigned short

// statically-typed-pointer subcalss so we can element-count: it
define /*exported*/ constant <NSPointArray> :: <class> = <NSPoint>;	

// statically-typed-pointer subcalss so we can element-count: it
define /*exported*/ constant <NSRectArray> :: <class> = <NSRect>;	


// Mac Toolboc

define /*exported*/ constant <OSErr> :: <class> = <integer>;			// SInt16
define /*exported*/ constant <OSType> :: <class> = <integer>;			// FourCharCode
define /*exported*/ constant <DescType> :: <class> = <integer>;			// ResType
define /*exported*/ constant <AEReturnID> :: <class> = <integer>;		// SInt16
define /*exported*/ constant <AETransactionID> :: <class> = <integer>;	// SInt32
define /*exported*/ constant <AEEventClass> :: <class> = <integer>;		// FourCharCode
define /*exported*/ constant <AEEventID> :: <class> = <integer>;		// FourCharCode
define /*exported*/ constant <AEArrayType> :: <class> = <character>;	// SInt8
define /*exported*/ constant <AEKeyword> :: <class> = <integer>;		// FourCharCode
define /*exported*/ constant <AEDataStorage> :: <class> = <statically-typed-pointer>;	// Pointer to typedef of Ptr (so char**)


// Values


// Cocoa

// NSRequestUserAttentionType
// This is declared inside the interface and we can't handle that yet, so we declare it by hand

define /*exported*/ constant $NSCriticalRequest :: <integer> = 0;
define /*exported*/ constant $NSInformationalRequest :: <integer> = 10;
define /*exported*/ constant <NSRequestUserAttentionType> :: <type> = one-of( $NSCriticalRequest, $NSInformationalRequest );


// Structs


// Cocoa


// NSAffineTransformStruct

define /*exported*/ functional class <NSAffineTransformStruct> (<statically-typed-pointer>)
end;

define method content-size( cls == <NSAffineTransformStruct> )
=>( result :: <integer> )
	c-expr( int: "sizeof(NSAffineTransformStruct)" );
end method content-size;

define /*exported*/ method m11( object :: <NSAffineTransformStruct> )
=> ( m11 :: <float> )
	float-at( object, offset: 0 );
end;

define /*exported*/ method m11-setter( object :: <NSAffineTransformStruct>, value :: <float> )
=> ( m11 :: <float> )
	float-at( object, offset: 0 ) := value;
end;

define /*exported*/ method m12( object :: <NSAffineTransformStruct> )
=> ( m12 :: <float> )
	float-at( object, offset: 4 );
end;

define /*exported*/ method m12-setter( object :: <NSAffineTransformStruct>, value :: <float> )
=> ( m12 :: <float> )
	float-at( object, offset: 4 ) := value;
end;

define /*exported*/ method m21( object :: <NSAffineTransformStruct> )
=> ( m21 :: <float> )
	float-at( object, offset: 8 );
end;

define /*exported*/ method m21-setter( object :: <NSAffineTransformStruct>, value :: <float> )
=> ( m21 :: <float> )
	float-at( object, offset: 8 ) := value;
end;

define /*exported*/ method m22( object :: <NSAffineTransformStruct> )
=> ( m22 :: <float> )
	float-at( object, offset: 12 );
end;

define /*exported*/ method m22-setter( object :: <NSAffineTransformStruct>, value :: <float> )
=> ( m22 :: <float> )
	float-at( object, offset: 12 ) := value;
end;

define /*exported*/ method tX( object :: <NSAffineTransformStruct> )
=> ( tX :: <float> )
	float-at( object, offset: 16 );
end;

define /*exported*/ method tX-setter( object :: <NSAffineTransformStruct>, value :: <float> )
=> ( tX :: <float> )
	float-at( object, offset: 16 ) := value;
end;

define /*exported*/ method tY( object :: <NSAffineTransformStruct> )
=> ( tY :: <float> )
	float-at( object, offset: 20 );
end;

define /*exported*/ method tY-setter( object :: <NSAffineTransformStruct>, value :: <float> )
=> ( tY :: <float> )
	float-at( object, offset: 20 ) := value;
end;


// NSTypesetter

define functional /*exported*/ class <NSTypesetter> ( <statically-typed-pointer> ) end class; // Where is this even defined???

define method content-size( cls == <NSTypesetter> )
=>( result :: <integer> )
	c-expr( int: "sizeof(id)" );
end method content-size;


//NSSwappedFloat 
//NSSwappedDouble


// NSModalSession

define /*exported*/ functional class <NSModalSession> ( <statically-typed-pointer> ) end class; // Typedef'd pointer to private struct

define method content-size( cls == <NSModalSession> )
=>( result :: <integer> )
	c-expr( int: "sizeof(id)" );
end method content-size;


// NSDecimal
//TODO: Accessors

define /*exported*/ functional class <NSDecimal> (<statically-typed-pointer>)
end;

define method content-size( cls == <NSDecimal> )
=>( result :: <integer> )
	c-expr( int: "sizeof(NSDecimal)" );
end method content-size;


// NSPoint

define /*exported*/ functional class <NSPoint> (<statically-typed-pointer>)
end;

define method content-size( cls == <NSpoint> )
=>( result :: <integer> )
	c-expr( int: "sizeof(NSPoint)" );
end method content-size;

define /*exported*/ method NSMakePoint( x :: <float>, y :: <float> )
=> ( result :: <NSPoint> )
	let point :: <NSPoint> = make( <NSPoint> );
	float-at( point, offset: 0 ) := as( <single-float>, x );
	float-at( point, offset: 4 ) := as( <single-float>, y );
	point;
end method NSMakePoint;

define /*exported*/ method x( object :: <NSPoint> )
=> ( x :: <float> )
	float-at( object, offset: 0 );
end;

define /*exported*/ method x-setter( object :: <NSPoint>, value :: <float> )
=> ( x :: <float> )
	float-at( object, offset: 0 ) := value;
end;

define /*exported*/ method y( object :: <NSPoint> )
=> ( y :: <float> )
	float-at( object, offset: 4 );
end;

define /*exported*/ method y-setter( object :: <NSPoint>, value :: <float> )
=> ( y :: <float> )
	float-at( object, offset: 4 ) := value;
end;


// NSSize

define /*exported*/ functional class <NSSize> (<statically-typed-pointer>)
end;

define method content-size( cls == <NSSize> )
=>( result :: <integer> )
	c-expr( int: "sizeof(NSSize)" );
end method content-size;

define /*exported*/ method NSMakeSize( width :: <float>, height :: <float> )
=> ( result :: <NSSize> )
	let size :: <NSSize> = make( <NSSize> );
	float-at( size, offset: 0 ) := as( <single-float>, width );
	float-at( size, offset: 4 ) := as( <single-float>, height );
	size;
end method NSMakeSize;

define /*exported*/ method width( object :: <NSSize> )
=> ( width :: <float> )
	float-at( object, offset: 0 );
end;

define /*exported*/ method width-setter( object :: <NSSize>, value :: <float> )
=> ( width :: <float> )
	float-at( object, offset: 0 ) := value;
end;

define /*exported*/ method height( object :: <NSSize> )
=> ( height :: <float> )
	float-at( object, offset: 4 );
end;

define /*exported*/ method height-setter( object :: <NSSize>, value :: <float> )
=> ( height :: <float> )
	float-at( object, offset: 4 ) := value;
end;


// NSRect

define /*exported*/ functional class <NSRect> (<statically-typed-pointer>)
end;

define method content-size( cls == <NSRect> )
=>( result :: <integer> )
	c-expr( int: "sizeof(NSRect)" );
end method content-size;

define /*exported*/ method NSMakeRect( x :: <float>, y :: <float>, width :: <float>, height :: <float> )
=> ( result :: <NSRect> )
	let rect :: <NSRect> = make( <NSRect> );
	float-at( rect, offset: 0 ) := as( <single-float>, x );
	float-at( rect, offset: 4 ) := as( <single-float>, y );
	float-at( rect, offset: 8 ) := as( <single-float>, width );
	float-at( rect, offset: 12 ) := as( <single-float>, height );
	rect;
end method NSMakeRect;

define /*exported*/ method origin( object :: <NSRect> )
=> ( origin :: <NSPoint> )
	pointer-at( object, class: <NSPoint>, offset: 0 );
end;

define /*exported*/ method origin-setter( object :: <NSRect>, value :: <NSPoint> )
=> ( origin :: <NSPoint> )
	pointer-at( object, class: <NSPoint>, offset: 0 ) := value;
end;

// Cocoa has a "size" elsewhere
define /*exported*/ open generic size( object :: <object> )
=> ( size :: <object> );

define /*exported*/ method size( object :: <NSRect> )
=> ( size :: <NSSize> )
	pointer-at( object, class: <NSSize>, offset: c-expr( int: "sizeof(NSPoint)" ) );
end;

define /*exported*/ method size-setter( object :: <NSRect>, value :: <NSSize> )
=> ( size :: <NSSize> )
	pointer-at( object, class: <NSSize>, offset: c-expr( int: "sizeof(NSPoint)" ) ) := value;
end;


//NSObjCvalue
//NSMapTable


// NSRange

define /*exported*/ functional class <NSRange> (<statically-typed-pointer>)
end;

define method content-size( cls == <NSRange> )
=>( result :: <integer> )
	c-expr( int: "sizeof(NSRange)" );
end method content-size;

define /*exported*/ method location( object :: <NSRange> )
=> ( location :: <integer> )
	signed-long-at( object, offset: 0 );
end;

define /*exported*/ method location-setter( object :: <NSRange>, value :: <integer> )
=> ( location :: <integer> )
	signed-long-at( object, offset: 0 ) := value;
end;

define /*exported*/ method length( object :: <NSRange> )
=> ( length :: <integer> )
	signed-long-at( object, offset: 4 );
end;

define /*exported*/ method length-setter( object :: <NSRange>, value :: <integer> )
=> ( length :: <integer> )
	signed-long-at( object, offset: 4 ) := value;
end;


// NSZone

define /*exported*/ functional class <NSZone> (<statically-typed-pointer>)
end;

define method content-size( cls == <NSZone> )
=>( result :: <integer> )
	c-expr( int: "sizeof(id)" );
end method content-size;


// Mac Toolbox


// AEDesc

define /*exported*/ functional class <AEDesc> (<statically-typed-pointer>)
end;

define method content-size( cls == <AEDesc> )
=>( result :: <integer> )
	c-expr( int: "sizeof(AEDesc)" );
end method content-size;

// Cocoa has a "descriptorType" elsewhere as well
define /*exported*/ open generic descriptorType( object :: <object> )
=> ( descriptorType :: <object> );

define /*exported*/ method descriptorType( object :: <AEDesc> )
=> ( descriptorType :: <DescType> )
	unsigned-long-at( object, offset: 0 );
end;

define /*exported*/ method descriptorType-setter( object :: <AEDesc>, value :: <DescType> )
=> ( descriptorType :: <DescType> )
	unsigned-long-at( object, offset: 0 ) := value;
end;

define /*exported*/ method dataHandle( object :: <AEDesc> )
=> ( dataHandle :: <AEDataStorage> )
	pointer-at( object, offset: 4, class: <AEDesc> );
end;

define /*exported*/ method dataHandle-setter( object :: <AEDesc>, value :: <AEDataStorage> )
=> ( dataHandle :: <AEDataStorage> )
	pointer-at( object, offset: 4, class: <AEDataStorage> ) := value;
end;


// AEKeyDesc

define /*exported*/ functional class <AEKeyDesc> (<statically-typed-pointer>)
end;

define method content-size( cls == <AEKeyDesc> )
=>( result :: <integer> )
	c-expr( int: "sizeof(AEKeyDesc)" );
end method content-size;

define /*exported*/ method descKey( object :: <AEKeyDesc> )
=> ( descKey :: <AEKeyword> )
	unsigned-long-at( object, offset: 0 );
end;

define /*exported*/ method descKey-setter( object :: <AEKeyDesc>, value :: <AEKeyword> )
=> ( descKey :: <AEKeyword> )
	unsigned-long-at( object, offset: 0 ) := value;
end;

define /*exported*/ method descContent( object :: <AEKeyDesc> )
=> ( descContent :: <AEDesc> )
	pointer-at( object, offset: 4, class: <AEKeyDesc> );
end;

define /*exported*/ method descContent-setter( object :: <AEKeyDesc>, value :: <AEDesc> )
=> ( descContent :: <AEDesc> )
	pointer-at( object, offset: 4, class: <AEDesc> ) := value;
end;


// AppleEvent

define /*exported*/ functional class <AppleEvent> (<statically-typed-pointer>)
end class; // typedef of AERecord <- AEDescList <- AEDesc 

define method content-size( cls == <AppleEvent> )
=>( result :: <integer> )
	c-expr( int: "sizeof(AppleEvent)" );
end method content-size;

define /*exported*/ method descriptorType( object :: <AppleEvent> )
=> ( descriptorType :: <DescType> )
	unsigned-long-at( object, offset: 0 );
end;

define /*exported*/ method descriptorType-setter( object :: <AppleEvent>, value :: <DescType> )
=> ( descriptorType :: <DescType> )
	unsigned-long-at( object, offset: 0 ) := value;
end;

define /*exported*/ method dataHandle( object :: <AppleEvent> )
=> ( dataHandle :: <AEDataStorage> )
	pointer-at( object, offset: 4, class: <AEDataStorage> );
end;

define /*exported*/ method dataHandle-setter( object :: <AppleEvent>, value :: <AEDataStorage> )
=> ( dataHandle :: <AEDataStorage> )
	pointer-at( object, offset: 4 ) := value;
end;