module: objective-c

c-system-include("/usr/include/objc/objc.h");

// Types

//TODO: content-sizes

define functional class <Objective-C-Class> (<statically-typed-pointer>)
end;

define functional class <Protocol> (<statically-typed-pointer>)
end;

define functional class <id> (<statically-typed-pointer>)
end;

define functional class <SEL> (<statically-typed-pointer>)
end;

define functional class <IMP> (<statically-typed-pointer>)
end;

define constant <BOOL> :: <type> = <boolean>;

// Values

define constant $YES :: <boolean> = #t;
define constant $NO :: <boolean> = #f;
define constant $nil :: <id> = as( <id>, 0 );

// Getting Runtime values


// objc-class
// Get the Objective-C class for a Dylan class as fast as possible.

define constant $objective-c-classes :: <table> = make( <table> );

define function objc-class( dylan-class :: <Class> )
=> ( objective-c-class :: <id> )
	let objective-c-class :: false-or( <id> ) =
		element( $objective-c-classes, dylan-class, default: #f );
	if( ~ objective-c-class )
		let name :: <c-string> = as( <c-string>,
			copy-sequence( dylan-class.class-name, start: 1, 
				end: dylan-class.class-name.collection-size - 1 ) );
		objective-c-class := make( <id>, pointer: 
				call-out( "objc_getClass", ptr:, ptr: name.raw-value ) );
		$objective-c-classes[ dylan-class ] := objective-c-class;
	end if;
	
	objective-c-class;
end function objc-class;

// Registering classes and methods