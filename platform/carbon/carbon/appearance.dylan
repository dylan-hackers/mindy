module: carbon

c-include( "Carbon.h" );


/*
	Constants
*/

define constant $kThemeBackgroundSecondaryGroupBox :: <integer> = 
	c-expr(int: "kThemeBackgroundSecondaryGroupBox");

/*
kThemeSystemFont = 0,
    kThemeSmallSystemFont = 1,
    kThemeSmallEmphasizedSystemFont = 2,
    kThemeViewsFont = 3,
    kThemeEmphasizedSystemFont = 4,
    kThemeApplicationFont = 5,
    kThemeLabelFont = 6,
    kThemeCurrentPortFont = 200
*/


/*
	Classes and types
*/

define constant <ThemeBrush> :: <type> = <integer>;
define constant <ThemeBackgroundKind> :: <type> = <integer>;

define functional class <ThemeDrawingState> (<statically-typed-pointer>)
end class <ThemeDrawingState>;


/*	
    RegisterAppearanceClient
*/

define method RegisterAppearanceClient()
=> ( result :: <OSStatus> )

	let result = call-out( "RegisterAppearanceClient", int: );
        as( <OSStatus>, result );

end method RegisterAppearanceClient;

/*	
    UnregisterAppearanceClient
*/

define method UnregisterAppearanceClient()
=> ( result :: <OSStatus> )

	let result = call-out( "UnregisterAppearanceClient", int: );
        as( <OSStatus>, result );

end method UnregisterAppearanceClient;

/*
	GetThemeDrawingState
*/

define method GetThemeDrawingState()
=> ( result :: <OSStatus> )
	let temp :: <Handle> = make( <Handle> );
	
	let result = call-out( "GetThemeDrawingState", int:, ptr: temp.raw-value );
	
  values( as( <OSStatus>, result ), pointer-at( temp, class: <ThemeDrawingState>, offset: 0 ) );
end method GetThemeDrawingState;

/*
	SetThemeDrawingState
*/

define method SetThemeDrawingState( state :: <ThemeDrawingState>, disposeNow :: <boolean> )
=> ( result :: <OSStatus> )
	let result = call-out( "SetThemeDrawingState", int:, ptr: state.raw-value,
								int: if( disposeNow ) 1 else 0 end );
	
  as( <OSStatus>, result );
end method SetThemeDrawingState;

/*
	DisposeThemeDrawingState
*/

define method DisposeThemeDrawingState( state :: <ThemeDrawingState> )
=> ( result :: <OSStatus> )
	let result = call-out( "DisposeThemeDrawingState", int:, ptr: state.raw-value );
	
  as( <OSStatus>, result );
end method DisposeThemeDrawingState;