module: appearance

c-include( "Carbon/Carbon.h" );

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
