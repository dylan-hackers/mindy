module: cocoa-test
synopsis: 
author: 
copyright: 

define function main(name, arguments)
    with-autorelease-pool()
        // Test Foundation
        // String
        let c-string :: <c-string> = as( <c-string>, "I should Cocoa" );
        let string :: <NSString> = initWithCString( alloc( <NSString> ), c-string );
        //string.uppercaseString;
        format-out("%=\n", string.cString );
        // Test Appkit
        // Set up the application
        let app :: <NSApplication> = sharedApplication( <NSApplication> );
        let nib-string :: <NSString> = initWithCString( alloc( <NSString> ), as( <c-string>, "cocoa-test" ) );
        loadNibNamedOwner( <NSBundle>, nib-string, app );
    	run( app );
        // Window
        /*let bounds :: <NSRect> = NSMakeRect( 100.0, 100.0, 200.0, 200.0 );
        let window :: <NSWindow> = initWithContentRectStyleMaskBackingDefer( 
            alloc( <NSWindow> ),
            bounds, 0, $NSBackingStoreRetained, #f );
        SetTitle( window, string );
        //MakeKeyAndOrderFront( window, $nil );
        //SetStringValue( textField, string );
        let session :: <NSModalSession> = beginModalSessionForWindow( app, window );
        while( runModalSession( app, session ) ~= $NSRunContinuesResponse )
        end while;
        EndModalSession( app, session);*/
    end with-autorelease-pool;
    exit-application(0);
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());
