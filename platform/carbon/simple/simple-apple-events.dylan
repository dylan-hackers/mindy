module: simple

/*
        Core Apple Event Code
        -Based on code from Gareth Baker's HelloCarbonApp.
        -We make methods to call methods on the global application object, 
        wrap those in callbacks, make the callbacks into UPPs,
        and install the UPPs as Apple Event Handlers.
*/

/*
    Event Handler Methods
*/

/*
    Open Application Apple Event Handler Method
*/

define method open-application-Apple-Event-handler( appleEvt :: <AppleEvent*>, reply :: <AppleEvent*>, refcon :: <integer> )
=> (res :: <OSErr>)

    //- We don't do anything. We should call a method on *application*
    values($noErr)
        
end method;

/*
    Open Document Apple Event Handler Method
*/

define method open-document-Apple-Event-handler( appleEvt :: <AppleEvent*>, reply :: <AppleEvent*>, refcon :: <integer> )
=> (res :: <OSErr>)

    //- We don't do anything. We should call a method on *application*
    values($noErr)
        
end method;

/*
    Print Document Apple Event Handler Method
*/

define method print-document-Apple-Event-handler( appleEvt :: <AppleEvent*>, reply :: <AppleEvent*>, refcon :: <integer> )
=> (res :: <OSErr>)

    //- We don't do anything. We should call a method on *application*
    values($noErr)
        
end method;

/*
    Quit Apple Event Handler Method
*/

define method quit-Apple-Event-handler( appleEvt :: <AppleEvent*>, reply :: <AppleEvent*>, refcon :: <integer> )
=> (res :: <OSErr>)

	*application*.quit := #t;
	values($noErr)
        
end method;

/*
    Callbacks
*/

/*
    Open Application Apple Event Handler Callback
*/

define constant $open-application-handler-callback =
        callback-method(arg1 :: <raw-pointer>, arg2 :: <raw-pointer>, refcon :: <integer>) => (res :: <OSErr>);
		let appleEvt = make(<AppleEvent*>, pointer: arg1);
		let reply = make(<AppleEvent*>, pointer: arg2);
		let res = open-application-Apple-Event-handler( appleEvt, reply, refcon );
		values(res);
	end;

/*
    Open Document Apple Event Handler Callback
*/

define constant $open-document-handler-callback =
        callback-method(arg1 :: <raw-pointer>, arg2 :: <raw-pointer>, refcon :: <integer>) => (res :: <OSErr>);
		let appleEvt = make(<AppleEvent*>, pointer: arg1);
		let reply = make(<AppleEvent*>, pointer: arg2);
		let res = open-document-Apple-Event-handler( appleEvt, reply, refcon );
		values(res);
	end;

/*
    Print Document Apple Event Handler Callback
*/

define constant $print-document-handler-callback =
        callback-method(arg1 :: <raw-pointer>, arg2 :: <raw-pointer>, refcon :: <integer>) => (res :: <OSErr>);
		let appleEvt = make(<AppleEvent*>, pointer: arg1);
		let reply = make(<AppleEvent*>, pointer: arg2);
		let res = print-document-Apple-Event-handler( appleEvt, reply, refcon );
		values(res);
	end;

/*
    Quit Apple Event Handler Callback
*/

define constant $quit-handler-callback =
        callback-method(arg1 :: <raw-pointer>, arg2 :: <raw-pointer>, refcon :: <integer>) => (res :: <OSErr>);
		let appleEvt = make(<AppleEvent*>, pointer: arg1);
		let reply = make(<AppleEvent*>, pointer: arg2);
		let res = quit-Apple-Event-handler( appleEvt, reply, refcon );
		values(res);
	end;
  
/*
    UPPs
*/  

/*
    Open Application Apple Event Handler UPP
*/        
        
define constant $open-application-handler-upp = NewAEEventHandlerUPP($open-application-handler-callback.callback-entry);

/*
    Open Document Apple Event Handler UPP
*/        
        
define constant $open-document-handler-upp = NewAEEventHandlerUPP($open-document-handler-callback.callback-entry);

/*
    Print Document Apple Event Handler UPP
*/        
        
define constant $print-document-handler-upp = NewAEEventHandlerUPP($print-document-handler-callback.callback-entry);
        
/*
    Quit Apple Event Handler UPP
*/        
        
define constant $quit-handler-upp = NewAEEventHandlerUPP($quit-handler-callback.callback-entry);

/*
    Install and remove Apple Events
*/

/*
    install-Apple-Events
*/

define method install-Apple-Events()
=>()

    let err = AEInstallEventHandler( $kCoreEventClass, $kAEOpenApplication, $open-application-handler-upp, 0, #f );
    if (err ~= $noErr)
            error("Failed to install Open Application Apple Event handler.");
    end if;
    let err = AEInstallEventHandler( $kCoreEventClass, $kAEOpenDocuments, $open-document-handler-upp, 0, #f );
    if (err ~= $noErr)
            error("Failed to install Open Document Apple Event handler.");
    end if;
    let err = AEInstallEventHandler( $kCoreEventClass, $kAEPrintDocuments, $print-document-handler-upp, 0, #f );
    if (err ~= $noErr)
            error("Failed to install Print Document Apple Event handler.");
    end if;
    let err = AEInstallEventHandler( $kCoreEventClass, $kAEQuitApplication, $quit-handler-upp, 0, #f );
    if (err ~= $noErr)
            error("Failed to install Quit Apple Event handler.");
    end if;

    //- UNREGISTER & DISPOSE OF UPPS IF THERE'S A PROBLEM

    values();

end method install-apple-events;

/*
    remove-Apple-Events
*/

define method remove-Apple-Events()
=>()

    let err = AERemoveEventHandler( $kCoreEventClass, $kAEOpenApplication, $open-application-handler-upp, #f );
    if (err ~= $noErr)
            error("Failed to remove Open Application Apple Event handler.");
    end if;
    let err = AERemoveEventHandler( $kCoreEventClass, $kAEOpenDocuments, $open-document-handler-upp, #f );
    if (err ~= $noErr)
            error("Failed to remove Open Document Apple Event handler.");
    end if;
    let err = AERemoveEventHandler( $kCoreEventClass, $kAEPrintDocuments, $print-document-handler-upp, #f );
    if (err ~= $noErr)
            error("Failed to remove Print Document Apple Event handler.");
    end if;
    let err = AERemoveEventHandler( $kCoreEventClass, $kAEQuitApplication, $quit-handler-upp, #f );
    if (err ~= $noErr)
            error("Failed to remove Quit Apple Event handler.");
    end if;

    //- DISPOSE OF UPPS

    values();

end method remove-apple-events;