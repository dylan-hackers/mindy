module: dylan-user

/*
	events
*/

define module events

	use Dylan;
	//use Extensions;						// <extended-integer>
        use System; 							// <raw-pointer>
	use melange-support;
	use mac-types;
	use memory;
	use quickdraw;
	use windows;
	
	export	// Event Manager.
		$everyEvent,
		$nullEvent, $mouseDown, $mouseUp, $keyDown, $keyUp, $autoKey, $updateEvt, $diskEvt, $activateEvt,
		$osEvt, $kHighLevelEvent,
		$cmdKey, $cmdKeyBit, $suspendResumeMessage,
		$activeFlag, $charCodeMask, $keyCodeMask,
		
		<EventRecord>, event-what, event-message, event-when, event-where, event-modifiers,
		//GetNextEvent, SystemTask, 
		WaitNextEvent, FlushEvents,
                <EventModifiers>,
		//DIBadMount,
		
		// AppleEvents.
		$kCoreEventClass,
		$kAEOpenApplication, $kAEOpenDocuments, $kAEPrintDocuments, $kAEQuitApplication,
		
		<RoutineDescriptor>, <UniversalProcPtr>,
		<AEEventClass>, <AEEventID>, <AppleEvent>,
		<AEDesc>, <AEDescList>,
		<AEEventHandlerUPP>, $uppAEEventHandlerProcInfo,
                NewAEEventHandlerUPP,
		AEInstallEventHandler, AERemoveEventHandler, AEProcessAppleEvent,
		
		// Misc Event Stuff
		TickCount, Button, StillDown, WaitMouseUp, GetMouse, GlobalToLocal;
		//SystemClick;
		
end module events;




