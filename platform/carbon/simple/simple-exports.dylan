Module: dylan-user

define library simple

	use dylan;
	
	use melange-support;
        
        use format;
	
	use carbon;
	
	export simple;
	
end library simple;

define module simple

	use dylan;
	
    use System;
	use melange-support;
        
    use format;
	
	use carbon;
	
	export
	
		<simple-window>,
		window-to-object,
		modal, closeable, draggable, zoomable, resizeable, floating,
		finalize,
		initialize-window, finalize-window,
		finished, finished-setter, 
		windowRef, grafPort,
		focus, refresh, draw, update,
		activate, resize,
		window-idle, window-click, window-key,
		window-menu-choice,
		can-close?, close-window, 
		
		<simple-dialog>,
		dialogRef,
		dialog-to-object,
		dialog-selection,
		handle-dialog-item,
		handle-radio-button,
		set-radio-buttons, 
		get-dialog-item-text, set-dialog-item-text,
		get-radio-button-state,
		get-checkbox-state, set-checkbox-state,
		get-popup-string,
		
		<simple-application>,
		*application*,
		front-window,
		quit, quit-setter,
		background, initialized,
		can-run?, run, 
		initialize-application, finalize-application,
		initialize-menus,
		next-event, dispatch-event, dispatch-mouse-event,
		application-idle,
		application-key, application-menu-choice,
		application-window-activate, application-window-update,
		application-apple-event, application-os-event,
		about-box;
	
end module simple;
