module: my-simple

//	This is the code for your custom application class.


define class <my-application> ( <simple-application> )
end class <my-application>;


define method initialize-application( app :: <my-application> )
=> ()

	let windRect1 :: <Rect> = make( <Rect>, top: 60, left: 60, right: 310, bottom: 210 );
	make( <my-window>, bounds: windRect1, title: as( <pascal-string>, "three" ), can-resize: #t );
	
	let windRect2 :: <Rect> = make( <Rect>, top: 70, left: 70, right: 320, bottom: 220 );
	make( <my-window>, bounds: windRect2, title: as( <pascal-string>, "two" ) );
	
	let windRect3 :: <Rect> = make( <Rect>, top: 80, left: 80, right: 330, bottom: 230 );
	make( <my-window>, bounds: windRect3, title: as( <pascal-string>, "one" ) );

	let dialog = make( <simple-dialog>, DLOG: 1000, default: 1, cancel: 2, is-modal: #t );	
	handle-radio-button( dialog.dialogRef, 6 ); // Configure the radio buttons
        
        SelectWindow( dialog.windowRef );
	
	values();
	
end method initialize-application;

