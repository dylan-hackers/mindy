module: my-simple

//	This is the code for your custom application class.


define class <my-application> ( <simple-application> )
end class <my-application>;


define method initialize-application( app :: <my-application> )
=> ()

	let windRect1 :: <Rect> = make( <Rect>, top: 60, left: 60, right: 310, bottom: 210 );
	make( <my-window>, bounds: windRect1, title: as( <pascal-string>, "controls" ), can-resize: #t );
	
	values();
	
end method initialize-application;

