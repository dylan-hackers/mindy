Module:       vanilla-duim


define macro ignoring
    { ignoring (?:*) }
     => {}
end macro ignoring;


define macro not-yet-implemented
    { not-yet-implemented (?:*) }
     => {}
end macro not-yet-implemented;


define method make-native-pixarray( drawable :: <drawable>, width :: <integer>, height :: <integer> )
=> (  result :: <array>  )

    make(<array>, dimensions: #(640, 480)); //* dummy return

end method make-native-pixarray;


define method make-native-image(drawable :: <drawable>, image)
=> ()

    values();

end method make-native-image;


define method convert-color-to-native( mirror, r, g, b )
=> ( result :: <integer> )

    1;
    
end method convert-color-to-native;


define method native-color-rgb( color )
=>( r ::<integer>, g :: <integer>, b :: <integer> )

	values( 0, 0, 0 );

end method native-color-rgb;


define method convert-color( rep, color :: <symbol> )
=>( color )

	#f;

end method convert-color;
