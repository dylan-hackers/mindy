module: my-simple
	
define method main (argv0 :: <byte-string>, #rest noise)

    let app = make( <my-application> );
    run( app );
        
end method main;