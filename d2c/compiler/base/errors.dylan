module: errors
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/errors.dylan,v 1.5 1996/03/20 19:23:18 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.




// Error message output:


define abstract class <compiler-condition> (<format-string-condition>)
  //
  // The source location this condition happened at.
  constant slot condition-at :: <source-location>,
    required-init-keyword: at:;
end class <compiler-condition>;

define sealed domain make (singleton(<compiler-condition>));
define sealed domain initialize (<compiler-condition>);

define method report-condition
    (condition :: <compiler-condition>, stream :: <stream>)
    => ();
  pprint-logical-block
    (stream,
     body: method (stream :: <stream>)
	     describe-source-location(condition.condition-at, stream);
	     apply(condition-format, stream, condition.condition-format-string,
		   condition.condition-format-arguments);
	   end method);
end method report-condition;



define class <compiler-error> (<error>, <compiler-condition>)
end class <compiler-error>;

define sealed domain make (singleton(<compiler-error>));


define class <compiler-warning> (<warning>, <compiler-condition>)
end class <compiler-warning>;

define sealed domain make (singleton(<compiler-warning>));
define sealed domain initialize (<compiler-warning>);

define variable *warnings* :: <integer> = 0;

define method default-handler
    (warning :: <compiler-warning>, #next next-method)
  *warnings* := *warnings* + 1;
  format(*debug-output*, "%s\n\n", warning);
end method default-handler;


// compiler-warning-location -- exported
//
// Signal compiler warning with explicit source location.
//
define method compiler-warning-location
    (loc :: type-union(<source-location-mixin>, <source-location>),
     format-string :: <byte-string>, #rest format-arguments)
    => ();
  signal(make(<compiler-warning>,
	      at: select (loc by instance?)
		    <source-location> => loc;
		    <source-location-mixin> => loc.source-location;
		  end select,
	      format-string: stringify("Warning: ", format-string),
	      format-arguments: format-arguments));
end method compiler-warning-location;

// compiler-error-location -- exported.
//
// Signal a compiler error.
//
define constant compiler-error-location = method
    (loc :: type-union(<source-location-mixin>, <source-location>),
     format-string :: <byte-string>, #rest format-arguments)
    => ();
  error(make(<compiler-error>,
	     at: select (loc by instance?)
		   <source-location> => loc;
		   <source-location-mixin> => loc.source-location;
		 end select,
	     format-string: format-string,
	     format-arguments: format-arguments));
end method;

// compiler-{warning,error} -- external.
//
// Call compiler-{warning,error}-location with any location we can extract
// from the args.
// 
define constant compiler-warning = method (string, #rest args) => ();
  apply(compiler-warning-location, find-source-loc(args), string, args);
end method;
//
define constant compiler-error = method (string, #rest args) => ();
  apply(compiler-error-location, find-source-loc(args), string, args);
end method;

// Look in the rest args, hoping to find an object with a source location.
//
define method find-source-loc (args :: <sequence>) 
    => res :: <source-location>;
  block (return)
    for (x in args)
      if (instance?(x, <source-location-mixin>))
	return(x.source-location);
      end;
    finally
      make(<unknown-source-location>);
    end for;
  end block;
end method find-source-loc;



// Extract-source:
//
// Utility used to extract a source location from some thing.
// For now, and maybe always, we just take the source from the token.
//

define /* exported */ generic extract-source (wot) => res :: <source-location>;

define method extract-source (wot) => res :: <unknown-source-location>;
  make(<unknown-source-location>);
end method;

define method extract-source (wot :: <source-location-mixin>)
 => res :: <source-location>;
  wot.source-location;
end method;
