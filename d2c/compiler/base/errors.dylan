module: errors
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/errors.dylan,v 1.4 1996/03/17 00:30:07 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

// pretty format
//
// User for error message printing.  Turns each space in the control string
// into a conditional newline, and turns literal newlines into a mandatory
// newline and 2 space indent.
//

define method pretty-format (stream :: <stream>,
			     string :: <byte-string>,
			     #rest args)
  let length = string.size;
  local
    method scan-for-space (stream, start, posn, arg-index)
      if (posn == length)
	maybe-spew(stream, start, posn);
      else
	let char = string[posn];
	if (char == ' ')
	  scan-for-end-of-spaces(stream, start, posn + 1, arg-index);
	elseif (char == '%')
	  maybe-spew(stream, start, posn);
	  let directive = string[posn + 1];
	  if (directive == '%')
	    scan-for-space(stream, posn + 1, posn + 2, arg-index);
	  else
	    format(stream, copy-sequence(string, start: posn, end: posn + 2),
		   args[arg-index]);
	    scan-for-space(stream, posn + 2, posn + 2, arg-index + 1);
	  end;
	elseif (char == '\n')
	  maybe-spew(stream, start, posn);
	  pprint-newline(#"mandatory", stream);
	  pprint-indent(#"block", 2, stream);
	  scan-for-end-of-spaces(stream, posn + 1, posn + 1, arg-index);
	else
	  scan-for-space(stream, start, posn + 1, arg-index);
	end;
      end;
    end,
    method scan-for-end-of-spaces(stream, start, posn, arg-index)
      if (posn < length & string[posn] == ' ')
	scan-for-end-of-spaces(stream, start, posn + 1, arg-index);
      else
	maybe-spew(stream, start, posn);
	pprint-newline(#"fill", stream);
	scan-for-space(stream, posn, posn, arg-index);
      end;
    end,
    method maybe-spew (stream, start, stop)
      unless (start == stop)
	write(string, stream, start: start, end: stop);
      end;
    end;
  pprint-logical-block(stream,
		       body: method (stream)
			       scan-for-space(stream, 0, 0, 0);
			       pprint-indent(#"block", 0, stream);
			       pprint-newline(#"mandatory", stream);
			     end);
end;

define method report-condition (condition :: type-union(<simple-error>,
						     <simple-warning>,
						     <simple-restart>),
				stream :: <stream>)
  apply(pretty-format, stream,
	condition.condition-format-string,
	condition.condition-format-arguments);
end;


// Error message output:


// ### will flame out if the file can't be found, which is likely if the
// location is in another library.
// 
define method print-message
    (wot :: <source-location-mixin>, stream :: <stream>) => ();
  let loc = wot.source-location;
  if (instance?(loc, <file-source-location>))
    print(extract-string(loc), stream);
  else
    print(wot, stream);
  end;
end;


define variable *warnings* = 0;


// Look in the rest args, hoping to find an object with a source location.
//
define method find-source-loc (args :: <sequence>) 
 => res :: false-or(<source-location-mixin>);
  block (punt)
    for (x in args)
      if (instance?(x, <source-location-mixin>))
        punt(x);
      end;
      finally #f;
    end for;
  end block;
end method;


// Call compiler-{warning,error}-location with any location we can extract from
// the args.
// 
define constant compiler-warning = method (string, #rest args) => ();
  apply(compiler-warning-location, find-source-loc(args), string, args);
end method;
//
define constant compiler-error = method (string, #rest args) => ();
  apply(compiler-error-location, find-source-loc(args), string, args);
end method;


// Signal compiler warning with explicit source location (or #F if unknown.)
//
define constant compiler-warning-location = method
    (loc :: false-or(<source-location-mixin>), string, #rest args) => ();
  if (loc & instance?(loc.source-location, <file-source-location>))
    let fs = loc.source-location;
    apply(pretty-format, *debug-output*,
	  concatenate("%S:%=:Warning:\n  ", string, "\n"),
	  fs.source-file.file-name, fs.start-line,
	  args);
  else
    apply(pretty-format, *debug-output*,
	  concatenate("Warning: ", string, "\n"),
	  args);
  end if;
  *warnings* := *warnings* + 1;
end method;


// Signal compiler error with explicit source location (or #F if unknown.)
//
define constant compiler-error-location = method 
    (loc :: false-or(<source-location-mixin>), string, #rest args)
 => ();
  if (loc & instance?(loc.source-location, <file-source-location>))
    let fs = loc.source-location;
    apply(error,
    concatenate("%S:%=:Error:\n  ", string, "\n"),
	  fs.source-file.file-name, fs.start-line,
	  args);
  else
    apply(error,
	  concatenate("Error: ", string, "\n"),
	  args);
  end if;
end method;


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
