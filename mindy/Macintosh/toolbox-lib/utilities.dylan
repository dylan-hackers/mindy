module: toolbox
author: Patrick Beard, maintenance by Rob Myers (yarrel@netscape.net)
copyright: (c)1999 Patrick Beard

// InterfaceLib

define constant *InterfaceLib* = load-object-file(#("InterfaceLib"));

// This is potentially useful, but will probably be overshadowed by Melange.
// It combines the functionality of "find-c-function" and
// "constrain-c-function" to get usable function in one step.

define constant gcf-unbound = pair(#f, #f); // hack

define method get-c-function (name :: <string>, #key args, rest = ~args,
								result = <object>, file = gcf-unbound)
 => (result :: <c-function>);
  let real-args = if (args) as(<list>, args) else #() end if;
  let real-result = if (instance?(result, <sequence>)) as(<list>, result)
		    else list(result)
		    end if;
  let fun = if (file == gcf-unbound)
	      find-c-function(name)
	    else
	      find-c-function(name, file: file);
	    end if;
  fun & constrain-c-function(fun, real-args, rest, real-result);
end method get-c-function;

// OSErr.

define constant <OSErr> = <integer>;

// OSType.

define constant <OSType> = <extended-integer>;

define constant os-type = method (typestr :: <string>) => (result :: <OSType>);
	let type = as(<OSType>, as(<integer>, typestr[0]));
	for (i from 1 below 4)
		type := type * 256 + as(<integer>, typestr[i]);
	finally
		type;
	end for;
end method;

// Low-Level Debugger.

define constant Debugger = get-c-function("Debugger", args: #(),
											result: #(), file: *InterfaceLib*);
define constant DebugStr = get-c-function("DebugStr", args: list(<Pascal-string>),
											result: #(), file: *InterfaceLib*);
