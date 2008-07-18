module: parse-conditions
author: Eric Kidd <eric.kidd@pobox.com>
copyright: Copyright 1998 Gwydion Dylan Maintainers
license: CMU Gwydion Dylan License

//======================================================================
//  Parse Conditions
//======================================================================
//  This module is being written for use with Melange's two parsers:
//  the C parser and the interface declaration parser. However, it is
//  intended to be useful for other programs as well.
//
//  One of the design goals of this module is to simplify retrofitting
//  a formal error-reporting system onto pre-existing parser code
//  without making major changes.


//======================================================================
//  Public Parameters
//======================================================================
//  These parameters control the global behavior of this module.

//  What kind of progress messages to show while parsing, if any.
define constant $parse-progress-level-none = 0;
define constant $parse-progress-level-headers = 1;
define constant $parse-progress-level-all = 2; // add new levels above and increase this value.
define constant <parse-progress-level> = limited(<integer>,
                                                 min: $parse-progress-level-none,
                                                 max: $parse-progress-level-all);
define /* dynamic */ variable *show-parse-progress-level* :: <parse-progress-level>
  = $parse-progress-level-none;


//======================================================================
//  Private Module Variables
//======================================================================

//  Optional default parse context. If the user specifies
//  $unknown-source-location when calling parse-error, use this value
//  to help derive a meaningful source location.
define constant *default-parse-context* :: <deque> = make(<deque>);
push(*default-parse-context*, #f);


//======================================================================
//  Parse Conditions
//======================================================================
//  We define a small number of condition classes which are used by
//  exising programs. It would be possible to add <parse-error> and
//  other such classes if they are needed.

define abstract class <parse-condition> (<condition>)
  slot parse-condition-source-location :: <source-location>,
    required-init-keyword: source-location:;
end;

define class <format-string-parse-condition> (<format-string-condition>,
					      <parse-condition>)
end;

define class <simple-parse-error> (<error>,
				   <format-string-parse-condition>)
end;

define class <simple-parse-warning> (<warning>,
				     <format-string-parse-condition>)
end;

define class <parse-progress-report> (<format-string-parse-condition>)
  slot parse-progress-level :: <parse-progress-level> = $parse-progress-level-all,
    init-keyword: level:;
end;


//======================================================================
//  with-default-parse-context
//======================================================================
//  Establish a default parse context to use when none is supplied.

define function push-default-parse-context(context) => ()
  push(*default-parse-context*, context);
end;

define function pop-default-parse-context() => (context);
  pop(*default-parse-context*);
end;

/*
define macro with-default-parse-context
  { with-default-parse-context (?context:expression) ?:body end }
    => { push-default-parse-context(?context);
         block ()
	   ?body;
	 cleanup
	   pop-default-parse-context();
         end }
end;
*/

//======================================================================
//  Reporting parse conditions
//======================================================================
//  Replacements for error, signal, etc.

define function find-source-location(context)
  => (loc :: <source-location>)
  let effective-context = context | *default-parse-context*[0];
  if (effective-context)
    source-location(effective-context);
  else
    make(<unknown-source-location>);
  end if;
end function find-source-location;

define function parse-error
    (context, format-string, #rest format-args)
 => does-not-return :: <bottom>;
  error(make(<simple-parse-error>,
	     source-location: find-source-location(context),
	     format-string: format-string,
	     format-arguments: format-args));
end function parse-error;

define function parse-warning
    (context, format-string, #rest format-args)
  signal(make(<simple-parse-warning>,
	      source-location: find-source-location(context),
	      format-string: format-string,
	      format-arguments: format-args));
end function;

define function parse-progress-report
    (context, format-string, #rest format-args)
  signal(make(<parse-progress-report>,
	      source-location: find-source-location(context),
	      format-string: format-string,
	      format-arguments: format-args));
end function;

define function parse-header-progress-report
    (context, format-string, #rest format-args)
  signal(make(<parse-progress-report>,
              level: $parse-progress-level-headers,
	      source-location: find-source-location(context),
	      format-string: format-string,
	      format-arguments: format-args));
end function;


//======================================================================
//  Methods on <parse-condition>
//======================================================================
//  We define some methods related to the <condition> protocol in d2c.
//  Note that these assume default-handler methods get passed a real
//  <stream> and not #"Cheap-IO". To make these work, use the Dylan
//  extensions and standard-io library and write:
//    *warning-output* := *standard-output*;
//
//  Note: I've added back in Cheap-IO support, but now print warnings
//  when I catch someone using it. There's too many codepaths in the
//  Dylan libraries which might use it, and I want to find them all.

define method print-message
    (parse-condition :: <format-string-parse-condition>,
     stream :: <stream>,
     #next next-method)
 => ();
  describe-source-location(parse-condition.parse-condition-source-location,
			   stream);
  apply(format, stream, parse-condition.condition-format-string,
	parse-condition.condition-format-arguments);
end method print-message;

define method default-handler(condition :: <parse-progress-report>)
  if (*show-parse-progress-level* >= condition.parse-progress-level)
    format(*standard-output*, "%s\n", condition);
  end;
end method default-handler;
