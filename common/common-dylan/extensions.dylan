module: common-extensions

//=========================================================================
//  Unsupplied, unfound.
//=========================================================================
//  Unique objects which can be used as default values for keywords and
//  passed to 'default:'. These cannot be confused with any other Dylan
//  values.

define class <not-found-marker> (<object>)
end;

define constant $unfound = make(<not-found-marker>);


//=========================================================================
//  Locators
//=========================================================================
//  A very abstract interface to locators.

define open abstract class <locator> (<object>)
end class;

define open generic supports-open-locator?
    (locator :: <locator>)
 => (supported? :: <boolean>);
  
define method supports-open-locator?
    (locator :: <locator>)
 => (supported? :: <boolean>)
  #f;                                                                         
end method;

define open generic open-locator
    (locator :: <locator>)
 => (stream :: <stream>);

define open generic supports-list-locator?
    (locator :: <locator>)
 => (supported? :: <boolean>);

define method supports-list-locator?
    (locator :: <locator>)
 => (supported? :: <boolean>)
  #f;                                                                         
end method;

define open generic list-locator
    (locator :: <locator>)
 => (listing :: <sequence>);


//=========================================================================
//  Locators
//=========================================================================
//  A very abstract interface to locators.

define open generic condition-to-string
    (condition :: <condition>)
 => (string :: <string>);

// XXX - need method on <format-string-condition>
// XXX - need method on any conditions defined by Dylan library


//=========================================================================
//  Debugging & assertions
//=========================================================================
//  Some of this may move to simple-debugging.

define function debug-message
    (format-string, #rest format-arguments)
 => ()
  // XXX - need implementation
end function;

// XXX - assert and debug-assert work right, but need to be enhanced.
define constant debug-assert = assert;


//=========================================================================
//  Ignore & ignorable
//=========================================================================
//  Control compiler warnings about unused variables.

define function ignorable (object) => ()
  // XXX - This has the right API, but doesn't do anything. Also fix
  // ignorable.
end;


//=========================================================================
//  Conversions
//=========================================================================
//  Convert numbers to and from strings.

// XXX - float-to-string
// XXX - integer-to-string
// XXX - number-to-string
// XXX - string-to-integer


//=========================================================================
//  Macros
//=========================================================================
//  Miscellaneous macros exported from common-extensions. These are not
//  available under Mindy.

#if (d2c)

/*
XXX - table-definer is broken!

define macro table-definer
  { define table ?:name = { } }
    => { define constant ?name :: <table> = make(<table>) }
  { define table ?:name :: ?type:* = { } }
    => { define constant ?name :: ?type = make(?type) }
end macro;
*/

define macro iterate
  { iterate ?:name (?clauses:*) ?:body end }
    => { iterate-aux ?name
	   iterate-param-helper(?clauses)
           iterate-value-helper(?clauses)
	   ?body
         end }
end;

define macro iterate-aux
  { iterate-aux ?:name
      ?param-clauses:macro
      ?value-clauses:macro
      ?:body
    end }
    => { local method ?name (?param-clauses)
                 ?body
	       end;
         ?name(?value-clauses) }
end macro;

define macro iterate-param-helper
  { iterate-param-helper(?clauses) }
    => { ?clauses }
clauses:
  { ?:name = ?value:*, ... }
    => { ?name :: <object>, ... }
  { ?:name :: ?type:* = ?value:*, ... }
    => { ?name :: ?type, ... }
  { } => { }
end;

define macro iterate-value-helper
  { iterate-value-helper(?clauses) }
    => { ?clauses }
clauses:
  { ?:name = ?value:*, ... }
    => { ?value, ... }
  { ?:name :: ?type:* = ?value:*, ... }
    => { ?value, ... }
  { } => { }
end;

define macro when
  { when (?:expression) ?:body end }
    => { if (?expression) ?body end }
end macro;

#endif
