Module:       duim-utilities
Synopsis:     DUIM utilities
Author:       Rob Myers
Copyright:    Gwydion Dylan Maintainers 2000
License:      GNU Library General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/*
    NOTES
*/

// Change define /*thread*/ variable to define variable;
// d2c has no threads yet

// Change define /*sideways*/ method to define method;
// this may require that create be changed to export in some places.
// Watch for compile errors.


// assert
// Gwydion defines a traditional assert on <boolean> in misc in extentions, 
// exported from common-dylan.
// We exclude that version and declare our own here.
// FIXME:   3rd clause should have a #rest as its 3rd argument for 
//          format arguments and apply this to format-string

define macro assert
    { assert( ?clauses ) }
     => { ?clauses }
clauses:
  { ?ok:expression }
    => { if( ~ ?ok ) error( "An assertion failed" ); end if; }
  { ?ok:expression, ?message:expression }
    => { if( ~ ?ok ) error( ?message ); end if; }
  { ?ok:expression, ?message:expression, ?arg:expression }
    => { if( ~ ?ok ) error( format-to-string( ?message, ?arg ) ); end if; }
  { ?ok:expression, ?message:expression, ?arg:expression, ?arg2:expression }
    => { if( ~ ?ok ) error( format-to-string( ?message, ?arg, ?arg2 ) ); end if; }
  { ?ok:expression, ?message:expression, ?arg:expression, ?arg2:expression, ?arg3:expression }
    => { if( ~ ?ok ) error( format-to-string( ?message, ?arg, ?arg2, ?arg3 ) ); end if; }
  { ?ok:expression, ?message:expression, ?arg:expression, ?arg2:expression, ?arg3:expression, ?arg4:expression }
    => { if( ~ ?ok ) error( format-to-string( ?message, ?arg, ?arg2, ?arg3, ?arg4 ) ); end if; }
end macro assert;

