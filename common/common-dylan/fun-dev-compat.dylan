Module:       functional-extensions
Synopsis:     Functional Developer compatibility functions
Author:       Rob Myers
Copyright:    Gwydion Dylan Maintainers 2000
License:      GNU Library General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// assert
// Gwydion defines a traditional assert on <boolean> in misc in extentions, 
// exported from common-dylan.
// We exclude that version and declare our own here.


// find-value
// simple alias to differently named equivalent d2c method

define constant find-value = find-element;

/// Profiling macro
//  Do-nothing version
define macro profiling
  { profiling
        (?options:*)
      ?body:body
    results
      ?result-body:body
    end }
 => { ?body;
      ?result-body }
end macro profiling;
