Module:       functional-extensions
Synopsis:     Functional Developer compatibility functions
Author:       Rob Myers
Copyright:    Gwydion Dylan Maintainers 2000
License:      GNU Library General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//
//	Utilities
//

// element-range-check
//
define inline method element-range-check
    (index :: <integer>, limit :: <integer>)
 => (res :: <boolean>);
  0 <= index & index < limit;
end method;

// without-bounds-checks
// Note: intentional violation of hygiene required

define macro without-bounds-checks
  {without-bounds-checks () ?:body end}
    => {without-bounds-checks ?body end}

  {without-bounds-checks ?:body end}
    => {let ?=element = %element;
        let ?=element-setter = %element-setter;
        ?body}
end;

define macro with-bounds-checks
  {with-bounds-checks () ?:body end}
    => {with-bounds-checks ?body end}

  {with-bounds-checks ?:body end}
    => {let ?=element = element;
        let ?=element-setter = element-setter;
        ?body}
end;

// find-value
// simple alias to differently named equivalent d2c method

define constant find-value = find-element;
