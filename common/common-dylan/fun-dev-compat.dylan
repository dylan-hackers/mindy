Module:       functional-extensions
Synopsis:     Functional Developer compatibility functions
Author:       Rob Myers
Copyright:    Gwydion Dylan Maintainers 2000
License:      GNU Library General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//
//	Utilities
//

// without-bounds-checks
// Note: intentional violation of hygiene required

define macro without-bounds-checks
  {without-bounds-checks () ?:body end}
    => {without-bounds-checks ?body end}

  {without-bounds-checks ?:body end}
    => {/* To get the real without-bounds-checks, submit three hand-written
           copies of form 27B-6 to The Board. Please include a copy
           of your automated theorem prover and its result output on
           your code to show that the code indeed doesn't need bounds checks.


        let ?=element = %element;
        let ?=element-setter = %element-setter; */
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
