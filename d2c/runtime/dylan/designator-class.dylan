rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/designator-class.dylan,v 1.5 2002/01/29 00:53:42 housel Exp $
copyright: see below
module: dylan-viscera

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
// All rights reserved.
// 
// Use and copying of this software and preparation of derivative
// works based on this software are permitted, including commercial
// use, provided that the following conditions are observed:
// 
// 1. This copyright notice must be retained in full on any copies
//    and on appropriate parts of any derivative works.
// 2. Documentation (paper or online) accompanying any system that
//    incorporates this software, or any part of it, must acknowledge
//    the contribution of the Gwydion Project at Carnegie Mellon
//    University, and the Gwydion Dylan Maintainers.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
// comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
// Also, see http://www.gwydiondylan.org/ for updates and documentation. 
//
//======================================================================

// Metaclass for Foreign Function Interface designator classes.
//
define class <designator-class> (<class>)
  //
  // How big the designated type is, in C sizeof units
  constant slot size-of :: <integer>,
    required-init-keyword: size:;
  //
  // Type's required alignment, a power of 2 in C sizeof units
  constant slot alignment-of :: <integer>,
    required-init-keyword: alignment:;
  //
  // Type that a pointer designator type refers to
  constant slot referenced-type :: false-or(<designator-class>),
    init-value: #f, init-keyword: referenced-type:;
  //
  // struct slots
  constant slot class-struct-slot-descriptors :: <simple-object-vector>,
    init-keyword: class-struct-slot-descriptors:, init-value: #[];
end class;

// Descriptor information for FFI struct/union slots
//
define class <struct-slot-descriptor> (<object>)
  slot struct-slot-c-type :: <designator-class>,
    required-init-keyword: c-type:;
  slot struct-slot-offset :: <integer>,
    required-init-keyword: offset:;
end class;

// Core defining macro for <designator-class> objects
//
define macro designator-class-definer
    { define ?class-adjectives designator-class ?:name (?supers)
        options ?type-options;
	?slots
      end }
      => make-define-designator-class({?name}, {?supers}, {?slots},
				      {?class-adjectives, ?type-options})

    { define ?class-adjectives designator-class ?:name (?supers)
	?slots
      end }
      => make-define-designator-class({?name}, {?supers}, {?slots},
				      {?class-adjectives})

  class-adjectives:
    { } => { dummy: #f }
    { abstract ... } => { abstract: #t, ... }
    { concrete ... } => { abstract: #f, ... }
    { primary ... } => { primary: #t, ... }
    { free ... } => { primary: #f, ... }
    { sealed ... } => { sealed: #t, ... }
    { open ... } => { sealed: #f, ... }
    { functional ... } => { functional: #t, ... }
    { instantiable ... } => { instantiable: #t, ... }

  type-options:
    { #rest ?all:*, 
      #key ?c-name:expression = #f,
           ?referenced-type:expression = #f,
           ?pack:expression = #f,
           ?c-rep:expression = #f,
           ?import-type:expression = #f,
           ?export-type:expression = #f,
           ?import-function:expression = #f,
           ?export-function:expression = #f,
           ?pointer-type-name:expression = #f,
           ?pointer-type-superclass:expression = #f,
           ?indirect-getter:expression = #f,
           ?indirect-setter:expression = #f,
           ?pointer-value-getter-function:expression = #f,
           ?pointer-value-setter-function:expression = #f }
      => { ?all }
    
  supers:
    { } => { }
    { ?:expression, ... } => { ?expression, ... }

  slots:
    { } => { }
    { ?slot; ... } => { ?slot, ... }

  slot:
    { ?struct-adjectives struct slot ?:name :: ?:expression,
        #rest ?struct-options }
      => make-struct-slot({?name}, {c-type: ?expression,
				    ?struct-adjectives,
				    ?struct-options})

    { inherited slot ?:name, #rest ?inherited-options }
      => make-inherited-slot({?name}, {?inherited-options})
    { inherited slot ?:name = ?:expression, #rest ?inherited-options }
      => make-inherited-slot({?name},
			     {init-expr: ?expression, ?inherited-options})

    { slot ?:name, #rest ?slot-options}
      => make-slot({?name}, {?slot-options})
    { ?slot-adjectives slot ?:name, #rest ?slot-options}
      => make-slot({?name}, {?slot-adjectives, ?slot-options})
    { ?slot-adjectives slot ?:name :: ?type:expression, #rest ?slot-options}
      => make-slot({?name}, {type: ?type, ?slot-adjectives, ?slot-options})
    { ?slot-adjectives slot ?:name :: ?type:expression = ?:expression,
	#rest ?slot-options }
      => make-slot({?name},
		   {type: ?type, init-expr: ?expression,
		    ?slot-adjectives, ?slot-options})

    { keyword ?key:token, #rest ?init-arg-options }
      => make-init-arg({?key}, {?init-arg-options})
    { required keyword ?key:token, #rest ?init-arg-options }
      => make-init-arg({?key}, {required: #t, ?init-arg-options})

  struct-adjectives:
    { } => { }
    { constant ... } => { setter: #f, ... }
    { sealed ... } => { sealed: #t, ... }
    { bitmap ... } => { allocation: bitmap, ... }
    { array ... } => { allocation: array, ... }

  struct-options:
    { #rest ?all:*,
      #key ?c-type:expression = #f,
           ?setter:expression = #f,
           ?address-getter:expression = #f,
           ?c-name:expression = #f,
           ?length:expression = #f,
     	   ?width:expression = #f,
           ?dimensions:expression = #f }
      => {?all}

  inherited-options:
    { #rest ?all:*, 
      #key ?init-value:expression = #f,
	   ?init-function:expression = #f }
      => {?all}

  slot-adjectives:
    { } => { }
    { constant ... } => { setter: #f, ... }
    { sealed ... } => { sealed: #t, ... }
    { instance } => { allocation: instance }
    { class } => { allocation: class }
    { each-subclass } => { allocation: each-subclass }
    { virtual } => { allocation: virtual }

  slot-options:
    { #rest ?all:*,
      #key ?setter:expression = #f,
	   ?init-keyword:token = foo:,
	   ?required-init-keyword:token = foo:,
	   ?init-value:expression = #f,
	   ?init-function:expression = #f,
	   ?type:expression = #f,
	   ?sizer:expression = #f,
	   ?size-init-keyword:token = foo:,
	   ?required-size-init-keyword:token = foo:,
	   ?size-init-value:expression = #f,
	   ?size-init-function:expression = #f }
      => {?all}

  init-arg-options:
    { #rest ?all:*,
      #key ?init-value:expression = #f,
	   ?init-function:expression = #f,
	   ?type:expression = #f }
      => {?all}
end;
