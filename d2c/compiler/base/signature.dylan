Module: signature
Description: Method/GF signatures and operations on them
rcs-header: $Header: /scm/cvs/src/d2c/compiler/base/signature.dylan,v 1.1 1998/05/03 19:55:31 andreas Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
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
//    University.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports, questions, comments, and suggestions should be sent by
// E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
//
//======================================================================

// A <signature> represents what we know at compile-time about the
// argument/return-value protocol of a GF or method.
// 
define class <signature> (<object>)

  // List of <ctype>s representing the specializers for required arguments.
  slot specializers :: <list>, required-init-keyword: specializers:;

  // True if there was a #next.
  slot next? :: <boolean>,
    init-value: #f, init-keyword: next:;

  // If no #rest args, #f, otherwise the rest arg type.
  slot rest-type :: false-or(<ctype>),
    init-value: #f, init-keyword: rest-type:;

  // List of <key-info>s describing the specified keyword args.  #f if #key was
  // not specified.
  slot key-infos :: false-or(<list>),
    init-value: #f, init-keyword: keys:;
  slot all-keys? :: <boolean>,
    init-value: #f, init-keyword: all-keys:;

  // <values-ctype> representing the result types.
  slot returns :: <values-ctype>, init-keyword: returns:,
    init-function: wild-ctype;

end;

define method print-object (sig :: <signature>, stream :: <stream>) => ();
  pprint-fields(sig, stream,
		specializers: sig.specializers,
		sig.rest-type & (rest-type:), sig.rest-type,
		sig.key-infos & (key-infos:), sig.key-infos,
		sig.all-keys? & (all-keys:), #t,
		returns:, sig.returns);
end;

add-make-dumper(#"function-signature", *compiler-dispatcher*, <signature>,
  list(specializers, specializers:, #f,
       next?, next:, #f,
       rest-type, rest-type:, #f,
       key-infos, keys:, #f,
       all-keys?, all-keys:, #f,
       returns, returns:, #f)
);


define class <key-info> (<object>)

  // name of this keyword arg.
  slot key-name :: <symbol>, required-init-keyword: key-name:;

  // type restriction.
  slot key-type :: <ctype>, required-init-keyword: type:;

  // True if a required keyword.  Keywords can be required in two different
  // ways: if the default is known to not be of the correct type or a 
  // required-init-keyword: in a slot decl.  Note: this is an advisory flag
  // only.  Calls that are missing the keyword will still be generated.
  // But the compiler can use this flag to decide to report a warning.
  slot required? :: <boolean>,
    init-value: #f, init-keyword: required:;

  // The default, if it is a compile-time constant.  Otherwise, #f.
  slot key-default :: false-or(<ct-value>),
    init-value: #f, init-keyword: default:;
end;

define method print-object (key :: <key-info>, stream :: <stream>) => ();
  pprint-fields(key, stream,
		name: key.key-name,
		type: key.key-type,
		required: key.required?,
		default: key.key-default);
end;

define method key-needs-supplied?-var (key-info :: <key-info>)
    => res :: <boolean>;
  if (key-info.key-default)
    #f;
  else
    let rep = pick-representation(key-info.key-type, #"speed");
    ~rep.representation-has-bottom-value?;
  end;
end;

add-make-dumper(#"function-key-info", *compiler-dispatcher*, <key-info>,
  list(key-name, key-name:, #f,
       key-type, type:, #f,
       required?, required:, #f,
       key-default, default:, #f)
);

// Seals for file signature.dylan

// <signature> -- subclass of <object>
define sealed domain make(singleton(<signature>));
define sealed domain initialize(<signature>);
// <key-info> -- subclass of <object>
define sealed domain make(singleton(<key-info>));
define sealed domain initialize(<key-info>);
