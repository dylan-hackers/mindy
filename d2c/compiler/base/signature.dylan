Module: signature
Description: Method/GF signatures and operations on them
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/signature.dylan,v 1.5 1995/05/18 13:26:45 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

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

define class <key-info> (<object>)

  // name of this keyword arg.
  slot key-name :: <symbol>, required-init-keyword: key-name:;

  // type restriction.
  slot key-type :: <ctype>, required-init-keyword: type:;

  // true if a required keyword.
  // ??? if this means anything, it means the non-strictly-Dylan
  // concept of keywords that are effictively required, e.g. due to an
  // error-default.  Or a required-init-keyword on a make method?
  slot required? :: <boolean>,
    init-value: #f, init-keyword: required:;

  // The default, if it is a compile-time constant.  Otherwise, #f.
  slot key-default :: union(<false>, <ct-value>),
    init-value: #f, init-keyword: default:;
  //
  // #t if this keyword has an explicit supplied? variable after it.  Only
  // needed if there is no key-default and the key-type doesn't allow for
  // a bottom value.
  slot key-supplied?-var :: <boolean>,
    init-value: #f, init-keyword: supplied?-var:;
end;

define method print-object (key :: <key-info>, stream :: <stream>) => ();
  pprint-fields(key, stream,
		name: key.key-name,
		type: key.key-type,
		required: key.required?,
		default: key.key-default);
end;

/* 

sorted-statically-applicable-methods methods operation
    See what compiler methods are statically applicable to operation.  This
    basically deals with official Dylan method selection, looking only at the
    specializers (but if some of them are funny types like direct-instance,
    that will be taken into consideration.)  Some specializers may be
    duplicated between builtin methods and other methods.  Builtin methods will
    be sorted before non-builtin methods having the same specializers.  We
    return #(), #F if type uncertainty prevented us from determining
    applicability or sorting.


congruent-signatures? gf-sig meth-sig
    True if a method signature is/might-be congruent to a GF signature.

signatures-equal? sig1 sig2
    True if two signatures are identical.  Probably not worth hash-consing
    signatures.  Not an = method because we want to do funny stuff
    to indicate any confusion or differnce we come across.

legal-signature? signature operation output-type
    True if a signature is/might-be legal for an operation, assuming that
    specializers are applicable.  This deals with the possibility that
    it would be an error to actually call due to illegal keywords, missing
    keywords or bad key or rest types.
*/

