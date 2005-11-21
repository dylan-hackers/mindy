module: cheese
copyright: see below

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

//
// Utilities used during optimization.
// 


// listify-dependencies -- internal.
//
// Convert a .depends-on chain into a list.
// 
define method listify-dependencies (dependencies :: false-or(<dependency>))
    => res :: <list>;
  for (res = #() then pair(dep.source-exp, res),
       dep = dependencies then dep.dependent-next,
       while: dep)
  finally
    reverse!(res);
  end;
end;



// enclosing-loop-or-function -- internal.
//
// Return the closest enclosing loop or function region.
// 
define method enclosing-loop-or-function
    (region :: <region>)
    => res :: type-union(<loop-region>, <fer-function-region>);
  if (instance?(region, <loop-region>)
	| instance?(region, <fer-function-region>))
    region;
  else
    enclosing-loop-or-function(region.parent);
  end if;
end method enclosing-loop-or-function;



// maybe-copy -- internal.
//
define generic maybe-copy
    (component :: <component>, leaf :: <leaf>, before :: <dependent-mixin>,
     ref-site :: <fer-function-region>)
    => copy :: <leaf>;

define method maybe-copy
    (component :: <component>, leaf :: <leaf>, before :: <dependent-mixin>,
     ref-site :: <fer-function-region>)
    => copy :: <leaf>;
  leaf;
end method maybe-copy;

define method maybe-copy
    (component :: <component>, leaf :: <initial-variable>,
     before :: <dependent-mixin>, ref-site :: <fer-function-region>)
    => copy :: <leaf>;
  make-copy(component, leaf, before, before.home-function-region ~== ref-site);
end method maybe-copy;
    
define method maybe-copy
    (component :: <component>, leaf :: <ssa-variable>,
     before :: <dependent-mixin>, ref-site :: <fer-function-region>)
    => copy :: <leaf>;
  if (~instance?(leaf.var-info, <lexical-var-info>)
	& before.home-function-region ~== ref-site)
    make-copy(component, leaf, before, #t);
  else
    leaf;
  end if;
end method maybe-copy;
    

define method make-copy
    (component :: <component>, leaf :: <abstract-variable>,
     before :: <dependent-mixin>, lexical? :: <boolean>)
    => copy :: <leaf>;
  let builder = make-builder(component);
  let policy = $Default-Policy;
  let source = make(<source-location>);
  if (lexical?)
    let copy = make-lexical-var(builder, leaf.var-info.debug-name, source,
				leaf.derived-type);
    build-let(builder, policy, source, copy, leaf);
    insert-before(component, before, builder-result(builder));
    copy;
  else
    let copy = make-ssa-var(builder, leaf.var-info.debug-name,
			    leaf.derived-type);
    build-assignment(builder, policy, source, copy, leaf);
    insert-before(component, before, builder-result(builder));
    copy;
  end if;
end method make-copy;



define generic only-possible-value (type :: <ctype>)
    => res :: false-or(<ct-value>);

define method only-possible-value (type :: <ctype>)
    => res :: false-or(<ct-value>);
  #f;
end method only-possible-value;

define method only-possible-value (type :: <singleton-ctype>)
    => res :: false-or(<ct-value>);
  type.singleton-value;
end method only-possible-value;

define method only-possible-value (type :: <limited-integer-ctype>)
    => res :: false-or(<ct-value>);
  if (type.low-bound & type.low-bound == type.high-bound)
    let class = type.base-class;
    if (class == specifier-type(#"<integer>"))
      make(<literal-integer>, value: type.low-bound);
    elseif (class == specifier-type(#"<extended-integer>"))
      make(<literal-extended-integer>, value: type.low-bound);
    end if;
  end if;
end method only-possible-value;

define method only-possible-value (type :: <cclass>)
    => res :: false-or(<ct-value>);
  if (type == specifier-type(#"<false>"))
    make(<literal-false>);
  elseif (type == specifier-type(#"<true>"))
    make(<literal-true>);
  elseif (type == specifier-type(#"<empty-list>"))
    make(<literal-empty-list>);
  end if;
end method only-possible-value;
