copyright: see below
module: dylan-viscera

//======================================================================
//
// Copyright (c) 2003  Gwydion Dylan Maintainers
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

//  This file implements sets


// <set>

define open abstract primary class <set>  (<mutable-explicit-key-collection>)
  // no slots
end class <set>;

define sealed method make
    (class == <set>, #rest key-value-pairs, #key, #all-keys)
 => (set :: <object-set>);
  apply(make, <object-set>, key-value-pairs);
end method make;


// <object-set>

define sealed primary class <object-set> (<set>)
  slot set-elements :: <object-table>;
end class <object-set>;

define sealed method key-test(set :: <object-set>) => (key-test :: <function>);
  \==;
end method;

define sealed method remove-all-keys!
    (set :: <object-set>)
 => (set :: <object-set>);
  remove-all-keys!(set.set-elements);
  set;
end;

define sealed method remove!
    (set :: <object-set>, object :: <object>, #key test, count)
 => (set :: <object-set>);
  ignore(test);
  ignore(count);
  remove-key!(set.set-elements, object);
  set;
end method;

define sealed method add!
    (set :: <object-set>, object :: <object>)
 => (set :: <object-set>);
  set.set-elements[object] := object;
  set;
end method;

define sealed method element
    (set :: <object-set>, key :: <object>, #rest keys, #key default)
 => (object :: <object>);
  apply(element, set.set-elements, key, keys);
end method;

define sealed method element-setter
    (value :: <object>, set :: <object-set>, key :: <object>)
 => (value :: <object>);
  ignore(key);
  set.set-elements[value] := value;
end method;

define sealed method member?
    (object :: <object>, set :: <object-set>, #key test)
 => (bool :: <boolean>);
  ignore(test);
  element(set.set-elements, object, default: $not-supplied) == object;
end method;

define sealed method initialize
    (set :: <object-set>, #next next-method, #key size)
 => (#rest results);
  if(size)
    set.set-elements := make(<object-table>, size: size);
  else
    set.set-elements := make(<object-table>);
  end if;
end method;

define sealed method forward-iteration-protocol
    (set :: <object-set>)
 => (initial-state :: <table-iterator>,
     limit :: <integer>,
     next-state :: <function>,
     finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>,
     current-element-setter :: <function>,
     copy-state :: <function>);
  let (initial-state :: <table-iterator>,
     limit :: <integer>,
     next-state :: <function>,
     finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>,
     current-element-setter :: <function>,
     copy-state :: <function>) = forward-iteration-protocol(set.set-elements);
  values(initial-state,
         limit,
         next-state,
         finished-state?,
         current-key,
         current-element,
         method
             (value :: <object>, ht :: <table>, state :: <table-iterator>)
          => (value :: <object>);
           error("current-element-setter not supported for <object-set>");
         end method,
         copy-state);
end method;