Module: type-dump
Description: OD dump/load methods for type system
rcs-header: $Header: /scm/cvs/src/d2c/compiler/base/type-dump.dylan,v 1.1 1998/05/03 19:55:31 andreas Exp $
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

// Non-class types:

define method set-or-check-extent
    (extent :: false-or(<values-ctype>), intent :: <values-ctype>) => ();
  if (extent)
    if (intent.%ctype-extent)
      assert(intent.%ctype-extent == extent);
    else
      intent.%ctype-extent := extent;
    end if;
  end if;
end method set-or-check-extent;


add-make-dumper
  (#"union-type", *compiler-dispatcher*, <union-ctype>,
   list(info, #f, #f,
	%ctype-extent, #f, #f,
	members, members:, #f),
   dumper-only: #t);

add-od-loader
  (*compiler-dispatcher*, #"union-type",
   method (state :: <load-state>)
       => res :: type-union(<union-ctype>, <forward-ref>);
     let info = load-object-dispatch(state);
     let extent = load-object-dispatch(state);
     let members = load-object-dispatch(state);
     assert-end-object(state);
     local method make-obj () => res :: <union-ctype>;
	     let res = make(<union-ctype>, members: map(actual-obj, members));
	     if (info.obj-resolved?)
	       merge-and-set-info(info.actual-obj, res);
	     else
	       request-backpatch
		 (info, method (x) merge-and-set-info(x, res) end);
	     end if;
	     if (extent.obj-resolved?)
	       set-or-check-extent(extent.actual-obj, res);
	     else
	       request-backpatch
		 (extent, method (x) set-or-check-extent(x, res) end);
	     end if;
	     res;
	   end method make-obj;
     let unresolved-members = 0;
     let forward = make(<forward-ref>);
     for (member in members)
       unless (member.obj-resolved?)
	 unresolved-members := unresolved-members + 1;
	 request-backpatch
	   (member,
	    method (actual) => ();
	      if ((unresolved-members := unresolved-members - 1).zero?)
		resolve-forward-ref(forward, make-obj());
	      end if;
	    end method);
       end unless;
     end for;
     if (unresolved-members.zero?)
       make-obj();
     else
       forward;
     end if;
   end method);

add-make-dumper
  (#"unknown-type", *compiler-dispatcher*, <unknown-ctype>,
   list(%ctype-extent, #f, set-or-check-extent,
	//
	// ### maybe should just drop the type exp, or reduce it to
	// something dumpable.
	type-exp, type-exp: #f));

add-make-dumper
  (#"limited-integer-type", *compiler-dispatcher*, <limited-integer-ctype>,
   list(info, #f, merge-and-set-info,
	%ctype-extent, #f, set-or-check-extent,
	base-class, base-class:, #f,
	low-bound, low-bound:, #f,
	high-bound, high-bound:, #f));

add-make-dumper
  (#"direct-instance-type", *compiler-dispatcher*, <direct-instance-ctype>,
   list(info, #f, merge-and-set-info,
	%ctype-extent, #f, set-or-check-extent,
	base-class, base-class: #f));

add-make-dumper
  (#"singleton-type", *compiler-dispatcher*, <singleton-ctype>,
   list(info, #f, merge-and-set-info,
	%ctype-extent, #f, set-or-check-extent,
	base-class, base-class:, #f,
	singleton-value, value:, #f));

add-make-dumper
  (#"byte-character-type", *compiler-dispatcher*, <byte-character-ctype>,
   list(info, #f, merge-and-set-info,
	%ctype-extent, #f, set-or-check-extent,
	base-class, base-class:, #f));

add-make-dumper
  (#"multi-value-type", *compiler-dispatcher*, <multi-value-ctype>,
   list(%ctype-extent, #f, set-or-check-extent,
	positional-types, positional-types:, #f,
	min-values, min-values:, #f,
	rest-value-type, rest-value-type:, #f));

add-make-dumper
  (#"subclass-type", *compiler-dispatcher*, <subclass-ctype>,
   list(info, #f, merge-and-set-info,
	%ctype-extent, #f, set-or-check-extent,
	base-class, base-class:, #f,
	subclass-of, of:, #f));


add-make-dumper
  (#"class-proxy", *compiler-dispatcher*, <proxy>,
   list(info, #f, merge-and-set-info,
	proxy-for, for:, #f),
   load-external: #t);


// Classes:

define constant $class-dump-slots =
  list(info, #f, info-setter,
       %ctype-extent, #f, set-or-check-extent,
       cclass-name, name:, #f,
       direct-superclasses, direct-superclasses:, #f,
       closest-primary-superclass, #f, closest-primary-superclass-setter,
       not-functional?, not-functional:, #f,
       functional?, functional: #f,
       sealed?, sealed:, #f,
       abstract?, abstract:, #f,
       primary?, primary:, #f,
       precedence-list, precedence-list:, #f,
       unique-id, #f, set-and-record-unique-id,
       subclass-id-range-min, subclass-id-range-min:, #f,
       subclass-id-range-max, subclass-id-range-max:, #f,
       direct-speed-representation, direct-speed-representation:,
         direct-speed-representation-setter,
       direct-space-representation, direct-space-representation:,
         direct-space-representation-setter,
       general-speed-representation, general-speed-representation:,
         general-speed-representation-setter,
       general-space-representation, general-space-representation:,
         general-space-representation-setter
	 /* ### -- currently recomputed, so we don't really need to dump it.
	 , each-subclass-slots-count, each-subclass-slots-count:, #f
	 */
       );


define constant $slot-info-dump-slots =
  list(info, #f, info-setter,
       slot-introduced-by, introduced-by:, #f,
       slot-type, type:, slot-type-setter,
       slot-getter, getter:, #f,
       slot-read-only?, read-only:, #f,
       slot-init-value, init-value:, slot-init-value-setter,
       slot-init-function, init-function:, #f,
       slot-init-keyword, init-keyword:, #f,
       slot-init-keyword-required?, init-keyword-required:, #f);


add-make-dumper(#"instance-slot-info", *compiler-dispatcher*,
  <instance-slot-info>,
  concatenate(
    $slot-info-dump-slots
    /* ### -- currently recomputed, so we don't really need to dump it.
    ,
    list(slot-positions, slot-positions:, #f,
	 slot-initialized?-slot, slot-initialized?-slot:, #f) */),
  load-external: #t
);


add-make-dumper(#"vector-slot-info", *compiler-dispatcher*, <vector-slot-info>,
   concatenate(
     $slot-info-dump-slots,
     list(slot-size-slot, size-slot:, slot-size-slot-setter)),
   load-external: #t
);

add-make-dumper(#"class-slot-info", *compiler-dispatcher*, <class-slot-info>,
  $slot-info-dump-slots,
  load-external: #t
);

add-make-dumper(#"each-subclass-slot-info", *compiler-dispatcher*,
  <each-subclass-slot-info>,
  concatenate(
    $slot-info-dump-slots
    /* ### -- currently recomputed, so we don't really need to dump it.
    ,
    list(slot-positions, slot-positions:, #f) */),
  load-external: #t
);

add-make-dumper(#"virtual-slot-info", *compiler-dispatcher*,
		<virtual-slot-info>, $slot-info-dump-slots,
		load-external: #t);



add-make-dumper(#"override-info", *compiler-dispatcher*,
  <override-info>,
  list(override-introduced-by, introduced-by:, override-introduced-by-setter,
       override-getter, getter:, #f,
       override-init-value, init-value:, override-init-value-setter,
       override-init-function, init-function:, override-init-function-setter),
  load-external: #t
);

/* ### -- currently recomputed, so we don't really need to dump it.
add-make-dumper(#"layout-table", *compiler-dispatcher*,
  <layout-table>,
  list(layout-length, length:, #f,
       layout-holes, holes:, #f)
);
*/

add-make-dumper(#"defined-class", *compiler-dispatcher*, <defined-cclass>,
  $class-dump-slots,
  load-external: #t
);

add-make-dumper(#"limited-class", *compiler-dispatcher*, <limited-cclass>,
		$class-dump-slots, load-external: #t);


