Module: type-dump
Description: OD dump/load methods for type system
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/type-dump.dylan,v 1.14 1996/02/16 03:37:28 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.


// Non-class types:


define method dump-od (obj :: <union-ctype>, buf :: <dump-state>) => ();
  dump-simple-object(#"union-type", buf, obj.members);
end method;

add-od-loader(*compiler-dispatcher*, #"union-type",
  method (state :: <load-state>) => res :: <union-ctype>;
    reduce(ctype-union, empty-ctype(), load-sole-subobject(state));
  end method
);


// ### maybe should just drop the type exp, or reduce it to something dumpable.
//
define method dump-od (obj :: <unknown-ctype>, buf :: <dump-state>) => ();
  dump-simple-object(#"unknown-type", buf, obj.type-exp);
end method;

add-od-loader(*compiler-dispatcher*, #"unknown-type",
  method (state :: <load-state>) => res :: <unknown-ctype>;
    make(<unknown-ctype>, type-exp: load-sole-subobject(state));
  end method
);


define method dump-od (obj :: <limited-integer-ctype>, buf :: <dump-state>)
 => ();
  dump-simple-object(#"limited-integer-type", buf, obj.base-class,
  		     obj.low-bound, obj.high-bound);
end method;

add-od-loader(*compiler-dispatcher*, #"limited-integer-type",
  method (state :: <load-state>) => res :: <limited-integer-ctype>;
    let base = load-object-dispatch(state);
    let low = load-object-dispatch(state);
    let high = load-object-dispatch(state);
    assert-end-object(state);
    make(<limited-integer-ctype>, base-class: base, low-bound: low,
         high-bound: high);
  end method
);


define method dump-od (obj :: <direct-instance-ctype>, buf :: <dump-state>)
 => ();
  dump-simple-object(#"direct-instance-type", buf, obj.base-class);
end method;

add-od-loader(*compiler-dispatcher*, #"direct-instance-type",
  method (state :: <load-state>) => res :: <direct-instance-ctype>;
    direct-type(load-sole-subobject(state), loading?: #t);
  end method
);


define method dump-od (obj :: <singleton-ctype>, buf :: <dump-state>)
 => ();
  dump-simple-object(#"singleton-type", buf,
		     obj.base-class, obj.singleton-value);
end method;

add-od-loader(*compiler-dispatcher*, #"singleton-type",
  method (state :: <load-state>) => res :: <singleton-ctype>;
    let base-class = load-object-dispatch(state);
    let object = load-object-dispatch(state);
    assert-end-object(state);
    make-canonical-singleton(object, base-class: base-class);
  end method
);


define method dump-od (obj :: <byte-character-ctype>, buf :: <dump-state>)
 => ();
  dump-simple-object(#"byte-character-type", buf, obj.base-class);
end method;

add-od-loader(*compiler-dispatcher*, #"byte-character-type",
  method (state :: <load-state>) => res :: <byte-character-ctype>;
    make(<byte-character-ctype>, base-class: load-sole-subobject(state));
  end method
);


define method dump-od (obj :: <multi-value-ctype>, buf :: <dump-state>)
 => ();
  dump-simple-object(#"multi-value-type", buf, obj.positional-types,
  		     obj.min-values, obj.rest-value-type);
end method;

add-od-loader(*compiler-dispatcher*, #"multi-value-type",
  method (state :: <load-state>) => res :: <multi-value-ctype>;
    let positional = load-object-dispatch(state);
    let min-val = load-object-dispatch(state);
    let rest-type = load-object-dispatch(state);
    assert-end-object(state);
    make(<multi-value-ctype>, positional-types: positional,
         min-values: min-val, rest-value-type: rest-type);
  end method
);


// Classes:

define constant $class-dump-slots =
  list(info, #f, info-setter,
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
       speed-representation,speed-representation:,speed-representation-setter,
       space-representation,space-representation:,space-representation-setter,
       each-subclass-slots-count, each-subclass-slots-count:, #f);


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
    $slot-info-dump-slots,
    list(slot-positions, slot-positions:, #f,
	 slot-initialized?-slot, slot-initialized?-slot:, #f)),
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
    $slot-info-dump-slots,
    list(slot-positions, slot-positions:, #f)),
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

add-make-dumper(#"layout-table", *compiler-dispatcher*,
  <layout-table>,
  list(layout-length, length:, #f,
       layout-holes, holes:, #f)
);

add-make-dumper(#"defined-class", *compiler-dispatcher*, <defined-cclass>,
  $class-dump-slots,
  load-external: #t
);

add-make-dumper(#"limited-class", *compiler-dispatcher*, <limited-cclass>,
		$class-dump-slots, load-external: #t);

add-make-dumper(#"class-proxy", *compiler-dispatcher*, <proxy>,
  list(proxy-for, for:, #f),
  load-external: #t
);

