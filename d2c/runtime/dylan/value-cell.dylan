rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/value-cell.dylan,v 1.4 1996/03/17 00:11:23 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera

/*

define open primary abstract class <value-cell> (<object>)
end;

define class <limited-value-cell-info> (<object>)
  slot lvci-type :: <type>, required-init-keyword: type:;
  slot lvci-class :: <class>, required-init-keyword: class:;
  slot lvci-next :: type-union(<limited-value-cell-info>, <false>),
    required-init-keyword: next:;
end;

define variable *limited-value-cells*
  :: type-union(<limited-value-cell-info>, <false>)
  = #f;

define method limited (class == <value-cell>, #key type :: <type>)
  block (return)
    for (entry = *limited-value-cells* then entry.lvci-next,
	 while: entry)
      if (subtype?(type, entry.lvci-type) & subtype?(entry.lvci-type, type))
	return(entry.lvci-class);
      end;
    end;
    let new = make(<class>, superclasses: <value-cell>,
		   slots: vector(vector(getter: value, setter: value-setter,
					type: type,
					required-init-keyword: value:)));
    *limited-value-cells*
      := make(<limited-value-cell-info>, type: type, class: new,
	      next: *limited-value-cells*);
    new;
  end;
end;

*/

define class <value-cell> (<object>)
  slot value, required-init-keyword: value:
end;

define sealed domain make (singleton(<value-cell>));
define sealed domain initialize (<value-cell>);

