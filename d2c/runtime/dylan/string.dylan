module: dylan


// General string stuff.

define open abstract class <string> (<mutable-sequence>)
end;

define method \< (str1 :: <string>, str2 :: <string>) => res :: <boolean>;
  block (return)
    for (char1 in str1, char2 in str2)
      if (char1 < char2)
	return(#t);
      elseif (char2 < char1)
	return(#f);
      end;
    end;
    str1.size < str2.size;
  end;
end;

define method as-lowercase (str :: <string>)
    => res :: <string>;
  map(as-lowercase, str);
end;

define method as-lowercase! (str :: <string>)
    => res :: <string>;
  map-into(str, as-lowercase, str);
end;

define method as-uppercase (str :: <string>)
    => res :: <string>;
  map(as-uppercase, str);
end;

define method as-uppercase! (str :: <string>)
    => res :: <string>;
  map-into(str, as-uppercase, str);
end;


// Built-in strings.

/*

define variable *limited-strings* = #();

define method limited (class == <string>,
		       #key type :: <type>, fill :: <character> = ' ')
  unless (subtype?(type, <character>))
    error("Limited strings can only hold subtypes of <character>.");
  end;
  block (return)
    for (entry in *limited-vectors*)
      if (subtype?(type, entry.head) & subtype?(entry.head, type))
	return(entry.tail);
      end;
    end;
    let new = make(<class>, superclasses: list(<builtin-vector>, <string>),
		   slots: vector(vector(getter: %element,
					setter: element-setter,
					type: type, init-keyword: fill:,
					init-value: fill,
					size: size,
					size-init-keyword: size:)));
    *limited-vectors* := pair(pair(type, new), *limited-vectors*);
    new;
  end;
end;

define constant <byte-string>
  = limited(<string>, type: <byte-character>);

define constant <unicode-string>
  = limited(<string>, type: <character>);

*/

define class <unicode-string> (<builtin-vector>, <string>)
  sealed slot %element :: <character>,
    init-value: ' ', init-keyword: fill:,
    sizer: size, required-size-init-keyword: size:;
end;

seal generic make (singleton(<unicode-string>));
seal generic initialize (<unicode-string>);

define class <byte-string> (<builtin-vector>, <string>)
  sealed slot %element :: <byte-character>,
    init-value: ' ', init-keyword: fill:,
    sizer: size, required-size-init-keyword: size:;
end;

seal generic make (singleton(<byte-string>));
seal generic initialize (<byte-string>);

define sealed inline method element
    (vec :: <byte-string>, index :: <fixed-integer>,
     #key default = $not-supplied)
    => element :: <byte-character>;
  if (index >= 0 & index < vec.size)
    %element(vec, index);
  elseif (default == $not-supplied)
    element-error(vec, index);
  else
    default;
  end;
end;

define sealed inline method element-setter
    (new-value :: <byte-character>, vec :: <byte-string>, index :: <fixed-integer>)
    => new-value :: <object>;
  if (index >= 0 & index < vec.size)
    %element(vec, index) := new-value;
  else
    element-error(vec, index);
  end;
end;

define method copy-sequence
    (vector :: <byte-string>, #key start :: <fixed-integer> = 0, end: last)
 => (result :: <byte-string>);
  let src-sz :: <fixed-integer> = size(vector);
  let last :: <fixed-integer>
    = if (last & last < src-sz) last else src-sz end if;
  let start :: <fixed-integer> = if (start < 0) 0 else start end if;
  let sz :: <fixed-integer> = last - start;

  let result :: <byte-string> = make(<byte-string>, size: sz);
  for (from-index :: <fixed-integer> from start below last,
       to-index :: <fixed-integer> from 0)
    %element(result, to-index) := %element(vector, from-index);
  end for;
  result;
end method copy-sequence;
