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
