module: debugger

define class <foo> (<object>)
  slot bar :: <integer>;
  slot baz :: <boolean> = #t;
  slot quux = #f;
//  class slot quuux = 23;
end class <foo>;

begin
/*
  let foo = make(<foo>);
  dump-object(foo);
  foo.bar := 23;
  dump-object(foo);
  dump-object(foo.object-class);
  */
  let dll-handle = dlopen(as(<c-string>, $null-pointer), $RTLD-LAZY);
  if(dll-handle == as(<dll-handle>, $null-pointer))
    format-out("An error occurred dlopen()ing 0.\n");
  else
    let object-address =
      dlsym(dll-handle, export-value(<c-string>, 
                                     application-arguments()[0]));
    if(object-address == as(<raw-pointer>, $null-pointer))
      format-out("dlsym returned NULL.\n");
    else
      dump-object(object-at(object-address));
    end if;
  end;
end;

define method dump-object(o)
  let oc = o.object-class;
  format-out("%s at %=\n", oc.class-name, o.object-address);
  let sorted-slots = sort(oc.class-all-slot-descriptors,
                          test: method(x, y) 
                                    find-slot-offset(oc, x) <
                                    find-slot-offset(oc, y) end);
  for(slot in sorted-slots)
    format-out("%= %s :: %s == %= (%=)\n", 
               find-slot-offset(oc, slot), 
               slot.slot-name | "(unnamed)", 
               slot.slot-type.debug-name,
               generic-slot-getter(o, slot),
               slot.slot-representation);
  end for;
end method dump-object;

define method debug-name(c :: <class>)
  c.class-name;
end method debug-name;

define method debug-name(c :: <union>)
  concatenate("type-union(",
              reduce1(method(x, y) concatenate(x, ", ", y) end,
                      map(debug-name, c.union-members)),
              ")");
  // XXX: missing the singletons...
end method debug-name;

define method generic-slot-getter(o :: <object>, slot)
//  if(~slot-initialized?(o, slot.slot-getter))
//    "<uninitialized-slot>"
//  else
    select(slot.slot-representation)
      #"boolean-char" =>   
        if(pointer-deref(#"unsigned-char", o.object-address, 
                         find-slot-offset(o.object-class, slot)) = 0)
          #f
        else
          #t
        end if;
      otherwise       =>   
        format-to-string("0x%x", 
                         pointer-deref(#"long", o.object-address, 
                                       find-slot-offset(o.object-class, slot)));
      
    end;
//  end if;
end method generic-slot-getter;