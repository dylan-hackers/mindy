module: dylan

// <object> -- exported.
//
// We have to use define %%class, because define class insists on there being
// at least one superclass.
//
define primary abstract open %%class <object> ()
  //
  // The class of the instance.  Non-abstract classes automatically override
  // the init-value to be the correct class.
  slot %object-class, type: <class>, setter: #f, init-value: <object>
end;

// object-class -- exported function.
//
// Return the class of thing.  We don't just call the slot object-class because
// we don't want people outside the dylan module to be able to name the slot
// (e.g. add their own override) which would just confuse the compiler.
//
define inline method object-class (thing :: <object>)
    => res :: <class>;
  %object-class(thing);
end;

define abstract open class <functional-object> (<object>)
end;
