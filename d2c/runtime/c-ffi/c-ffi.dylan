Module:   c-ffi
Author:   Eric Kidd
Synopsis: An implementation of the low-level Harlequin C API, as faked on
          top of the low-level Gwydion API.

//=========================================================================
//  <machine-word>
//=========================================================================
//  Must be able to represent either a "long" or a "void*". We implement
//  this using d2c's internal <raw-pointer> type. I don't know how we'll
//  implement 64-bit values on 32-bit systems.

define sealed functional class <machine-word> (<object>)
  slot machine-word-value :: <raw-pointer>,
    required-init-keyword: value:;
end class <machine-word>;

define inline function %as-machine-word
    (value :: <integer>)
 => (result :: <machine-word>)
  make(<machine-word>, value: as(<raw-pointer>, value));
end function;

define inline function %as-integer
    (machine-word :: <machine-word>)
 => (value :: <integer>)
  as(<integer>, machine-word.machine-word-value);
end function;

define inline method \=
    (m1 :: <machine-word>, m2 :: <machine-word>)
 => (equal? :: <boolean>)
  m1.machine-word-value == m2.machine-word-value;
end method;

//=========================================================================
//  Primitive Designator Types
//=========================================================================
//  These are the top-level designator types. None of are instantiable,
//  and none of them have input or output mappings.

define sealed abstract class <C-value> (<object>)
end class <C-value>;

define sealed abstract class <C-void> (<C-value>)
end class <C-void>;

/* XXX - size-of, alignment-of really require each-subclass slots */

define sealed abstract class <C-number> (<C-value>)
end class <C-number>;

//=========================================================================
//  Pointer Designator Types
//=========================================================================
//  These designator types represent pointers. All but the top-level types
//  are directly instantiable and map to themselves for import and export.

define primary open abstract class <C-pointer> (<C-value>)
  // XXX - Why is this class open? It has two complete, mutually
  // exclusive subclasses which could be open.
  slot pointer-address :: <machine-word>,
    required-init-keyword: %address:,
    setter: #f;
end class <C-pointer>;

define function pointer-cast
    (class :: subclass(<C-pointer>), pointer :: <C-pointer>)
 => (cast-pointer :: <C-pointer)
  make(class, %address: pointer.pointer-address);
end function;

define function null-pointer
    (class :: subclass(<C-pointer>))
 => (null-pointer :: <C-pointer>)
  make(class, %address: %as-machine-word(0));
end function;

define function null-pointer?
    (pointer :: <C-pointer>)
 => (null? :: <boolean>)
  pointer.pointer-address = %as-machine-word(0);
end function;

define open class <C-void*> (<C-pointer>)
end class <C-void*>;

define open abstract class <C-statically-typed-pointer> (<C-pointer>)
end class <C-statically-typed-pointer>;

define macro C-pointer-type-definer
  { define C-pointer-type ?pointer-class:name => ?designator-class:name; }
    => { /* XXX - How on earth do I return previously existing types? */ }
end macro;

/* XXX - referenced-type really requires each-subclass slots */

define open generic pointer-value
    (pointer :: <C-statically-typed-pointer>, #key index :: <integer>)
 => (value :: <object>);

define open generic pointer-value-setter
    (new-value :: <object>, pointer :: <C-statically-typed-pointer>,
     #key index :: <integer>)
 => (new-value :: <object>);

define open generic pointer-value-address
    (pointer :: <C-statically-typed-pointer>, #key index)
 => (offset-pointer :: <C-statically-typed-pointer>);

define inline method element
    (pointer :: <C-statically-typed-pointer>, index :: <integer>)
 => (value :: <object>)
  pointer-value(pointer, index: index);
end method element;

define inline method element-setter
    (new-value :: <object>, pointer :: <C-statically-typed-pointer>,
     index :: <integer>)
 => (new-value :: <object>)
  pointer-value(pointer, index: index) := new-value;
end method element;

define inline method \=
    (p1 :: <C-pointer>, p2 :: <C-pointer>)
 => (equal? :: <boolean>)
  p1.pointer-address = p2.pointer-address;
end method;

/* XXX - How should I define "\<"? */

//=========================================================================
//  Defining Types
//=========================================================================
//  Macros for creating new designator types.

define macro C-subtype-definer
  { define ?modifiers:* C-subtype ?:name (?superclasses:*)
      /* XXX - need slots and property list */
    end }
    => { /* XXX - need expansion */ }
end macro;

define macro C-mapped-subtype-definer
  { define ?modifiers:* C-mapped-subtype ?:name (?superclasses:*)
      /* XXX - a bunch of messy clauses */
    end }
    => { /* XXX - need expansion */ }
end macro;

//=========================================================================
//  Structs and Unions
//=========================================================================
//  These designator classes correspond to structures and unions. Note that
//  a structure or union type designator will import and export as the
//  corresponding pointer type, since the structure and union types are
//  not instantiable.

define open abstract class <C-struct> (<C-value>)
end class <C-struct>;

define open abstract class <C-union> (<C-value>)
end class <C-union>;

define macro C-struct-definer
  { define C-struct ?:name
      /* slot specs and options */
    end; }
    => { /* XXX - need expansion */ }
end macro;

define macro C-union-definer
  { define C-union ?:name
      /* slot specs and options */
    end; }
    => { /* XXX - need expansion */ }
end macro;

//=========================================================================
//  C functions
//=========================================================================
//  Using Dylan functions from C, and vice versa.

define macro C-function-definer
  { define C-function ?:name
      /* paramters and results */
    end }
    => { /* XXX - need expansion */ }
end macro;

define macro C-callable-wrapper-definer
  { define C-callable-wrapper ?:name
      /* paramters and results */
    end }
    => { /* XXX - need expansion */ }
end macro;

//=========================================================================
//  C Variables
//=========================================================================
//  Access to C globals.

define macro C-variable-definer
  /* XXX - Need implementation. */
end macro;

define macro C-address-definer
  /* XXX - Need implementation. */
end macro;

//=========================================================================
//  C Storage
//=========================================================================
//  Wrappers for malloc and free.

define C-function malloc
  /* XXX - Need <C-size-t> and <C-raw-pointer> */
  parameter size :: <C-size-t>;
  result memory :: <C-raw-pointer>;
  c-name: "malloc";
end;

define C-function free
  parameter pointer :: <C-pointer>;
  c-name: "free";
end;

/* XXX - can we *please* seal this? */
define sealed inline method make
    (class :: subclass(<C-pointer>),
     #key allocator = malloc, element-count = 1, extra-bytes = 0, address,
     #next next-method, #all-keys)
 => (pointer :: <C-pointer>)
  if (address)
    next-method(class, %address: address);
  else
    let memory :: <machine-word> =
      allocator(size-of(class.referenced-type) * element-count + extra-bytes);
    next-method(class, %address: memory);
  end if;
end method;

define open generic destroy
    (pointer :: <C-pointer>, #key de-allocator = free)
 => ();

define method destroy
    (pointer :: <C-pointer>, #key de-allocator = free)
 => ();
  de-allocator(pointer.pointer-address);
end method destroy;

define macro with-stack-structure
  /* XXX - double check type specifier rules f*r macros, keys */
  { with-stack-structure (?:name :: ?type:name,
			  #key element-count = 1, extra-bytes = 0)
      ?:body
    end }
    => { let ?name = make(?type,
			  element-count: ?element-count,
			  extra-bytes: ?extra-bytes);
	 block ()
	   ?body
	 cleanup
	   destroy(?name);
	 end }
end macro;

//=========================================================================
//  Utility designator classes
//=========================================================================
//  Some C types can correspond to more than one Dylan type, depending
//  on the circumstances. These designator classes allow the prefered Dylan
//  type to be specified precisely.

/* XXX - double check that <C-boolean> should be a <C-int>. */
define open C-mapped-subtype <C-boolean> (<C-int>)
  map <boolean>,
    export-function: method (v :: <boolean>) => (result :: <integer>)
		       if (v) 1 else 0 end
		     end method,
    import-function: method (v :: <integer>) => (result :: <boolean>)
		       v ~= 0;
		     end method;
end;

define open C-mapped-subtype <C-string> (<C-char*>, <string>)
  /* XXX - Need implementation */
  /* XXX - Pointers to byte-strings, without copying? Yikes! */
end;

define open C-mapped-subtype <C-character> (<C-char>)
  /* XXX - Need implementation */
end;

/* XXX - Need with-c-string */

//=========================================================================
//  Memory manipulation functions.
//=========================================================================

/* XXX - Need clear-memory!, copy-bytes!, copy-into!, equal-memory? */
/* XXX - Names of copy-into!, copy-bytes! are not consistent. */

//=========================================================================
//  Passing Dylan objects to C
//=========================================================================

/* XXX - Is this the right mapping? The manual is really unclear about
   what's going on here. */
define open C-mapped-subtype <C-Dylan-object> (<C-void*>)
  map <object>
    export-function: export-C-Dylan-object,
    import-function: import-C-Dylan-object;
end;

define constant $C-Dylan-object-table = make(<table>);

define function register-C-Dylan-object (object :: <object>) => ()
  $C-Dylan-object-table[object] = #t; // Arbitrary value
end function;

define function unregister-C-Dylan-object (object :: <object>) => ()
  remove-key!($C-Dylan-object-table, object);
end function;

/* XXX - Need export-C-Dylan-object, import-C-Dylan-object */
