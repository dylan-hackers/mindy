Module:   c-ffi
Author:   Eric Kidd
Synopsis: An implementation of the low-level Harlequin C API, as faked on
          top of the low-level Gwydion API.

//=========================================================================
//  Primitive Designator Types
//=========================================================================
//  These are the top-level designator types. None of are instantiable,
//  and none of them have input or output mappings.

define sealed abstract designator-class <C-value> (<object>)
end designator-class <C-value>;

define sealed abstract designator-class <C-void> (<C-value>)
end designator-class <C-void>;

define sealed abstract designator-class <C-number> (<C-value>)
end designator-class <C-number>;

//=========================================================================
//  Pointer Designator Types
//=========================================================================
//  These designator types represent pointers. All but the top-level types
//  are directly instantiable and map to themselves for import and export.

define primary open abstract designator-class <C-pointer> (<C-value>)
  options c-rep: #"ptr";
  // XXX - Why is this class open? It has two complete, mutually
  // exclusive subclasses which could be open.
  constant slot %pointer-address :: <raw-pointer>,
    required-init-keyword: %address:;
end designator-class <C-pointer>;

define function pointer-address
    (C-pointer :: <C-pointer>)
 => (address :: <machine-word>);
  as(<machine-word>, C-pointer.%pointer-address);
end function;

define function pointer-cast
    (class :: <designator-class>, pointer :: <C-pointer>)
 => (cast-pointer :: <C-pointer>)
  make(class, %address: pointer.%pointer-address);
end function;

define function null-pointer
    (class :: subclass(<C-pointer>))
 => (null-pointer :: <C-pointer>)
  make(class, %address: as(<raw-pointer>, 0));
end function;

define function null-pointer?
    (pointer :: <C-pointer>)
 => (null? :: <boolean>)
  pointer.pointer-address == as(<raw-pointer>, 0);
end function;

define open designator-class <C-void*> (<C-pointer>)
  options c-rep: #"ptr",
          referenced-type: <C-void>,
          pointer-type-superclass: <C-statically-typed-pointer>;
end designator-class;

define open abstract designator-class <C-statically-typed-pointer>
    (<C-pointer>, <mutable-sequence>)
  options c-rep: #"ptr";
end designator-class <C-statically-typed-pointer>;

// XXX - This doesn't define the concrete version
// need a primitive or an "instantiable" adjective to designator-class-definer
define macro C-pointer-type-definer
  { define C-pointer-type ?pointer-class:name => ?designator-class:expression }
    => { define open abstract designator-class ?pointer-class
             (<C-statically-typed-pointer>)
           options referenced-type: ?designator-class,
                   c-rep: #"ptr",
                   pointer-type-superclass: <C-statically-typed-pointer>;
	 end designator-class; }
end macro;

define open generic c-type-cast
    (type :: <class>, value :: <object>)
 => (value :: <object>);

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
    (pointer :: <C-statically-typed-pointer>, index :: <integer>, #key default)
 => (value :: <object>)
  pointer-value(pointer, index: index);
end method element;

define inline method element-setter
    (new-value :: <object>, pointer :: <C-statically-typed-pointer>,
     index :: <integer>)
 => (new-value :: <object>)
  pointer-value(pointer, index: index) := new-value;
end method element-setter;

define inline method \=
    (p1 :: <C-pointer>, p2 :: <C-pointer>)
 => (equal? :: <boolean>)
  p1.%pointer-address = p2.%pointer-address;
end method;

define inline method \<
    (p1 :: <C-pointer>, p2 :: <C-pointer>)
 => (equal? :: <boolean>)
  p1.%pointer-address < p2.%pointer-address;
end method;

//=========================================================================
//  Pointer Dereferencing Operators
//=========================================================================

define sealed inline method C-char-at
    (ptr :: <C-pointer>,
     #key byte-index :: <integer> = 0, scaled-index :: <integer> = 0)
 => (result :: <machine-word>);
  as(<machine-word>,
     pointer-deref(char:, ptr.%pointer-address, byte-index + scaled-index));
end method C-char-at;

define sealed inline method C-char-at-setter
    (new :: <machine-word>, ptr :: <C-pointer>,
     #key byte-index :: <integer> = 0, scaled-index :: <integer> = 0)
 => (result :: <machine-word>);
  pointer-deref(char:, ptr.%pointer-address, byte-index + scaled-index)
    := as(<integer>, new);
  new;
end method C-char-at-setter;

//=========================================================================
//  Structs and Unions
//=========================================================================
//  These designator classes correspond to structures and unions. Note that
//  a structure or union type designator will import and export as the
//  corresponding pointer type, since the structure and union types are
//  not instantiable.

define open abstract designator-class <C-struct> (<C-value>)
  // no additional slots
end designator-class <C-struct>;

define open abstract designator-class <C-union> (<C-value>)
  // no additional slots
end designator-class <C-union>;

define macro C-struct-definer
  { define C-struct ?:name
      ?slots
      ?type-options
    end }
    => { define abstract designator-class ?name (<C-struct>)
           options pointer-type-superclass: <C-statically-typed-pointer>,
                   ?type-options;
           ?slots
         end designator-class; }
end macro;

define macro C-union-definer
  { define C-union ?:name
      /* slot specs and options */
    end }
    => { /* XXX - need expansion */ }
end macro;

//=========================================================================
//  Defining Types
//=========================================================================
//  Macros for creating new designator types.

define macro C-subtype-definer
  { define ?modifiers:* C-subtype ?:name (?superclasses:*)
      /* XXX - need slots and property list */
      ?FIXME:*
    end }
    => { /* XXX - need expansion */ }
end macro;

define macro C-mapped-subtype-definer
  { define ?modifiers:* C-mapped-subtype ?:name (?superclasses:*)
      /* XXX - a bunch of messy clauses */
      ?FIXME:*
    end }
    => { /* XXX - need expansion */ }
end macro;

//=========================================================================
//  C functions
//=========================================================================
//  Using Dylan functions from C, and vice versa.

define open abstract designator-class <C-function-pointer> (<C-pointer>)
  // no additional slots
end designator-class;

define macro C-function-definer
  { define C-function ?:name
      ?FIXME:*
    end }
    => { /* XXX need expansion */}
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
    { define C-variable ?:name :: ?type:expression,
	  #key ?setter:expression = #f,
	       ?c-name:expression = #f,
	       ?import:expression = #f
      end }
      => { /* XXX need expansion */ }
end macro;

define macro C-address-definer
    { define C-address ?:name :: ?type:expression
        ?options
      end }
      => { /* XXX need expansion */ }

  options:
    { #rest ?all:*,
      #key ?c-name:expression = #f,
           ?import:expression = #f }
      => { ?all }
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
     #next next-method,
     #key allocator = #f /* XXX malloc */, element-count = 1,
          extra-bytes = 0, address,
     #all-keys)
 => (pointer :: <C-pointer>)
  if (address)
    next-method(class, %address: as(<raw-pointer>, address));
  else
    let memory :: <machine-word> =
      allocator(size-of(class.referenced-type) * element-count + extra-bytes);
    next-method(class, %address: memory);
  end if;
end method;

define open generic destroy
    (pointer :: <C-pointer>, #key de-allocator)
 => ();

define method destroy
    (pointer :: <C-pointer>, #key de-allocator = #f /* XXX free */)
 => ();
  de-allocator(pointer.pointer-address);
end method destroy;

define macro with-stack-structure
  /* XXX - double check type specifier rules f*r macros, keys */
  { with-stack-structure (?:name :: ?type:name,
			  #key ?element-count = 1, ?extra-bytes = 0)
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
  element($C-Dylan-object-table, object)
    := element($C-Dylan-object-table, object, default: 0) + 1;
end function;

define function unregister-C-Dylan-object (object :: <object>) => ()
  if((element($C-Dylan-object-table, object)
        := element($C-Dylan-object-table, object) - 1) = 0)
    remove-key!($C-Dylan-object-table, object);
  end if;
end function;

/* XXX - Need export-C-Dylan-object, import-C-Dylan-object */
