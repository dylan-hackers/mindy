Module:    patchup
Author:    Eric Kidd
Copyright: 2000, Gwydion Dylan Maintainers
Synopsis:  Handle tricky forward references on behalf of the heap builder.
           We do this by maintaining a list of "patchup" requests and
           generating code for them later.

// This doesn't build yet. I've never even compiled it. Just think of
// it as really detailed notes. :-)
// We need modules flow, front & the builder stuff. What else?


//=========================================================================
//  <patchup-request>
//=========================================================================
//  When dumping the heap, we may encounter an object which isn't ready
//  for dumping yet. We have two choices:
//    1) Delay dumping the object until the global heap.
//    2) Dump as much of the object as we can now, and patch things
//       up at runtime.
//  Choice (1) is bad because it bloats the global heap. Choice (2) is
//  bad because it slows down application startup. So we need to make the
//  tradeoff appropriately in each case.
//
//  This class represents an abstract request to patch something up.
//  Various subclasses do all the work.

define /* public */ abstract class <patchup-request> (<object>)
end class <patchup-request>;


//=========================================================================
//  build-patchup-function
//=========================================================================
//  Given a component, a function name, and a list of patchup requests,
//  create an appropriately-named function containing all the necessary
//  patchup code. (You'll definitely want to call 'optimize-component' on
//  the component afterwards.)
//
//  This function *will* generate lots of stuff that needs to be properly
//  dumped in the global heap.
//  
//  Most of this function body was cribbed from build-command-line-entry
//  in main/main.dylan.

define function build-patchup-function
    (component :: <component>,
     name :: <name>,
     patchup-requests :: <simple-object-vector>)
 => ()
  let builder = make-builder(component);
  let source = make(<source-location>);
  let policy = $Default-Policy;
  let result-type = make-values-ctype(#(), #f);
  let func
    = build-function-body
        (builder, policy, source, #f, name, #(), result-type, #t);

  // Emit code for each patchup request.
  for (req in patchup-requests)
    emit-patchup-code(builder, policy, source, req);
  end for;

  end-body(builder);  
  let sig = make(<signature>, specializers: #(), returns: result-type);
  let ctv = make(<ct-function>, name: name, signature: sig);
  make-function-literal(builder, ctv, #"function", #"global", sig, func);
end function build-patchup-function;


//=========================================================================
//  emit-patchup-code
//=========================================================================
//  Our workhorse. This generic function dumps the FER code for one
//  patchup.

define generic emit-patchup-code
    (builder :: <builder>, policy :: <policy>, source :: <source-location>,
     req :: <patchup-request>)
 => ();


//=========================================================================
//  <slot-patchup-request>
//=========================================================================
//  Install a value into an instance-slot at run time.

define class <slot-patchup-request> (<patchup-request>)
  // The <ct-value> representing the instance we need to patch up.
  slot patchup-instance :: <ct-value>,
    required-init-keyword: instance:;

  // A function which we can apply to 'patchup-instance' (in the compiler,
  // not the runtime) to get the value we should store. It should
  // return a <ct-value>, not a bare Dylan value.
  // We represent this with a symbol, because we can't dump functions
  // into our *.du files. See 'register-patchup-slot-getter' below.
  slot patchup-slot-compiler-getter :: <symbol>,
    required-init-keyword: compiler-getter:;

  // The setter function which will do the patchup (in the runtime).
  // You can normally get a value for this by calling
  // 'dylan-defn(#"slotname-setter")'.
  slot patchup-slot-runtime-setter :: <definition>,
    required-init-keyword: runtime-setter:;
end class <slot-patchup-request>;

// Map symbols to getter functions.
define constant $Patchup-Slot-Getters = make(<table>);

// Call this function to define a mapping between symbols in the
// 'patchup-slot-compiler-getter' slot and actual getter functions.
define function register-patchup-slot-getter
    (name :: <symbol>, getter :: <function>) => ()
  $Patchup-Slot-Getters[name] := getter;
end function register-patchup-slot-getter;

// Generate code to assign a value into a slot.
define method emit-patchup-code
    (builder :: <builder>, policy :: <policy>, source :: <source-location>,
     req :: <slot-patchup-request>)
 => ()
  let instance = req.patchup-instance;
  let getter = $Patchup-Slot-Getters[req.patchup-slot-compiler-getter];
  let value = instance.getter;
  let setter = build-defn-ref(builder, policy, source,
			      req.patchup-slot-runtime-setter);

  // Generate a call to the appropriate slot setter, and throw away the
  // return value.
  build-assignment(builder, policy, source, #(),
		   make-unknown-call(builder, setter, #f,
				     list(value, instance)));
end method emit-patchup-code;
