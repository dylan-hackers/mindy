module: main
author: Peter S. Housel
copyright: see below

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998-2003  Gwydion Dylan Maintainers
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
//    University, and the Gwydion Dylan Maintainers.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
// comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
// Also, see http://www.gwydiondylan.org/ for updates and documentation. 
//
//======================================================================

// do-dump-testworks-spec
//
define method do-dump-testworks-spec (state :: <lid-mode-state>) => ();
  format(*debug-output*, "Dumping testworks specification.\n");

  let spec-name = concatenate(state.unit-mprefix, "-specification.dylan");
  let spec = make(<file-stream>, locator: spec-name,
                  direction: #"output", if-exists: #"overwrite");

  format(spec, "Module: %s-test-suite\n\n", state.unit-mprefix);

  for (tlfs in state.unit-tlf-vectors)
    for (tlf in tlfs)
      dump-spec(tlf, spec);
    end;
  end;
  
  close(spec);
end method;

// dump-spec -- internal
//
define method dump-spec(tlf :: <top-level-form>, stream :: <stream>) => ();
  // Do nothing
end method;

define method dump-spec(tlf :: <define-library-tlf>, stream :: <stream>) => ();
  format(stream, "define library-spec %s ()\n",
         tlf.define-library-name.token-symbol);

  let library = tlf.define-library-library;
  do-exported-modules(library,
                      method(name :: <symbol>, module :: <module>)
                        format(stream, "  module %s;\n", name);
                      end);
  
  format(stream, "end library-spec %s;\n\n",
         tlf.define-library-name.token-symbol);
end method;

define method dump-spec(tlf :: <define-module-tlf>, stream :: <stream>) => ();
  let module = tlf.define-module-module;

  format(stream, "define module-spec %s ()\n",
         tlf.define-module-name.token-symbol);
  
  do-exported-variables(module,
                        method(name :: <symbol>, variable :: <variable>)
                            dump-spec-clause(name,
                                             variable.variable-definition,
                                             stream);
                        end);
  
  format(stream, "end module-spec %s;\n\n",
         tlf.define-module-name.token-symbol);
end method;

// dump-spec-clause -- internal
//
define method dump-spec-clause
    (name :: <symbol>, defn :: <definition>, stream :: <stream>) => ();
  format(stream, "  // %s;\n", name);
end method;

define method dump-spec-clause
    (name :: <symbol>, defn :: <macro-definition>, stream :: <stream>) => ();
  format(stream, "  macro-test %s-test;\n", name);
end method;

define method dump-spec-clause
    (name :: <symbol>, defn :: <constant-definition>, stream :: <stream>)
 => ();
  format(stream, "  constant %s :: %s;\n", name, defn.defn-type);
end method;

define method dump-spec-clause
    (name :: <symbol>, defn :: <variable-definition>, stream :: <stream>)
 => ();
  format(stream, "  variable %s :: %s;\n", name, defn.defn-type);
end method;

define method dump-spec-clause
    (name :: <symbol>, defn :: <class-definition>, stream :: <stream>)
 => ();
  let cclass = defn.class-defn-cclass;
  format(stream, "  ");

  if (cclass.sealed?)
    format(stream, "sealed ");
  else
    format(stream, "open ");
  end;
  
  if (cclass.abstract?)
    format(stream, "abstract ");
  else
    format(stream, "instantiable ");
  end;

  if (cclass.primary?)
    format(stream, "primary ");
  end;

  format(stream, "class %s (", name);
  for (superclass in cclass.direct-superclasses, first? = #t then #f)
    unless (first?) format(stream, ", "); end;
    print-message(superclass, stream);
  end for;
  format(stream, ");\n");
end method;

define method dump-spec-clause
    (name :: <symbol>, defn :: <function-definition>, stream :: <stream>)
 => ();
  format(stream, "  function %s ", name);
  dump-signature(defn.function-defn-signature, stream);
  format(stream, ";\n");
end method;

define method dump-spec-clause
    (name :: <symbol>, defn :: <generic-definition>, stream :: <stream>,
     #next next-method)
 => ();
  if (defn.generic-defn-sealed?)
    if (defn.generic-defn-methods.size = 1)
      next-method(name, defn.generic-defn-methods[0], stream);
    else
      next-method();
    end;
  else
    format(stream, "  open generic-function %s ", name);
    dump-signature(defn.function-defn-signature, stream);
    format(stream, ";\n");
  end if;
end method;

// dump-signature -- internal
//
define method dump-signature
    (signature :: <signature>, stream :: <stream>)
 => ();
  format(stream, "(");
  let first? = #t;
  for (specializer in signature.specializers)
    unless (first?) format(stream, ", "); end;
    first? := #f;
    print-message(specializer, stream);
  end for;

  if (signature.rest-type)
    unless (first?) format(stream, ", "); end;
    first? := #f;
    format(stream, "#\"rest\"");
  end;

  if (signature.key-infos | signature.all-keys?)
    unless (first?) format(stream, ", "); end;
    format(stream, "#\"key\"");

    if(signature.all-keys?)
      format(stream, ", #\"all-keys\"");
    end if;

    if (signature.key-infos)
      for (key-info in signature.key-infos)
        format(stream, ", #\"%s\"", key-info.key-name);
      end for;
    end if;
  end if;
  
  format(stream, ") => (");
  dump-returns(signature.returns, stream);
  format(stream, ")");
end method;

// dump-returns -- internal
//
define method dump-returns
    (return :: <ctype>, stream :: <stream>)
 => ();
  print-message(return, stream);
end method;

define method dump-returns
    (return :: <multi-value-ctype>, stream :: <stream>)
 => ();
  for (type in return.positional-types, first? = #t then #f)
    unless (first?) format(stream, ", "); end;
    print-message(type, stream);
  end for;

  unless (return.rest-value-type == empty-ctype())
    unless (empty?(return.positional-types)) format(stream, ", "); end;
    format(stream, "#\"rest\"");
  end unless;
end method;

