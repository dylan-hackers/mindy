module: front
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/front/fer-dump.dylan,v 1.34 1995/12/16 02:35:49 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.


// Id stuff.

define variable *id-table* = make(<table>);
define constant $id-vector = make(<stretchy-vector>);

define method reset-ids () => ();
  *id-table* := make(<table>);
  $id-vector.size := 0;
end;

define constant <id-able>
  = type-union(<region>, <expression>, <assignment>);

define method id (thing :: <id-able>) => res :: <integer>;
  let id = element(*id-table*, thing, default: #f);
  if (id)
    id;
  else
    let id = $id-vector.size;
    add!($id-vector, thing);
    element(*id-table*, thing) := id;
    id;
  end;
end;

define method id (id :: <integer>) => res :: <id-able>;
  unless (id < $id-vector.size)
    error("Nothing with id %d\n", id);
  end;
  $id-vector[id];
end;


define method print-object (thing :: <id-able>, stream :: <stream>) => ();
  write('{', stream);
  write-class-name(thing, stream);
  write(" [", stream);
  print(thing.id, stream);
  write("]}", stream);
end;




// Dump-fer itself.

define method dump-fer (thing, #key stream = *debug-output*) => ();
  pprint-logical-block(stream, body: curry(dump, thing));
  write('\n', stream);
end;

define generic dump (thing, stream) => ();

define method dump (thing :: <object>, stream :: <stream>) => ();
  write("{a ", stream);
  write-class-name(thing, stream);
  write('}', stream);
end;

define method dump (component :: <component>, stream :: <stream>) => ();
  for (func in component.all-function-regions,
       first? = #t then #f)
    unless (first?)
      pprint-newline(#"mandatory", stream);
    end;
    dump(func, stream);
  end;
end;

define method dump (region :: <simple-region>, stream :: <stream>) => ();
  for (assign = region.first-assign then assign.next-op,
       first? = #t then #f,
       while: assign)
    unless (first?)
      pprint-newline(#"mandatory", stream);
    end;
    dump(assign, stream);
  end;
end;
      
define method dump (region :: <compound-region>, stream :: <stream>) => ();
  for (subregion in region.regions,
       first? = #t then #f)
    unless (first?)
      pprint-newline(#"mandatory", stream);
    end;
    dump(subregion, stream);
  end;
end;

define method dump (region :: <if-region>, stream :: <stream>) => ();
  pprint-logical-block
    (stream,
     body: method (stream)
	     format(stream, "IF[%d] (", region.id);
	     dump(region.depends-on.source-exp, stream);
	     write(')', stream);
	     pprint-indent(#"block", 2, stream);
	     pprint-newline(#"mandatory", stream);
	     dump(region.then-region, stream);
	     pprint-indent(#"block", 0, stream);
	     pprint-newline(#"mandatory", stream);
	     write("ELSE", stream);
	     pprint-indent(#"block", 2, stream);
	     pprint-newline(#"mandatory", stream);
	     dump(region.else-region, stream);
	     pprint-indent(#"block", 0, stream);
	     pprint-newline(#"mandatory", stream);
	     write("END", stream);
	   end);
end;

define method dump
    (region :: <unwind-protect-region>, stream :: <stream>) => ();
  pprint-logical-block
    (stream,
     body: method (stream)
	     format(stream, "UWP[%d] (", region.id);
	     dump(region.uwp-region-cleanup-function, stream);
	     write(')', stream);
	     pprint-indent(#"block", 2, stream);
	     pprint-newline(#"mandatory", stream);
	     dump(region.body, stream);
	     pprint-indent(#"block", 0, stream);
	     pprint-newline(#"mandatory", stream);
	     write("END", stream);
	   end);
end;

define method dump (region :: <block-region>, stream :: <stream>) => ();
  pprint-logical-block
    (stream,
     body: method (stream)
	     format(stream, "BLOCK[%d]", region.id);
	     pprint-indent(#"block", 2, stream);
	     pprint-newline(#"mandatory", stream);
	     dump(region.body, stream);
	     pprint-indent(#"block", 0, stream);
	     pprint-newline(#"mandatory", stream);
	     write("END", stream);
	   end);
end;

define method dump (region :: <loop-region>, stream :: <stream>) => ();
  pprint-logical-block
    (stream,
     body: method (stream)
	     format(stream, "LOOP[%d]", region.id);
	     pprint-indent(#"block", 2, stream);
	     pprint-newline(#"mandatory", stream);
	     dump(region.body, stream);
	     pprint-indent(#"block", 0, stream);
	     pprint-newline(#"mandatory", stream);
	     write("END", stream);
	   end);
end;

define method dump (region :: <exit>, stream :: <stream>) => ();
  let target = region.block-of;
  format(stream, "EXIT[%d] to %s[%d]",
	 region.id,
	 if (instance?(target, <component>)) "COMPONENT" else "BLOCK" end,
	 target.id);
end;

define method dump (region :: <return>, stream :: <stream>) => ();
  format(stream, "RETURN[%d]", region.id);
  dump-operands(region.depends-on, stream);
end;

define method dump
    (func :: <fer-function-region>, stream :: <stream>) => ();
  pprint-logical-block
    (stream,
     body:
       method (stream)
	 pprint-logical-block
	   (stream,
	    body:
	      method (stream)
		format(stream, "%= [%d]", func.name, func.id);
		write(' ', stream);
		pprint-indent(#"block", 4, stream);
		pprint-newline(#"fill", stream);
		pprint-logical-block
		  (stream,
		   prefix: "(",
		   body:
		     method (stream)
		       let prologue-assign-dep = func.prologue.dependents;
		       for (arg-type in func.argument-types,
			    var = (prologue-assign-dep
				     & prologue-assign-dep.dependent.defines)
			      then var & var.definer-next,
			    first? = #t then #f)
			 unless (first?)
			   write(", ", stream);
			   pprint-newline(#"fill", stream);
			 end;
			 if (var)
			   dump(var, stream);
			 else
			   write("???", stream);
			 end;
		       end;
		     end,
		   suffix: ")");
	      end);
	 pprint-indent(#"block", 2, stream);
	 pprint-newline(#"mandatory", stream);
	 dump(func.body, stream);
	 pprint-indent(#"block", 0, stream);
	 pprint-newline(#"mandatory", stream);
	 write("END", stream);
       end);
end;

define method dump (assignment :: <assignment>, stream :: <stream>) => ();
  pprint-logical-block
    (stream,
     body: method (stream)
	     format(stream, "[%d]: ", assignment.id);
	     if (instance?(assignment, <let-assignment>))
	       format(stream, "let ");
	     end;
	     dump-defines(assignment.defines, stream);
	     write(' ', stream);
	     pprint-indent(#"block", 2, stream);
	     pprint-newline(#"fill", stream);
	     write(":= ", stream);
	     dump(assignment.depends-on.source-exp, stream);
	     write(';', stream);
	   end);
end;

define method dump (assignment :: <join-assignment>, stream :: <stream>) => ();
  pprint-logical-block
    (stream,
     body: method (stream)
	     format(stream, "[%d]: ", assignment.id);
	     dump-defines(assignment.defines, stream);
	     write(' ', stream);
	     pprint-indent(#"block", 2, stream);
	     pprint-newline(#"fill", stream);
	     write("JOIN ", stream);
	     dump(assignment.depends-on.source-exp, stream);
	     write(';', stream);
	   end);
end;

define method dump-defines (defines :: false-or(<definition-site-variable>),
			    stream :: <stream>)
    => ();
  if (~defines)
    write("()", stream);
  elseif (~defines.definer-next)
    dump(defines, stream);
  else
    pprint-logical-block
      (stream,
       prefix: "(",
       body: method (stream)
	       for (def = defines then def.definer-next,
		    first? = #t then #f,
		    while: def)
		 unless (first?)
		   write(", ", stream);
		   pprint-newline(#"fill", stream);
		 end;
		 dump(def, stream);
	       end;
	     end,
       suffix: ")");
  end;
end;

define method dump (op :: <operation>, stream :: <stream>) => ();
  format(stream, "%s[%d]", op.kind, op.id);
  dump-operands(op.depends-on, stream);
end;

define method kind (op :: <operation>) => res :: <string>;
  let stream = make(<byte-string-output-stream>);
  write-class-name(op, stream);
  stream.string-output-stream-string;
end;

define method kind (op :: <known-call>) => res :: <string>;
  "KNOWN-CALL";
end;

define method kind (op :: <unknown-call>) => res :: <string>;
  if (op.use-generic-entry?)
    "UNKNOWN-CALL-W/-NEXT";
  else
    "UNKNOWN-CALL";
  end;
end;

define method kind (op :: <error-call>) => res :: <string>;
  "ERROR-CALL";
end;

define method kind (op :: <mv-call>) => res :: <string>;
  if (op.use-generic-entry?)
    "MV-CALL-W/-NEXT";
  else
    "MV-CALL";
  end;
end;

define method kind (op :: <prologue>) => res :: <string>;
  "PROLOGUE";
end;

define method kind (op :: <self-tail-call>) => res :: <string>;
  "SELF-TAIL-CALL";
end;

define method dump (op :: <primitive>, stream :: <stream>) => ();
  format(stream, "primitive %s[%d]", op.name, op.id);
  dump-operands(op.depends-on, stream);
end;

define method dump (op :: <module-var-ref>, stream :: <stream>) => ();
  write("ref ", stream);
  dump(op.variable.defn-name, stream);
  format(stream, "[%d]", op.id);
  dump-operands(op.depends-on, stream);
end;

define method dump (op :: <module-var-set>, stream :: <stream>) => ();
  write("set ", stream);
  dump(op.variable.defn-name, stream);
  format(stream, "[%d]", op.id);
  dump-operands(op.depends-on, stream);
end;

define method dump (op :: <slot-ref>, stream :: <stream>) => ();
  write("SLOT-REF ", stream);
  let slot = op.slot-info;
  if (slot.slot-getter)
    write(as(<string>, slot.slot-getter.variable-name), stream);
  else
    write("???", stream);
  end;
  format(stream, "[%d]", op.id);
  dump-operands(op.depends-on, stream);
end;

define method dump (op :: <slot-set>, stream :: <stream>) => ();
  write("SLOT-SET ", stream);
  let slot = op.slot-info;
  if (slot.slot-getter)
    write(as(<string>, slot.slot-getter.variable-name), stream);
  else
    write("???", stream);
  end;
  format(stream, "[%d]", op.id);
  dump-operands(op.depends-on, stream);
end;

define method dump-operands(dep :: false-or(<dependency>), stream :: <stream>)
    => ();
  pprint-logical-block
    (stream,
     prefix: "(",
     body: method (stream)
	     for (dep = dep then dep.dependent-next,
		  first? = #t then #f,
		  while: dep)
	       unless (first?)
		 write(", ", stream);
		 pprint-newline(#"fill", stream);
	       end;
	       dump(dep.source-exp, stream);
	     end;
	   end,
     suffix: ")");
end;

define method dump (var :: <abstract-variable>, stream :: <stream>) => ();
  dump(var.var-info, stream);
  format(stream, "[%d]", var.id);
end;

define method dump (var :: <initial-definition>, stream :: <stream>) => ();
  dump(var.var-info, stream);
  format(stream, "[%d]", var.definition-of.id);
end;

define method dump (info :: <debug-named-info>, stream :: <stream>) => ();
  write(as(<string>, info.debug-name), stream);
end;

define method dump (name :: <basic-name>, stream :: <stream>) => ();
  write(as(<string>, name.name-symbol), stream);
end;

define method dump (name :: <method-name>, stream :: <stream>) => ();
  pprint-logical-block
    (stream,
     body: method (stream)
	     dump(name.method-name-generic-function, stream);
	     pprint-logical-block
	       (stream,
		prefix: "{",
		body: method (stream)
			for (spec in name.method-name-specializers,
			     first? = #t then #f)
			  unless (first?)
			    write(", ", stream);
			    pprint-newline(#"fill", stream);
			  end;
			  dump(spec, stream);
			end;
		      end,
		suffix: "}");
	   end);
end;

define method dump (leaf :: <literal-constant>, stream :: <stream>) => ();
  dump(leaf.value, stream);
  format(stream, "[%d]", leaf.id);
end;

define method dump (ct-value :: <ct-value>, stream :: <stream>) => ();
  print(ct-value, stream, level: 1, length: 3);
end;

define method dump (lit :: type-union(<literal-number>, <literal-symbol>,
				   <literal-character>, <literal-string>,
				   <literal-empty-list>, <literal-boolean>),
		    stream :: <stream>) => ();
  print-message(lit, stream);
end;

define method dump (type :: <ctype>, stream :: <stream>) => ();
  print-message(type, stream);
end;

define method dump (leaf :: <definition-constant-leaf>, stream :: <stream>)
    => ();
  dump(leaf.const-defn.defn-name, stream);
  format(stream, "[%d]", leaf.id);
end;

define method dump
    (expr :: <function-literal>, stream :: <stream>) => ();
  format(stream, "FUNCTION %= [%d]", expr.main-entry.name, expr.id);
end;

define method dump
    (expr :: <method-literal>, stream :: <stream>) => ();
  format(stream, "METHOD %= [%d]", expr.main-entry.name, expr.id);
end;

define method dump
    (exit-function :: <exit-function>, stream :: <stream>) => ();
  format(stream, "EXIT-FUN[%d]", exit-function.id);
end;
