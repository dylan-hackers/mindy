module: front

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
  for (meth in component.all-methods,
       first? = #t then #f)
    unless (first?)
      pprint-newline(#"mandatory", stream);
    end;
    dump(meth, stream);
  end;
end;

define method dump (region :: <simple-region>, stream :: <stream>) => ();
  for (assign = region.first-assign then assign.next-op,
       first? = #t then #f,
       while assign)
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
	     write("IF (", stream);
	     dump(region.if-test, stream);
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

define method dump (region :: <block-region>, stream :: <stream>) => ();
  pprint-logical-block
    (stream,
     body: method (stream)
	     write("BLOCK ???", stream);
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
	     write("LOOP", stream);
	     pprint-indent(#"block", 2, stream);
	     pprint-newline(#"mandatory", stream);
	     dump(region.body, stream);
	     pprint-indent(#"block", 0, stream);
	     pprint-newline(#"mandatory", stream);
	     write("END", stream);
	   end);
end;

define method dump (region :: <exit>, stream :: <stream>) => ();
  write("EXIT ???", stream);
end;

define method dump (region :: <call-site>, stream :: <stream>) => ();
  write("CALL-SITE");
end;


define method dump (assignment :: <assignment>, stream :: <stream>) => ();
  pprint-logical-block
    (stream,
     body: method (stream)
	     pprint-logical-block
	       (stream,
		body: method (stream)
			dump-defines(assignment.defines, stream);
			write(' ', stream);
			pprint-indent(#"block", 2, stream);
			pprint-newline(#"fill", stream);
			write(":= ", stream);
		      end);
	     dump(assignment.expression, stream);
	     write(';', stream);
	   end);
end;

define method dump (assignment :: <join-assignment>, stream :: <stream>) => ();
  pprint-logical-block
    (stream,
     body: method (stream)
	     dump-defines(assignment.defines, stream);
	     write(' ', stream);
	     pprint-indent(#"block", 2, stream);
	     pprint-newline(#"fill", stream);
	     write("JOIN ", stream);
	     dump(assignment.expression, stream);
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
		    while def)
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

define method dump (call :: <call>, stream :: <stream>) => ();
  if (~call.operands)
    write("<call w/ no operands>", stream);
  else
    dump(call.operands.source-exp, stream);
    dump-operands(call.operands.dependent-next, stream);
  end;
end;

define method dump (call :: <mv-call>, stream :: <stream>) => ();
  write("mv-call", stream);
  dump-operands(call.operands, stream);
end;

define method dump-operands(dep :: false-or(<dependency>), stream :: <stream>)
    => ();
  pprint-logical-block
    (stream,
     prefix: "(",
     body: method (stream)
	     for (dep = dep then dep.dependent-next,
		  first? = #t then #f,
		  while dep)
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
end;

define method dump (info :: <debug-named-info>, stream :: <stream>) => ();
  write(as(<string>, info.debug-name), stream);
end;

define method dump (info :: <module-var-info>, stream :: <stream>) => ();
  dump(info.var-defn.defn-name, stream);
end;

define method dump (name :: <basic-name>, stream :: <stream>) => ();
  write(as(<string>, name.name-symbol), stream);
end;

define method dump (leaf :: <literal-constant>, stream :: <stream>) => ();
  dump(leaf.value, stream);
end;

define method dump (ct-value :: <ct-literal>, stream :: <stream>) => ();
  print(ct-value.ct-literal-value, stream, level: 1, length: 3);
end;

define method dump (ct-value :: <ct-value>, stream :: <stream>) => ();
  print(ct-value, stream, level: 1, length: 3);
end;

define method dump (leaf :: <definition-constant-leaf>, stream :: <stream>)
    => ();
  dump(leaf.const-defn.defn-name, stream);
end;

define method dump (lambda :: <lambda>, stream :: <stream>) => ();
  pprint-logical-block
    (stream,
     body: method (stream)
	     write("METHOD ", stream);
	     pprint-logical-block
	       (stream,
		prefix: "(",
		body: method (stream)
			for (var in lambda.vars, first? = #t then #f)
			  unless (first?)
			    write(", ", stream);
			    pprint-newline(#"fill", stream);
			  end;
			  dump(var, stream);
			end;
		      end,
		suffix: ")");
	     write(' ', stream);
	     pprint-indent(#"block", 4, stream);
	     pprint-newline(#"fill", stream);
	     write("=> ", stream);
	     let res = lambda.result;
	     if (res.size == 1)
	       dump(res[0], stream);
	     else
	       pprint-logical-block
		 (stream,
		  prefix: "(",
		  body: method (stream)
			  for (var in res, first? = #t then #f)
			    unless (first?)
			      write(", ", stream);
			      pprint-newline(#"fill", stream);
			    end;
			    dump(var, stream);
			  end;
			end,
		  suffix: ")");
	     end;
	     pprint-indent(#"block", 2, stream);
	     pprint-newline(#"mandatory", stream);
	     dump(lambda.body, stream);
	     pprint-indent(#"block", 0, stream);
	     pprint-newline(#"mandatory", stream);
	     write("END", stream);
	   end);
end;

