module: dump
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/front/dump.dylan,v 1.10 1996/03/20 22:30:07 rgs Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.


// dump state streams.

define class <dump-state-stream> (<stream>)
  slot target :: <stream>, required-init-keyword: target:;
  slot varinfo-table :: false-or(<table>), init-value: #f;
end;

define method stream-extension-get-output-buffer
    (stream :: <dump-state-stream>)
    => (buf :: <buffer>, next :: <buffer-index>, size :: <buffer-index>);
  get-output-buffer(stream.target);
end;

define method stream-extension-release-output-buffer
    (stream :: <dump-state-stream>, next :: <buffer-index>)
    => ();
  release-output-buffer(stream.target, next);
end;

define method stream-extension-empty-output-buffer
    (stream :: <dump-state-stream>,
     stop :: <buffer-index>)
    => ();
  empty-output-buffer(stream.target, stop);
end;

define method stream-extension-force-secondary-buffers
    (stream :: <dump-state-stream>)
    => ();
  force-secondary-buffers(stream.target);
end;  

define method stream-extension-synchronize (stream :: <dump-state-stream>)
    => ();
  synchronize(stream.target);
end;

define method close (stream :: <dump-state-stream>) => ();
end;

define method pprint-logical-block
    (stream :: <dump-state-stream>,
     #next next-method,
     #rest keys,
     #key column: column :: <integer> = 0,
          prefix :: false-or(<byte-string>),
          per-line-prefix :: false-or(<byte-string>),
          body :: <function>,
          suffix :: false-or(<byte-string>))
    => ();
  apply(pprint-logical-block,
	stream.target,
	body: method (pretty-stream)
		if (pretty-stream == stream.target)
		  body(stream);
		else
		  let orig-target = stream.target;
		  stream.target := pretty-stream;
		  body(stream);
		  stream.target := orig-target;
		end;
	      end,
	keys);
end;

define method pprint-newline (kind :: one-of(#"linear", #"miser", #"fill",
					     #"mandatory"),
			      stream :: <dump-state-stream>)
    => ();
  pprint-newline(kind, stream.target);
end;

define method pprint-indent (relative-to :: one-of(#"block", #"current"),
			     n :: <integer>,
			     stream :: <dump-state-stream>)
    => ();
  pprint-indent(relative-to, n, stream.target);
end;

define method pprint-tab (kind :: one-of(#"line", #"section", #"line-relative",
					 #"section-relative"),
			  colnum :: <integer>,
			  colinc :: <integer>,
			  stream :: <dump-state-stream>)
    => ();
  pprint-tab(kind, colnum, colinc, stream.target);
end;



// varinfos

define class <varinfo> (<object>)
  slot symbol :: <symbol>, required-init-keyword: symbol:;
  slot ids :: <table>, init-function: curry(make, <table>);
  slot last-id :: <integer>, init-value: 0;
end;

define method print-object (varinfo :: <varinfo>, stream :: <stream>) => ();
  pprint-fields(varinfo, stream,
		symbol: varinfo.symbol,
		last-id: varinfo.last-id);
end;


// dump itself.

define generic dump (thing, stream :: <stream>) => ();

define method dump (thing, stream :: <stream>) => ();
  dump(thing, make(<dump-state-stream>, target: stream));
end;

define method dump (thing, stream :: <dump-state-stream>) => ();
  print(thing, stream, level: 1);
end;

define method dump (vec :: <simple-object-vector>,
		    stream :: <dump-state-stream>)
    => ();
  unless (empty?(vec))
    pprint-indent(#"block", 2, stream);
    pprint-newline(#"linear", stream);
    for (form in vec, first? = #t then #f)
      unless (first?)
	write(' ', stream);
	pprint-newline(#"mandatory", stream);
      end;
      dump(form, stream);
      write(';', stream);
    end;
    write(' ', stream);
    pprint-indent(#"block", 0, stream);
  end;
end;

define method dump (form :: <let>, stream :: <dump-state-stream>) => ();
  pprint-logical-block
    (stream,
     body: method (stream)
	     write("let ", stream);
	     dump(form.let-bindings.bindings-parameter-list, stream);
	     write(' ', stream);
	     pprint-indent(#"block", 2, stream);
	     pprint-newline(#"linear", stream);
	     write("= ", stream);
	     dump(form.let-bindings.bindings-expression, stream);
	   end);
end;

define method dump (form :: <let-handler>, stream :: <dump-state-stream>)
    => ();
  pprint-logical-block
    (stream,
     body: method (stream)
	     write("let handler ", stream);
	     pprint-logical-block
	       (stream,
		prefix: "(",
		body: method (stream)
			dump(form.handler-type, stream);
			unless (empty?(form.handler-plist))
			  for (prop in form.handler-plist)
			    write(", ", stream);
			    pprint-newline(#"linear", stream);
			    dump(prop.prop-keyword, stream);
			    write(": ", stream);
			    dump(prop.prop-value, stream);
			  end;
			end;
		      end,
		suffix: ")");
	     write(' ', stream);
	     pprint-indent(#"block", 2, stream);
	     pprint-newline(#"linear", stream);
	     write("= ", stream);
	     dump(form.handler-expression, stream);
	   end);
end;

define method dump (form :: <local>, stream :: <dump-state-stream>) => ();
  pprint-logical-block
    (stream,
     body: method (stream)
	     write("local ", stream);
	     pprint-indent(#"block", 2, stream);
	     pprint-newline(#"linear", stream);
	     for (meth in form.local-methods, first? = #t then #f)
	       unless (first?)
		 write(", ", stream);
		 pprint-newline(#"linear", stream);
	       end;
	       dump(meth, stream);
	     end;
	   end);
end;

define method dump (form :: <funcall>, stream :: <dump-state-stream>) => ();
  pprint-logical-block
    (stream,
     body: method (stream)
	     dump(form.funcall-function, stream);
	     pprint-logical-block
	       (stream,
		prefix: "(",
		body: method (stream)
			for (arg in form.funcall-arguments,
			     first? = #t then #f)
			  unless (first?)
			    write(", ", stream);
			    pprint-newline(#"linear", stream);
			  end;
			  dump(arg, stream);
			end;
		      end,
		suffix: ")");
	   end);
end;

define method dump (form :: <dot>, stream :: <dump-state-stream>) => ();
  pprint-logical-block
    (stream,
     body: method (stream)
	     dump(form.dot-operand, stream);
	     pprint-indent(#"block", 2, stream);
	     pprint-newline(#"linear", stream);
	     write('.', stream);
	     dump(form.dot-name, stream);
	   end);
end;

define method dump (form :: <assignment>, stream :: <dump-state-stream>) => ();
  pprint-logical-block
    (stream,
     body: method (stream)
	     dump(form.assignment-place, stream);
	     write(' ', stream);
	     pprint-indent(#"block", 2, stream);
	     pprint-newline(#"linear", stream);
	     write(":= ", stream);
	     dump(form.assignment-value, stream);
	   end);
end;

define method dump (form :: <begin>, stream :: <dump-state-stream>) => ();
  pprint-logical-block
    (stream,
     body: method (stream)
	     write("begin ", stream);
	     dump(form.begin-body, stream);
	     pprint-newline(#"linear", stream);
	     write("end", stream);
	   end);
end;

define method dump (form :: <bind-exit>, stream :: <dump-state-stream>) => ();
  pprint-logical-block
    (stream,
     body: method (stream)
	     write("bind-exit (", stream);
	     dump(form.exit-name, stream);
	     write(") ", stream);
	     dump(form.exit-body, stream);
	     pprint-newline(#"linear", stream);
	     write("end", stream);
	   end);
end;

define method dump (form :: <if>, stream :: <dump-state-stream>) => ();
  pprint-logical-block
    (stream,
     body: method (stream)
	     write("if (", stream);
	     dump(form.if-condition, stream);
	     write(") ", stream);
	     dump(form.if-consequent, stream);
	     pprint-newline(#"linear", stream);
	     write("else ", stream);
	     dump(form.if-alternate, stream);
	     pprint-newline(#"linear", stream);
	     write("end", stream);
	   end);
end;

define method dump (form :: <method-ref>, stream :: <dump-state-stream>) => ();
  dump(form.method-ref-method, stream);
end;

define method dump (form :: <uwp>, stream :: <dump-state-stream>) => ();
  pprint-logical-block
    (stream,
     body: method (stream)
	     write("unwind-protect ", stream);
	     dump(form.uwp-body, stream);
	     pprint-newline(#"linear", stream);
	     write("cleanup ", stream);
	     dump(form.uwp-cleanup, stream);
	     pprint-newline(#"linear", stream);
	     write("end", stream);
	   end);
end;

define method dump (form :: <literal-ref>, stream :: <dump-state-stream>) => ();
  print(form.litref-literal, stream);
end;

define method dump (form :: <varref>, stream :: <dump-state-stream>) => ();
  dump(form.varref-id, stream);
end;

define method dump (meth :: <method-parse>, stream :: <dump-state-stream>)
    => ();
  pprint-logical-block
    (stream,
     body: method (stream)
	     write("method ", stream);
	     if (meth.method-name)
	       dump(meth.method-name, stream);
	       write(' ', stream);
	     end;
	     dump(meth.method-param-list, stream);
	     write(' ', stream);
	     pprint-indent(#"block", 4, stream);
	     pprint-newline(#"fill", stream);
	     write("=> ", stream);
	     dump(meth.method-returns, stream);
	     write("; ", stream);
	     dump(meth.method-body, stream);
	     pprint-newline(#"linear", stream);
	     write("end", stream);
	   end);
end;

define method dump (param-list :: <parameter-list>,
		    stream :: <dump-state-stream>)
    => ();
  pprint-logical-block
    (stream,
     prefix: "(",
     body: method (stream)
	     let first? = #t;
	     for (param in param-list.paramlist-required-vars)
	       unless (first?)
		 write(", ", stream);
		 pprint-newline(#"fill", stream);
	       end;
	       dump(param, stream);
	       first? := #f;
	     end;
	     if (param-list.paramlist-rest)
	       unless (first?)
		 write(", ", stream);
		 pprint-newline(#"fill", stream);
	       end;
	       write("#rest ", stream);
	       dump(param-list.paramlist-rest, stream);
	       first? := #f;
	     end;
	     if (param-list.paramlist-next)
	       unless (first?)
		 write(", ", stream);
		 pprint-newline(#"fill", stream);
	       end;
	       write("#next ", stream);
	       dump(param-list.paramlist-next, stream);
	       first? := #f;
	     end;
	     if (param-list.paramlist-keys)
	       unless (first?)
		 write(", ", stream);
		 pprint-newline(#"fill", stream);
	       end;
	       pprint-logical-block
		 (stream,
		  prefix: "#key ",
		  body: method (stream)
			  for (key in param-list.paramlist-keys,
			       first-key? = #t then #f)
			    unless (first-key?)
			      write(", ", stream);
			      pprint-newline(#"fill", stream);
			    end;
			    dump(key, stream);
			  end;
			end);
	     end;
	     if (param-list.paramlist-all-keys?)
	       write(", ", stream);
	       pprint-newline(#"fill", stream);
	       write("#all-keys", stream);
	     end;
	   end,
     suffix: ")");
end;

define method dump (param :: <parameter>, stream :: <dump-state-stream>) => ();
  pprint-logical-block
    (stream,
     body: method (stream)
	     dump(param.param-name, stream);
	     if (param.param-type)
	       write(' ', stream);
	       pprint-indent(#"block", 2, stream);
	       write(":: ", stream);
	       dump(param.param-type, stream);
	     end;
	   end);
end;

define method dump (token :: <core-word-token>, stream :: <dump-state-stream>)
    => ();
  write(as(<string>, token.token-symbol), stream);
end;

define method dump (token :: <identifier-token>, stream :: <dump-state-stream>)
    => ();
  write(as(<string>, token.token-symbol), stream);
  if (token.token-uniquifier)
    let table = stream.varinfo-table | (stream.varinfo-table := make(<table>));
    let varinfo = (element(table, token.token-symbol, default: #f)
		     | (element(table, token.token-symbol)
			  := make(<varinfo>, symbol: token.token-symbol)));
    let id = (element(varinfo.ids, token.token-uniquifier, default: #f)
		| (element(varinfo.ids, token.token-uniquifier)
		     := varinfo.last-id := varinfo.last-id + 1));
    format(stream, "#%d", id);
  end;
end;

define method dump (frag :: <fragment>, stream :: <dump-state-stream>)
    => ();
  pprint-logical-block
    (stream,
     prefix: "fragment {",
     body: method (stream)
	     let tail = frag.fragment-tail;
	     for (piece = frag.fragment-head then piece.piece-next,
		  first? = #t then #f,
		  while: piece & piece.piece-prev ~= tail)
	       unless (first?)
		 write(' ', stream);
		 pprint-newline(#"fill", stream);
	       end;
	       let token = piece.piece-token;
	       let source-loc = token.source-location;
	       if (source-loc)
		 write(source-loc.extract-string, stream);
	       else
		 dump(token, stream);
	       end;
	     end;
	   end,
     suffix: "}");
end;


define method dump-parse (thing, #key stream = *debug-output*) => ();
  dump(thing, stream);
  write('\n', stream);
end;

// Seals for file dump.dylan

// <dump-state-stream> -- subclass of <stream>
define sealed domain make(singleton(<dump-state-stream>));
define sealed domain initialize(<dump-state-stream>);
// <varinfo> -- subclass of <object>
define sealed domain make(singleton(<varinfo>));
define sealed domain initialize(<varinfo>);
