module: utils
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/utils.dylan,v 1.17 1996/01/12 00:58:21 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

#if (mindy)

remove-method(print-object,
	      find-method(print-object,
			  list(<object>, <stream>)));

define method print-object (object, stream :: <stream>) => ();
  write('{', stream);
  write-class-name(object, stream);
  write(' ', stream);
  write-address(object, stream);
  write('}', stream);
end;

*default-pretty?* := #t;

#end


// printing utilities.

define method write-class-name (thing, stream) => ();
  let name = thing.object-class.class-name;
  if (name)
    write(as(<string>, name), stream);
  else
    print(thing.object-class, stream);
  end;
end;

define constant $digit-mask = as(<extended-integer>, #xf);

define method write-address (thing, stream) => ();
  write("0x", stream);
  let address = thing.object-address;
  for (shift from -28 below 1 by 4)
    let digit = as(<integer>, logand(ash(address, shift), $digit-mask));
    if (digit < 10)
      write(digit + 48, stream);
    else
      write(digit + 87, stream);
    end;
  end;
end;

define method pprint-fields (thing, stream, #rest fields) => ();
  pprint-logical-block
    (stream,
     prefix: "{",
     body: method (stream)
	     write-class-name(thing, stream);
	     write(' ', stream);
	     write-address(thing, stream);
	     for (i from 0 below fields.size by 2)
	       if (fields[i])
		 write(", ", stream);
		 pprint-indent(#"block", 2, stream);
		 pprint-newline(#"linear", stream);
		 write(as(<string>, fields[i]), stream);
		 write(": ", stream);
		 pprint-indent(#"block", 4, stream);
		 pprint-newline(#"fill", stream);
		 print(fields[i + 1], stream);
	       end;
	     end;
	   end,
     suffix: "}");
end;



// Flush-happy stream

define class <flush-happy-stream> (<stream>)
  slot target :: <stream>, required-init-keyword: target:;
  slot buffer :: <buffer>;
  slot column :: <integer>, init-value: 0;
end;

define method stream-extension-get-output-buffer
    (stream :: <flush-happy-stream>)
    => (buf :: <buffer>, next :: <buffer-index>, size :: <buffer-index>);
  let (buf, next, size) = get-output-buffer(stream.target);
  stream.buffer := buf;
  values(buf, next, size);
end;

define constant $newline = as(<integer>, '\n');

define method after-last-newline (buf :: <buffer>, stop :: <buffer-index>)
    => res :: false-or(<buffer-index>);
  local
    method repeat (i)
      if (zero?(i))
	#f;
      else
	let i-1 = i - 1;
	if (buf[i-1] == $newline)
	  i;
	else
	  repeat(i-1);
	end;
      end;
    end;
  repeat(stop);
end;

define method stream-extension-release-output-buffer
    (stream :: <flush-happy-stream>, next :: <buffer-index>)
    => ();
  let buf = stream.buffer;
  let after-newline = after-last-newline(buf, next);
  if (after-newline)
    empty-output-buffer(stream.target, after-newline);
    force-secondary-buffers(stream.target);
    stream.column := 0;
    let remaining = next - after-newline;
    unless (zero?(remaining))
      copy-bytes(buf, 0, buf, after-newline, remaining);
    end;
    release-output-buffer(stream.target, remaining);
  else
    release-output-buffer(stream.target, next);
  end;
end;

define method stream-extension-empty-output-buffer
    (stream :: <flush-happy-stream>,
     stop :: <buffer-index>)
    => ();
  let buf = stream.buffer;
  let after-newline = after-last-newline(buf, stop);
  if (after-newline)
    empty-output-buffer(stream.target, after-newline);
    force-secondary-buffers(stream.target);
    let remaining = stop - after-newline;
    unless (zero?(remaining))
      copy-bytes(buf, 0, buf, after-newline, remaining);
      empty-output-buffer(stream.target, remaining);
    end;
    stream.column := remaining;
  else
    empty-output-buffer(stream.target, stop);
    stream.column := stream.column + stop;
  end;
end;

define method stream-extension-force-secondary-buffers
    (stream :: <flush-happy-stream>)
    => ();
  force-secondary-buffers(stream.target);
end;  

define method stream-extension-synchronize (stream :: <flush-happy-stream>)
    => ();
  synchronize(stream.target);
end;

define method close (stream :: <flush-happy-stream>) => ();
  force-output(stream);
end;

define method pprint-logical-block
    (stream :: <flush-happy-stream>,
     #next next-method,
     #key column: ignore :: <integer> = 0,
          prefix :: false-or(<byte-string>),
          per-line-prefix :: false-or(<byte-string>),
          body :: <function>,
          suffix :: false-or(<byte-string>))
    => ();
  let (buf, next) = get-output-buffer(stream);
  let column = stream.column + next;
  release-output-buffer(stream, next);
  next-method(stream, column: column, prefix: prefix,
	      per-line-prefix: per-line-prefix, body: body,
	      suffix: suffix);
end;


*debug-output* := make(<flush-happy-stream>, target: *debug-output*);


// pretty format

define method pretty-format (stream :: <stream>,
			     string :: <byte-string>,
			     #rest args)
  let length = string.size;
  local
    method scan-for-space (stream, start, posn, arg-index)
      if (posn == length)
	maybe-spew(stream, start, posn);
      else
	let char = string[posn];
	if (char == ' ')
	  scan-for-end-of-spaces(stream, start, posn + 1, arg-index);
	elseif (char == '%')
	  maybe-spew(stream, start, posn);
	  let directive = string[posn + 1];
	  if (directive == '%')
	    scan-for-space(stream, posn + 1, posn + 2, arg-index);
	  else
	    format(stream, copy-sequence(string, start: posn, end: posn + 2),
		   args[arg-index]);
	    scan-for-space(stream, posn + 2, posn + 2, arg-index + 1);
	  end;
	else
	  scan-for-space(stream, start, posn + 1, arg-index);
	end;
      end;
    end,
    method scan-for-end-of-spaces(stream, start, posn, arg-index)
      if (posn < length & string[posn] == ' ')
	scan-for-end-of-spaces(stream, start, posn + 1, arg-index);
      else
	maybe-spew(stream, start, posn);
	pprint-newline(#"fill", stream);
	scan-for-space(stream, posn, posn, arg-index);
      end;
    end,
    method maybe-spew (stream, start, stop)
      unless (start == stop)
	write(string, stream, start: start, end: stop);
      end;
    end;
  pprint-logical-block(stream,
		       body: method (stream)
			       scan-for-space(stream, 0, 0, 0);
			     end);
end;

define method report-condition (condition :: type-union(<simple-error>,
						     <simple-warning>,
						     <simple-restart>),
				stream :: <stream>)
  apply(pretty-format, stream,
	condition.condition-format-string,
	condition.condition-format-arguments);
end;


// Defines the Info slot used for back-end annotation.
//
define abstract open class <annotatable> (<object>)
  slot info, init-value: #f, init-keyword: info:;
end class;


// foo-IN collection-like functions for threaded lists.


define method find-in
    (next :: <function>, elt, coll, #key key :: <function> = identity,
     test :: false-or(<function>), test-not :: false-or(<function>))
  if (test & test-not)
    error("Both test: and test-not:");
  end;
  block (done)
    if (test-not)
      for (cur = coll then cur.next, while: cur)
        unless (test-not(cur.key, elt)) done(cur) end;
      end for;
    else
      let test = test | \==;
      for (cur = coll then cur.next, while: cur)
        if (test(cur.key, elt)) done(cur) end;
      end for;
    end if;
  end block;
end method;


define method size-in(next :: <function>, coll) => <integer>;
  for (cur = coll then coll.next, len from 0, while: cur)
    finally len;
  end;
end method;


// Simple utility functions.

define method dformat(#rest args) => ();
  apply(format, *debug-output*, args);
end;


define constant assert
  = method (value) => ();
      unless (value)
	error("Assertion failed.");
      end;
    end;

define variable *warnings* = 0;

define constant compiler-warning = method (string, #rest args) => ();
  apply(pretty-format, *debug-output*, concatenate("Warning: ", string, "\n"),
	args);
  *warnings* := *warnings* + 1;
end;

define constant compiler-error
  = method (#rest args) => ();
      apply(error, args);
    end;

define generic key-of
  (value, collection :: <collection>, #key test :: <function>, default)
 => res;

define method key-of
    (value, coll :: <collection>,
     #key test :: <function> = \==, default = #f)
 => res;
  find-key(method (x) test(value, x) end,
           coll, failure: default)
end method;

define method key-of
    (value, coll :: <list>,
     #key  test :: <function> = \==, default = #f)
 => res;
  block (done)
    for (els = coll then els.tail,
         pos :: <integer> from 0,
         until: els == #())
      if (test(value, els.head))
        done(pos);
      end;
      finally default;
    end for;
  end;
end method;


define method list?(obj);
  instance?(obj, <list>);
end;

define method pair?(obj);
  instance?(obj, <pair>);
end;


define method symcat (#rest things) => res :: <symbol>;
  as(<symbol>, apply(concatenate, "", map(curry(as, <string>), things)));
end;


define method stringify (#rest things) => res :: <byte-string>;
  let res = for (thing in things,
		 len = 0 then len + string-length(thing))
	    finally
	      make(<byte-string>, size: len);
	    end for;
  for (thing in things,
       offset = 0 then append(res, offset, thing))
  end for;
  res;
end method stringify;

define method string-length (char :: <byte-character>)
    => res :: <integer>;
  1;
end method string-length;

define method string-length (str :: <byte-string>) => res :: <integer>;
  str.size;
end method string-length;

define method string-length (int :: <integer>) => res :: <integer>;
  case
    int < 0 =>
      if (int == $minimum-integer)
	2 + string-length(- truncate/(int, 10));
      else
	1 + string-length(-int);
      end if;
    int < 10 => 1;
    int < 100 => 2;
    int < 1000 => 3;
    int < 10000 => 4;
    otherwise
      for (digits from 5,
	   num = floor/(int, 100000) then floor/(num, 10),
	   until: zero?(num))
      finally
	digits;
      end for;
  end case;
end method string-length;

define method append
    (res :: <byte-string>, offset :: <integer>, what :: <byte-character>)
    => new-offset :: <integer>;
  res[offset] := what;
  offset + 1;
end method append;

define method append
    (res :: <byte-string>, offset :: <integer>, what :: <byte-string>)
    => new-offset :: <integer>;
  let len = what.size;
  copy-bytes(res, offset, what, 0, len);
  offset + len;
end method append;

define method append
    (res :: <byte-string>, offset :: <integer>, what :: <integer>)
    => new-offset :: <integer>;
  if (what < 0)
    res[offset] := '-';
    if (what == $minimum-integer)
      let (rest, low) = truncate/(what, 10);
      let new-offset = append(res, offset + 1, -rest);
      res[new-offset] := as(<character>, low + 48);
      new-offset + 1;
    else
      append(res, offset + 1, -what);
    end if;
  elseif (what < 10)
    res[offset] := as(<character>, what + 48);
    offset + 1;
  elseif (what < 100)
    let (high, low) = floor/(what, 10);
    res[offset] := as(<character>, high + 48);
    res[offset + 1] := as(<character>, low + 48);
    offset + 2;
  elseif (what < 1000)
    let (temp, low) = floor/(what, 10);
    let (high, mid) = floor/(temp, 10);
    res[offset] := as(<character>, high + 48);
    res[offset + 1] := as(<character>, mid + 48);
    res[offset + 2] := as(<character>, low + 48);
    offset + 3;
  elseif (what < 10000)
    let (temp, low) = floor/(what, 10);
    let (temp, mid-low) = floor/(temp, 10);
    let (high, mid-high) = floor/(temp, 10);
    res[offset] := as(<character>, high + 48);
    res[offset + 1] := as(<character>, mid-high + 48);
    res[offset + 2] := as(<character>, mid-low + 48);
    res[offset + 3] := as(<character>, low + 48);
    offset + 4;
  else
    local method repeat (num :: <integer>)
	      => new-offset :: <integer>;
	    if (num < 10)
	      res[offset] := as(<character>, num + 48);
	      offset + 1;
	    else
	      let (num, digit) = floor/(num, 10);
	      let new-offset = repeat(num);
	      res[new-offset] := as(<character>, digit + 48);
	      new-offset + 1;
	    end if;
	  end method repeat;
    repeat(what);
  end if;
end method append;
