Module: scheme-read

define class <scheme-eof-object> (<object>)
  // no slots (placeholder class)
end class;

define constant $scheme-eof-object = make(<scheme-eof-object>);

define function scheme-eof-object?
    (obj :: <object>)
 => (eof? :: <boolean>);
  instance?(obj, <scheme-eof-object>)
end function;

// This is not a full R5RS scheme (read) function, but it could
// be made into one with a little more effort.  In particular, it
// currently makes no effort to recognize numbers.

define function scheme-read
    (stream :: <stream>)
 => (obj :: <object>);
  local
    method scheme-read-aux() => (obj :: <object>);
      let char = peek(stream, on-end-of-stream: $scheme-eof-object);
      select(char)
        $scheme-eof-object =>
          $scheme-eof-object;

        ';' =>
          scheme-read-comment();
          scheme-read-aux();

        ' ', '\t', '\r', '\n', '\f' =>
          read-element(stream);         // consume whitespace
          scheme-read-aux();

        '(' =>
          read-element(stream);         // consume opening paren
          scheme-read-list(#(), #());

        ')' =>
          read-element(stream);         // consume unexpected closing paren
          error("Unexpected closing ')'");

        '"' =>
          read-element(stream);         // consume opening quote
          scheme-read-string(make(<stretchy-vector>));

        '\'' =>
          read-element(stream);         // consume single quote
          list(#"quote", scheme-read-aux());

        '`' =>
          read-element(stream);         // consume backquote
          list(#"quasiquote", scheme-read-aux());

        ',' =>
          read-element(stream);         // consume comma
          let following = peek(stream, on-end-of-stream: $scheme-eof-object);
          if(following == '@')
            read-element(stream);       // consume at-sign
            list(#"unquote-splicing", scheme-read-aux());
          else
            list(#"unquote", scheme-read-aux());
          end if;

        '#' =>
          read-element(stream);         // consume sharp
          scheme-read-sharp(make(<stretchy-vector>));

        otherwise =>
          let accum = make(<stretchy-vector>);
          add!(accum, read-element(stream));
          scheme-read-identifier(accum);
      end select;
    end method,

    method scheme-read-comment() => ();
      let char = read-element(stream, on-end-of-stream: $scheme-eof-object);
      select(char)
        $scheme-eof-object =>
          $scheme-eof-object;
        '\r', '\n' =>
          #f;
        otherwise =>
          scheme-read-comment();
      end select;
    end method,

    method scheme-read-list
        (read-tail :: <list>, read-final :: <list>)
     => (obj :: <object>);
      let char = peek(stream, on-end-of-stream: $scheme-eof-object);
      select(char)
        $scheme-eof-object =>
          error("end-of-file within list");
        ';' =>
          scheme-read-comment();
          scheme-read-list(read-tail, read-final);
        ' ', '\t', '\r', '\n', '\f' =>
          read-element(stream);
          scheme-read-list(read-tail, read-final);
        ')' =>
          read-element(stream);
          read-final;
        otherwise =>
          let item = scheme-read-aux();
          if(item == #".")
            let tail-item = scheme-read-aux();
            if(instance?(read-tail, <pair>))
              tail(read-tail) := tail-item;
              scheme-read-list(#(), read-final);
            else
              error("Unexpected '.'");
            end if;
          else
            let new-pair = list(item);
            if(instance?(read-tail, <pair>))
              tail(read-tail) := new-pair;
              scheme-read-list(new-pair, read-final);
            elseif(read-final == #())
              scheme-read-list(new-pair, new-pair);
            else
              error("illegal dotted list");
            end if;
          end if;
      end select;
    end method,

    method scheme-read-string
        (accum :: <stretchy-vector>)
     => (obj :: <object>);
      let char = read-element(stream, on-end-of-stream: $scheme-eof-object);
      select(char)
        $scheme-eof-object =>
          error("end-of-file within string");
        '\r', '\n' =>
          error("end-of-line within string");
        '"' =>
          as(<string>, accum);
        '\\' =>
          let escaped
            = read-element(stream, on-end-of-stream: $scheme-eof-object);
          if(scheme-eof-object?(escaped))
            error("end-of-file within string");
          else
            add!(accum, escaped);
            scheme-read-string(accum);
          end if;
        otherwise =>
          add!(accum, char);
          scheme-read-string(accum);
      end select;      
    end method,

    method scheme-read-sharp
        (accum :: <stretchy-vector>)
     => (obj :: <object>);
      let char = peek(stream, on-end-of-stream: $scheme-eof-object);
      select(char)
        $scheme-eof-object,
        ' ', '\t', '\r', '\n', '\f',
        '(', ')', '"', ';' =>
          let str = as(<string>, accum);
          if(str = "t")
            #t;
          elseif(str = "f")
            #f;
          else
            error("Unknown syntax #%s", str);
          end if;
        otherwise =>
          add!(accum, read-element(stream));
          scheme-read-sharp(accum);
      end select;
    end method,

    method scheme-read-identifier
        (accum :: <stretchy-vector>)
     => (obj :: <object>);
      let char = peek(stream, on-end-of-stream: $scheme-eof-object);
      select(char)
        $scheme-eof-object,
        ' ', '\t', '\r', '\n', '\f',
        '(', ')', '"', ';' =>
          as(<symbol>, as(<string>, accum));
          
        otherwise =>
          add!(accum, read-element(stream));
          scheme-read-identifier(accum);
      end select;
    end method;

  scheme-read-aux();
end function;
