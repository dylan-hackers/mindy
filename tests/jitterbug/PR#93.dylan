module: PR-93
copyright: Copyright © 2002 Colin Walters <walters@debian.org>
synopsis: interpret dylan (really scheme)
license: public domain

define class <scheme-token> (<object>)
  slot type :: <symbol>, required-init-keyword: #"type";
  slot value, init-value: #f, init-keyword: #"value";
end class <scheme-token>;

define class <scheme-tokenizer> (<object>)
  slot stream :: <object>, required-init-keyword: #"stream";
end class <scheme-tokenizer>;

define class <pushback-scheme-tokenizer> (<scheme-tokenizer>)
  slot pushed :: false-or(<scheme-token>), init-value: #f;
end class <pushback-scheme-tokenizer>;

define class <tokenizer-error> (<condition>)
end class <tokenizer-error>;

define class <parser-error> (<condition>)
end class <parser-error>;

define class <parser-eof-error> (<parser-error>)
end class <parser-eof-error>;

define class <parser-parse-error> (<parser-error>)
  slot token :: <scheme-token>, required-init-keyword: #"token";
end class <parser-parse-error>;

define method report-condition (err :: <parser-error>, stream) => ()
end method report-condition;

define method report-condition (err :: <parser-eof-error>, stream) => ()
end method report-condition;

define method report-condition (err :: <parser-parse-error>, stream) => ()
end method report-condition;

define class <scheme-parser> (<object>)
  slot tokenizer :: <pushback-scheme-tokenizer>, required-init-keyword: #"tokenizer";
end class <scheme-parser>;

define class <scheme-object> (<object>)
end class <scheme-object>;

define class <scheme-nil> (<scheme-object>)
end class <scheme-nil>;

define constant $scheme-nil = make(<scheme-nil>);

define class <scheme-integer> (<scheme-object>)
  slot value :: <integer>, required-init-keyword: #"value";
end class <scheme-integer>;

define class <scheme-string> (<scheme-object>)
  slot value :: <string>, required-init-keyword: #"value";
end class <scheme-string>;

define class <scheme-identifier> (<scheme-object>)
  slot value :: <symbol>, required-init-keyword: #"value";
end class <scheme-identifier>;

define class <scheme-cons> (<scheme-object>)
  slot car :: <scheme-object>, required-init-keyword: #"car";
  slot cdr :: <scheme-object>, required-init-keyword: #"cdr";
end class <scheme-cons>;

define sealed method as(class == <list>, obj :: <scheme-cons>)
 => (res :: <list>)
  let ret = #();
  for (x = obj.cdr then x.cdr,
       while: ~instance?(x, <scheme-nil>))
    if (~instance?(x, <scheme-cons>))
      error("Malformed list");
    end if;
    add!(ret, x.car);
  end for;
  ret;
end method as;

define class <scheme-boolean> (<scheme-object>)
  slot value :: <boolean>, required-init-keyword: #"value";
end class <scheme-boolean>;

define class <scheme-environment> (<scheme-object>)
  slot env :: <table>, init-value: make(<object-table>);
  slot next :: false-or(<scheme-environment>), required-init-keyword: #"next";
end class <scheme-environment>;

define inline method env-set!(env :: <scheme-environment>, name :: <scheme-identifier>,
                              obj :: <scheme-object>) => ()
  let tab = env.env;
  tab[name] := obj;
end method env-set!;

define inline method env-get(env :: <scheme-environment>, name :: <scheme-identifier>)
 => (val :: <scheme-object>)
  let tab = env.env;
  let val = tab[name];
  if (val)
    val
  elseif (env.next ~= #f)
    env-get(env.next, name);
  else
    signal(make(<scheme-eval-error>));
  end if;
end method env-get;

define class <scheme-method> (<scheme-object>)
  slot params :: <scheme-cons>;
  slot rest :: false-or(<scheme-identifier>), init-value: #f;
  slot env :: <scheme-environment>;
end class <scheme-method>;

define class <scheme-closure> (<scheme-method>)
  slot code :: <scheme-cons>;
end class <scheme-closure>;

define class <scheme-primitive> (<scheme-method>)
  slot code :: <function>;
end class <scheme-primitive>;

define variable $builtin-environment =
  begin
    let e = make(<scheme-environment>, next: #f);
    let sid = method (name)
                      make(<scheme-identifier>, value: as(<symbol>, name));
              end method;
    let setsym = method (name, val)
                   env-set!(e, sid(name), val);
                 end method;
    setsym("interaction-environment", e);    
    let scons = method (a, b)
                  make(<scheme-cons>, car: a, cdr: b)
                end method;
    let sprim = method (name, params, rest, code)
                  env-set!(e, sid(name),
                           make(<scheme-primitive>,
                                params: reduce(method (cur, e)
                                                 scons(sid(e, cur));
                                               end method,
                                               $scheme-nil,
                                               params),
                                rest: rest,
                                env: e,
                                code: code));
                end method;
    sprim("binary-+", #("a", "b"), #f, method (a, b)
                                         make(<scheme-integer>, value: a.value + b.value);
                                       end method);
    sprim("binary--", #("a", "b"), #f, method (a, b)
                                         make(<scheme-integer>, value: a.value - b.value);
                                       end method);
    sprim("binary-*", #("a", "b"), #f, method (a, b)
                                         make(<scheme-integer>, value: a.value * b.value);
                                       end method);
    sprim("binary-/", #("a", "b"), #f, method (a, b)
                                         make(<scheme-integer>, value: a.value / b.value);
                                       end method);
    sprim("binary-/", #("a", "b"), #f, method (a, b)
                                         make(<scheme-integer>, value: a.value / b.value);
                                       end method);
    sprim("cons", #("a", "b"), #f, method (a, b)
                                     make(<scheme-cons>, car: a, cdr: b);
                                   end method);
  end;

define generic scheme-print(obj :: <scheme-object>, stream :: <object>) => ();

define method scheme-print(obj :: <scheme-nil>, stream :: <object>) => ()
  write(stream, "()");
end method scheme-print;

define method scheme-print(obj :: <scheme-integer>, stream :: <object>) => ()
  format(stream, "%=", obj.value);
end method scheme-print;

define method scheme-print(obj :: <scheme-string>, stream :: <object>) => ()
  format(stream, "%=", obj.value);
end method scheme-print;

define method scheme-print(obj :: <scheme-identifier>, stream :: <object>) => ()
  write(stream, as(<string>, obj.value));
end method scheme-print;

define method scheme-print(obj :: <scheme-boolean>, stream :: <object>) => ()
  format(stream, "%=\n", obj.value);
end method scheme-print;

define method scheme-print(obj :: <scheme-cons>, stream :: <object>) => ()
  write(stream, "(");
  scheme-print(obj.car, stream);
  write(stream, " . ");
  scheme-print(obj.cdr, stream);
  write(stream, ")");
end method scheme-print;

define method scheme-print(obj :: <scheme-environment>, stream :: <object>) => ()
  format(stream, "#<lexical environment metaobject at %= with %= entries>",
         object-address(obj),
         size(obj.env));
end method scheme-print;

define method scheme-print(obj :: <scheme-closure>, stream :: <object>) => ()
  format(stream, "#<interpreted function closure at %=>",
        object-address(obj));
end method scheme-print;

define method scheme-special-form?(obj :: <scheme-object>)
 => (ret :: <boolean>)
  instance?(obj, <scheme-cons>) & instance?(obj.car, <scheme-identifier>)
  & member?(obj.car.value, #(#"and", #"begin", #"define", #"if",
                             #"lambda", #"let", #"or", #"and",
                             #"quote", #"set!"));
end method scheme-special-form?;

define class <scheme-eval-error> (<condition>)
end class <scheme-eval-error>;

define generic scheme-eval(obj :: <scheme-object>, env :: <scheme-environment>)
 => (ret :: <scheme-object>);

define method scheme-eval(obj :: <scheme-nil>, env :: <scheme-environment>)
 => (ret :: <scheme-object>);
  obj;
end method scheme-eval;

define method scheme-eval(obj :: <scheme-integer>, env :: <scheme-environment>)
 => (ret :: <scheme-object>);
  obj;
end method scheme-eval;

define method scheme-eval(obj :: <scheme-string>, env :: <scheme-environment>)
 => (ret :: <scheme-object>);
  obj;
end method scheme-eval;

define method scheme-eval(obj :: <scheme-identifier>, env :: <scheme-environment>)
 => (ret :: <scheme-object>);
  find-key(env.env, curry(\==, obj.value));
end method scheme-eval;

define method scheme-eval(obj :: <scheme-boolean>, env :: <scheme-environment>)
 => (ret :: <scheme-object>);
  obj;
end method scheme-eval;

define method scheme-eval(obj :: <scheme-cons>, env :: <scheme-environment>)
 => (ret :: <scheme-object>);
  let func = scheme-eval(obj.car, env);
  let args = map(rcurry(scheme-eval, env), as(<list>, obj.cdr));
  scheme-apply(func, args);
end method scheme-eval;

define generic scheme-apply(func :: <scheme-method>, #rest args)
 => (ret :: <scheme-object>);
    
define method scheme-apply(func :: <scheme-closure>, #rest args)
 => (ret :: <scheme-object>);
  let repeat = #t;
  let code = as(<list>, func.code);
  let params = as(<list>, func.params);
  let envP :: <scheme-environment> = make(<scheme-environment>, next: func.env);
  block (return)
    while (repeat)
      repeat := #f;
      if ((params.size > args.size)
            | (params.size < args.size
                 & ~func.rest))
        signal(make(<scheme-eval-error>));
      end if;
      // Badass.
      map(curry(env-set!, envP), params, args);
      if (func.rest)
        let l = $scheme-nil;
        for (i from args.size - 1 above params.size - 1 by -1)
          l := make(<scheme-cons>,
                    car: args[i],
                    cdr: l);
        end for;
      end if;
      for (i from 0 below code.size - 1)
        scheme-eval(code[i], envP);
      end for;
      let last = code[code.size - 1];
      if (instance?(last, <scheme-cons>) & ~scheme-special-form?(last))
        let lastform = as(<list>, last);
        let proc = scheme-eval(last, envP);
        let newargs = map(rcurry(scheme-eval, envP), lastform.tail);
        if (instance?(proc, <scheme-closure>)
              & ~instance?(proc, <scheme-primitive>)
              & proc.code == func)
          args := newargs;
          repeat := #t;
        else
          return(scheme-apply(proc, newargs));
        end if;
      else
        return(scheme-eval(last, envP));
      end if;
    end while;
    scheme-eval(code[code.size - 1], envP);
  end block;
end method scheme-apply;

define method special-initial?(c :: <character>)
  member?(c, #('!', '$', '%', '&', '*', '/', ':', '<', '=', '>', '?', '^', '_', '~'))
end method special-initial?;

define method initial?(c :: <character>) => (b :: <boolean>)
  alphabetic?(c) | special-initial?(c)
end method initial?;

define method subsequent?(c :: <character>)
  initial?(c) | digit?(c) | member?(c, #('+', '-', '.', '@'))
end method subsequent?;

// The most important method for a scheme tokenizer
define method next-element(toker :: <scheme-tokenizer>)
 => (ret :: false-or(<scheme-token>))
  block (return)
    let in = toker.stream;
    let c = read-element(in);
    let read-integer = method (c :: <character>)
                         let ret = as(<integer>, c) - 48;
                         while (digit?(peek(in)))
                           c := read-element(in);
                           ret := ret * 10 + as(<integer>, c) - 48;
                         end while;
                         ret;
                       end method;
    if (c == #f)
      return(c);
    end if;
    while (whitespace?(c))
      c := read-element(in);
    end while;
    if (c == '\'')
      return(make(<scheme-token>, type: #"quote"));
    elseif (c == '(')
           return(make(<scheme-token>, type: #"lparen"));
    elseif (c == ')')
      return(make(<scheme-token>, type: #"rparen"));
    elseif (c == '.')
      if (peek(in) == '.')
        read-element(in);
        if (peek(in) == '.')
          read-element(in);
          return(make(<scheme-token>, type: "identifier", value: "..."));
        else
          return(make(<scheme-token>, type: "identifier", value: ".."));
        end if;
      else
        return(make(<scheme-token>, type: "dot"));
      end if;
    elseif (c == '#')
      c := read-element(in);
      select (c)
        't' => make(<scheme-token>, type: "true");
        'f' => make(<scheme-token>, type: "false");
        otherwise => signal(make(<tokenizer-error>));
      end select;
    elseif (c == '"')
      let stream = make(<buffered-byte-string-output-stream>);
      c := read-element(in);
      while (c ~= '"')
        if (c == '\\')
          c := read-element(in);
        end if;
        if (c == #f)
          signal(make(<tokenizer-error>));
        end if;
        write-element(stream, c);
        c := read-element(in);
      end while;
      return(make(<scheme-token>, type: #"string",
                  value: stream-contents(stream)));
    elseif (digit?(c))
      return(make(<scheme-token>, type: #"integer", value: read-integer(c)));
    elseif (initial?(c))
      let stream = make(<buffered-byte-string-output-stream>);
      write-element(stream, c);
      while (subsequent?(peek(in)))
        c := read-element(in);
        write-element(stream, c);
      end while;
      return(make(<scheme-token>, type: #"identifier",
                  value: stream-contents(stream)));
    elseif (c == '+' | c == '-')
      let plusp = c == '+';
      if (digit?(peek(in)))
        c := read-element(in);
        return(make(<scheme-token>,
                    type: #"integer",
                    value: begin
                             let val = read-integer(c);
                             if (plusp)
                               val
                             else
                               - val;
                             end if;
                           end));
      else
        return(make(<scheme-token>,
                    type: #"identifier",
                    value: if (c == '+') "+" else "-" end if));
      end if;
    else
      signal("Illegal character %=\n", c);
    end if;
  end block;
end method next-element;

// Methods on a pushback-scheme-tokenizer
define method next-element(toker :: <pushback-scheme-tokenizer>)
 => (ret :: false-or(<scheme-token>))
  if (toker.pushed)
    let ret = toker.pushed;
    toker.pushed := #f;
    ret;
  else
    next-method(toker);
  end if;
end method next-element;

define method push-element(toker :: <pushback-scheme-tokenizer>,
                           token :: <scheme-token>)
 => ()
  if (toker.pushed)
    error("Tokenizer pushback buffer full.");
  else
    toker.pushed := token;
  end if;
end method push-element;

define method parse-rest (parser :: <scheme-parser>)
 => (ret :: false-or(<scheme-object>))
  let tokenizer = parser.tokenizer;
  let tok = next-element(tokenizer);
//  debug-message("parse-rest: got tok %= : %=", tok.type, tok.value);
  if (tok == #f)
    #f;
  else
    select (tok.type)
      #"rparen" => $scheme-nil;
      #"dot" => begin
                 let tail = next-element(tokenizer);
                 if (tail == #f)
                   signal(make(<parser-parse-error>, token: tail));
// "Unexpected EOF while reading expression";
                 elseif (tail.type ~= #"rparen")
                   signal(make(<parser-parse-error>, token: tail));
// "Trailing garbage after . in expression"                          
                 end if;
                 tail;
               end;
      otherwise => begin
                     push-element(tokenizer, tok);
                     make(<scheme-cons>,
                          car: next-element(parser),
                          cdr: parse-rest(parser));
                   end;
    end select;
  end if;
end method parse-rest;

define method next-element (parser :: <scheme-parser>)
 => (ret :: false-or(<scheme-object>))
  block(return)
    let tokenizer = parser.tokenizer;
    let tok = next-element(tokenizer);
    debug-message("next-element: got tok %= : %=", tok.type, tok.value);
    if (~tok)
      return(#f);
    end if;
    select (tok.type)
      #"lparen" => parse-rest(parser);
      #"quote" => make(<scheme-cons>,
                       car: make(<scheme-identifier>, value: #"QUOTE"),
                       cdr: make(<scheme-cons>,
                                 car: next-element(parser),
                                 cdr: $scheme-nil));
      #"true" => make(<scheme-boolean>, value: #t); 
      #"false" => make(<scheme-boolean>, value: #f); 
      #"integer" => make(<scheme-integer>, value: tok.value); 
      #"string" => make(<scheme-string>, value: tok.value); 
      #"identifier" => make(<scheme-identifier>, value: as(<symbol>, tok.value)); 
      otherwise => signal(make(<parser-parse-error>, token: tok));
    end select;
  end block;
end method next-element;

define function main(name, arguments)
  let tokenizer = make(<pushback-scheme-tokenizer>, stream: *standard-input*);
  let parser = make(<scheme-parser>, tokenizer: tokenizer);
  debug-message("Created parser %=", parser);
  let obj = next-element(parser);
  debug-message("Read element %=", obj);
  block()
    while (obj)
      scheme-print(scheme-eval(obj, $builtin-environment),
                   *standard-output*);
      new-line(*standard-output*);
      force-output(*standard-output*);
      obj := next-element(parser);
      debug-message("Read element %=", obj);
    end while;
  exception(cond :: <condition>)
    report-condition(cond, *standard-output*);
    exit-application(1);
  end block;
  exit-application(0);
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());

// Local Variables:
// compile-command: "make"
// End:
