module: lexer
rcs-header: $Header: /scm/cvs/src/d2c/compiler/parser/lexer.dylan,v 1.17 2003/04/24 05:46:00 housel Exp $
copyright: see below


//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000, 2001  Gwydion Dylan Maintainers
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

// Constructors.

// make-binary-operator, make-tilde, make-minus, make-equal, make-double-equal
//   -- internal.
//
// Make various kinds of operators.
// 
define method make-binary-operator
    (lexer :: <lexer>, source-location :: <known-source-location>)
    => res :: <operator-token>;
  make(<operator-token>,
       source-location: source-location,
       kind: $other-binary-operator-token,
       symbol: as(<symbol>, extract-string(source-location)),
       module: *Current-Module*);
end method make-binary-operator;
//
define method make-tilde
    (lexer :: <lexer>, source-location :: <known-source-location>)
    => res :: <identifier-token>;
  make(<identifier-token>,
       source-location: source-location,
       kind: $tilde-token,
       symbol: #"~",
       module: *Current-Module*);
end method make-tilde;
//
define method make-minus
    (lexer :: <lexer>, source-location :: <known-source-location>)
    => res :: <operator-token>;
  make(<operator-token>,
       source-location: source-location,
       kind: $minus-token,
       symbol: #"-",
       module: *Current-Module*);
end method make-minus;
//
define method make-equal
    (lexer :: <lexer>, source-location :: <known-source-location>)
    => res :: <operator-token>;
  make(<operator-token>,
       source-location: source-location,
       kind: $equal-token,
       symbol: #"=",
       module: *Current-Module*);
end method make-equal;
//
define method make-double-equal
    (lexer :: <lexer>, source-location :: <known-source-location>)
    => res :: <operator-token>;
  make(<operator-token>,
       source-location: source-location,
       kind: $double-equal-token,
       symbol: #"==",
       module: *Current-Module*);
end method make-double-equal;


// make-quoted-name -- internal.
//
// Make a <quoted-name-token> for \-quoted operator.
// 
define method make-quoted-name
    (lexer :: <lexer>, source-location :: <known-source-location>)
    => res :: <identifier-token>;
  make(<identifier-token>,
       source-location: source-location,
       kind: $quoted-name-token,
       symbol: as(<symbol>,
		  extract-string(source-location,
				 start: source-location.start-posn + 1)),
       module: *Current-Module*);
end method make-quoted-name;

// make-identifier -- internal.
//
// Extract the name from the source location, figure out what kind of word it
// is, and make it.
// 
define method make-identifier
    (lexer :: <lexer>, source-location :: <known-source-location>)
    => res :: <identifier-token>;
  let name = as(<symbol>, extract-string(source-location));
  let module = *Current-Module*;
  make(<identifier-token>,
       source-location: source-location,
       kind: syntax-for-name(module.module-syntax-table, name),
       symbol: name,
       module: module);
end method make-identifier;


// make-constrainted-name -- internal.
//
// Make a constrained name.
// 
define method make-constrained-name
    (lexer :: <lexer>, source-location :: <known-source-location>)
  let colon-posn
    = block (return)
	let contents = source-location.source.contents;
	for (posn from source-location.start-posn
	       below source-location.end-posn)
	  if (contents[posn] == as(<integer>, ':'))
	    return(posn);
	  end if;
	end for;
	error("No : in a constrained-name?");
	#f;
      end block;
  let constraint
    = as(<symbol>, extract-string(source-location, start: colon-posn + 1));
  make(<constrained-name-token>,
       source-location: source-location,
       kind: $constrained-name-token,
       symbol: if (colon-posn == source-location.start-posn)
		 constraint;
	       else
		 as(<symbol>,
		    extract-string(source-location, end: colon-posn));
	       end if,
       constraint: constraint);
end method;

// decode-escape-character -- internal
//
// Decode the current escape character, returning its integral value. It is
// up to the caller to determine whether or not the value is in range for
// a given <character> instance. It is assumed that the escape code is
// complete.
//
define method decode-escape-character
    (source-location :: <known-source-location>, start :: <integer>)
 => result :: <integer>;
  let contents = source-location.source.contents;
  if (contents[start] ~= as(<integer>, '<'))
    let char = select (as(<character>, contents[start]))
                 'a' => '\a';
                 'b' => '\b';
                 'e' => '\e';
                 'f' => '\f';
                 'n' => '\n';
                 'r' => '\r';
                 't' => '\t';
                 '0' => '\0';
                 '\\' => '\\';
                 '\'' => '\'';
                 '"' => '"';
               end select;
    as(<integer>, char);
  else
    // this is provided inline since we need slightly different behavior
    // than that provided by parse-integer: we stop on the first non-hex
    // character encountered (like the C runtime's strtoul function) and
    // return the value.
    local method decode-unicode-escape(posn :: <integer>, value :: <integer>) => <integer>;
            let digit  = contents[posn];
          
            if (as(<integer>, '0') <= digit & digit <= as(<integer>, '9'))
              decode-unicode-escape(posn + 1, (value * 16) + (digit - as(<integer>, '0')));
            elseif (as(<integer>, 'a') <= digit & digit <= as(<integer>, 'f'))
              decode-unicode-escape(posn + 1, (value * 16) + (digit - as(<integer>, 'a') + 10));
            elseif (as(<integer>, 'A') <= digit & digit <= as(<integer>, 'F'))
              decode-unicode-escape(posn + 1, (value * 16) + (digit - as(<integer>, 'A') + 10));
            else
              value;
            end if;
          end method decode-unicode-escape;
    decode-unicode-escape(start + 1, 0);
  end if;
end method decode-escape-character;

// escape-character-width -- internal
//
// Return the number of bytes used by an escape character, including
// the initial '\'. This function also determines whether or not a
// Unicode escape sequence is within the supported range of the
// implementation (currently U+0000 -- U+00FF).
//
define method escape-character-width
    (source-location :: <known-source-location>, start :: <integer>, finish :: <integer>)
 => (decode-size :: <integer>, bytes-used :: <integer>)
  let contents = source-location.source.contents;

  if (contents[start + 1] ~= as(<integer>, '<'))
    values(1, 2);
  else
    local method repeat(posn, result)
            if (posn < finish)
              if (contents[posn] == as(<integer>, '>'))
                result + 1;
              else
                repeat(posn + 1, result + 1);
              end if;
            else
              compiler-fatal-error("unterminated Unicode escape sequence, line %=, character %=",
                                   source-location.start-line,
                                   source-location.start-column);
            end if;
          end method repeat;

    let escape-code-length = repeat(start + 1, 1);

    if (decode-escape-character(source-location, start + 1) < 256)
      values(1, escape-code-length);
    else
      compiler-fatal-error("Unicode escape out of range, line %=, character %=",
                           source-location.start-line,
                           source-location.start-column);
    end if;
  end if;

end method escape-character-width;

// decode-string -- internal.
//
// Like extract string, except process escape characters.  Also, we
// default to starting one character in from either end, under the
// assumption that the string will be surrounded by quotes.
//
define method decode-string
    (source-location :: <known-source-location>,
     #key start :: <integer> = source-location.start-posn + 1,
     end: finish :: <integer> = source-location.end-posn - 1)
 => result :: <byte-string>;
  let contents = source-location.source.contents;

  let length = begin
                 local method repeat(posn, result)
                         if (posn < finish)
                           if (contents[posn] == as(<integer>, '\\'))
                             let (char-len, skip-count) = escape-character-width(source-location, posn, finish);
                             repeat(posn + skip-count, result + char-len);
                           else
                             repeat(posn + 1, result + 1);
                           end if;
                         else
                           result;
                         end if;
                       end method repeat;
                 repeat(start, 0);
               end;

  let result = make(<string>, size: length);

  local method repeat(src, dst)
          if (dst < length)
            if (contents[src] == as(<integer>, '\\'))
              let (unused, skip-count) = escape-character-width(source-location, src, finish);
              let char-code = decode-escape-character(source-location, src + 1);
              result[dst] := as(<character>, char-code);
              repeat(src + skip-count, dst + 1);
            else
              result[dst] := as(<character>, contents[src]);
              repeat(src + 1, dst + 1);
            end if;
          end if;
        end method repeat;
  repeat(start,0);

  result;
end method decode-string;

// make-quoted-symbol -- internal.
//
// Make a <literal-token> when confronted with the #"foo" syntax.
//
define method make-quoted-symbol
    (lexer :: <lexer>, source-location :: <known-source-location>)
    => res :: <literal-token>;
  let sym = as(<symbol>,
	       decode-string(source-location,
			     start: source-location.start-posn + 2));
  make(<literal-token>,
       source-location: source-location,
       kind: $symbol-token,
       literal: make(<literal-symbol>, value: sym));
end method make-quoted-symbol;

// make-keyword-symbol -- internal.
//
// Make a <literal-token> when confronted with the foo: syntax.
// 
define method make-keyword-symbol
    (lexer :: <lexer>, source-location :: <known-source-location>)
    => res :: <literal-token>;
  let sym = as(<symbol>,
	       extract-string(source-location,
			      end: source-location.end-posn - 1));
  make(<literal-token>,
       source-location: source-location,
       kind: $symbol-token,
       literal: make(<literal-symbol>, value: sym));
end method make-keyword-symbol;
		    
// parse-integer -- internal.
//
// Parse and return an integer in the supplied radix.
// 
define method parse-integer
    (source-location :: <known-source-location>,
     #key radix :: <integer> = 10,
          start :: <integer> = source-location.start-posn,
          end: finish :: <integer> = source-location.end-posn)
    => res :: <extended-integer>;
  let contents = source-location.source.contents;
  local method repeat (posn :: <integer>, result)
	  if (posn < finish)
	    let digit = contents[posn];
	    if (as(<integer>, '0') <= digit & digit <= as(<integer>, '9'))
	      repeat(posn + 1, result * radix + digit - as(<integer>, '0'));
	    elseif (as(<integer>, 'A') <= digit & digit <= as(<integer>, 'F'))
	      repeat(posn + 1,
		     result * radix + digit - as(<integer>, 'A') + 10);
	    elseif (as(<integer>, 'a') <= digit & digit <= as(<integer>, 'f'))
	      repeat(posn + 1,
		     result * radix + digit - as(<integer>, 'a') + 10);
	    else
	      error("Bogus digit in integer: %=", as(<character>, digit));
	    end if;
	  else
	    result;
	  end if;
	end method repeat;
  let first = as(<character>, contents[start]);
  if (first == '-')
    - repeat(start + 1, as(<extended-integer>, 0));
  elseif (first == '+')
    repeat(start + 1, as(<extended-integer>, 0));
  else
    repeat(start, as(<extended-integer>, 0));
  end if;
end method parse-integer;

// parse-integer-literal -- all internal.
//
// Parse an integer and return a <literal-token> holding it.
// 
define method parse-integer-literal
    (lexer :: <lexer>, source-location :: <known-source-location>)
    => res :: <literal-token>;
  let contents = source-location.source.contents;
  let posn = source-location.start-posn;
  let extended = #f;
  let radix = 10;

  if (as(<character>, contents[posn]) == '#')
    posn := posn + 1;
    let char = as(<character>, contents[posn]);
    if (char == 'e' | char == 'E')
      posn := posn + 1;
      char := as(<character>, contents[posn]);
      extended := #t;
    end if;
    if (char == 'b' | char == 'B')
      posn := posn + 1;
      radix := 2;
    elseif (char == 'o' | char == 'O')
      posn := posn + 1;
      radix := 8;
    elseif (char == 'x' | char == 'X')
      posn := posn + 1;
      radix := 16;
    end if;
  end if;
  
  let int = parse-integer(source-location, radix: radix, start: posn);

  if (~extended &
	begin
	  let min-int = ash(as(<extended-integer>, -1),
			    *current-target*.platform-integer-length - 1);
	  int < min-int | int > lognot(min-int);
	end)
    compiler-warning("%d doesn't fit as a <integer>, "
		       "using <extended-integer> instead",
		     int);
    extended := #t;
  end if;

  make(<literal-token>,
       source-location: source-location,
       kind: $literal-token,
       literal: if (extended)
		  make(<literal-extended-integer>, value: int);
		else
		  make(<literal-integer>, value: int);
		end);
end method parse-integer-literal;

// make-character-literal -- internal.
//
// Return a <literal-token> holding the character token.
// 
define method make-character-literal
    (lexer :: <lexer>, source-location :: <known-source-location>)
    => res :: <literal-token>;
  let contents = source-location.source.contents;
  let posn = source-location.start-posn + 1;
  let char = as(<character>, contents[posn]);
  make(<literal-token>,
       source-location: source-location,
       kind: $literal-token,
       literal:
	 make(<literal-character>,
	      value: if (char == '\\')
                       let char = decode-escape-character(source-location, posn + 1);
                       if (char < 256)
                         as(<character>, char);
                       else
                         compiler-fatal-error("Unicode escape out of range, line %=, character %=",
                                              source-location.start-line,
                                              source-location.start-column);
                       end if;
		     else
		       char;
		     end));
end method make-character-literal;

// make-string-literal -- internal.
//
// Should be obvious by now.
//
define method make-string-literal
    (lexer :: <lexer>, source-location :: <known-source-location>)
    => res :: <literal-token>;
  make(<literal-token>,
       source-location: source-location,
       kind: $string-token,
       literal: make(<literal-string>,
		     value: decode-string(source-location)));
end method make-string-literal;

// parse-ratio-literal -- internal.
// 
define method parse-ratio-literal
    (lexer :: <lexer>, source-location :: <known-source-location>)
    => res :: <literal-token>;
  let slash
    = block (return)
	let contents = source-location.source.contents;
	for (posn from source-location.start-posn
	       below source-location.end-posn)
	  if (contents[posn] == as(<integer>, '/'))
	    return(posn);
	  end if;
	end for;
	error("No / in a ratio?");
	#f;
      end block;
  let numerator = parse-integer(source-location, end: slash);
  let denominator = parse-integer(source-location, start: slash + 1);
  make(<literal-token>,
       source-location: source-location,
       kind: $literal-token,
       literal: make(<literal-ratio>, value: ratio(numerator, denominator)));
end method parse-ratio-literal;

define method atof (string :: <byte-string>,
		    #key start :: <integer> = 0,
		         end: finish :: <integer> = string.size)
    => (class :: one-of(#f, #"single", #"double", #"extended"),
	value :: <ratio>);
  let class = #f;
  let posn = start;
  let sign = 1;
  let mantissa = as(<extended-integer>, 0);
  let scale :: false-or(<integer>) = #f;
  let exponent-sign = 1;
  let exponent = 0;

  // Parse the optional sign.
  if (posn < finish)
    let char = string[posn];
    if (char == '-')
      posn := posn + 1;
      sign := -1;
    elseif (char == '+')
      posn := posn + 1;
    end if;
  end if;

  block (return)
    block (parse-exponent)
      // Parse the mantissa.
      while (posn < finish)
	let char = string[posn];
	posn := posn + 1;
	if (char >= '0' & char <= '9')
	  let digit = as(<integer>, char) - as(<integer>, '0');
	  mantissa := mantissa * 10 + digit;
	  if (scale)
	    scale := scale + 1;
	  end if;
	elseif (char == '.')
	  if (scale)
	    error("bogus float.");
	  end if;
	  scale := 0;
	elseif (char == 'e' | char == 'E')
	  parse-exponent();
	elseif (char == 'd' | char == 'D')
	  class := #"double";
	  parse-exponent();
	elseif (char == 's' | char == 'S')
	  class := #"single";
	  parse-exponent();
	elseif (char == 'x' | char == 'X')
	  class := #"extended";
	  parse-exponent();
	else
	  error("bogus float.");
	end if;
      end while;
      return();
    end block;

    // Parse the exponent.
    if (posn < finish)
      let char = string[posn];
      if (char == '-')
	exponent-sign := -1;
	posn := posn + 1;
      elseif (char == '+')
	posn := posn + 1;
      end if;

      while (posn < finish)
	let char = string[posn];
	posn := posn + 1;
	if (char >= '0' & char <= '9')
	  let digit = as(<integer>, char) - as(<integer>, '0');
	  exponent := exponent * 10 + digit;
	else
	  error("bogus float");
	end if;
      end while;
    end if;
  end block;

  values(class,
	 sign * mantissa
	   * ratio(10,1) ^ (exponent-sign * exponent - (scale | 0)));
end method atof;

define variable *float-precision*
  :: one-of(#"single", #"double", #"extended") = #"double";

// parse-fp-literal -- internal.
// 
define method parse-fp-literal
    (lexer :: <lexer>, source-location :: <known-source-location>)
    => res :: <literal-token>;
  let (class, value) = atof(extract-string(source-location));
  let class = class | *float-precision*;

  make(<literal-token>,
       source-location: source-location,
       kind: $literal-token,
       literal: make(select (class)
		       #"single" => <literal-single-float>;
		       #"double" => <literal-double-float>;
		       #"extended" => <literal-extended-float>;
		     end select,
		     value: value));
end method parse-fp-literal;


// state machine.

// <state> -- internal.
//
// A particular state in the state machine.
// 
define class <state> (<object>)
  //
  // The name of this state, a symbol.  Not really used once the state
  // machine is built, but we keep it around for debugging purposes.
  slot name :: <symbol>, required-init-keyword: name:;
  //
  // The acceptance result if this state is an accepting state, or #f
  // if it is not.  Symbols are used for magic interal stuff that never
  // makes it out of the lexer (e.g. whitespace), classes for simple
  // tokens that don't need any extra parsing, and functions for more
  // complex tokens.
  slot result :: type-union(<false>, <symbol>, <integer>, <function>),
    required-init-keyword: result:;
  //
  // Either #f or a vector of next-states indexed by character code.
  // During construction, vector elements are either state names or #f.
  // After construction, the state names are replaced by the actual
  // state objects.
  slot transitions :: false-or(<simple-object-vector>),
    required-init-keyword: transitions:;
end class <state>;

define sealed domain make (singleton(<state>));
define sealed domain initialize (<state>);

define method print-object (state :: <state>, stream :: <stream>) => ();
  pprint-fields(state, stream, name: state.name);
end method print-object;


define method add-transition
    (table :: <simple-object-vector>,
     on :: type-union(<integer>, <character>, <byte-string>),
     new-state :: <symbol>)
    => ();
  //
  // Make as many entries are necessary to represent the transitions
  // from on to new-state.  On can be either an integer, a character,
  // or a byte-string.  If a byte-string, then it supports ranges
  // as in a-z.
  //
  // We also check to see if this entry classes with any earlier
  // entries.  If so, it means someone messed up editing the
  // state machine.
  // 

  local
    method add-integer-transition(on :: <integer>) => ();
      if (table[on])
	error("lexer input %= transitions to both %= and %=",
	      as(<character>, on), table[on], new-state);
      else
	table[on] := new-state;
      end if;
    end;

  select (on by instance?)
    <integer> =>
      add-integer-transition(on);
    <character> =>
      add-integer-transition(as(<integer>, on));
    <byte-string> =>
      let last = #f;
      let range = #f;
      for (char in on)
	if (range)
	  if (last)
	    let last-char :: <character> = last;
	    for (i from as(<integer>, last-char) + 1 to as(<integer>, char))
	      add-integer-transition(i);
	    end for;
	    last := #f;
	  else
	    add-integer-transition(as(<integer>, '-'));
	    add-integer-transition(as(<integer>, char));
	    last := char;
	  end if;
	  range := #f;
	elseif (char == '-')
	  range := #t;
	else 
	  add-integer-transition(as(<integer>, char));
	  last := char;
	end if;
      end for;
  end select;
end method add-transition;

define method state
    (name :: <symbol>,
     result :: type-union(<false>, <symbol>, <integer>, <function>),
     #rest transitions)
  //
  // Utility function for making states.  We expand the sequence
  // of transitions into a transition table and make the state object.
  //
  let table = size(transitions) > 0
    & make(<vector>, size: 128, fill: #f);
  for (transition in transitions)
    add-transition(table, head(transition), tail(transition));
  end for;
  make(<state>,
       name: name,
       result: result,
       transitions: table);
end method state;


define method compile-state-machine (#rest states)
    => start-state :: <state>;
  //
  // make a sorted table mapping state names to states.
  // 
  let state-table :: <simple-object-vector> =
    sort!(as(<simple-object-vector>, states),
	  test: method (a :: <state>, b :: <state>)
		  as(<byte-string>, a.name) < as(<byte-string>, b.name)
		end);

  // check that all state names are unique
  // and find the start state for later

  let start-state = #f;
  if (state-table.size > 0)
    let prev :: <state> = state-table[0];
    for (curr :: <state> in state-table)
      if (curr.name == prev.name & curr ~== prev)
	error("State %= multiply defined.", curr.name);
      end;
      if (curr.name == #"start")
	start-state := curr;
      end;
      prev := curr;
    end;
  end if;

  if (~start-state)
    error("No start state supplied in lexer");
  end;

  //
  // Now that we have a table mapping state names to states, change the
  // entries in the transition tables to refer to the new state
  // object themselves instead of just to the new state name.
  // 
  let cached-state :: <state> = state-table[0];
  for (state in state-table)
    let table = state.transitions;
    if (table)
      for (i from 0 below 128)
	let symbol :: false-or(<symbol>) = table[i];
	if (symbol)
	  let symbol-name = as(<byte-string>, symbol);

	  table[i] := // Binary search
	    block (found)
	      if (symbol == cached-state.name)
		found(cached-state);
	      end;

	      let left = 0;
	      let right = state-table.size - 1;
	      while (left <= right)
		let mid = ash(left + right, -1);
		let mid-elem :: <state> = state-table[mid];
		let mid-name = as(<byte-string>, mid-elem.name);
		if (symbol-name < mid-name)
		  right := mid - 1;
		elseif (symbol-name > mid-name)
		  left := mid + 1;
		else
		  cached-state := mid-elem;
		  found(mid-elem);
		end;
	      end while;
	      error("Symbol %= not found in binary search", symbol);
	    end block;
	end if;
      end for;
    end if;
  end for;
  //
  // Return the start state, 'cause that is what we want
  // $Initial-State to hold.
  start-state;
end method compile-state-machine;


// $Initial-State -- internal.
//
// Build the state graph and save the initial state.
// 
define constant $Initial-State
  = compile-state-machine
      (state(#"start", #f,
	     pair(" \t\f", #"whitespace"),
	     pair("\n\r", #"newline"),
	     pair('/', #"slash"),
	     pair('#', #"sharp"),
	     pair('(', #"lparen"),
	     pair(')', #"rparen"),
	     pair(',', #"comma"),
	     pair('.', #"dot"),
	     pair(';', #"semicolon"),
	     pair('[', #"lbracket"),
	     pair(']', #"rbracket"),
	     pair('{', #"lbrace"),
	     pair('}', #"rbrace"),
	     pair(':', #"colon"),
	     pair('-', #"minus"),
	     pair('=', #"equal"),
	     pair('?', #"question"),
	     pair('\\', #"backslash"),
	     pair('+', #"plus"),
	     pair('~', #"tilde"),
	     pair("*^&|", #"operator-graphic"),
	     pair("<>", #"operator-graphic-pre-equal"),
	     pair("!$%@_", #"leading-graphic"),
	     pair("A-Za-z", #"symbol"),
	     pair('\'', #"quote"),
	     pair('"', #"double-quote"),
	     pair("0-9", #"decimal")),
       state(#"whitespace", #"whitespace",
	     pair(" \t\f", #"whitespace")),
       state(#"newline", #"newline"),
       state(#"slash", make-binary-operator,
	     pair('/', #"double-slash"),
	     pair('*', #"slash-star")),
       state(#"double-slash", #"end-of-line-comment"),
       state(#"slash-star", #"multi-line-comment"),
       state(#"sharp", #f,
	     pair('(', #"sharp-paren"),
	     pair('[', #"sharp-bracket"),
	     pair('#', #"double-sharp"),
	     pair("tT", #"true"),
	     pair("fF", #"false"),
	     pair("nN", #"sharp-n"),
	     pair("rR", #"sharp-r"),
	     pair("kK", #"sharp-k"),
	     pair("aA", #"sharp-a"),
	     pair('"', #"sharp-quote"),
	     pair("bB", #"sharp-b"),
	     pair("oO", #"sharp-o"),
	     pair("xX", #"sharp-x"),
	     pair("eE", #"sharp-e"),
	     pair("iI", #"sharp-i")),
       state(#"sharp-paren", $sharp-paren-token),
       state(#"sharp-bracket", $sharp-bracket-token),
       state(#"double-sharp", $double-sharp-token),
       state(#"true", $true-token),
       state(#"false", $false-token),
       state(#"sharp-n", #f, pair("eE", #"sharp-ne")),
       state(#"sharp-ne", #f, pair("xX", #"sharp-nex")),
       state(#"sharp-nex", #f, pair("tT", #"sharp-next")),
       state(#"sharp-next", $next-token),
       state(#"sharp-r", #f, pair("eE", #"sharp-re")),
       state(#"sharp-re", #f, pair("sS", #"sharp-res")),
       state(#"sharp-res", #f, pair("tT", #"sharp-rest")),
       state(#"sharp-rest", $rest-token),
       state(#"sharp-k", #f, pair("eE", #"sharp-ke")),
       state(#"sharp-ke", #f, pair("yY", #"sharp-key")),
       state(#"sharp-key", $key-token),
       state(#"sharp-a", #f, pair("lL", #"sharp-al")),
       state(#"sharp-al", #f, pair("lL", #"sharp-all")),
       state(#"sharp-all", #f, pair('-', #"sharp-all-")),
       state(#"sharp-all-", #f, pair("kK", #"sharp-all-k")),
       state(#"sharp-all-k", #f, pair("eE", #"sharp-all-ke")),
       state(#"sharp-all-ke", #f, pair("yY", #"sharp-all-key")),
       state(#"sharp-all-key", #f, pair("sS", #"sharp-all-keys")),
       state(#"sharp-all-keys", $all-keys-token),
       state(#"sharp-i", #f, pair("fF", #"sharp-if")),
       state(#"sharp-if", $feature-if-token),
       state(#"sharp-quote", #f,
	     pair('"', #"quoted-keyword"), 
	     pair('\\', #"sharp-quote-escape"),
	     pair(" !#-[]-~", #"sharp-quote")),
       state(#"sharp-quote-escape", #f,
	     pair("\\abefnrt0\"<", #"sharp-quote")),
       state(#"quoted-keyword", make-quoted-symbol),
       state(#"sharp-b", #f, pair("01", #"binary-integer")),
       state(#"binary-integer", parse-integer-literal,
	     pair("01", #"binary-integer")),
       state(#"sharp-o", #f, pair("0-7", #"octal-integer")),
       state(#"octal-integer", parse-integer-literal,
	     pair("0-7", #"octal-integer")),
       state(#"sharp-x", #f, pair("0-9a-fA-F", #"hex-integer")),
       state(#"hex-integer", parse-integer-literal,
	     pair("0-9a-fA-F", #"hex-integer")),
       state(#"sharp-e", #f,
	     pair('-', #"sharp-e-minus"),
	     pair("0-9", #"extended-integer"),
	     pair("bB", #"sharp-b"),
	     pair("oO", #"sharp-o"),
	     pair("xX", #"sharp-x"),
	     pair("lL", #"sharp-el"),
	     pair("nN", #"sharp-en")),
       state(#"sharp-e-minus", #f,
	     pair("0-9", #"extended-integer")),
       state(#"sharp-el", #f, pair("sS", #"sharp-els")),
       state(#"sharp-els", #f, pair("eE", #"sharp-else")),
       state(#"sharp-else", $feature-else-token,
	     pair("iI", #"sharp-elsei")),
       state(#"sharp-elsei", #f, pair("fF", #"sharp-elseif")),
       state(#"sharp-elseif", $feature-elseif-token),
       state(#"sharp-en", #f, pair("dD", #"sharp-end")),
       state(#"sharp-end", #f, pair("iI", #"sharp-endi")),
       state(#"sharp-endi", #f, pair("fF", #"sharp-endif")),
       state(#"sharp-endif", $feature-endif-token),
       state(#"extended-integer", parse-integer-literal,
	     pair("0-9", #"extended-integer")),
       state(#"lparen", $left-paren-token),
       state(#"rparen", $right-paren-token),
       state(#"comma", $comma-token),
       state(#"dot", $dot-token,
	     pair('.', #"dot-dot"),
	     pair("0123456789", #"fp-frac")),
       state(#"dot-dot", #f, pair('.', #"ellipsis")),
       state(#"ellipsis", $ellipsis-token),
       state(#"semicolon", $semicolon-token),
       state(#"colon", #f,
	     pair('=', #"colon-equal"),
	     pair(':', #"double-colon"),
	     pair("a-zA-Z", #"cname"),
	     pair("0-9", #"cname-leading-numeric"),
	     pair("!$%@_", #"cname-leading-graphic"),
	     pair("+/", #"cname-binop"),
	     pair('-', #"cname-binop"),
	     pair("*^&|", #"cname-graphic-binop"),
	     pair('~', #"cname-tilde"),
	     pair("<>", #"cname-angle")),
       state(#"colon-equal", make-binary-operator,
	     pair('=', #"cname-binop")),
       state(#"double-colon", $double-colon-token,
	     pair('=', #"cname-binop")),
       state(#"lbracket", $left-bracket-token),
       state(#"rbracket", $right-bracket-token),
       state(#"lbrace", $left-brace-token),
       state(#"rbrace", $right-brace-token),
       state(#"minus", make-minus,
	     pair("0-9", #"signed-decimal")),
       state(#"equal", make-equal,
	     pair('=', #"double-equal"),
	     pair('>', #"arrow"),
	     pair("a-zA-Z", #"symbol"),
	     pair("-0-9!&*<|^$%@_+~?/", #"leading-graphic")),
       state(#"double-equal", make-double-equal,
	     pair("a-zA-Z", #"symbol"),
	     pair("-0-9!&*<=>|^$%@_+~?/", #"leading-graphic")),
       state(#"arrow", $arrow-token,
	     pair("a-zA-Z", #"symbol"),
	     pair("-0-9!&*<=>|^$%@_+~?/", #"leading-graphic")),
       state(#"question", $question-token,
	     pair('?', #"double-question"),
	     pair('=', #"question-equal")),
       state(#"double-question", $double-question-token),
       state(#"question-equal", $question-equal-token),
       state(#"backslash", #f,
	     pair("-+/", #"backslash-done"),
	     pair('~', #"backslash-tilde"),
	     pair(':', #"backslash-colon"),
	     pair("a-zA-Z", #"backslash-symbol"),
	     pair("0-9", #"backslash-digit"),
	     pair("!$%@_", #"backslash-graphic"),
	     pair("&*^|", #"backslash-graphic-done"),
	     pair("=<>", #"backslash-graphic-pre-equal")),
       state(#"backslash-done", make-quoted-name),
       state(#"backslash-tilde", make-quoted-name,
	     pair('=', #"backslash-tilde-equal")),
       state(#"backslash-tilde-equal", make-quoted-name,
	     pair('=', #"backslash-done")),
       state(#"backslash-colon", #f,
	     pair('=', #"backslash-done")),
       state(#"backslash-graphic", #f,
	     pair("-0-9!&*<=>|^$%@_+~?/", #"backslash-graphic"),
	     pair("a-zA-Z", #"backslash-symbol")),
       state(#"backslash-graphic-done", make-quoted-name,
	     pair("-0-9!&*<=>|^$%@_+~?/", #"backslash-graphic"),
	     pair("a-zA-Z", #"backslash-symbol")),
       state(#"backslash-graphic-pre-equal", make-quoted-name,
	     pair('=', #"backslash-graphic-done"),
	     pair("-0-9!&*<>|^$%@_+~?/",#"backslash-graphic"),
	     pair("a-zA-Z", #"backslash-symbol")),
       state(#"backslash-symbol", make-quoted-name,
	     pair("-+~?/!&*<=>|^$%@_0-9a-zA-Z", #"backslash-symbol")),
       state(#"backslash-digit", #f,
	     pair("-0-9!&*<=>|^$%@_+~?/", #"backslash-digit"),
	     pair("a-zA-Z", #"backslash-digit-alpha")),
       state(#"backslash-digit-alpha", #f,
	     pair("-0-9!&*<=>|^$%@_+~?/", #"backslash-digit"),
	     pair("a-zA-Z", #"backslash-symbol")),
       state(#"plus", make-binary-operator,
	     pair("0-9", #"signed-decimal")),
       state(#"tilde", make-tilde,
	     pair('=', #"tilde-equal")),
       state(#"tilde-equal", make-binary-operator,
	     pair('=', #"tilde-equal-equal")),
       state(#"tilde-equal-equal", make-binary-operator),
       state(#"operator-graphic", make-binary-operator,
	     pair("a-zA-Z", #"symbol"),
	     pair("-0-9!&*<=>|^$%@_+~?/", #"leading-graphic")),
       state(#"operator-graphic-pre-equal", make-binary-operator,
	     pair('=', #"operator-graphic"),
	     pair("a-zA-Z", #"symbol"),
	     pair("-0-9!&*<>|^$%@_+~?/", #"leading-graphic")),
       state(#"leading-graphic", #f,
	     pair("0-9!&*<=>|^$%@_+~?/", #"leading-graphic"),
	     pair('-', #"leading-graphic"),
	     pair("a-zA-Z", #"symbol")),
       state(#"symbol", make-identifier,
	     pair("a-zA-Z0-9!&*<=>|^$%@_+~?/", #"symbol"),
	     pair('-', #"symbol"),
	     pair(':', #"colon-keyword")),
       state(#"colon-keyword", make-keyword-symbol,
	     pair("a-zA-Z", #"cname"),
	     pair("0-9", #"cname-leading-numeric"),
	     pair("!$%@_", #"cname-leading-graphic"),
	     pair("+/", #"cname-binop"),
	     pair('-', #"cname-binop"),
	     pair("*^&|", #"cname-graphic-binop"),
	     pair('~', #"cname-tilde"),
	     pair("<>", #"cname-angle"),
	     pair('=', #"cname-equal"),
	     pair(':', #"cname-colon")),
       state(#"cname-binop", make-constrained-name),
       state(#"cname-graphic-binop", make-constrained-name,
	     pair("0-9!&*<>|^$%@_+~?/=", #"cname-leading-graphic"),
	     pair('-', #"cname-leading-graphic"),
	     pair("a-zA-Z", #"cname")),
       state(#"cname-tilde", #f,
	     pair('=', #"cname-tilde-equal")),
       state(#"cname-tilde-equal", make-constrained-name,
	     pair('=', #"cname-tilde-double-equal")),
       state(#"cname-tilde-double-equal", make-constrained-name),
       state(#"cname-angle", make-constrained-name,
	     pair('=', #"cname-angle-equal"),
	     pair("0-9!&*<>|^$%@_+~?/", #"cname-leading-graphic"),
	     pair('-', #"cname-leading-graphic"),
	     pair("a-zA-Z", #"cname")),
       state(#"cname-angle-equal", make-constrained-name,
	     pair("0-9!&*<>|^$%@_+~?/=", #"cname-leading-graphic"),
	     pair('-', #"cname-leading-graphic"),
	     pair("a-zA-Z", #"cname")),
       state(#"cname-equal", make-constrained-name,
	     pair('=', #"cname-binop")),
       state(#"cname-colon", #f,
	     pair('=', #"cname-binop")),
       state(#"cname-leading-numeric", #f,
	     pair("0-9!&*<>|^$%@_+~?/=", #"cname-leading-numeric"),
	     pair('-', #"cname-leading-numeric"),
	     pair("a-zA-Z", #"cname-numeric-alpha")),
       state(#"cname-numeric-alpha", #f,
	     pair("0-9!&*<>|^$%@_+~?/=",
		  #"cname-leading-numeric"),
	     pair('-', #"cname-leading-numeric"),
	     pair("a-zA-Z", #"cname")),
       state(#"cname-leading-graphic", #f,
	     pair("0-9!&*<>|^$%@_+~?/=", #"cname-leading-graphic"),
	     pair('-', #"cname-leading-graphic"),
	     pair("a-zA-Z", #"cname")),
       state(#"cname", make-constrained-name,
	     pair("a-zA-Z0-9!&*<>|^$%@_+~?/=", #"cname"),
	     pair('-', #"cname")),
       state(#"quote", #f,
	     pair(" -&(-[]-~", #"quote-char"),
	     pair('\\', #"quote-escape")),
       state(#"quote-char", #f,
	     pair('\'', #"character")),
       state(#"character", make-character-literal),
       state(#"quote-unicode-escape", #f,
             pair('>', #"quote-char"),
             pair("a-fA-F0-9", #"quote-unicode-escape")),
       state(#"quote-escape", #f,
             pair('<', #"quote-unicode-escape"),
             pair("\\abefnrt0'\"", #"quote-char")),
       state(#"double-quote", #f,
	     pair('"', #"string"), 
	     pair('\\', #"double-quote-escape"),
	     pair(" !#-[]-~", #"double-quote")),
       state(#"string", make-string-literal),
       state(#"double-quote-escape", #f,
	     pair("\\abefnrt0'\"<", #"double-quote")),
       state(#"decimal", parse-integer-literal,
	     pair("0-9", #"decimal"),
	     pair('/', #"decimal-slash"),
	     pair('.', #"fp-frac"),
	     pair("eEsSdDxX", #"decimal-e"),
	     pair("abcf-rt-wyzABCF-RT-WYZ", #"numeric-alpha"),
	     pair("!&*<=>|^$%@_+~?", #"leading-numeric"),
	     pair('-', #"leading-numeric")),
       state(#"decimal-slash", #f,
	     pair("0-9", #"ratio"),
	     pair("a-zA-Z", #"numeric-alpha"),
	     pair("!&*<=>|^$%@_+~?/", #"leading-numeric"),
	     pair('-', #"leading-numeric")),
       state(#"numeric-alpha", #f,
	     pair("a-zA-Z", #"symbol"),
	     pair("0-9!&*<=>|^$%@_+~?/", #"leading-numeric"),
	     pair('-', #"leading-numeric")),
       state(#"leading-numeric", #f,
	     pair("a-zA-Z", #"numeric-alpha"),
	     pair("0-9!&*<=>|^$%@_+~?/", #"leading-numeric"),
	     pair('-', #"leading-numeric")),

       state(#"ratio", parse-ratio-literal,
	     pair("0-9", #"ratio"),
	     pair("a-zA-Z", #"numeric-alpha"),
	     pair("-!&*<=>|^$%@_+~?/", #"leading-numeric")),

       state(#"signed-decimal", parse-integer-literal,
	     pair("0-9", #"signed-decimal"),
	     pair('/', #"signed-decimal-slash"),
	     pair('.', #"fp-frac")),
       state(#"signed-decimal-slash", #f,
	     pair("0-9", #"signed-ratio")),
       state(#"signed-ratio", parse-ratio-literal,
	     pair("0-9", #"signed-ratio")),
       
       state(#"fp-frac", parse-fp-literal,
	     pair("0-9", #"fp-frac"),
	     pair("eEsSdDxX", #"fp-e")),
       state(#"fp-e", #f,
	     pair('-', #"fp-e-sign"),
	     pair('+', #"fp-e-sign"),
	     pair("0-9", #"fp-exp")),
       state(#"fp-e-sign", #f,
	     pair("0-9", #"fp-exp")),
       state(#"fp-exp", parse-fp-literal,
	     pair("0-9", #"fp-exp")),
       
       state(#"decimal-e", #f,
	     pair("a-zA-Z", #"symbol"),
	     pair("0-9", #"decimal-exp"),
	     pair("!&*<=>|^$%@_~?/", #"leading-numeric"),
	     pair('-', #"decimal-e-sign"),
	     pair('+', #"decimal-e-sign")),
       state(#"decimal-exp", parse-fp-literal,
	     pair("0-9", #"decimal-exp"),
	     pair("a-zA-Z", #"numeric-alpha"),
	     pair("!&*<=>|^$%@_+~?/", #"leading-numeric"),
	     pair('-', #"leading-numeric")),
       state(#"decimal-e-sign", #f,
	     pair("0-9", #"decimal-exp"),
	     pair("a-zA-Z", #"numeric-alpha"),
	     pair("!&*<=>|^$%@_+~?/", #"leading-numeric"),
	     pair('-', #"leading-numeric")));


// Features.

define variable *features* :: <list> = #();

define method add-feature (feature :: <symbol>) => ();
  *features* := add-new!(*features*, feature);
end method add-feature;

define method remove-feature (feature :: <symbol>) => ();
  *features* := remove!(*features*, feature);
end method remove-feature;

define method feature-present? (feature :: <symbol>) => present? :: <boolean>;
  member?(feature, *features*);
end method feature-present?;
						    

// Conditional compilation stuff.

define class <conditional-state> (<object>)
  slot active? :: <boolean>,
    required-init-keyword: active:;
  slot do-else? :: <boolean>,
    required-init-keyword: do-else:;
  slot seen-else? :: <boolean>,
    init-value: #f;
  slot old-state :: false-or(<conditional-state>),
    required-init-keyword: old-state:;
end class <conditional-state>;

define sealed domain make (singleton(<conditional-state>));
define sealed domain initialize (<conditional-state>);


define method is-active? (state :: false-or(<conditional-state>)) => res :: <boolean>;
  ~state | state.active?;
end method is-active?;


define method parse-error (token :: <token>) => ();
  compiler-fatal-error
    ("syntax error in feature condition at or before %=", token);
end method parse-error;


define method parse-feature-term (lexer :: <lexer>) => res :: <boolean>;
  let token = internal-get-token(lexer);
  let kind = token.token-kind;
  if (kind == $left-paren-token)
    parse-feature-expr(lexer);
  elseif (kind == $tilde-token)
    ~parse-feature-term(lexer);
  elseif (kind >= $define-token & kind <= $quoted-name-token)
    feature-present?(token.token-symbol);
  else
    parse-error(token);
  end if;
end method parse-feature-term;


define method parse-feature-expr (lexer :: <lexer>) => res :: <boolean>;
  block (return)
    let res = parse-feature-term(lexer);
    while (#t)
      let token = internal-get-token(lexer);
      let kind = token.token-kind;
      if (kind == $right-paren-token)
	return(res);
      elseif (kind == $other-binary-operator-token)
	select (token.token-symbol)
	  #"&" =>
	    if (~parse-feature-term(lexer))
	      res := #f;
	    end if;
	  #"|" =>
	    if (parse-feature-term(lexer))
	      res := #t;
	    end if;
	  otherwise =>
	    parse-error(token);
	end select;
      else
	parse-error(token);
      end if;
    end while;
  end block;
end method parse-feature-expr;
    
define method parse-conditional (lexer :: <lexer>) => res :: <boolean>;
  let token = internal-get-token(lexer);
  unless (token.token-kind == $left-paren-token)
    parse-error(token);
  end unless;
  parse-feature-expr(lexer);
end method parse-conditional;


// lexer

// <lexer> -- exported.
//
// An object holding the current lexer state.
//
define class <lexer> (<tokenizer>)
  //
  // The source file we are currently tokenizing.
  slot lexer-source :: <source>, required-init-keyword: source:;
  //
  // The position we are currently at in the source file.
  slot posn :: <integer>, required-init-keyword: start-posn:;
  //
  // The line number we are currently working on.
  slot line :: <integer>, required-init-keyword: start-line:;
  //
  // The position that this line started at.
  slot line-start :: <integer>, required-init-keyword: start-posn:;
  //
  // A list of tokens that have been unread.
  slot pushed-tokens :: <list>, init-value: #();
  //
  slot conditional-state :: false-or(<conditional-state>), init-value: #f;
end class <lexer>;

define sealed domain make (singleton(<lexer>));
define sealed domain initialize (<lexer>);

define method print-object (lexer :: <lexer>, stream :: <stream>) => ();
  pprint-fields(lexer, stream,
		source: lexer.lexer-source,
		posn: lexer.posn,
		line: lexer.line,
		column: lexer.posn - lexer.line-start + 1);
end method print-object;

// skip-multi-line-comment -- internal.
//
// Skip a multi-line comment, taking into account nested comments.
// This is just a simple state machine implemented via a loop and
// a select statement.
//
define function skip-multi-line-comment (lexer :: <lexer>,
				       start :: <integer>,
				       #key inhibit-nesting :: <boolean>)
    => result :: false-or(<integer>);
  block (return)
    let contents = lexer.lexer-source.contents;
    let length = contents.size;
    let depth = 1;
    let state = #"seen-nothing";
    for (posn from start below length)
      let char = as(<character>, contents[posn]);
      select (state)
	#"seen-nothing" =>
	  // Seen nothing of interest.  Look for the start of any of
	  // /*, //, or */.
	  //
	  if (char == '/')
	    state := #"seen-slash";
	  elseif (char == '*')
	    state := #"seen-star";
	  elseif (char == '\n' | char == '\r')
	    lexer.line := lexer.line + 1;
	    lexer.line-start := posn + 1;
	    state := #"seen-nothing";
	  else
	    state := #"seen-nothing";
	  end if;
	#"seen-slash" =>
	  // Okay, we've seen a slash.  Look to see if it was
	  // one of /*, //, or just a random slash in the source code.
	  //
	  if (char == '/')
	    if (inhibit-nesting)
	      state := #"seen-nothing";
	    else
	      state := #"seen-slash-slash";
	    end if;
	  elseif (char == '*')
	    inhibit-nesting | (depth := depth + 1);
	    state := #"seen-nothing";
	  elseif (char == '\n' | char == '\r')
	    lexer.line := lexer.line + 1;
	    lexer.line-start := posn + 1;
	    state := #"seen-nothing";
	  else
	    state := #"seen-nothing";
	  end if;
	#"seen-star" =>
	  // Okay, we've seen a star.  Look to see if it was */ or a
	  // random star. We also have to check to see if this next
	  // character is another star, because if so, it might be
	  // the start of a */.
	  //
	  if (char == '/')
	    if (depth == 1)
	      return(posn + 1); 
	    else
	      depth := depth - 1;
	      state := #"seen-nothing";
	    end if;
	  elseif (char == '*')
	    state := #"seen-star";
	  elseif (char == '\n' | char == '\r')
	    lexer.line := lexer.line + 1;
	    lexer.line-start := posn + 1;
	    state := #"seen-nothing";
	  else
	    state := #"seen-nothing";
	  end if;
	#"seen-slash-slash" =>
	  // We've seen a //, so skip until the end of the line.
	  //
	  if (char == '\n' | char == '\r')
	    lexer.line := lexer.line + 1;
	    lexer.line-start := posn + 1;
	    state := #"seen-nothing";
	  else
	    state := #"seen-slash-slash";
	  end if;
	otherwise =>
	  error("Unknown lexer state while reading comment.");
      end select;
    end for;
    #f;
  end block;
end function skip-multi-line-comment;


// lexed-location -- internal.
//
// Extract location from lexer given token start and end position.
//
define function lexed-location
  (token-start :: <integer>,
   token-end :: <integer>,
   lexer :: <lexer>,
   #key start-line :: <integer> = lexer.line,
	start-position :: <integer> = lexer.line-start)
 => res :: <known-source-location>;
make(<known-source-location>,
     source: lexer.lexer-source,
     start-posn: token-start,
     start-line: start-line,
     start-column: token-start - start-position,
     end-posn: token-end,
     end-line: lexer.line,
     end-column: token-end - lexer.line-start);
end function;

// internal-get-token -- internal.
//
// Tokenize the next token and return it.
//
define method internal-get-token (lexer :: <lexer>) => res :: <token>;
  //
  // Basically, just record where we are starting, and keep
  // advancing the state machine until there are no more possible
  // advances.  We don't stop at the first accepting state we find,
  // because the longest token is supposed to take precedence.  We
  // just note where the last accepting state we came across was,
  // and then when the state machine jams, we just use that latest
  // accepting state's result.
  // 
  let contents :: <file-contents> = lexer.lexer-source.contents;
  let length :: <integer> = contents.size;
  let result-kind = #f;
  let result-start :: <integer> = lexer.posn;
  let result-end :: false-or(<integer>) = #f;
  local
    method repeat (state, posn :: <integer>) => ();
      if (state.result)
	//
	// It is an accepting state, so record the result and where
	// it ended.
	// 
	result-kind := state.result;
	result-end := posn;
      end if;
      //
      // Try advancing the state machine once more if possible.
      // 
      if (posn < length)
	let table = state.transitions;
	if (table)
	  let char :: <byte> = contents[posn];
	  let new-state = char < 128 & table[char];
	  if (new-state)
	    repeat(new-state, posn + 1);
	  else
	    maybe-done();
	  end if;
	else
	    maybe-done();
	end;
      else
	maybe-done();
      end if;
    end method repeat,
    method maybe-done () => ();
      //
      // maybe-done is called when the state machine cannot be
      // advanced any further.  It just checks to see if we really
      // are done or not.
      //
      if (instance?(result-kind, <symbol>))
	//
	// The result-kind is a symbol if this is one of the magic
	// accepting states.  Instead of returning some token, we do
	// some special processing depending on exactly what symbol
	// it is, and then start the state machine over at the
	// initial state.
	//
	select (result-kind)
	  #"whitespace" =>
	    #f;
	  #"newline" =>
	    lexer.line := lexer.line + 1;
	    lexer.line-start := result-end;
	  #"end-of-line-comment" =>
	    for (i :: <integer> from result-end below length,
		 until: (contents[i] == as(<integer>, '\n')
			| contents[i] == as(<integer>, '\r')))
	    finally
	      result-end := i;
	    end for;
	  #"multi-line-comment" =>
	    let line-start :: <integer> = lexer.line-start;
	    let line :: <integer> = lexer.line;
	    let prev-result-end = result-end;
	    result-end := skip-multi-line-comment(lexer, result-end);
	    
	    unless (result-end)
	      lexer.line-start := line-start;
	      lexer.line := line;
	      result-end := skip-multi-line-comment(lexer,
						    prev-result-end,
						    inhibit-nesting: #t);
	      if (result-end)
		compiler-warning-location
		  (lexed-location
		    (prev-result-end - 2, result-end, lexer,
		     start-line: line, start-position: line-start),
		   "nested comment unterminated, ignoring nesting");
	      else
		lexer.line-start := line-start;
		lexer.line := line;
	      end;
	    end;
	end select;
	result-kind := #f;
	if (result-end)
	  result-start := result-end;
	  result-end := #f;
	  repeat($Initial-State, result-start);
	end if;
      end if;
    end method maybe-done;
  repeat($Initial-State, lexer.posn);
  if (~result-kind)
    //
    // If result-kind is #f, that means we didn't find an accepting
    // state.  Check to see if that means we are at the end or hit
    // an error.
    // 
    if (result-start == length)
      result-kind := $eof-token;
      result-end := result-start;
    else
      result-kind := $error-token;
      result-end := result-start + 1;
    end if;
  end if;
  //
  // Save the current token's end position so that the next token
  // starts here.
  //
  let known-result-end :: <integer> = result-end;
  lexer.posn := known-result-end;
  //
  // Make a source location for the current token.
  // 
  let source-location // lexed-location!!!
    = make(<known-source-location>,
	   source: lexer.lexer-source,
	   start-posn: result-start,
	   start-line: lexer.line,
	   start-column: result-start - lexer.line-start,
	   end-posn: known-result-end,
	   end-line: lexer.line,
	   end-column: known-result-end - lexer.line-start);
  //
  // And finally, make and return the actual token.
  // 
  select(result-kind by instance?)
    <integer> =>
      make(<token>, source-location: source-location, kind: result-kind);
    <function> =>
      result-kind(lexer, source-location);
    otherwise =>
      error("oops");
      #f;
  end select;
end method internal-get-token;

define method get-token (lexer :: <lexer>)
    => (token :: <token>, srcloc :: <source-location>);
  block (return)
    if (lexer.pushed-tokens ~= #())
      //
      // There are some unread tokens, so extract one of them instead of
      // consuming any more stuff from the source.
      // 
      let result = lexer.pushed-tokens.head;
      lexer.pushed-tokens := lexer.pushed-tokens.tail;
      return(result, result.head, result.tail);
    end if;
    //
    // There are no pending unread tokens, so extract the next one.
    while (#t)
      let token = internal-get-token(lexer);
      select (token.token-kind)
	$feature-if-token =>
	  let cond = parse-conditional(lexer);
	  lexer.conditional-state
	    := if (lexer.conditional-state.is-active?)
		 make(<conditional-state>, active: cond, do-else: ~cond,
		      old-state: lexer.conditional-state);
	       else
		 make(<conditional-state>, active: #f, do-else: #f,
		      old-state: lexer.conditional-state);
	       end if;
	  
	$feature-elseif-token =>
	  if (lexer.conditional-state == #f)
	    compiler-fatal-error("#elseif with no matching #if");
	  elseif (lexer.conditional-state.seen-else?)
	    compiler-fatal-error("#elseif after #else in one #if");
	  elseif (parse-conditional(lexer))
	    lexer.conditional-state.active?
	      := lexer.conditional-state.do-else?;
	    lexer.conditional-state.do-else? := #f;
	  else
	    lexer.conditional-state.active? := #f;
	  end if;

	$feature-else-token =>
	  if (lexer.conditional-state == #f)
	    compiler-fatal-error("#else with no matching #if");
	  elseif (lexer.conditional-state.seen-else?)
	    compiler-fatal-error("#else after #else in one #if");
	  else
	    lexer.conditional-state.seen-else? := #t;
	    lexer.conditional-state.active?
	      := lexer.conditional-state.do-else?;
	  end if;

	$feature-endif-token =>
	  if (lexer.conditional-state == #f)
	    compiler-fatal-error("#endif with no matching #if");
	  else
	    lexer.conditional-state := lexer.conditional-state.old-state;
	  end if;
	  
	otherwise =>
	  if (lexer.conditional-state.is-active?)
	    return(token, token.source-location);
	  end if;
      end select;
    end while;
  end block;
end method get-token;

// unget-token -- exported.
//
// Pushes token back so that the next call to get-token will return
// it.  Used by the parser when it wants to put back its lookahead
// token.
//
define method unget-token
    (lexer :: <lexer>, token :: <token>, srcloc :: <source-location>) => ();
  lexer.pushed-tokens := pair(token, pair(lexer.pushed-tokens, srcloc));
end method unget-token;
