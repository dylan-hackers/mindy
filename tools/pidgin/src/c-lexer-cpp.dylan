module:  c-lexer
author:  Robert Stockton (rgs@cs.cmu.edu)
synopsis: Encapsulates the lexical conventions of the C language.  Along with
          c-lexer-cpp.dylan, this file also incorporates most of the
          functionality of CPP.
copyright: see below
	   This code was produced by the Gwydion Project at Carnegie Mellon
	   University.  If you are interested in using this code, contact
	   "Scott.Fahlman@cs.cmu.edu" (Internet).

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998 - 2003  Gwydion Dylan Maintainers
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

//======================================================================
// This file contains functions which emulate the functionality of CPP.  These
// functions are then called by the general lexing routines in
// "c-lexer.dylan".  The only items directly exported from this file are
// "default-cpp-table" and "include-path", which are made available to other
// modules so that they can add elements before the parse begins.  In
// particular, module "portability" is expected to define the "standard"
// definitions for whatever machine we are compiling for as well as the
// standard "include" directories.
//======================================================================

// This table maps strings defined by the preprocessor into tokens.  Initial
// values are taken from the appropriate portability module.  Entries should
// be sequences of tokens in reverse order.  These sequences will not
// themselves be "expanded".  In other words, some of the tokens may
// themselves have entries in the table.  Macro expansion will, therefore,
// recursively expand each "expanded" token, recursing as deeply as necessary.
//
define constant default-cpp-table = make(<string-table>);

// This routine grabs tokens from within the "parameter list" of a
// parameterized macro use.  The calling routine should have already consumed
// the opening paren.  The result is a reversed list of reversed token lists.
// In other words "foo, (bar + baz))" would result in
//   #(#(rparen, baz, plus, bar, lparen), #(foo)).
//
// Although this seems an odd order, it turns out to be fairly convenient for
// matching to the formal parameters and for actually expanding the token
// sequences when they are matched.
//
define function get-macro-params
    (state :: <tokenizer>,
     params :: <list>,
     #key expand)
 => (params :: <list>);
  let paren-count = 0;
  for (token = get-token(state) then get-token(state, expand: expand),
       list = #() then pair(token, list),
       until: (paren-count == 0
		& instance?(token, type-union(<rparen-token>, <comma-token>))))
    select (token by instance?)
      <eof-token>, <error-token> =>
	parse-error(state, "Badly formed macro use.");
      <lparen-token> => paren-count := paren-count + 1;
      <rparen-token> => paren-count := paren-count - 1;
      otherwise => #f;
    end select;
  finally
    if (instance?(token, <comma-token>))
      get-macro-params(state, pair(list, params));
    else
      pair(list, params);
    end if;
  end for;
end function get-macro-params;

// When we are generating expansions, we wish to make copies of the token
// rather than return the original.  This will put the right character
// position and "generator" in the token.
//
define method copy-token
    (token :: <token>, tokenizer :: <tokenizer>) => (result :: <token>);
  make(object-class(token), position: tokenizer.position,
       string: string-value(token), generator: tokenizer);
end method copy-token;

define constant empty-table = make(<self-organizing-list>);

// check-cpp-expansion -- exported function.
//
// Recursively handle expansion of preprocessor tokens.  Returns #f if the
// string has no expansion.  Otherwise, adds a series of tokens to the
// "unget-stack", so that the next get-token call will get the first expanded
// token.  This routine will recurse as deeply as necessary to make sure that
// all tokens are expanded.  The recursive expansions are actually done from
// back to front, but this seems not to yield any particular problems on
// existing header files.  It is, however, possible that some *very* obscure
// hacks might fail.
// 
// Note that the pushed tokens are newly generated copies of the ones in
// cpp-table.  Thus they will have appropriate location information for error
// reporting. 
//
// Added forbidden-expansions to prevent recursive macro
// expansion. [obsolete: This is insufficient by itself; we're also supposed to tag
// tokens which we weren't allow to expand to ensure that they never get
// expanded in any other place. But this should handle the most common
// problems.]
// Now the called functions accept #key expand to be a list of forbidden-expansions
// from the outer level. This means that the expansion will be done for all but the
// names included in the list. This way we can expand macro calls to "foo" that appear
// as actuals of an outer macro call to "foo" (as in foo(foo(XXX))) but suppress
// expansion in a "#define foo(P) foo(P)" situation, because "foo" in the rhs will
// be put in the forbidden-expansions.

define constant $maximum-cpp-expansion-depth = 32;

define /* exported */ function check-cpp-expansion
    (string :: <string>, tokenizer :: <tokenizer>,
     #key parameters: parameter-table = empty-table,
     forbidden-expansions = #(),
     current-depth = 0)
 => (result :: <boolean>);
  let headless-string 
    = if (string.first == '#') copy-sequence(string, start: 1) else string end;
  let token-list :: type-union(<sequence>, <false>)
    = (element(parameter-table, headless-string, default: #f)
	 | element(tokenizer.cpp-table, string, default: #f));

  local method expand-inner(token-list :: <sequence>, #key parameters = empty-table)
  	let forbidden = pair(string, forbidden-expansions);
	for (token in token-list)
	  unless (check-cpp-expansion(token.string-value, tokenizer,
				      parameters: parameters,
				      current-depth: current-depth + 1,
				      forbidden-expansions: forbidden))
	    // Successful call will have already pushed the expanded tokens
	    let cls = element(reserved-word-table,
			      token.string-value, default: #f);
	    if (cls)
	      let reserved-word-token = make(cls,
					     position: tokenizer.position,
					     string: string-value(token),
					     generator: tokenizer);
	      push(tokenizer.unget-stack, reserved-word-token);
	    else
	      push(tokenizer.unget-stack, copy-token(token, tokenizer));
	    end if;
	  end unless;
	finally
	  #t;
	end for;
      end method expand-inner;

  case
    current-depth >= $maximum-cpp-expansion-depth =>
      parse-error(tokenizer, "Preprocessor macro expansion of ~s too deep",
		  string);
    member?(string, forbidden-expansions, test: \=) =>
      #f;
    string.first == '#' =>
      if (string = "##")
	// Special case for <pound-pound-token>
	#f;
      else
	if (~token-list)
	  parse-error(tokenizer, "%s in macro not matched.", string)
	end if;

	// Concatenate the parameter's string-values, bracketed by double
	// quotes so that we get a string literal.  We won't do expansion --
	// hopefully this won't cause problems in "real" code.
	let reversed-strings = map(string-value, token-list);
	let quoted = pair("\"", reverse!(pair("\"", reversed-strings)));
	push(tokenizer.unget-stack,
	     make(<string-literal-token>, position: tokenizer.position,
		  generator: tokenizer, string: apply(concatenate, quoted)));
	#t;
      end if;
    ~token-list =>
      #f;
    token-list.empty? =>
      #t;
    instance?(token-list.head, <list>) =>
      // This is a parameterized macro.  Therefore we have to do some really
      // hairy expansion.
      let lparen-token = get-token(tokenizer);
      if (~instance?(lparen-token, <lparen-token>))
	// Apparently some systems (i.e. VC++) accept non-parenthesized uses
	// of parameterized macros as ordinary symbols.  Therefore, we'd
	// better do likewise.
	push(tokenizer.unget-stack, lparen-token);
	#f;
      else
	let params = get-macro-params(tokenizer, #(), expand: forbidden-expansions);
	let formal-params = token-list.head;
	if (params.size ~= formal-params.size)
	  parse-error(tokenizer, "Wrong number of parameters in macro use.")
	end if;
	let params-table = make(<self-organizing-list>, test: \=);
	// Add params to params table, keyed by formal params.
	for (key in formal-params, value in params)
	  params-table[key] := value;
	end for;
	expand-inner(token-list.tail, parameters: params-table);
      end if;
    otherwise =>
      // Depends upon the fact that tokens are stored in reverse order in the
      // stored macro expansion.
      expand-inner(token-list);
  end case;
end function check-cpp-expansion;

define function partially-expand-cpp-tokens
    (tokens :: <sequence>,
     tokenizer :: <tokenizer>,
     #key parameters: parameter-table :: <self-organizing-list> = empty-table)
 => (tokens :: <deque>)
  // Depends upon the fact that tokens are stored in reverse order in the
  // stored macro expansion. We return the partially expanded tokens in the
  // same order.
  let return-tokens = make(<deque>);
  
  local
    method concatenate-tokens (token-1 :: <token>, token-2 :: <token>)
     => (token-3 :: <token>)
      lex-identifier(tokenizer,
		     token-1.position,
		     concatenate(token-1.string-value,
				 token-2.string-value),
		     expand: #f,
		     cpp-line: #f);
    end method,
    method check-stack (current-token :: <token>) => ()
      if (empty?(return-tokens))
	parse-error(current-token,
		    "%= cannot be last token in macro expansion",
		    current-token.string-value);
      end if;
    end method,
    method expand-parameter (token :: <token>) => (expanded :: <token>)
      // XXX - should probably check forbidden-expansions
      let param = element(parameter-table, token.string-value, default: #f);
      case
	~param => token;
	param.size == 1 => param.first;
	otherwise =>
	  parse-error(token,
		      "the expansion of %= was not a single token",
		      token.string-value);
      end case;
    end method;

  // Munge the token list.
  let concatenate-next? = #f;
  for (token in tokens)
    case
      concatenate-next? =>
	let previous = expand-parameter(pop-last(return-tokens));
	let current = expand-parameter(token);
	push-last(return-tokens, concatenate-tokens(current, previous));
	concatenate-next? := #f;
      instance?(token, <pound-token>) =>
	check-stack(token);
	// XXX - Just generate a garbage string token for now.
	push-last(return-tokens,
		  make(<string-literal-token>,
		       string: "This isn't the string you're looking for.",
		       position: pop-last(return-tokens).position,
		       generator: tokenizer));
      instance?(token, <pound-pound-token>) =>
	concatenate-next? := #t;
	check-stack(token);
      otherwise =>
	push-last(return-tokens, token);
    end case;
  end for;

  // Make sure we didn't end on '##'.
  if (concatenate-next?)
    let offending-token = last(tokens);
    parse-error(offending-token,
		"%= cannot be first token in macro expansion",
		offending-token.string-value);
  end if;
  return-tokens;
end function partially-expand-cpp-tokens;

define function expand-cpp-tokens
    (tokens :: <sequence>,
     tokenizer :: <tokenizer>,
     #key parameters: parameter-table :: <self-organizing-list> = empty-table,
     forbidden-expansions :: <list>,
     current-depth :: <integer>)
 => ()
  // Depends upon the fact that tokens are stored in reverse order in the
  // stored macro expansion.
  // XXX - Figure out if we want to change the order of token storage.
  let clean-tokens = partially-expand-cpp-tokens(tokens, tokenizer,
						 parameters: parameter-table);
  for (token in clean-tokens)
    unless (check-cpp-expansion(token.string-value, tokenizer,
				parameters: parameter-table,
				current-depth: current-depth + 1,
				forbidden-expansions: forbidden-expansions))
      // Successful call will have already pushed the expanded tokens
      let cls = element(reserved-word-table,
			token.string-value, default: #f);
      if (cls)
	let reserved-word-token = make(cls,
				       position: tokenizer.position,
				       string: string-value(token),
				       generator: tokenizer);
	push(tokenizer.unget-stack, reserved-word-token);
      else
	push(tokenizer.unget-stack, copy-token(token, tokenizer));
      end if;
    end unless;
  end for;
end function expand-cpp-tokens;

// Check to see if a macro has parameters. Used when creating <c-define>
// objects.
//
define function parameterized-macro?
    (name :: <byte-string>, tokenizer :: <tokenizer>)
 => (parameterized? :: <boolean>)
  let entry = element(tokenizer.cpp-table, name, default: #f);
  case
    ~entry => #f;
    entry.empty? => #f;
    instance?(entry.first, <list>) => #t;
    otherwise => #f;
  end case;
end function parameterized-macro?;

// Creates a nested tokenizer corresponding to a new file specified by an
// "#include" directive.  The file location is computed from the '<>' or '""'
// string combined with the enclosing file's directory or the "include-path".
//
define method cpp-include (state :: <tokenizer>, pos :: <integer>) => ();
  unless (state.include-path)
    parse-error(state, "not allowed to use #include here");
  end unless;
  let contents :: <string> = state.contents;
  let (found, match-end, angle-start, angle-end, quote-start, quote-end)
    = regexp-position(contents, "^(<[^>]+>)|(\"[^\"]+\")", start: pos);
  state.position := match-end;
  let (name, full-name) =
    if (~found)
      parse-error(state, "Ill formed #include directive.");
    elseif (angle-start & angle-end)
      // We've got a '<>' name, so we need to successively try each of the
      // directories in include-path until we find it.  (Of course, if a
      // full pathname is specified, we just use that.)
      let name = copy-sequence(contents, start: angle-start + 1,
			       end: angle-end - 1);
      values(name, find-in-include-path(name,
					state.include-path,
					system-header?: #t));
    elseif (quote-start & quote-end)
      // We've got a '""' name, so we should look in the same directory as
      // the current ".h" file.  (Of course, if a full pathname is
      // specified, we just use that.)
      let name = copy-sequence(contents, start: quote-start + 1,
			       end: quote-end - 1);
      // XXX - work around "<top-level>" grik in c-parser-engine.input.
      let current-header =
	if (state.file-name = "<top-level>")
	  #f;
	else
	  state.file-name;
	end if;
      values(name, find-in-include-path(name,
					state.include-path,
					system-header?: #f,
					current-header: current-header));
    else
      parse-error(state, "Fatal error handling #include: %= %= %= %= %= %=",
		  found, match-end,
		  angle-start, angle-end,
		  quote-start, quote-end);
    end if;

  let generator =
    if (full-name)
      state.include-tokenizer
	:= make(<tokenizer>, name: full-name, parent: state);
    else
      parse-error(state, "file not found: %s", name);
    end if;

  parse-progress-report(generator, ">>> entered header >>>");
  unget-token(generator, make(<begin-include-token>, position: pos,
			      generator: generator,
			      string: generator.file-name));
end method cpp-include;

// Processes a preprocessor macro definition.  For "simple" macros, this only
// involves building a reversed sequence of tokens from the remainder of the
// line and putting it in cpp-table.  However, if it is a parameterized macro
// than we must also parse the parameter list and place it at the front of the
// token sequence.  The expander identifies parameterized macros by the fact
// that the first element of the token sequence is itself a sequence.
//
define method cpp-define (state :: <tokenizer>, pos :: <integer>) => ();
  let name = try-identifier(state, pos, expand: #f);
  if (~name)
    parse-error(state, "Ill formed #define directive.");
  end if;
  parse-progress-report(name, "Processing define %=", name.value);

  // Simply read the rest of the line and build a reversed list of tokens.
  local method grab-tokens (list :: <list>)
	  let token = get-token(state, cpp-line: #t);
	  select (token by instance?)
	    <eof-token> =>
	      list;
	    otherwise =>
	      grab-tokens(pair(token, list));
	  end select;
	end method grab-tokens;

  if (state.contents[state.position] == '(')
    // Check whether this is a parameterized macro.
    // We can't just ask for the next token, as this is the one place in C
    // where whitespace between tokens is significant.
    get-token(state, cpp-line: #t);  // Eat the open paren
    local method grab-params (state :: <tokenizer>, param-list :: <list>)
	    let name = get-token(state, cpp-line: #t);
	    if (empty?(param-list) & instance?(name, <rparen-token>))
	      // Parameter lists may be empty, in which case we won't get an
	      // identifier here.
	      param-list;
	    elseif (instance?(name, <name-token>))
	      let next-token = get-token(state, cpp-line: #t);
	      select (next-token by instance?)
		<comma-token> =>
		  grab-params(state, pair(name.value, param-list));
		<rparen-token> =>
		  pair(name.value, param-list);
		otherwise =>
		  parse-error(state,"Badly formed parameter list in #define.");
	      end select;
            elseif (instance?(name, <ellipsis-token>))
              unless (instance?(get-token(state, cpp-line: #t), <rparen-token>))
                parse-error(state, "Badly formed parameter list in #define,"
                                   " ellipsis must be last parameter.");
              end unless;
	    else
	      parse-error(state, "Badly formed parameter list in #define.");
	    end if;
	  end method grab-params;
    let params = grab-params(state, #());
    state.cpp-table[name.string-value] := pair(params, grab-tokens(#()));
    if (state.cpp-decls) push-last(state.cpp-decls, name.string-value) end if;
  else
    state.cpp-table[name.string-value] := grab-tokens(#());
    if (state.cpp-decls) push-last(state.cpp-decls, name.string-value) end if;
  end if;
end method cpp-define;

//define constant preprocessor-match
//  = make-regexp-positioner("^#[ \t]*(define|undef|include|ifdef|ifndef|if"
//			     "|else|elif|line|endif|error|pragma)\\b",
//			   byte-characters-only: #t, case-sensitive: #t);
#if (~mindy)
define multistring-checker preprocessor-match
  ("define", "undef", "include", "include_next", "ifdef", "ifndef", "if",
   "else", "elif", "line", "endif", "error", "pragma");
define multistring-positioner do-skip-matcher("#", "/*");
#else
define constant preprocessor-match
  = make-multistring-checker("define", "undef", "include", "include_next",
			     "ifdef", "ifndef", "if", "else", "elif", "line",
			     "endif", "error", "pragma");
define constant do-skip-matcher
  = make-multistring-positioner("#", "/*");
#endif

//define constant do-skip-matcher
//  = make-regexp-positioner("#|/\\*",
//			   byte-characters-only: #t, case-sensitive: #t);

// Checks to see whether we are looking at a preproccessor directive.  If so,
// we handle the directive and return #t.  The state may change drastically,
// so we expect the caller to re-invoke "get-token" afterwards.  If we aren't
// looking at a preprocessor directive, we return #f and the caller can
// continue as normal.
//
define method try-cpp
    (state :: <tokenizer>, start-pos :: <integer>) => (result :: <boolean>);
  let contents = state.contents;

  if (contents[start-pos] ~= '#')
    #f;
  else
    let start-pos
      = for (i from start-pos + 1 below contents.size,
	     until: contents[i] ~== ' ' & contents[i] ~== '\t')
	finally i;
	end for;
      
    let (word) = preprocessor-match(contents, start: start-pos);
    
//    if (found)
    if (word)
      // If an #if killed off a region of code, this routine will quickly skip
      // over it.  Because we may have to deal with nested #ifs, we don't
      // directly look for #else or #endif.  Instead we re-call "try-cpp" and
      // then check to see if it changed the "cpp-stack".  If so, we must be
      // done.  Note that nested #ifs are eliminated by recursive calls to
      // do-skip, even if their conditions would normally evaluate to true.
      //
      local method do-skip(pos, current-stack) => ();
	      let i = do-skip-matcher(contents, start: pos);
	      unless (i)
		parse-error(state, "Unmatched #if in include file.")
	      end;
	      let i = skip-whitespace(contents, i);
	      if (~try-cpp(state, i))
		// We may get false matches -- if so, just move on
		do-skip(i + 1, current-stack);
	      elseif (current-stack == state.cpp-stack)
		do-skip(state.position, current-stack);
	      end if;
	    end method do-skip;

//      let word = copy-sequence(contents, start: word-start, end: word-end);
//      let pos = skip-cpp-whitespace(contents, pos);
      let pos = skip-cpp-whitespace(contents, start-pos + word.size);
      state.position := pos;
      select (word by \=)
	"define" =>
	  if (empty?(state.cpp-stack) | head(state.cpp-stack) == #"accept")
	    cpp-define(state, pos)
	  end if;
	"undef" =>
	  if (empty?(state.cpp-stack) | head(state.cpp-stack) == #"accept")
	    let name = try-identifier(state, pos, expand: #f);
	    if (~name)
	      parse-error(state, "Ill formed #undef directive.");
	    end if;
	    remove-key!(state.cpp-table, name.string-value);
	  end if;
	"ifdef" =>
	  let name = try-identifier(state, pos, expand: #f);
	  if (~name)
	    parse-error(state, "Ill formed #ifdef directive.");
	  end if;
	  if (element(state.cpp-table, name.string-value, default: #f)
		& (empty?(state.cpp-stack)
		     | head(state.cpp-stack) == #"accept"))
	    state.cpp-stack := pair(#"accept", state.cpp-stack);
	  else
	    do-skip(state.position,
		    state.cpp-stack := pair(#"retry", state.cpp-stack));
	  end if;
	"ifndef" =>
	  let name = try-identifier(state, pos, expand: #f);
	  if (~name)
	    parse-error(state, "Ill formed #ifndef directive.");
	  end if;
	  if (~element(state.cpp-table, name.string-value, default: #f)
		& (empty?(state.cpp-stack)
		     | head(state.cpp-stack) == #"accept"))
	    state.cpp-stack := pair(#"accept", state.cpp-stack);
	  else
	    do-skip(state.position,
		    state.cpp-stack := pair(#"skip", state.cpp-stack));
	  end if;
	"if" =>
	  let stack = state.cpp-stack;
	  if ((empty?(stack) | head(stack) == #"accept")
		& cpp-parse(state) ~= 0)
	    state.cpp-stack := pair(#"accept", stack);
	  else
	    do-skip(pos, state.cpp-stack := pair(#"retry", stack));
	  end if;
	"else" =>
	  let stack = state.cpp-stack;
	  if (empty?(stack))
	    parse-error(state, "Mismatched #else.");
	  else
	    let rest = tail(stack);
	    if (head(stack) == #"accept")
	      do-skip(pos, state.cpp-stack := pair(#"done", tail(stack)));
	    elseif (head(stack) == #"retry"
		  & (empty?(rest) | head(rest) == #"accept"))
	      state.cpp-stack := pair(#"accept", rest);
	    else
	      do-skip(pos, stack);
	    end if;
	  end if;
	  // For SUN4 headers, kill to end of line
	  for (i from state.position below contents.size,
	       until: contents[i] == '\n')
	  finally
	    state.position := i;
	  end for;
	"elif" =>
	  let stack = state.cpp-stack;
	  if (empty?(stack))
	    parse-error(state, "Mismatched #elif.");
	  else
	    let rest = tail(stack); 
	    if (head(stack) == #"accept")
	      do-skip(pos, state.cpp-stack := pair(#"done", tail(stack)));
	    elseif (head(stack) == #"retry"
		  & (empty?(rest) | head(rest) == #"accept")
		  & cpp-parse(state) ~= 0)
	      state.cpp-stack := pair(#"accept", rest);
	    else 
	      do-skip(pos, stack);
	    end if;
	  end if;
	  // For SUN4 headers, kill to end of line
	  for (i from state.position below contents.size,
	       until: contents[i] == '\n')
	  finally
	    state.position := i;
	  end for;
	"endif" =>
	  let old-stack = state.cpp-stack;
	  if (empty?(old-stack))
	    parse-error(state, "Unmatched #endif.");
	  end if;
	  state.cpp-stack := tail(old-stack);
	  // For SUN4 headers, kill to end of line
	  for (i from state.position below contents.size,
	       until: contents[i] == '\n')
	  finally
	    state.position := i;
	  end for;
	"error" =>
	  if (empty?(state.cpp-stack) | head(state.cpp-stack) == #"accept")
	    parse-error(state, "Encountered #error directive.");
	  end if;
	"line", "pragma" =>
	  // Kill to end of line
	  for (i from pos below contents.size, until: contents[i] == '\n')
	  finally
	    state.position := i;
	  end for;
	"include" =>
	  if (empty?(state.cpp-stack) | head(state.cpp-stack) == #"accept")
	    cpp-include(state, pos);
	  end if;
	"include_next" =>
	  //signal("Warning: doing the wrong thing with #include_next.");
	  if (empty?(state.cpp-stack) | head(state.cpp-stack) == #"accept")
	    cpp-include(state, pos);
	  end if;
	otherwise =>
	  parse-error(state, "Unhandled preprocessor directive.");
      end select;
      #t;
    else
      // Certain compilers might accept additional directives.  As long as
      // they are within failed #ifdefs, we can ignore them.
      if (empty?(state.cpp-stack) | head(state.cpp-stack) == #"accept")
	parse-error(state, "Unknown preprocessor directive starting with %=",
		    // Take a wild guess at how much context is enough
		    copy-sequence(contents, start: start-pos, 
				  end: start-pos + 30));
      end if;
      #f;
    end if;
  end if;
end method try-cpp;
