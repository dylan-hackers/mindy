module: ini-files
author: Nick Kramer
rcs-header: $Header: /scm/cvs/src/d2c/compiler/base/Attic/ini-files.dylan,v 1.1 1998/05/03 19:55:31 andreas Exp $
copyright: Copyright (c) 1995, 1996  Carnegie Mellon University
	   All rights reserved.

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
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
//    University.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports, questions, comments, and suggestions should be sent by
// E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
//
//======================================================================

// This file defines stuff for reading in compiler target information
// from disk and presenting it to the rest of the program in a
// reasonable way.

define sealed class <parse-error> (<error>)
  constant slot parser-format-string :: <string>, 
    required-init-keyword: #"format-string";
  constant slot parser-format-arguments :: <sequence>,
    required-init-keyword: #"format-arguments";
  constant slot parser-error-line-number :: <integer>,
    required-init-keyword: #"line-number";
end class <parse-error>;

define sealed domain make(singleton(<parse-error>));
define sealed domain initialize(<parse-error>);

// Does not return
//
define method default-handler (condition :: <parse-error>, #next next-method)
 => ();
  format(*standard-error*, "Line %d of targets.ini:\n\t", 
	 condition.parser-error-line-number);
  format(*standard-error*,
	 condition.parser-format-string, condition.parser-format-arguments);
  format(*standard-error*, "\n");
  force-output(*standard-error*);
  next-method();
end method default-handler;

// Doesn't return
//
define method parse-error
    (line-number :: <integer>, format-string :: <string>, #rest format-args)
 => ();
  let err = make(<parse-error>, line-number: line-number,
		 format-string: format-string,
		 format-arguments: format-args);
  signal(err);
end method parse-error;

define constant strip-leading-whitespace 
  = make-regexp-replacer("^\\s+", replace-with: "");
define constant match-section-header
  = make-regexp-positioner("^\\[([-a-zA-Z0-9]+)\\]");
define constant match-key-value-pair
  = make-regexp-positioner("^([-a-zA-Z0-9]+)" // key
			   "\\s*=\\s*"        // = 
			   "\"(.*)\""         // value
			   "\\s*(;.*)?$");    // trailing whitespace and 
                                              // optional comment

// These are modified .ini files, not quite the Microsoft format...
//
// The return value is a table whose keys are symbols and whose
// elements are themselves tables (with symbols for keys and
// strings for elements).
//
// A section is created when someone adds an attribute to it, not when
// someone references it with [section-name].
//
// Perhaps this should be integrated with the ODF dumper, as it
// basically is just a human-readable version of the ODF..
//
// No longer detects redefined attributes.
//
define method parse-ini-file
    (stream :: <stream>, setter-vector :: <vector>, make-section :: <function>)
 => results :: <object-table>;
  let current-section :: false-or(<symbol>) = #f;
  let results = make(<object-table>);

  let setter-table = make(<object-table>);  // maps keys into setter functions
  for (i from 0 below setter-vector.size by 2)
    setter-table[setter-vector[i]] := setter-vector[i + 1];
  end for;

  // Add key/value pair to the current section
  //
  local method add-key-value-pair
	    (key :: <symbol>, value :: <string>, line-number :: <integer>)
	 => ();
	  if (current-section == #f)
	    parse-error(line-number, "Attribute is not in a section.");
	  else
	    let section 
	      = if (key-exists?(results, current-section))
		  results[current-section];
		else
		  results[current-section] := make-section(current-section);
		  // and return that value
		end if;
	    if (~key-exists?(setter-table, key))
	      parse-error(line-number, "Unknown attribute");
	    elseif (setter-table[key] == #f)
	      parse-error(line-number, "Can't redefine attribute");
	    else
	      setter-table[key](value, section);
	      // setter-table[key] := #f;  // Used to detect redefined attrs
	    end if;
	  end if;
	end method add-key-value-pair;

  local method parse-line (line :: <string>, line-number :: <integer>) => ();
	  let line = strip-leading-whitespace(line);
	  if (line.empty? | line.first == ';')
	    #f;  // ignore line
	  elseif (line.first == '[')
	    let (start, finish, group1-start, group1-end) 
	      = match-section-header(line);
	    if (~start)
	      parse-error(line-number, "Parse error--bad section header");
	    else
	      current-section := as(<symbol>, 
				    copy-sequence(line, start: group1-start, 
						  end: group1-end));
	    end if;
	  else
	    let (start, regexp-finish, group1-start, group1-finish,
		 group2-start, group2-finish)
	      = match-key-value-pair(line);
	    // That regexp means strings can't contain quotes, even if escaped.
	    if (~start)
	      parse-error(line-number, 
			  "Parse error--expected an attribute but couldn't "
			  "parse one");
	    else
	      let key = as(<symbol>, 
			   copy-sequence(line, start: 0, end: group1-finish));
	      let value = copy-sequence(line, start: group2-start,
					end: group2-finish);
	      let value
		= substring-replace(substring-replace(value, "\\n", "\n"),
				    "\\t", "\t");
	      add-key-value-pair(key, value, line-number);
	    end if;
	  end if;
	end method parse-line;
  
  block ()
    for (line-number from 1)
      let line = read-line(stream);
      parse-line(line, line-number);
    end for;
  exception (<end-of-stream-error>)
    #f;      // Catch it and do nothing
  end block;
  close(stream);
  results;
end method parse-ini-file;
