Module: dylan-user

define library c-parser
  // Standard Dylan
  use dylan;
  use streams;
  use standard-io;
  use format;

  // Gwydion Libraries
  use string-extensions;
  use collection-extensions;
  use regular-expressions;
  use table-extensions;

  // Included with Pidgin
  use ansi-c;
  use parser-utilities;
  
  export c-parser;
end library;

define module c-lexer
  // Standard Dylan
  use dylan;
  use extensions;
  use streams;

  // Gwydion Libraries
  use table-extensions, exclude: {<string-table>};
  use self-organizing-list;
  use string-conversions;
  use regular-expressions;
  use substring-search;
  use character-type;

  // Included with Pidgin
  use source-locations;
  use parse-conditions;
  use multistring-match;

  create cpp-parse;
  export
    *handle-c++-comments*,
    <tokenizer>, cpp-table, cpp-decls, <token>, token-id, generator,
    <simple-token>, <reserved-word-token>, <punctuation-token>,
    <literal-token>, <ei-token>, <name-token>, <type-specifier-token>,
    <identifier-token>, <integer-token>, <character-token>, <struct-token>,
    <short-token>, <long-token>, <int-token>, <char-token>, <signed-token>,
    <unsigned-token>, <float-token>, <double-token>, <void-token>,
    <union-token>, <enum-token>, <minus-token>, <tilde-token>, <bang-token>,
    <alien-name-token>, <macro-parse-token>, <cpp-parse-token>, string-value,
    value, unget-token, add-typedef, get-token,
    check-cpp-expansion, open-in-include-path
end module c-lexer;

define module c-parser-engine
  // Standard Dylan
  use dylan;
  use extensions;
  use streams;
  use format;
  use standard-io;

  // Gwydion Libraries
  use self-organizing-list;

  // Included with Pidgin
  use ansi-c;
  use parse-conditions;
  use c-lexer;

  // Defined by c-parser.
  create
    <parse-state>, <parse-file-state>, <parse-type-state>, <parse-cpp-state>,
    <parse-macro-state>, repository, tokenizer, verbose, verbose-setter,
    push-include-level, pop-include-level, objects, process-type-list,
    process-declarator, declare-objects, processing-typedef?,
    processing-typedef?-setter;

  // Defined by c-decl.dylan, which we don't have.
  create
    make-tagged-type, c-type-size,
    add-cpp-declaration,
    <integer-type-declaration>, true-type, make-enum-slot,
    make-struct-or-union-slot;

  export
    parse, parse-type, parse-macro;
end module c-parser-engine;

define module c-parser
  // Standard Dylan
  use dylan;
  use extensions;

  // Included with Pidgin
  use source-locations;
  use parse-conditions;
  use ansi-c;
  use c-lexer;
  use c-parser-engine;

  export
    parse-c-file,
    <c-platform>,
    $i386-linux-platform;
end module c-parser;


