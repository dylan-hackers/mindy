Module: dylan-user

define library c-parser
  use dylan;
  use ansi-c;
  use parser-utilities;
  // From melange:
  use streams;
  use string-extensions;
  use collection-extensions;
  use regular-expressions;
  use table-extensions;
  use standard-io;
  use format;
  
  export c-parser;
end library;

define module c-lexer
  use dylan;
  use extensions;
  use table-extensions, exclude: {<string-table>};
  use self-organizing-list;
  use string-conversions;
  use regular-expressions;
  use substring-search;
  use character-type;
  use streams;
  use source-locations;
  use parse-conditions,
    // XXX - These should probably go away.
    export: {parse-error,
	     parse-warning,
	     parse-progress-report};
  use multistring-match;
//  create cpp-parse;
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
    value, unget-token, add-typedef, get-token, include-path,
    check-cpp-expansion, open-in-include-path
end module c-lexer;

define module c-parser
  use dylan;
  use extensions;

  use source-locations;

  use ansi-c;
  use c-lexer;

  export
    parse-header;
end module;


