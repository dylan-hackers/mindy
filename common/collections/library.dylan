Module:       dylan-user
Synopsis:     Collection extensions library
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1998-2001 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library collections
  use Dylan;
  use Common-Dylan;
  use table-extensions,
    import: { table-extensions },
    export: all;
  export
    bit-vector,
    bit-set,
    collectors,
    plists,
    set;
end;

define module bit-vector
  use Extensions,
    import: { <bit> },
    export: all;
  create <bit-vector>,
         word-size, bit-vector-word,
         bit-vector-and, bit-vector-and!,
         bit-vector-andc2, bit-vector-andc2!,
         bit-vector-or, bit-vector-or!,
         bit-vector-xor, bit-vector-xor!,
         bit-vector-not, bit-vector-not!,
         bit-count;
end module bit-vector;

define module bit-set
  create <bit-set>,
         set-add, set-add!,
         set-remove, set-remove!,
         set-union, set-union!,
         set-intersection, set-intersection!,
         set-difference, set-difference!,
         set-complement, set-complement!,
         infinite?,
         copy-bit-set!,
         empty-bit-set!,
         universal-bit-set!;
end module bit-set;

define module collectors
  create collector-protocol,
         \collecting, 
         \collect-into, \collect-first-into, \collect-last-into,
         \collect, \collect-first, \collect-last,
         \collected;
end module collectors;

define module plists
  create get-property,
	 \put-property!, do-put-property!,
         keyword-sequence, value-sequence,
	 \remove-property!, do-remove-property!,
	 remove-keywords, \with-keywords-removed;
end module plists;

define module set
  use Extensions,
    import: { <set>, <object-set> },
    export: all;
end module set;

/*
define module table-extensions
  use dylan-extensions,
    import: { <string-table>, <hash-state>,
              collection-hash, sequence-hash, 
              collection-hash!, sequence-hash!, 
              values-hash,
              string-hash,
              case-insensitive-string-hash,
              case-insensitive-equal,
	      remove-all-keys!,
	      merge-hash-ids },
    export: all;
end module table-extensions;
*/

define module collections-internals
  use Common-Dylan;
  use Extensions;
  use Functional-Extensions;
  use machine-words;
  use bit-set;
  use bit-vector;
  use collectors;
  use plists;
  use set;
  use table-extensions;
end module collections-internals;
