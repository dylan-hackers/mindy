Module: od-format
RCS-header: $Header: /scm/cvs/src/d2c/compiler/base/od-format.dylan,v 1.1 1998/05/03 19:55:30 andreas Exp $

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

/*

The basic requirement of the "Object Description Format" (ODF) is to allow an
open-ended variety of objects to be dumped into a file, where:
 -- instances may refer to each other in an arbitrary graph structure, and
 -- some of the references may be to objects that are defined in distinct
    object description units (files, etc.)

In order to make the design interesting, the ODF attempts to satisfy various
incompatible requirements:
 -- To support efficient parsing and unparsing on a variety of architectures,
    but also to potentially serve as a cross-platform interchange format.
 -- To work well on sequential media (like a socket), but also to potentially
    support random-access demand-loading of objects when possible (e.g. on
    files.)


Word/Byte format:

To allow efficiency as well as potential interchange, objects are described by
a word sequence which vaguely resembles the in-core representation.  The word
size is implementation defined, and dumper/loader methods dump/load data as
words having the native byte ordering.  The format is defined so that any
necessary byte-swapping and word-size adjustment can be done as a pre-pass
filter to the actual loader.


Notation:
    A word is represented as:
        <ms-use: ms-size; next-use: next-size...>

    Where the first field holds the most-significant bits and the last field
    holds the least significant.  One of the fields must have size "*",
    indicating that to gets all of the remaining bits.  If a field has constant
    value, then we instead say "use = #bDDD" or "use = #xDDD".  The width is in
    this case implied by the literal, with leading zeroes being significant.
    If #x is used, the field must be a multiple of four bits.  For example,
    "format = #x1" says that there is a four bit field called "format".

    In a word-sequence, words are tagged with the word offset from the start of
    the sequence:
        0: <first word>
	1: <second word> 

    For notational convenience, the first word in the pattern is usually taken
    to be 0 even though this pattern might be a subsequence of an enclosing
    object.
     
    A sequence of words in the same format is notated by following the start
    tag with the repeat count in "[]"'s:
        10[5]: <word format> repeated 5 times.


Object description format:

The general format of an object description is a potentially nested stream of
word-chunks in this format:
    0: <Header? = #b1, Etype: 2, header data: *>
    1[N]: <data: *>

More specifically, there are four header formats, flagged by the Etype field:

<Header? = #b1, Etype = #b00, Subobjects?: 1, raw-format: 4, Class-ID: *>
    This represents an object-definition.  The MS byte holds format
    information about the data content, and the object class ID tells us what
    kind of object the data is supposed to represent.

<Header? = #b1, Etype = #b01, Start-Offset: *>
    This is an end-entry, and marks the end of the subobjects portion of an
    object-definition having subobjects.  The start offset is interpreted as a
    positive number of words to go back from the end header to reach the header
    for the object being ended.

<Header? = #b1, Etype = #b10, Local-ID: *>
<Header? = #b1, Etype = #b11, External-ID: *>
    These represent a local-reference and an external-reference.  A local
    reference refers to an object defined within this top-level object, whereas
    an external ID uses a locally-defined index to refer to an object defined
    in some other top-level object.  See the local-index and extern-index
    pseudo-objects (below.)  Local references may be to any type of object.
    The objects corresponding to external references must be some subclass of
    <external-reference-mixin> in order to use the framework.

*/
define /* exported */ constant $odf-header-flag              = #b10000000;
define /* exported */ constant $odf-etype-mask               = #b01100000;
define /* exported */ constant $odf-object-definition-etype  = #b10000000;
define /* exported */ constant $odf-end-entry-etype          = #b10100000;
define /* exported */ constant $odf-local-reference-etype    = #b11000000;
define /* exported */ constant $odf-external-reference-etype = #b11100000;

// The following constants are used to quickly check for etype
// headers.  ash(header-byte, $odf-etype-shift) should equal one of
// the "-shifted" constants
//
define constant $odf-etype-shift  = -5;
define /* exported */ constant $odf-object-definition-shifted  = #b100;
define /* exported */ constant $odf-end-entry-shifted          = #b101;
define /* exported */ constant $odf-local-reference-shifted    = #b110;
define /* exported */ constant $odf-external-reference-shifted = #b111;
/*


More about object-definition entries:

Except for the object-definition entry, all entries are one word in length. The
object-definition is the heart of ODF, since it is the only one that actually
contains data.  Object definitions can contain both raw data and subobjects:
 -- If present, the raw data comes first, preceded by a byte count.  This byte
    count must be rounded up to the next word boundary to find the start of the
    subobjects (or the end of the object if no subobjects.)  The presence of
    raw data is indicated by a non-zero value for the "raw data format" field.
 -- If present, the subobjects come after any raw data, and are terminated by
    an end-entry.  Subobjects may be either recursive object definitions or
    references to objects defined elsewhere.  Each nested subobject must have a
    valid header, but could also contain arbitrary raw data at its start.  The
    presence of subobjects (and of an end-entry) is indicated by the
    subobjects? bit being 1.
*/
define /* exported */ constant $odf-subobjects-flag	     = #b00010000;
/*

Raw data format:

The Raw-Format field in an object-definition header encodes information needed
to byte/word format translation, and in the degenerate case, indicates there is
no raw data at all.  These format codes are defined:
*/
define /* exported */ constant $odf-raw-format-mask = #b00001111;
define /* exported */ constant $odf-no-raw-data-format = 0;
define /* exported */ constant $odf-byte-raw-data-format = 1;
define /* exported */ constant $odf-16bit-raw-data-format = 2;
define /* exported */ constant $odf-32bit-raw-data-format = 3;
define /* exported */ constant $odf-64bit-raw-data-format = 4;
define /* exported */ constant $odf-untranslatable-raw-data-format = 5;

// 32 or 64 as appropriate, used for "words" in format pseudo-objects.
define /* exported */ constant $odf-word-raw-data-format = 6; 
/*

Examples:

These examples show the bits for some literals, assuming with 32bit word size
and big-endian order, and assigning convenient values for the object class-IDs.
See the actual code for the real definitions and class IDs.

For example, we could describe a byte-string like this:
    0: <Header? = #b1, Etype = #b00, Subobjects? = #b0, Raw-Format = #x1,
	Class-ID = byte-string-odf-id>
    1: string byte-count
    2[ceiling(byte-count, word-bytes)]: 
       <string-chars: *>

Assuming byte-string-$odf-id is #x666, the actual bits for "Foo" would be:
    #x81000666
    #x00000003
    #x466F6F00


A list has subobjects, but no raw data, so it would look like:
    0: <Header? = #b1, Etype = #b00, Subobjects? = #b1, Raw-Format = #x0,
        Raw-Size = #x00, Class-ID = list-odf-id>

    1[N]: <header of subobject 0, subobject data>
       ...

    M: <Header? = #b1, Etype = #b01, Start-Offset: * = M>

If list-odf-id were #x667, then #("Foo", "Bar") would be:
    0: #x90000667
    1: #x81000666
    2: #x00000003
    4: #x466F6F00
    5: #x81000666
    6: #x00000003
    9: #x42617200
    7: #xA0000007

This demonstrates the nesting of objects, and shows how the end is marked.
Note that:
 -- "Foo" is represented by exactly the same bits here (it is
    position-independent), and that
 -- The entire sequence is position-independent, since the end-entry has a
    relative offset, and that
 -- The basic subobject protocol doesn't say in advance how many
    subobjects there's going to be; you have to wait for the end header.  This
    could be avoided for e.g. vectors by considering subobject 0 to be the
    length.

*/

// Object ID registry:
//
// To avoid creating zillions of variables and to allow introspective
// back-mapping, we maintain a registry of object IDs named by symbols.  This
// does incurr some lookup overhead at dump time, but not at load time, which
// is probably more important.
//
// Note that it is intended that all IDs be registered in this file so that
// we are sure that IDs are globally unique.

define constant $object-id-registry :: <object-table>
  = make(<object-table>);
define constant $object-id-registry-inverse :: <stretchy-vector>
  = make(<stretchy-vector>, size: 0);

define method register-object-id (name :: <symbol>, id :: <integer>)
 => ();
  assert(~element($object-id-registry, name, default: #f));
  assert(id >= 0);
  assert(id > $object-id-registry-inverse.size
	   | $object-id-registry-inverse[id] == #f);
  $object-id-registry[name] := id;
  $object-id-registry-inverse[id] := name;
end method;


////  Pseudo-object format definitions:
//
//    The nested-object structure is used to build the OD format itelf.  There
// are various pseudo-objects which represent the indexing, etc.


// Data unit object:
//     0: <Header? = #b1, Etype = #b00, Subobjects? = #b1, Raw-Format = #x6,
// 	Class-ID = appropriate ID>
//     1: $word-bytes * 6
//     2: OD format major version code 
//     3: OD format minor version code
//     4: length of unit in words, including header.
//     5: creator platform characteristics code
//     6: unit type code
//     7: unit identity check hash
//     8[n]: local-index object defn
//           local-object-map defn
//           external-index defn
//           content object definitions
//     end: end header
// 
// The data unit object serves as the "file header" and establishes the
// parameters of the data format.  It also contains a length so that the entire
// unit can be skipped if desired.  Different word sizes will have different
// data-unit IDs.  The byte order can be guessed by looking at the header and
// confirmed by the platform-characteristics code.  We choose an
// out-of-sequence ID so that every byte is different, e.g. #x96000DF4.
// 

define constant $32bit-data-unit-odf-id = #x0DF4;
register-object-id(#"32bit-data-unit", $32bit-data-unit-odf-id);

// Major version must be the same between dumper and loader, thus it is
// incremented on a format change that breaks existing data units.
define /* exported */ constant $od-format-major-version = 1;

// Minor version in dumper must be <= to version in loader, thus it is
// incremented on changes which don't obsolete existing data.
define /* exported */ constant $od-format-minor-version = 0;

define /* exported */ constant $like-an-hp-platform-characteristics = #x34;

define /* exported */ constant $library-summary-unit-type = 0;

// Maps data unit types to short strings (used to generate filenames.)
define constant $unit-type-strings = #["lib"];

define constant $data-unit-header-size = 8;

// Things that can be the name of a data unit.  Compare with ==.
// 
define constant <data-unit-name> = <symbol>;

// A hint of where to find a data unit.  Currently an absolute(?) pathname.
//
define constant <location-hint> = <byte-string>;


// Local-index object:
//     0: <Header? = #b1, Etype = #b00, Subobjects? = #b0, Raw-Format = #x6>
//     1: index-size * word-byte;
//     2[index-size]: word offset of object defn from data unit header
//
// The local-index object maps object local IDs to the object start offset.
// The local ID is local in the sense that it is allocated relative to a
// particular data unit.  It is actually used by external references
// references (across data units) as well as local references.

register-object-id(#"local-index", #x0001);

// + num local IDs
define constant $local-index-size = 2;


// Local-object-map object:
//     0: <Header? = #b1, Etype = #b00, Subobjects? = #b0, Raw-Format = #x6,
//         Class-ID = local-object-map-odf-id>
//     1: index-size * word-bytes
//     2[index-size]: 
//         local IDs of objects that have them, in order of their definitions.
// 
// In conjunction with the local-index, the local-object-map serves to tell the
// loader what the local ID of an object is (if any.)  It is actually redundant
// with the local-index, but duplicating means that the loader doesn't have to
// sort the local IDs by offset.  The idea is that the loader can determine the
// ID of the next labeled object by grabbing the next entry from
// this table and the offset by looking the ID looking it up in the
// local-index.  The loader needs to efficiently be able to determine both,
// since there is no in-line indication that an object has an ID.  This
// situation supports dumpers where the need for a reference may not be known
// until after the reference is dumped.
//

register-object-id(#"local-object-map", #x0002);

// + num local IDs
define constant $local-object-map-size = 2;


// Extern-index object:
// 
// The extern-index object maps extern ref IDs to extern-handle objects.  All of
// the extern-handle objects describing the reference targets are subobjects of
// the extern-index object.  It has no raw data.

register-object-id(#"extern-index", #x0003);

// Extern-Handle object:
//     0: <Header? = #b1, Etype = #b00, Subobjects? = #b1, Raw-Format = #x6,
// 	Class-ID = extern-handle-odf-id>
//     1: 3 * word-bytes
//     2: defining unit check hash
//     3: defining unit type
//     4: local ID of referenced object in defining unit
//     5[n]: two subobjects:
//          -- data-unit (e.g. library) name
// 	    -- location hint (e.g. absolute pathname)
//     end: end header
// 
// The extern-handle represents a link to an object in another data unit.  It
// contains information used to locate and verify the link.  Note that dumping
// an external reference implies that we want to dump an object which has
// already been loaded from some other data unit.  That is, this provides the
// mechanism by which one data unit builds on another.

register-object-id(#"extern-handle", #x0004);


// Dylan object IDs:

// We wrap these in a begin with an initial local declaration so that the
// compiler doesn't treat each one as a seperate top level form.
//
begin
  let ignore = 5;

  // A byte character, string or symbol.  Data is raw in byte format.
  register-object-id(#"byte-character", #x0010);
  register-object-id(#"byte-string", #x0011);
  register-object-id(#"byte-symbol", #x0012);

  // A unicode character, string or symbol.  Each character is two bytes, byte
  // format.(???)
  register-object-id(#"unicode-character", #x0013);
  register-object-id(#"unicode-string", #x0014);
  register-object-id(#"unicode-symbol", #x0015);

  // A list or simple-object-vector of the subobjects.
  register-object-id(#"list", #x0016);
  register-object-id(#"simple-object-vector", #x0017);

  // Improper list, with tail last subobject.  Must have at least two
  // subobjects.
  //
  register-object-id(#"list*", #x0018);

  // Magic values, no data:
  register-object-id(#"true", #x0019);
  register-object-id(#"false", #x001A);

  // Numbers, data is raw, 32 or 64bit.
  register-object-id(#"fixed-integer", #x0030);
  register-object-id(#"extended-integer", #x0031);
  register-object-id(#"single-float", #x0032);
  register-object-id(#"double-float", #x0033);
  register-object-id(#"extended-float", #x0034);

  // Numbers with components as subobjects:
  register-object-id(#"ratio", #x0035);

end;


// Compiler object IDs:

// We wrap these in a begin with an initial local declaration so that the
// compiler doesn't treat each one as a seperate top level form.
//
begin
  let ignore = 5;

  // Literals:

  // A literal where the literal-value slot holds some other object that we
  // know how to dump and that can be unambiguously coerced (via AS) to a
  // ct-value.  The single subobject is the dylan value.
  //
  register-object-id(#"simple-literal", #x0050);

  // Two subobject: head and tail.
  register-object-id(#"literal-pair", #x0051);

  // Subobject is an extended-integer or ratio.
  register-object-id(#"literal-fixed-integer", #x0052);
  register-object-id(#"literal-single-float", #x0053);
  register-object-id(#"literal-double-float", #x0054);
  register-object-id(#"literal-extended-float", #x0055);

  // One subobject, the vector dylan value.
  register-object-id(#"literal-vector", #x0056);

  // Similar in concept to literals, but not quite one because there is no
  // literal syntax for it.
  register-object-id(#"not-supplied-marker", #x0057);

  // Types:
  //
  // All simple objects w/ subobjects, except for byte-charater, which
  // has no subobjects.
  //
  register-object-id(#"union-type", #x0060);
  register-object-id(#"unknown-type", #x0061);
  register-object-id(#"limited-integer-type", #x0062);
  register-object-id(#"direct-instance-type", #x0063);
  register-object-id(#"singleton-type", #x0064);
  register-object-id(#"byte-character-type", #x0065);
  register-object-id(#"multi-value-type", #x0066);
  register-object-id(#"instance-slot-info", #x0067);
  register-object-id(#"vector-slot-info", #x0068);
  register-object-id(#"class-slot-info", #x0069);
  register-object-id(#"each-subclass-slot-info", #x006A);
  // #x006B unused (was constant-slot-info).
  register-object-id(#"virtual-slot-info", #x006C);
  register-object-id(#"override-info", #x006D);
  register-object-id(#"layout-table", #x006E);
  register-object-id(#"defined-class", #x006F);
  register-object-id(#"limited-class", #x0070);
  register-object-id(#"class-proxy", #x0071);
  register-object-id(#"subclass-type", #x0072);


  // Compile-time functions:
  register-object-id(#"ct-function", #x0080);
  register-object-id(#"ct-sealed-generic", #x0081);
  register-object-id(#"ct-open-generic", #x0082);
  register-object-id(#"ct-method", #x0083);
  register-object-id(#"ct-accessor-method", #x0084);
  register-object-id(#"ct-entry-point", #x0085);
  register-object-id(#"function-signature", #x0086);
  register-object-id(#"function-key-info", #x0087);

  // Names, modules, & the like
  register-object-id(#"basic-name", #x0088);
  register-object-id(#"anonymous-name", #x0089);
  register-object-id(#"method-name", #x008A);
  register-object-id(#"internal-name", #x008B); 
  register-object-id(#"module-variable", #x008C);
  register-object-id(#"module", #x008D);
  register-object-id(#"library", #x008E);
  register-object-id(#"derived-name", #x008F);

  // FER:
  register-object-id(#"compiler-policy", #x0090);
  register-object-id(#"unknown-source-location", #x0091);
  register-object-id(#"source-file", #x0092);
  register-object-id(#"file-source-location", #x0093);

  register-object-id(#"linear-region", #x0094);
  register-object-id(#"if-region", #x0095);
  register-object-id(#"block-region", #x0096);
  register-object-id(#"loop-region", #x0097);
  register-object-id(#"exit-region", #x0098);
  register-object-id(#"return-region", #x0099);

  register-object-id(#"ssa-variable", #x009A);
  register-object-id(#"initial-variable", #x009B);
  register-object-id(#"values-cluster-info", #x009C);
  register-object-id(#"local-var-info", #x009D);
  register-object-id(#"lexical-var-info", #x009E);

  register-object-id(#"let-assignment", #x00A0);
  register-object-id(#"set-assignment", #x00A1);
  register-object-id(#"join-operation", #x00A2);
  register-object-id(#"primitive-operation", #x00A3);
  register-object-id(#"known-call-operation", #x00A4);
  register-object-id(#"unknown-call-operation", #x00A5);
  register-object-id(#"mv-call-operation", #x00A6);
  register-object-id(#"error-call-operation", #x00A7);
  register-object-id(#"module-var-ref-operation", #x00A8);
  register-object-id(#"module-var-set-operation", #x00A9);
  register-object-id(#"self-tail-call-operation", #x00AA);
  register-object-id(#"heap-slot-ref-operation", #x00AB);
  register-object-id(#"heap-slot-set-operation", #x00AC);
  register-object-id(#"literal-constant", #x00AD);
  register-object-id(#"definition-constant-leaf", #x00AE);
  register-object-id(#"uninitialized-value-leaf", #x00AF);
  register-object-id(#"method-literal", #x00B0);
  register-object-id(#"exit-function-literal", #x00B1);
  register-object-id(#"fer-function-region", #x00B2);
  register-object-id(#"fer-component", #x00B3);
  register-object-id(#"prologue-operation", #x00B4);
  register-object-id(#"primitive-info", #x00B5);
  register-object-id(#"truly-the-operation", #x00B6);
  register-object-id(#"instance?-operation", #x00B7);
  register-object-id(#"catch-operation", #x00B8);
  register-object-id(#"throw-operation", #x00B9);
  register-object-id(#"disable-catcher-operation", #x00BA);
  register-object-id(#"unwind-protect-region", #x00BB);
  register-object-id(#"function-literal", #x00BC);
  register-object-id(#"nlx-info", #x00BD);
  register-object-id(#"make-catcher-operation", #x00BE);
  register-object-id(#"data-word-ref-operation", #x00BF);

  register-object-id(#"generic-definition", #x00C0);
  register-object-id(#"implicit-generic-definition", #x00C1);
  register-object-id(#"method-definition", #x00C2);
  register-object-id(#"getter-method-definition", #x00C3);
  register-object-id(#"setter-method-definition", #x00C4);
  register-object-id(#"constant-definition", #x00C5);
  register-object-id(#"constant-method-definition", #x00C6);
  register-object-id(#"variable-definition", #x00C7);
  register-object-id(#"class-definition", #x00C8);
  register-object-id(#"init-function-definition", #x00C9);
  register-object-id(#"maker-function-definition", #x00CA);
  register-object-id(#"define-bindings-macro-definition", #x00CB);
  register-object-id(#"define-macro-definition", #x00CC);
  register-object-id(#"function-macro-definition", #x00CD);
  register-object-id(#"statement-macro-definition", #x00CE);
  register-object-id(#"seal-info", #x00CF);
  register-object-id(#"sealed-domain", #x00D0);

  register-object-id(#"backend-var-info", #x00E0);
  // register-object-id(#"function-info", #x00E1);  ### Needed?
  register-object-id(#"constant-info", #x00E2);
  register-object-id(#"constant-function-info", #x00E3);
  register-object-id(#"constant-method-info", #x00E4);

  register-object-id(#"general-representation", #x00E8);
  register-object-id(#"heap-representation", #x00E9);
  register-object-id(#"immediate-representation", #x00EA);
  register-object-id(#"data-word-representation", #x00EB);

  register-object-id(#"define-library-tlf", #x00F0);
  register-object-id(#"define-module-tlf", #x00F1);
  register-object-id(#"define-binding-tlf", #x00F2);
  register-object-id(#"unit-info", #x00F3);
  register-object-id(#"extra-label", #x00F4);

  register-object-id(#"use", #x00F8);
  register-object-id(#"all-marker", #x00F9);
  register-object-id(#"renaming", #x00FA);


  // Tokens
  //
  register-object-id(#"token", #x100);
  register-object-id(#"identifier-token", #x101);
  register-object-id(#"uniquifier", #x102);
  register-object-id(#"operator-token", #x103);
  register-object-id(#"constrained-name-token", #x104);
  register-object-id(#"literal-token", #x105);
  register-object-id(#"pre-parsed-token", #x106);

  // Macros
  //
  register-object-id(#"macro-source", #x110);
  register-object-id(#"macro-section-marker", #x111);
  register-object-id(#"simple-macro-source-location", #x112);
  register-object-id(#"compound-macro-source-location", #x113);

  register-object-id(#"main-rule-set", #x118);
  register-object-id(#"aux-rule-set", #x119);

  register-object-id(#"body-style-define-rule", #x120);
  register-object-id(#"list-style-define-rule", #x121);
  register-object-id(#"statement-rule", #x122);
  register-object-id(#"function-rule", #x123);
  register-object-id(#"auxiliary-rule", #x124);

  register-object-id(#"empty-pattern", #x130);
  register-object-id(#"semicolon-pattern", #x131);
  register-object-id(#"comma-pattern", #x132);
  register-object-id(#"sequential-pattern", #x133);
  register-object-id(#"bracketed-pattern", #x134);
  register-object-id(#"variable-pattern", #x135);
  register-object-id(#"bindings-pattern", #x136);
  register-object-id(#"name-pattern", #x137);
  register-object-id(#"arrow-pattern", #x138);
  register-object-id(#"pattern-variable", #x139);
  register-object-id(#"property-list-pattern", #x13A);
  register-object-id(#"pattern-keyword", #x13B);

  register-object-id(#"procedural-template", #x140);
  register-object-id(#"literal-template", #x141);
  register-object-id(#"bracketed-element", #x142);
  register-object-id(#"simple-pattern-variable-reference", #x143);
  register-object-id(#"ellipsis-pattern-variable-reference", #x144);
  register-object-id(#"concatenating-pattern-variable-reference", #x145);
  register-object-id(#"sequence-pattern-variable-reference", #x146);
  register-object-id(#"unhygienic-pattern-variable-reference", #x147);
end;


// Buffer interface:

// ### not portable, should be generalized.  Should be provided somewhere else.
//

define /* exported */ constant $word-bytes = 4;
define /* exported */ constant $word-bits = 32;

#if (mindy)
define constant <word> = <general-integer>;
#else
define constant <word> = <integer>;
#endif

// Read a word from a buffer at a word-aligned byte offset.
// 
define method buffer-word(bbuf :: <buffer>, i :: <buffer-index>)
 => word :: <word>;
  
  let high-end = bbuf[i];

  // ### big-endian 32 assumption.  Should be a primitive.
  bbuf[i + 3] + ash(bbuf[i + 2], 8) + ash(bbuf[i + 1], 16)
    + 
#if (mindy)
    // for mindy, return extended if too big to be fixed...
      if (high-end.zero?)
	0;
      elseif (high-end < 64)
#endif
	ash(high-end, 24);
#if (mindy)
      else
	ash(as(<extended-integer>, high-end), 24);
      end if;
#endif
    
end method;


// Write a word to a buffer at a word-aligned byte offset.
// 
define method buffer-word-setter
    (new-val :: <word>, bbuf :: <buffer>, i :: <buffer-index>)
 => res :: <word>;
  // ### big-endian 32 assumption.  Should be a primitive.
#if (mindy)
  let (rest, byte4) = floor/(new-val, 256);
  let (rest, byte3) = floor/(as(<integer>, rest), 256);
  // This assumes that the word is unsigned (i.e. new-val is a positive int)
  let (byte1, byte2) = floor/(rest, 256);
#else
  let byte1 = logand(ash(new-val, -24), 255);
  let byte2 = logand(ash(new-val, -16), 255);
  let byte3 = logand(ash(new-val, -8), 255);
  let byte4 = logand(new-val, 255);
#endif

  bbuf[i + 0] := byte1;
  bbuf[i + 1] := byte2;
  bbuf[i + 2] := byte3;
  bbuf[i + 3] := as(<integer>, byte4);
end method;

#if (mindy)
define method buffer-word-setter
    (new-val :: <integer>, bbuf :: <buffer>, i :: <buffer-index>)
 => res :: <word>;
  // ### big-endian 32 assumption.  Should be a primitive.
  let (rest, byte4) = floor/(new-val, 256);
  let (rest, byte3) = floor/(rest, 256);
  // This assumes that the word is unsigned (i.e. new-val is a positive int)
  let (byte1, byte2) = floor/(rest, 256);
  bbuf[i + 0] := byte1;
  bbuf[i + 1] := byte2;
  bbuf[i + 2] := byte3;
  bbuf[i + 3] := byte4;
end method;
#endif

// #### HACK to allow us to dump headers w/o creating bignums in mindy.  This
// is particularly incorrect for e.g. end entries and references, since they
// might conceivably need more than 24 bits for their data part.
//
define method dump-header-word
    (hi :: <integer>, obj :: <integer>, buf :: <dump-buffer>) => ();

  if (buf.buffer-pos == buf.dump-end) grow-dump-buffer(buf, $word-bytes) end;
  let i = buf.buffer-pos;

  let bbuf = buf.dump-buffer;
  // ### big-endian 32 assumption.  Should be a primitive.
#if (mindy)
  let (rest, byte4) = floor/(obj, 256);
  let (rest, byte3) = floor/(rest, 256);
  // This assumes that the word is unsigned (i.e. new-val is a positive int)
  let (byte1, byte2) = floor/(rest, 256);
#else
  let byte2 = logand(ash(obj, -16), 255);
  let byte3 = logand(ash(obj, -8), 255);
  let byte4 = logand(obj, 255);
#endif

  bbuf[i + 0] := hi;
  bbuf[i + 1] := byte2;
  bbuf[i + 2] := byte3;
  bbuf[i + 3] := byte4;

  buf.buffer-pos := i + $word-bytes;
end method;


// Approximate inverse of dump-header-word, reads a header word from a buffer
// and returns the high byte and rest as two values.
//
define method buffer-header-word(bbuf :: <buffer>, i :: <buffer-index>) 
 => (hi-byte :: <integer>, low :: <integer>);
  // ### big-endian 32 assumption.  Should be a primitive.
  values(bbuf[i + 0],
	 bbuf[i + 3] + ash(bbuf[i + 2], 8) + ash(bbuf[i + 1], 16));
end method;


// Rounds a byte offset up to the next word boundary.
// ### assumes 4byte words.
//
define method round-to-word (x :: <integer>) => res :: <integer>;
  logand(x + 3, -4);
end method;


// Dumper framework:

define constant $od-initial-buffer-size = #x2000;

//
// Buffer of pending OD output.  Not a stream because we need to buffer the
// entire contents before actually writing any data to the OS.  Also, probably
// faster since it's specialized.
//
define /* exported */ class <dump-buffer> (<object>)
  //
  // List of pairs (buffer . length) of previously filled buffers (in reverse
  // order.)   This output comes before the current buffer contents.
  slot previous-buffers :: <list>, init-value: #();
  //
  // Total size of all previously dumped stuff.  This must be added to
  // current-pos to get the actual current file position.
  slot previous-size :: <integer>, init-value: 0;

  //
  // Area where we are currently writing output.
  slot dump-buffer :: <buffer>, 
    init-function: curry(make, <buffer>, size: $od-initial-buffer-size);

  // Current position and end are byte offsets in dump-buffer, but are always
  // word-aligned.
  slot buffer-pos :: <buffer-index>, init-value: 0;
  slot dump-end :: <buffer-index>, init-value: $od-initial-buffer-size;

  slot dump-stack :: <list>, init-value: #();
end class;

define sealed domain make (singleton(<dump-buffer>));
define sealed domain initialize (<dump-buffer>);

// The current "file position" in the dump.  User primarily to record object
// starting positions.
//
define /* exported */ method current-pos (buf :: <dump-buffer>) 
 => res :: <integer>;
  buf.previous-size + buf.buffer-pos;
end method;


// Overall state of dumping, probably involves at least one other buffer (for
// building the header.)
//
define /* exported */ class <dump-state> (<dump-buffer>)
  //
  // Name, Type and Where args to begin-dumping.
  slot dump-name :: <data-unit-name>, required-init-keyword: dump-name:;
  slot dump-type :: <integer>, required-init-keyword: dump-type:;
  slot dump-where :: false-or(<string>), required-init-keyword: dump-where:;
  //
  // A vector mapping local IDs to their offsets in the buffer.  An entry of #f
  // is created add!'ed when a new ID is created.  The actual dumped local
  // index has to to be adjusted to take the header size into account.
  slot dump-local-index :: <stretchy-vector>, 
    init-function: curry(make, <stretchy-vector>);
  //
  // Dump buffer used to accumulate the extern-index (and its enclosed
  // extern-handle objects.)
  slot extern-buf :: <dump-buffer>, init-function: curry(make, <dump-buffer>);
  //
  // Next extern ID to be allocated.
  slot next-extern-id :: <integer>, init-value: 0;
end class;

define sealed domain make (singleton(<dump-state>));

//
// Actually dump an object.
//
define /* exported */ open generic dump-od
    (obj :: <object>, buf :: <dump-buffer>)
 => ();


// begin-dumping and end-dumping 

// Just make a dump state.  We actually write the header last because it
// contains the entire size, etc.  However, we do write the start of the extern
// index into the extern-buffer.
//
define /* exported */ method begin-dumping
  (name :: <data-unit-name>, type :: <integer>,
   #key where :: false-or(<string>))
 => res :: <dump-state>;
  let res = make(<dump-state>, dump-name: name, dump-type: type,
                 dump-where: where);
  dump-definition-header(#"extern-index", res.extern-buf, subobjects: #t);
  res;
end method;


// Return a list of pairs describing all of the buffers in a dump-buffer.  This
// is the previous-buffers appended to a pair describing the current buffer &
// reversed to put in forward order.
//
define method all-dump-buffers (buf :: <dump-buffer>) => res :: <list>;
  reverse(pair(pair(buf.dump-buffer, buf.buffer-pos), buf.previous-buffers));
end method;


// A big prime scaled by word-bytes.
define constant $hash-inc = 134217689 * $word-bytes;

define constant $rot-mask 
#if (mindy)
  = lognot(ash(as(<extended-integer>, -1), $word-bits - 3));
#else
  = lognot(ash(-1, $word-bits - 3));
#endif

// Note that this hash is completely arbitrary.  Nobody expects to able to
// regenerate this hash.  We could use a random number if we had a handy one.
//
// What we do do is do a logxor & rotate hack on 100 words in the buffer,
// starting at an index determined by the object-hash (e.g. address) of the
// state.  Use of a large prime increment ensures that the hashed words will be
// scattered about the data.
//
define method compute-unit-hash (state :: <dump-state>)
    => hash :: <word>;
#if (mindy)
  let res = as(<extended-integer>, get-time-of-day());
#else
  let res = call-out("time", int:, int: 0);
#endif
  let hash-idx = object-hash(state) * $word-bytes;
  for (wot in state.all-dump-buffers)
    let buf = wot.head;
    let len = wot.tail;
    for (count from 0 to 20)
      hash-idx := modulo(hash-idx + $hash-inc, len);
      res := logxor(logior(ash(res, 3 - $word-bits), 
                           ash(logand(res, $rot-mask), 3)),
		    buffer-word(buf, hash-idx));
    end for;
  end for;
  res;
end method;


// Dump the header for the overall data-unit.
//
define method dump-unit-header
    (state :: <dump-state>, buf :: <dump-buffer>, oa-len :: <integer>)
 => ();
  dump-definition-header(#"32bit-data-unit", buf, subobjects: #t,
  		         raw-data: $odf-word-raw-data-format);

  // subtract header words to get raw data words.			 
  dump-word($data-unit-header-size - 2, buf);

  dump-word($od-format-major-version, buf);
  dump-word($od-format-minor-version, buf);
  dump-word(oa-len, buf);
  dump-word($like-an-hp-platform-characteristics, buf);
  dump-word(state.dump-type, buf);
  dump-word(compute-unit-hash(state), buf);
end method;


// Dump a vector as the raw data of an object definition with the specified ID.
// Data is dumped as words, with no subobjects.
//
define method dump-word-vector
    (vec :: <vector>, buf :: <dump-buffer>, name :: <symbol>)
 => ();
  dump-definition-header(name, buf, raw-data: $odf-word-raw-data-format);
  dump-word(vec.size, buf);
  for (word in vec)
    dump-word(word, buf);
  end;
end method;


// Compute the size in *words* of the header (e.g. everything before the main
// buffer contents.  Return this and the total size of the data unit (the
// header size plus the main buffer size.)
//
// We must anticipate what size that the header is going to be, since we have to
// know the size of the header before we can dump it.
//
define method compute-header-size (state :: <dump-state>) 
 => (header-size :: <integer>, oa-size :: <integer>);
  let num-local = state.dump-local-index.size;
  let res = $data-unit-header-size
  	    + ($local-index-size + num-local)
	    + ($local-object-map-size + num-local)
	    + truncate/(state.extern-buf.current-pos, $word-bytes);
  values(res,
  	 res
	   + truncate/(state.current-pos, $word-bytes)
	   + 1 // Because we haven't dumped the final end yet.
         )
end method;


// Build the local object map by sorting the local index.
//
define method build-local-map (state :: <dump-state>) 
 => res :: <simple-object-vector>;
  let lindex = state.dump-local-index;
  let lsize = lindex.size;
  let res = make(<simple-object-vector>, size: lsize);
  for (i from 0 below lsize)
    res[i] := pair(lindex[i], i);
  end;
  res := sort!(res, test: method (x, y) x.head < y.head end);
  for (i from 0 below lsize)
    res[i] := res[i].tail;
  end;
  res;
end method;


// Write all the data in a dump buffer to Stream.
// 
define method write-dump-buffer (buf :: <dump-buffer>, stream :: <stream>)
 => ();
  for (wot in buf.all-dump-buffers)
    write(stream, wot.head, end: wot.tail);
  end for;
end method;


// Build the index and actually write stuff out.
//
define /* exported */ method end-dumping (state :: <dump-state>) => ();

  // End the external index object definition.
  dump-end-entry(0, state.extern-buf);

  let (hsize, oa-len) = compute-header-size(state);

  // Adjust local index by header size.
  let lindex = state.dump-local-index;
  for (i from 0 below lindex.size)
    lindex[i] := lindex[i] + hsize;
  end;

  // End the main buffer now that we know the offset back to the start.
  dump-header-word($odf-end-entry-etype,
                   oa-len - 1, // for end header word size
		   state);

  let header-buffer = make(<dump-buffer>);
  dump-unit-header(state, header-buffer, oa-len);
  dump-word-vector(state.dump-local-index, header-buffer,
  		   #"local-index");
  dump-word-vector(build-local-map(state), header-buffer,
  		   #"local-object-map");

		   
  let fname = state.dump-where
  	      | concatenate(as-lowercase(as(<string>, state.dump-name)),
	      		    ".",
	                    $unit-type-strings[state.dump-type],
			    ".du");

  log-target(fname);
  let stream = make(<file-stream>, locator: fname, direction: #"output");

  write-dump-buffer(header-buffer, stream);
  write-dump-buffer(state.extern-buf, stream);
  write-dump-buffer(state, stream);
  close(stream);
end method;



// Dumper utilities:

// Largest buffer we'll create.  Works around mindy object size limits, and
// probably also saves some gratuitous copying/paging.
//
define constant $dump-buffer-maximum-size = #x10000;

// Increase the size of buffer, leaving room for at least min-bytes more data 
// (a word multiple.)  If the buffer would be bigger than the maximum size,
// then stash the buffer in the previous-buffers slot and make a new
// maximum-size one.
//
// Note that both the buffer and position may be changed by this operation.
//
define method grow-dump-buffer
  (buf :: <dump-buffer>, min-bytes :: <integer>)
 => ();
  let cur-size = buf.dump-end;
  let new-size = max(cur-size + min-bytes, cur-size * 2);
  if (new-size > $dump-buffer-maximum-size)
    buf.previous-buffers
      := pair(pair(buf.dump-buffer, buf.buffer-pos),
              buf.previous-buffers);
    buf.previous-size := buf.previous-size + buf.buffer-pos;
    buf.buffer-pos := 0;
    buf.dump-buffer := make(<buffer>, size: $dump-buffer-maximum-size);
    buf.dump-end := $dump-buffer-maximum-size;
  else
    let new-buf = make(<buffer>, size: new-size);
    copy-into-buffer!(new-buf, 0, buf.dump-buffer);
    buf.dump-buffer := new-buf;
    buf.dump-end := new-size;
  end if;
end method;


// Dump an integer into the buffer as one word.
//
define /* exported */ method dump-word
    (obj :: <word>, buf :: <dump-buffer>)
 => ();
  if (buf.buffer-pos == buf.dump-end) grow-dump-buffer(buf, $word-bytes) end;
  let start = buf.buffer-pos;
  buffer-word(buf.dump-buffer, start) := obj;
  buf.buffer-pos := start + $word-bytes;
end method;


// BLT the contents of obj into the buffer.  Currently we do whatever
// copy-into-buffer! does.
//
define /* exported */ method dump-raw-data
  (obj :: <collection>, bsize :: <integer>, buf :: <dump-buffer>) 
 => ();
  dump-word(bsize, buf);
  let rounded-bsize = round-to-word(bsize);
  if (buf.buffer-pos + rounded-bsize >= buf.dump-end)
    // may change buffer-pos...
    grow-dump-buffer(buf, rounded-bsize)
  end;
  copy-into-buffer!(buf.dump-buffer, buf.buffer-pos, obj);
  buf.buffer-pos := buf.buffer-pos + rounded-bsize;
end method;


// Dump an object definition header, specifying subobjects, raw-data and class
// ID.
//
define /* exported */ method dump-definition-header
  (name :: <symbol>, buf :: <dump-buffer>,
   #key subobjects = #f, raw-data = $odf-no-raw-data-format)
 => ();
  dump-header-word(logior($odf-object-definition-etype,
  		          if (subobjects) $odf-subobjects-flag else 0 end,
			  raw-data),
		   $object-id-registry[name], buf);
  if (subobjects)
    buf.dump-stack := pair(name, buf.dump-stack);
  end;
end method;


// Dump an end-entry for the object that started at start-posn.
//
define /* exported */ method dump-end-entry
  (start-posn :: <integer>, buf :: <dump-buffer>)
 => ();
  dump-header-word($odf-end-entry-etype,
                   buf.current-pos - start-posn,
		   buf);
  buf.dump-stack := tail(buf.dump-stack);
end method;


// Dump an object that has only a constant number of subobjects (no raw data).
// The subobjects are in the #rest args.
//
define /* exported */ method dump-simple-object
  (name :: <symbol>, buf :: <dump-buffer>, #rest slots)
 => ();
  let start-pos = buf.current-pos;
  dump-definition-header(name, buf, subobjects: #t);
  for (thing in slots)
    dump-od(thing, buf);
  end;
  dump-end-entry(start-pos, buf);
end method;


// Dumper local reference support:

// Return a new local object ID.
// 
define /* exported */ method new-local-id(buf :: <dump-state>) 
 => res :: <integer>;
  let lidx = buf.dump-local-index;
  add!(lidx, #f);
  lidx.size - 1;
end method;

// Associate the next object dumped with ID so that references to the ID refer
// to this object.  The argument ID is also returned (so that new-local-id(buf)
// can be the arg.)
//
define /* exported */ method label-next-object
  (id :: <integer>, buf :: <dump-state>)
 => res :: <integer>;
  let lidx = buf.dump-local-index;
  assert(id >= 0 & id < lidx.size);
  assert(lidx[id] == #f);
  lidx[id] := truncate/(buf.current-pos, $word-bytes);
  id;
end method;

// Dump a reference to a local ID (in lieu of an actual object definition.)
//
define /* exported */ method dump-local-reference
  (id :: <integer>, buf :: <dump-buffer>) => ();
  dump-header-word($odf-local-reference-etype, id, buf);
end method;


// Loader framework:


// <data-unit> represents a data unit that is being or has been loaded.
// 
define class <data-unit> (<object>)
  //
  // The name of this data unit.
  slot unit-name :: <data-unit-name>, required-init-keyword: unit-name:;
  //
  // Stuff pulled out of the data unit header.  Except for the check-hash and
  // unit-type, this is probably gratuitous.
  slot minor-version :: <integer>, required-init-keyword: minor-version:;
  slot platform-characteristics :: <integer>,
    required-init-keyword: platform-characteristics:;
  slot unit-type :: <integer>, required-init-keyword: unit-type:;
  slot check-hash :: <word>, required-init-keyword: check-hash:;
  slot location-hint :: false-or(<location-hint>),
    required-init-keyword: location-hint:;
  //
  // True if this unit has been completely loaded.
  slot done-loading? :: <boolean>, init-value: #f;
  //
  // When done-loading? is true, this vector maps local IDs to the actual
  // objects.  During loading elements can be $empty-object or <forward-ref>
  // objects.
  slot local-index :: <simple-object-vector>;
end class;

define sealed domain make (singleton(<data-unit>));
define sealed domain initialize (<data-unit>);

// Table mapping data unit names to a list of pairs (unit-type . data-unit)
//
define variable *data-units* :: <object-table> = make(<table>);

//------------------------------------------------------------------------

// A substitute for limited(<vector>, type: <integer>).  This will be
// faster and more space-efficient to use for the local-index &
// local-map vectors.
//
#if (mindy)
define constant <int-vector> = <simple-object-vector>;
#else
define class <int-vector> (<vector>)
  sealed slot %element :: <integer>,
    init-value: 0, init-keyword: fill:,
    sizer: size, size-init-value: 0, size-init-keyword: size:;
end class <int-vector>;

define sealed domain make (singleton(<int-vector>));
define sealed domain initialize (<int-vector>);

define sealed inline method element
    (vec :: <int-vector>, index :: <integer>,
     #key default = $not-supplied)
    => element :: <object>;
  if (index >= 0 & index < vec.size)
    %element(vec, index);
  elseif (default == $not-supplied)
    error("Vector %= does not contain element %d.", vec, index);
  else
    default;
  end;
end;

define sealed inline method element-setter
    (new-value :: <integer>, vec :: <int-vector>, index :: <integer>)
    => new-value :: <integer>;
  if (index >= 0 & index < vec.size)
    %element(vec, index) := new-value;
  else
    error("Vector %= does not contain element %d.", vec, index);
  end;
end;
#endif

//------------------------------------------------------------------------

define /* exported */ class <load-state> (<object>)
  //
  // The data-unit we're loading.
  slot load-unit :: <data-unit>;
  //
  // The stream we're reading from.  Only used for refilling the buffer.
  /* exported */ slot od-stream :: <stream>, required-init-keyword: stream:;
  //
  // State of the stream buffer, which we hold for the duration of the load.
  /* exported */ slot od-buffer :: <buffer>, required-init-keyword: buffer:;
  // ### These slots should probably be replaced by the buffer-next
  //  and buffer-end slots of od-buffer, now that those exist.
  //  That's a lot of code to change though, so I'm leaving it for now.....
  /* exported */ slot od-next :: <buffer-index>, required-init-keyword: next:,
    /* exported */ setter: od-next-setter ;
  /* exported */ slot od-end :: <buffer-index>, required-init-keyword: end:;
  //
  // The total size in bytes of the data unit, from the header.
  slot overall-length :: <integer>;
  //
  // When added to the od-next pointer, this offset yields the byte offset of
  // our current position from the data-unit start.  This must be updated
  // whenever we refill the buffer.
  slot position-offset :: <integer>,
    required-init-keyword: position-offset:,
    setter: %position-offset-setter;
  //
  // The index in buffer of the next labeled object, or beyond the end when
  // there is no labeled object within the buffer.  Our sentinel also makes the
  // end of the data unit look like a label.  This must be updated whenever the
  // buffer is refilled (when we change position-offset.)
  slot label-index :: <integer>;
  //
  // The dispatcher used for this load.
  slot dispatcher :: <dispatcher>, required-init-keyword: dispatcher:;
  // 
  // The raw vectors of integers for the local-index and local-map vectors.
  // These vectors are supplemented with a sentinel entry at the end which
  // looks like an object at the location of the final end header.  This
  // handles some boundary conditions in the case when we've already loaded all
  // of the labeled objects (or there weren't any.)
  //
  // Word offsets of tagged object in data unit.
  slot raw-local-index :: <int-vector>
    = make(<int-vector>, size: 1,
	   fill: truncate/($maximum-integer, $word-bytes));
  //
  // Local IDs of the tagged objects, in order that the definitions appear.
  slot raw-local-map :: <int-vector> = make(<int-vector>, size: 1, fill: 0);
  //
  // The index in the local-map of the next labeled object to be loaded.
  slot next-labeled :: <integer>, init-value: 0,
    setter: %next-labeled-setter;
  //
  // Vector mapping extern IDs in this data unit to the actual in-core
  // objects.  Will contain <forward-ref>s where there are circular
  // cross-unit dependencies.
  slot extern-index :: <simple-object-vector>;
  //
  // User by debugging code to trace loaders in effect.
  slot load-stack :: <list>, init-value: #();
end class;

define sealed domain make (singleton(<load-state>));
define sealed domain initialize (<load-state>);

// This method automatically updates the label-index slot when the
// position-offset changes.
//
define method position-offset-setter
    (new-val :: <integer>, state :: <load-state>)
 => res :: <integer>;

  state.label-index
    := (state.raw-local-index[state.raw-local-map[state.next-labeled]]
        * $word-bytes)
       - new-val;
  state.%position-offset := new-val;
end method;

// Similar to above, but updates the label-index when we set the next-labeled
// slot.
//
define method next-labeled-setter
    (new-val :: <integer>, state :: <load-state>)
 => res :: <integer>;

  state.label-index
    := (state.raw-local-index[state.raw-local-map[new-val]]
        * $word-bytes)
       - state.position-offset;
  state.%next-labeled := new-val;
end method;


// Dispatchers:

define constant undefined-entry-type = method (#rest args)
  ignore(args);
  error("Undefined ODF entry type.");
end method;

define constant $dispatcher-table-size = #x400;
define /* exported */ class <dispatcher> (<object>)
  slot table :: <simple-object-vector>,
    init-function: curry(make, <vector>, size: $dispatcher-table-size,
    			 fill: undefined-entry-type);
end class;

define sealed domain make (singleton(<dispatcher>));
define sealed domain initialize (<dispatcher>);

// If initialize-from: is specified, fill the dispatcher from that other
// dispatcher.  By default, dispatchers are initialized from
// *default-dispatcher* which provides loaders for od-format pseudo-objects and
// of other useful dylan types (defined elsewhere.)
//
define method initialize
    (disp :: <dispatcher>, #next next-method,
     #key initialize-from = *default-dispatcher*)
 => ();
  next-method();
  if (initialize-from)
    disp.table := shallow-copy(initialize-from.table);
  end;
end method;

// Default dispatcher used for initialization.
define /* exported */ variable *default-dispatcher*
    = make(<dispatcher>, initialize-from: #f);

//
// Add a loader for a particular entry type to the supplied dispatcher.
// The method is passed the <load-state> as its arg, and returns the loaded
// object.
//
// The object definition header will have already been loaded, and it is
// guaranteed that there will be at least one word in the buffer
// (e.g. the raw size.)
//
define /* exported */ method add-od-loader
    (dispatcher :: <dispatcher>, name :: <symbol>, func :: <function>)
 => ();
  let etype = $object-id-registry[name];
  unless (dispatcher.table[etype] == undefined-entry-type)
    signal("Already an OD loader for etype %=\n", etype);
  end;
  dispatcher.table[etype] := func;
end method;


// Searching for a data-unit file

define variable *Data-Unit-Search-Path* :: <sequence> = #[];

define method search-for-file
    (name :: <byte-string>, hint :: false-or(<location-hint>))
    => (stream :: false-or(<stream>), found-loc :: false-or(<byte-string>));
  find-and-open-file(name, if (hint)
			     concatenate(*Data-Unit-Search-Path*, 
					 vector(hint));
			   else
			     *Data-Unit-Search-Path*;
			   end if);
end method search-for-file;


// find-data-unit

// Check-unit-header parses the data-unit header, checking that all of the
// appropriate fields match.  We return a new data-unit and the total data
// length as values.
//
define method check-unit-header 
    (state :: <load-state>, name :: <data-unit-name>,
     expected-type :: <integer>,
     expected-hash :: false-or(<word>),
     location-hint :: false-or(<location-hint>))
 => (res :: <data-unit>, oa-len :: <integer>);
  let buf = state.od-buffer;
  let base = fill-at-least($data-unit-header-size * $word-bytes, state);

  let (tag, id) = buffer-header-word(buf, base);
  unless (tag = logior($odf-object-definition-etype,
  		       $odf-subobjects-flag, $odf-word-raw-data-format))
    error("Invalid ODF header on %=", state.od-stream);
  end;
  unless (id = $32bit-data-unit-odf-id)
    error("Unrecognised entry ID %=.  Bad data?", id);
  end;

  let hsize =	buffer-word(buf, base + ($word-bytes * 1));
  let major =	buffer-word(buf, base + ($word-bytes * 2));

  // Check major version now, in case the unit header format changed.
  unless (major = $od-format-major-version)
    error("Incompatible OD major version %=, current version is %=.",
    	  major, $od-format-major-version);
  end;

  let minor =	buffer-word(buf, base + ($word-bytes * 3));
  let oa-len =	buffer-word(buf, base + ($word-bytes * 4));
  let platfm =	buffer-word(buf, base + ($word-bytes * 5));
  let type =	buffer-word(buf, base + ($word-bytes * 6));
  let hash =	buffer-word(buf, base + ($word-bytes * 7));

  // Change in header size should be flagged by a new major version, but...
  unless (hsize = $data-unit-header-size - 2)
    error("Header size is wrong? (%=)", hsize);
  end;

  unless (minor <= $od-format-minor-version)
    error("Incompatible OD minor version %=, current version is %=.",
    	  minor, $od-format-minor-version);
  end;

  unless (platfm = $like-an-hp-platform-characteristics)
    error("Data unit was created on an incompatible platform:", state);
  end;

  unless (type = expected-type)
    error("Expected unit type %=, got %=", $unit-type-strings[expected-type],
          $unit-type-strings[type]);
  end;

  unless (~expected-hash | hash = expected-hash)
    error("Unit hash mismatch; version mismatch between data units.\n  %=",
          state);
  end;

  let res = make(<data-unit>, unit-name: name, minor-version: minor,
  	         platform-characteristics: platfm, unit-type: type,
		 check-hash: hash, location-hint: location-hint);
  *data-units*[name] := pair(pair(type, res),
  			     element(*data-units*, name, default: #()));

  state.od-next := base + ($data-unit-header-size * $word-bytes);
  values(res, oa-len);

end method;


define class <empty-object> (<object>)
end class <empty-object>;

define sealed domain make (singleton(<empty-object>));
define sealed domain initialize (<empty-object>);

define constant $empty-object :: <empty-object> = make(<empty-object>);


// Load a data unit from a file.
//
define method load-data-unit
 (name :: <data-unit-name>, type :: <integer>,
  loc :: false-or(<location-hint>), hash :: false-or(<word>),
  dispatcher :: <dispatcher>)
 => res :: <data-unit>;

  let name-guess
    = concatenate(as-lowercase(as(<string>, name)), ".",
    		  $unit-type-strings[type], ".du");

  let (stream, found-loc) = search-for-file(name-guess, loc);
  unless (stream)
    error("Can't open object dump file %=", name-guess);
  end;

  log-dependency(found-loc);

//dformat("\nLoading %=\n", name-guess);
  let buf :: false-or(<buffer>) = get-input-buffer(stream);
  if (~ buf) error(make(<end-of-stream-error>, stream: stream)) end;
  let initial-next :: <buffer-index> = buf.buffer-next;
  let buf-end :: <buffer-index> = buf.buffer-end;
  let state = make(<load-state>, stream: stream, buffer: buf,
  		   next: initial-next, end: buf-end,
		   position-offset: -initial-next,
		   dispatcher: dispatcher);

  let (unit, oa-len)
    = check-unit-header(state, name, type, hash,
			found-loc[0] == '/' & found-loc);
  state.load-unit := unit;
  state.overall-length := oa-len;
  let rlocal = load-object-dispatch(state);
  let nlocals = rlocal.size;

  // Note that we add a sentinel entry at the end of the local index/map.
  state.raw-local-index
    := concatenate-as(<int-vector>, rlocal, vector(oa-len - 1));
  state.raw-local-map
    := concatenate-as(<int-vector>, load-object-dispatch(state),
		      vector(nlocals));

  // initializes label-index...
  state.position-offset := state.position-offset;

  state.extern-index := load-object-dispatch(state);

  let lindex = make(<simple-object-vector>, size: nlocals, fill: $empty-object);
  unit.local-index := lindex;

  block (punt)
    while (#t)
      let obj = load-object-dispatch(state);
      if (obj == $end-object) punt() end;
    end while;
  end block;
  unit.done-loading? := #t;

  buf := state.od-buffer; // in case it got demolished somewhere
  buf.buffer-next := state.od-next;
  buf.buffer-end := state.od-end;
  release-input-buffer(state.od-stream);
  close(state.od-stream);

  for (entry in lindex)
    assert(~instance?(entry, <forward-ref>) & entry ~== $empty-object);
  end;

  unit;
end;


// Find-data-unit, when given a data unit name and type, returns the
// corresponding data unit.  This is the main entry for causing loading to
// happen.  Also used recursively.
//
define /* exported */ method find-data-unit
  (name :: <data-unit-name>, type :: <integer>,
   #key location-hint :: false-or(<location-hint>),
        check-hash: expected-hash :: false-or(<word>),
	dispatcher :: <dispatcher> = *default-dispatcher*)
 => res :: <data-unit>;
  let types = element(*data-units*, name, default: #());
  block (punt)
    for (elt in types)
      if (elt.head = type)
        let found = elt.tail;
	unless (~expected-hash | expected-hash = found.check-hash)
          error("Unit hash mismatch; version mismatch between data units.\n"
	  	"%=",
		found.unit-name);
	end unless;
	punt(found);
      end;
    end for;
    load-data-unit(name, type, location-hint, expected-hash, dispatcher);
  end block;
end method;


// load-object-dispatch

// The $end-object, used to mark loading an end header.
//
define class <end-object> (<object>)
end;

define sealed domain make (singleton(<end-object>));
define sealed domain initialize (<end-object>);

define /* exported */ constant $end-object :: <end-object>
  = make(<end-object>);

define constant $load-debug = #f;


// Start loading some objects from a load-state.  Dispatches to an appropriate
// loader method depending on the dispatcher and the entry type.  Returns the
// object.
//
define /* exported */ method load-object-dispatch (state :: <load-state>)
 => res :: <object>;

  // ### Shouldn't need this -- temporarily fixing something else
  // Make sure label-index is initialized...
  state.position-offset := state.position-offset;

  let buf = state.od-buffer;
  let next = state.od-next;
  // Check if we at a label (or the end.)
  if (next >= state.label-index)
    assert(next = state.label-index);
    let position = next + state.position-offset;

    // If about to read the final end, 
    if (position = (state.overall-length - 1) * $word-bytes)
      // Just return end object.
      state.od-next := next + $word-bytes;
      $end-object;
    else
      // An actual label.  Remember where we are, bump the label pointer and
      // recurse to get the value.
      let nextlab = state.next-labeled;
      let id = state.raw-local-map[nextlab];
      state.next-labeled := nextlab + 1;
      let res = load-object-dispatch(state);
      let lidx = state.load-unit.local-index;
      let old = lidx[id];
      if (instance?(old, <forward-ref>))
        resolve-forward-ref(old, res);
      else
        assert(old == $empty-object);
      end;
      if (res.obj-resolved?)
	lidx[id] := res.actual-obj;
      else
	request-backpatch(res, method (actual) lidx[id] := actual end);
	lidx[id] := res;
      end if;
    end if;

  // Normal case, load an entry.
  else
    // This check is just to avoid a do-nothing call to fill-at-least.
    unless (state.od-end - next > $word-bytes * 2)
      next := fill-at-least($word-bytes * 2, state);
    end unless;

    let (flags, code) = buffer-header-word(buf, next);
    state.od-next := next + $word-bytes;

    select (ash(flags, $odf-etype-shift))
     $odf-object-definition-shifted =>

if ($load-debug)
       assert(code < $dispatcher-table-size);
       let orig-stack = state.load-stack;
       block ()
	 state.load-stack := pair(code, orig-stack);
	 state.dispatcher.table[code](state);
       cleanup
	 state.load-stack := orig-stack;
       end;
else
       state.dispatcher.table[code](state);
end if;

     $odf-end-entry-shifted =>
       $end-object;

     $odf-local-reference-shifted =>
       maybe-forward-ref(state.load-unit, code);

     $odf-external-reference-shifted =>
       let wot = state.extern-index[code];
       if (obj-resolved?(wot))
         let res = wot.actual-obj;
	 unless (instance?(res, <identity-preserving-mixin>)
	           & instance?(res.handle, <extern-handle>))
           error("Externally referencing an object that wasn't loaded with\n"
	         "load-external-definition.",
		 wot);
	 end unless;
	 res;
       else
         wot;
       end;

    end select;
  end if;
end method;


// Loading utilities:

// Fill input buffer so that it holds at least nbytes.  If that much data is
// already there, just return.  For convenience, the value of next is returned.
//
define /* exported */ method fill-at-least
  (nbytes :: <buffer-index>, state :: <load-state>)
 => next :: <buffer-index>;
  let next = state.od-next;
  let buf-end = state.od-end;
  let avail = buf-end - next;
  if (avail >= nbytes)
    next;
  else
    let buf = state.od-buffer;
    let stream = state.od-stream;
    assert(nbytes <= buf.size);
    if (next == buf-end)
      // Reflect this fact in buffer-next, buffer-end so that
      // next-input-buffers sees it...
      buf.buffer-next := buf.buffer-end;
    else
      // Move everything to the front of the buffer, to maximize the
      // amount we can fit at the end.
      copy-into-buffer!(buf, 0, buf, start: next, end: buf-end);
      buf.buffer-next := 0;
      // ### Setting buffer-end is a no-no. For now, it's the only way
      // to do what was being done under the old stream spec. It should
      // work with our implementation of the new streams spec, but it is
      // not a defined by the spec to do so.
      buf.buffer-end := avail;
    end if;
    state.position-offset := state.position-offset + next;
    buf := next-input-buffer(stream, bytes: nbytes);
    if (~ buf) error(make(<end-of-stream-error>, stream: stream)) end;
    // Update od-next and od-end to reflect the changes we've made in
    // buffer-next and buffer-end. 
    state.od-end := buf.buffer-end;
    state.od-next := (buf.buffer-next := 0);
  end if;
end method;


// Load data out of a stream into a string.  Could be generalized to other
// sequences.
//
// This implementation is somewhat optimized toward short strings, in that it
// pulls all data through the buffer, and doesn't ever read directly into the
// result sequence.  The idea is that most times the loop will run only once,
// since there will be enough data in the buffer.
// 
define /* exported */ method load-raw-data
  (res :: <byte-string>, elsize :: <integer>, state :: <load-state>)
 => nnext :: <buffer-index>;
  let buf = state.od-buffer;
  let next :: <buffer-index> = state.od-next;
  let buf-end :: <buffer-index> = state.od-end;
  let sofar :: <integer> = 0;
  block (punt)
    while (#t)
      let nnext = min(next + (elsize - sofar), buf-end);
      let nsofar = sofar + (nnext - next);
      copy-from-buffer!(buf, next, res, start: sofar, end: nsofar);
      sofar := nsofar;
      next := nnext;
      if (sofar = elsize)
        let next = round-to-word(next);
	buf.buffer-next := (state.od-next := next);
        punt(next);
      else
        assert(next = buf-end);
	buf.buffer-next := buf-end;
        buf := next-input-buffer(state.od-stream);
	if (~ buf)
	  error(make(<end-of-stream-error>, stream: state.od-stream));
	end;
	state.position-offset := state.position-offset + next;
	state.od-end := (buf-end := buf.buffer-end);
	next := buf.buffer-next;
      end if;
    end while;
  end block;
end method;

// Utility that loads an object's subobjects into a stretchy-vector.
//
define method load-subobjects-vector
    (state :: <load-state>, #key size-hint :: false-or(<integer>))
 => (res :: <simple-object-vector>);
  if (size-hint)
    let contents :: <simple-object-vector>
      = make(<simple-object-vector>, size: size-hint);
    for (part = load-object-dispatch(state) then load-object-dispatch(state),
	 i :: <integer> from 0,
	 until: part == $end-object)
      contents[i] := part;
    finally
      assert(i == size-hint);
      contents;
    end for;
  else
    let contents :: <stretchy-vector> = make(<stretchy-vector>);
    for (part = load-object-dispatch(state) then load-object-dispatch(state),
	 i :: <integer> from 0,
	 until: part == $end-object)
      contents[i] := part;
    finally
      as(<simple-object-vector>, contents);
    end for;
  end if;
end method;


// Load a one-slot object.  The value is returned, and we check that there was
// in fact only one subobject.
//
define /* exported */ method load-sole-subobject (state :: <load-state>)
 => res :: <object>;
  let res = load-object-dispatch(state);
  assert(load-object-dispatch(state) == $end-object);
  res;
end method;


// Call after reading fixed number of subobjects to eat the end header.
//
define /* exported */ method assert-end-object (state :: <load-state>) => ();
  assert(load-object-dispatch(state) == $end-object);
end method;


// Load raw data of an entry as words into a <simple-object-vector>
// ### should really be a word vector.
//
define constant load-word-vector = method (state :: <load-state>) 
 => res :: <simple-object-vector>;
  let buf = state.od-buffer;
  let nwords = buffer-word(buf, state.od-next);
  state.od-next := state.od-next + $word-bytes;
  let res = make(<simple-object-vector>, size: nwords);
  for (i :: <integer> from 0 below nwords)
    res[i] := buffer-word(buf, fill-at-least($word-bytes, state));
    state.od-next := state.od-next + $word-bytes;
  end for;
  res;
end method;


// Forward references:

// <forward-ref> is an IOU that that can be returned by any call to the loader
// in a place where there might be a reference to an object that hasn't been
// loaded yet.  A loader can resolve forward references either by backpatching
// or by requiring users of the result to be prepared to stumble across
// <forward-ref> objects (probably by using actual-obj everywhere.)
//
// When created, the forward-ref is stored in the local index of the
// <load-state> that is supposed to define the object.  We discover
// that there was a forward ref when we go to store the actual value.
// 
define /* exported */ class <forward-ref> (<object>)
  //
  // Once resolved, this is the actual object.  Unbound until resolved.
  /* exported */ slot actual-obj :: <object>;
  //
  // True when resolved.
  /* exported */ slot obj-resolved? :: <boolean>, init-value: #f;
  //
  // List of registered backpatch functions.
  slot patchers :: <list>, init-value: #();
end class;

define sealed domain make (singleton(<forward-ref>));
define sealed domain initialize (<forward-ref>);

// Anything that isn't a <forward-ref> is resolved, and is itself.
//
define method obj-resolved? (obj :: <object>) => res :: <boolean>;
  #t;
end method;
//
define method actual-obj (obj :: <object>) => res :: <object>;
  obj;
end method;


// Register a backpatching function (a closure) for a particular forward
// reference.  This function is called with the actual value when the reference
// is resolved, and is responsible for storing this value in the appropriate
// place.
//
define /* exported */ method request-backpatch
  (ref :: <forward-ref>, fun :: <function>)
 => ();
  assert(~ref.obj-resolved?);
  ref.patchers := pair(fun, ref.patchers);
end method;


// Resolve a forward reference, doing any necessary backpatching.  This may be
// called by loaders to resolve a reference early (before the loader for the
// object has returned.)
//
define /* exported */ method resolve-forward-ref
    (ref :: <forward-ref>, value :: <object>) => ();
  if (instance?(value, <forward-ref>))
    if (value.obj-resolved?)
      resolve-forward-ref(ref, value.actual-obj);
    else
      request-backpatch
	(value, method (actual) resolve-forward-ref(ref, actual) end);
    end if;
  else
    if (ref.obj-resolved?)
      assert(value == ref.actual-obj);
    else
      ref.actual-obj := value;
      ref.obj-resolved? := #t;
      for (x in ref.patchers)
	x(value);
      end;
      ref.patchers := #(); // for GC
    end if;
  end if;
end method;


// Given a data unit and a local ID, return the referenced object, or a forward
// reference if it doesn't exist yet.
//
define method maybe-forward-ref (unit :: <data-unit>, id :: <integer>)
 => res :: <object>;
  let lidx = unit.local-index;
  let thing = lidx[id];
  if (thing == $empty-object)
    lidx[id] := make(<forward-ref>);
  elseif (thing.obj-resolved?)
    thing.actual-obj;
  else
    thing;
  end;
end method;


// External references:

// A handle on some object having <identity-preserving-mixin>.  This is where
// we record the ID used to reference the object in this dump.
//
define abstract class <basic-handle> (<object>)
  //
  // The (local or external) ID in the current dump of this object.
  slot dump-id :: false-or(<integer>), init-value: #f,
    init-keyword: dump-id:;
  //
  // <dump-state> for the dump that dump-id is valid in.  Currently only for a
  // sanity check.
  slot dump-state :: false-or(<dump-state>), init-value: #f,
    init-keyword: dump-state:;
end class;

define sealed domain make (singleton(<basic-handle>));
define sealed domain initialize (<basic-handle>);

// Handle on an object that has been loaded from another data unit and marked
// as external by the loader method using note-external-definition-loaded.
//
define class <extern-handle> (<basic-handle>)
  //
  // Data unit that defines this object.
  slot defining-unit :: <data-unit>, required-init-keyword: defining-unit:;
  //
  // Local ID in the defining unit.
  slot local-id :: <integer>, required-init-keyword: local-id:;
end class;

define sealed domain make (singleton(<extern-handle>));

// Represents an object created in this current program.  Note that the object
// may actually have external reference semantics in the case where we are
// currently defining the dumping unit.  The external-handle is only used when
// we've loaded and external reference (as flagged by load-external-definition)
//
define class <local-handle> (<basic-handle>)
end class;

define sealed domain make (singleton(<local-handle>));

// This mixin is inherited by objects that want to preserve object identity,
// either within or across data unit boundaries (local or external references.
// The maybe-dump-reference function can be used by dumpers for objects that
// have this mixin in order to ensure that only one copy is dumped, and other
// mentions of the object become references.
// 
define open abstract /* exported */ class <identity-preserving-mixin>
    (<object>)
  //
  // The <handle> describing how this object has been dumped.
  slot handle :: false-or(<basic-handle>), init-value: #f;
end class;


// This predicate can be used to check to see if some object has been defined
// externally.  If so, then any local changes to it will not be saved, because
// the one true dump of the object has already been made.
// 
define /* exported */ generic defined-externally? (object :: <object>)
    => res :: <boolean>;

define method defined-externally? (thing :: <object>)
    => res :: <boolean>;
  #f;
end method defined-externally?;

define method defined-externally? (thing :: <identity-preserving-mixin>)
    => res :: <boolean>;
  instance?(thing.handle, <extern-handle>);
end method defined-externally?;



// maybe-dump-reference

// If Obj has already been dumped, dump a reference to it, and return #F.
// Otherwise, mark the next object dumped as being the definition of this
// object, and return #T.  In other words, the work of dumping is complete when
// we #F.  When we return #T, the caller needs to actually dump the guts of the
// object.
//
// Callers that want to use the ref mechanism say this in their dump-od method:
//   when (maybe-dump-reference(obj, buf))
//     ... actually dump othe object....
//   end unless;
//
// Currently, Obj must have <identity-preserving-mixin>, though we could use a
// table to preserve local idenity of arbitrary objects.  We dispatch according
// to the location handle in the object.
//
define /* exported */ method maybe-dump-reference
    (obj :: <identity-preserving-mixin>, buf :: <dump-state>)
 => res :: <boolean>;
  maybe-dump-reference-dispatch(obj, obj.handle, buf);
end method;


// If an object has an <extern-handle>, we need to dump an external
// reference instead of the referenced object.  If this object hasn't been
// referenced yet in the current dump, then we need to dump an extern-handle
// object too.  This object goes in the special buffer we reserve for the
// extern index.
//
define method maybe-dump-reference-dispatch
    (obj :: <identity-preserving-mixin>, handle :: <extern-handle>,
     buf :: <dump-state>)
 => res :: <false>;

  ignore(obj);
  let d-state = handle.dump-state;
  let d-id = handle.dump-id;
  if (d-state)
    assert(d-state == buf);
  else
    let xbuf = buf.extern-buf;
    let unit = handle.defining-unit;
    let cpos = xbuf.current-pos;
    dump-definition-header(#"extern-handle", xbuf, subobjects: #t,
    			   raw-data: $odf-word-raw-data-format);
    dump-word(3, xbuf);
    dump-word(unit.check-hash, xbuf);
    dump-word(unit.unit-type, xbuf);
    dump-word(handle.local-id, xbuf);
    dump-od(unit.unit-name, xbuf);
    dump-od(unit.location-hint, xbuf);
    dump-end-entry(cpos, xbuf);
    d-id := buf.next-extern-id;
    handle.dump-id := d-id;
    buf.next-extern-id := d-id + 1;
    handle.dump-state := buf;
  end if;

  dump-header-word($odf-external-reference-etype, d-id, buf);
  #f;
end method;


// If an object has a local handle, then it has already been defined in this
// dump, so we just reference it.
//
define method maybe-dump-reference-dispatch
    (obj :: <identity-preserving-mixin>, handle :: <local-handle>,
     buf :: <dump-state>)
 => res :: <false>;

  ignore(obj);
  let d-state = handle.dump-state;
  let d-id = handle.dump-id;
  assert(d-state == buf);
  dump-local-reference(d-id, buf);
  #f;
end method;


// If an object has no handle, then we only need to preserve identity locally,
// and the object hasn't yet been dumped.  Make a local handle, label the next
// object, and return #T to signal that the guts should be dumped.
//
define method maybe-dump-reference-dispatch
    (obj :: <identity-preserving-mixin>, handle == #f, buf :: <dump-state>)
 => res :: <true>;
  let d-id = label-next-object(new-local-id(buf), buf);
  obj.handle := make(<local-handle>, dump-id: d-id, dump-state: buf);
  #t;
end method;


// load-external-definition
//
// This function wraps around some loading code (passed in as a function).  The
// body returns the value of the load method, which must have
// <identity-preserving-mixin>.
//
// This function is called by od-loader methods to indicate that the object
// should have external reference semantics: if the object is dumped again,
// dump an external reference to the place where we loaded it, instead of
// dumping a copy.
//
define /* exported */ method load-external-definition
    (state :: <load-state>, body :: <function>) 
 => res :: <identity-preserving-mixin>;

  let unit = state.load-unit;
  let id = state.raw-local-map[state.next-labeled - 1];
  assert(state.raw-local-index[id] * $word-bytes
  	   = state.od-next + state.position-offset - $word-bytes);

  local method set-handle (obj :: <identity-preserving-mixin>) => ();
	  assert(~obj.handle);
	  obj.handle
	    := make(<extern-handle>, defining-unit: unit, local-id: id);
	end method set-handle;
  let res = body(state);
  if (instance?(res, <forward-ref>))
    request-backpatch(res, set-handle);
  else
    set-handle(res);
  end if;
  res;
end method;


// Pseudo-object loaders:

add-od-loader(*default-dispatcher*, #"local-index", load-word-vector);

add-od-loader(*default-dispatcher*, #"local-object-map",
	      load-word-vector);

add-od-loader(*default-dispatcher*, #"extern-index",
  method (state :: <load-state>)
    load-subobjects-vector(state);
  end method
);


// Load an extern-handle.  After parsing the contents, we look up the
// referenced data unit and try to find the object.  If it isn't there yet, we
// make a forward reference.
//
add-od-loader(*default-dispatcher*, #"extern-handle", 
  method (state :: <load-state>) => res :: <object>;
    state.od-next := state.od-next + $word-bytes;
    let buf = state.od-buffer;
    let next = fill-at-least($word-bytes * 3, state);
    let hash = buffer-word(buf, next);
    let du-type = buffer-word(buf, next + $word-bytes);
    let localid = buffer-word(buf, next + ($word-bytes * 2));
    state.od-next := next + ($word-bytes * 3);
    let name = load-object-dispatch(state);
    let hint = load-object-dispatch(state);
    assert(load-object-dispatch(state) == $end-object);

    let ext-unit
      = find-data-unit(name, du-type, location-hint: hint,
      		       check-hash: hash,
		       dispatcher: state.dispatcher);

    maybe-forward-ref(ext-unit, localid);		       
  end method
);



// "make dumper" support:

// A "make dumper" may be used when an object can be reconstructed
// from some accessor values by reinstantiating the object with Make and
// backpatching some slots.  This is pretty general, since the accessor and
// setter function don't really have to be slot accessors and setters.
//
// This function and add-od-make-loader take a list describing the slots which
// is in this format:
//    #(slot-accessor-function-1, init-keyword-1, setter-function-1,
//      slot-accessor-function-2, init-keyword-2, setter-function-2,
//      ...)
//
// Note that the stuff is implicitly grouped in clumps of 3.  Either of the
// init-keyword or the setter may be #f, but not both.
// The "make dumper" requests backpatching when an unresolved forward reference
// is encountered (due to circularities.)  Any slot which might have a forward
// reference must have a setter function (it can also have an init keyword,
// which will be used if possible.)
// 

// Info about some class we have a make dumper for.  Slots are represented as
// parallel vectors for iteration convenience.
//
define class <make-info> (<object>)
  slot accessor-funs :: <simple-object-vector>,
    required-init-keyword: accessor-funs:;
  slot init-keys :: <simple-object-vector>,
    required-init-keyword: init-keys:;
  slot setter-funs :: <simple-object-vector>,
    required-init-keyword: setter-funs:;
  slot obj-class :: <class>,
    required-init-keyword: obj-class:;
  slot obj-name :: <symbol>,
    required-init-keyword: obj-name:;
  slot dump-side-effect :: false-or(<function>),
    required-init-keyword: dump-side-effect:;
end class;

define sealed domain make (singleton(<make-info>));
define sealed domain initialize (<make-info>);

// Table mapping classes to <make-info> objects.
//
define constant *make-dumpers* :: <object-table> = make(<object-table>);

// Keep track of what we've complained about already, so we don't spam
// the reader with the same warnings
//
define constant *classes-I-cant-dump* :: <object-table> = make(<object-table>);

// In order to avoid using add-method (so dump-od can be sealed), we have a
// default method for dump-od which does its own dispatching based on the
// class.
//
// Specialized on dump-buffer so that it's clear this method is least specific.
//
define method dump-od (obj :: <object>, buf :: <dump-buffer>) => ();
  check-type(buf, <dump-state>);
  let oclass = object-class(obj);
  let found = element(*make-dumpers*, oclass, default: #f);
  if (~found)
    // Now check to see if we've complained once before about this class
    unless (element(*classes-I-cant-dump*, oclass, default: #f))
      *classes-I-cant-dump*[oclass] := #t;
      signal("Don't know how to dump instances of %=.  Dump stack: %=\n",
	     oclass, buf.dump-stack);
    end unless;
  elseif (~instance?(obj, <identity-preserving-mixin>)
            | maybe-dump-reference(obj, buf))
    apply(dump-simple-object, found.obj-name, buf,
          map(method (fun) obj.fun end, found.accessor-funs));
    if (found.dump-side-effect)
      found.dump-side-effect(obj, buf);
    end if;
  end if;
end method;


// Actual body of make-loader loading.  Iterate across the slots and handle
// ones w/o init-keywords or that need to be backpatched.
//
define method make-loader-guts 
    (state :: <load-state>, info :: <make-info>)
 => res :: <object>;
  let key-count = info.init-keys.size;
  let vec = load-subobjects-vector(state, size-hint: key-count);
  assert(vec.size == key-count);

  let keys = #();
  let setter-losers = #();
  let key-losers = #();

  for (i :: <integer> from 0 below key-count)
    let key = info.init-keys[i];
    let val = vec[i];
    if (obj-resolved?(val))
      if (key)
	keys := pair(key, pair(val.actual-obj, keys));
      else
	setter-losers := pair(i, setter-losers);
      end if;
    else
      if (info.setter-funs[i])
	setter-losers := pair(i, setter-losers);
      else
	key-losers := pair(i, key-losers);
      end if;
    end if;
  end for;

  local method make-obj () => x :: <object>;
	  let obj = apply(make, info.obj-class, keys);
  
	  for (i :: <integer> in setter-losers)
	    let setter = info.setter-funs[i];
	    let val = vec[i];
	    assert(setter);
	    if (obj-resolved?(val))
	      setter(val.actual-obj, obj);
	    else
	      request-backpatch(val, method (actual) setter(actual, obj) end);
	    end;
	  end;
	  
	  obj;
	end method make-obj;


  if (key-losers == #())
    make-obj();
  else
    let unresolved-keys = key-losers.size;
    let forward = make(<forward-ref>);

    for (i :: <integer> in key-losers)
      let key = info.init-keys[i];
      let val = vec[i];
      request-backpatch
	(val,
	 method (actual) => ();
	   keys := pair(key, pair(actual, keys));
	   if ((unresolved-keys := unresolved-keys - 1).zero?)
	     resolve-forward-ref(forward, make-obj());
	   end if;
	 end method);
    end for;

    forward;
  end if;

end method;


// This function adds dumping information for objects of the specified
// obj-class.  Note that obj-class must be the direct class of the instances we
// are to dump (and of course, must also be instantiable via make.)
//
// We parse the slots into a <make-info>, stick it in *make-dumpers* and add
// and od-loader with a closure as the function.
//
define /* exported */ method add-make-dumper
  (name :: <symbol>, dispatcher :: <dispatcher>,
   obj-class :: <class>, slots :: <list>,
   #key load-external :: <boolean>, dumper-only :: <boolean>,
        load-side-effect :: false-or(<function>),
        dump-side-effect :: false-or(<function>))
 => ();
  let acc = make(<stretchy-vector>);
  let key = make(<stretchy-vector>);
  let set = make(<stretchy-vector>);
  for (current = slots then current.tail.tail.tail,
       until: current = #())
    let a = current.first;
    let k = current.second;
    let s = current.third;
    check-type(a, <function>);
    check-type(k, false-or(<symbol>));
    check-type(s, false-or(<function>));
    assert(dumper-only | k | s);
    add!(acc, a);
    add!(key, k);
    add!(set, s);
  end for;

  let info =
    make(<make-info>,
         accessor-funs: as(<simple-object-vector>, acc),
	 init-keys: as(<simple-object-vector>, key),
         setter-funs: as(<simple-object-vector>, set),
	 obj-class: obj-class,
	 obj-name: name,
	 dump-side-effect: dump-side-effect);

  *make-dumpers*[obj-class] := info;

  unless (dumper-only)
    let loader
      = if (load-side-effect)
	  method (state :: <load-state>) => res :: <object>;
	    let res = make-loader-guts(state, info);
	    if (instance?(res, <forward-ref>))
	      request-backpatch(res, load-side-effect);
	    else
	      load-side-effect(res);
	    end if;
	    res;
	  end;
	else
	  method (state :: <load-state>) => res :: <object>;
	    make-loader-guts(state, info);
	  end;
	end;

    add-od-loader(dispatcher, name,
		  if (load-external)
		    method (state :: <load-state>) => res :: <object>;
		      load-external-definition(state, loader);
		    end;
		  else
		    loader;
		  end);
  end;

end method;

// This function is used by the dump file profiler
//
define method invert-registry () => inverted-registery :: <vector>;
  let registry = $object-id-registry;
  let vec = make(<vector>, size: // $dispatcher-table-size);
		   4000);
  let keys = key-sequence(registry);
  for (key in keys)
    let id = registry[key];
    vec[id] := key;
  end for;
  vec;
end method invert-registry;
