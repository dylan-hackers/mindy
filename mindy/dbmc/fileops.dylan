Module: fileops

define constant $dbc-MagicNumber = #x6e789abe;

define constant $file-MajorVersion = 4;
define constant $file-MinorVersion = 1;

define constant $fop-FLAME = 0;
define constant $fop-HEADER = 1;
define constant $fop-STORE = 2;
define constant $fop-SHORT-REF = 3;
define constant $fop-REF = 4;

define constant $fop-FALSE = 5;
define constant $fop-TRUE = 6;
define constant $fop-UNBOUND = 7;

define constant $fop-SIGNED-BYTE = 8;
define constant $fop-SIGNED-SHORT = 9;
define constant $fop-SIGNED-INT = 10;
define constant $fop-SIGNED-LONG = 11;
define constant $fop-CHAR = 12;
define constant $fop-SINGLE-FLOAT = 13;
define constant $fop-DOUBLE-FLOAT = 14;
define constant $fop-EXTENDED-FLOAT = 15;

define constant $fop-SHORT-STRING = 16;
define constant $fop-STRING = 17;
define constant $fop-SHORT-SYMBOL = 18;
define constant $fop-SYMBOL = 19;

define constant $fop-NIL = 20;
define constant $fop-LIST1 = 21;
define constant $fop-LIST2 = 22;
define constant $fop-LIST3 = 23;
define constant $fop-LIST4 = 24;
define constant $fop-LIST5 = 25;
define constant $fop-LIST6 = 26;
define constant $fop-LIST7 = 27;
define constant $fop-LIST8 = 28;
define constant $fop-LISTN = 29;

define constant $fop-DOTTED-LIST1 = 30;
define constant $fop-DOTTED-LIST2 = 31;
define constant $fop-DOTTED-LIST3 = 32;
define constant $fop-DOTTED-LIST4 = 33;
define constant $fop-DOTTED-LIST5 = 34;
/* Note: 35 is taken by '#' */
define constant $fop-DOTTED-LIST6 = 36;
define constant $fop-DOTTED-LIST7 = 37;
define constant $fop-DOTTED-LIST8 = 38;
define constant $fop-DOTTED-LISTN = 39;

define constant $fop-VECTOR0 = 40;
define constant $fop-VECTOR1 = 41;
define constant $fop-VECTOR2 = 42;
define constant $fop-VECTOR3 = 43;
define constant $fop-VECTOR4 = 44;
define constant $fop-VECTOR5 = 45;
define constant $fop-VECTOR6 = 46;
define constant $fop-VECTOR7 = 47;
define constant $fop-VECTOR8 = 48;
define constant $fop-VECTORN = 49;

define constant $fop-VALUE-CELL = 50;
define constant $fop-WRITABLE-VALUE-CELL = 51;
define constant $fop-BUILTIN-VALUE-CELL = 52;
define constant $fop-BUILTIN-WRITABLE-VALUE-CELL = 53;
define constant $fop-NOTE-REFERENCE = 54;

define constant $fop-SHORT-COMPONENT = 55;
define constant $fop-COMPONENT = 56;
define constant $fop-SHORT-METHOD = 57;
define constant $fop-METHOD = 58;

define constant $fop-IN-LIBRARY = 60;
define constant $fop-IN-MODULE = 61;
define constant $fop-SOURCE-FILE = 62;
define constant $fop-TOP-LEVEL-FORM = 63;
define constant $fop-DEFINE-CONSTANT $= 64;
define constant $fop-DEFINE-VARIABLE = 65;
define constant $fop-DEFINE-GENERIC = 66;
define constant $fop-DEFINE-METHOD = 67;
define constant $fop-DEFINE-CLASS = 68;
define constant $fop-DEFINE-LIBRARY = 69;
define constant $fop-DEFINE-MODULE = 70;

define constant $fop-VARREF-EXPR = 100;
define constant $fop-LITERAL-EXPR = 101;
define constant $fop-CALL-EXPR = 102;
define constant $fop-DOT-EXPR = 103;
define constant $fop-METHOD-EXPR = 104;
define constant $fop-BODY-EXPR = 105;
define constant $fop-BLOCK-EXPR = 106;
define constant $fop-IF-EXPR = 107;
define constant $fop-VARSET-EXPR = 108;
define constant $fop-LOOP-EXPR = 109;
define constant $fop-REPEAT-EXPR = 110;

define constant $fop-EXPR-CONSTITUENT = 120;
define constant $fop-LOCAL-CONSTITUENT = 121;
define constant $fop-HANDLER-CONSTITUENT = 122;
define constant $fop-LET-CONSTITUENT = 123;

define constant $fop-DONE = 255;
