Module: component
Synopsis: Dylan-based Mindy Compiler
Author:  Peter S. Housel

define class <component> (<object>)
  constant slot component-debug-name :: <symbol>,
    required-init-keyword: debug-name:;
  constant slot component-constants :: <stretchy-vector>
    = make(<stretchy-vector>);
  constant slot component-blocks :: <stretchy-vector>
    = make(<stretchy-vector>);
end;

define constant $op-TRAP = 0;
define constant $op-BREAKPOINT = 1;
define constant $op-RETURN-SINGLE = 2;
define constant $op-MAKE-VALUE-CELL = 3;
define constant $op-VALUE-CELL-REF = 4;
define constant $op-VALUE-CELL-SET = 5;
define constant $op-MAKE-METHOD = 6;
define constant $op-CHECK-TYPE = 7;
define constant $op-CHECK-TYPE-FUNCTION = 8;
define constant $op-CANONICALIZE-VALUE = 9;
define constant $op-PUSH-BYTE = 10;
define constant $op-PUSH-INT = 11;
define constant $op-CONDITIONAL-BRANCH = 12;
define constant $op-BRANCH = 13;
define constant $op-PUSH-NIL = 14;
define constant $op-PUSH-UNBOUND = 15;
define constant $op-PUSH-TRUE = 16;
define constant $op-PUSH-FALSE = 17;
define constant $op-DUP = 18;
define constant $op-DOT-TAIL = 19;
define constant $op-DOT-FOR-MANY = 20;
define constant $op-DOT-FOR-SINGLE = 21;

define constant $op-PUSH-CONSTANT = 0x20;
define constant $op-PUSH-ARG = 0x30;
define constant $op-POP-ARG = 0x40;
define constant $op-PUSH-LOCAL = 0x50;
define constant $op-POP-LOCAL = 0x60;
define constant $op-CALL-TAIL = 0x70;
define constant $op-CALL-FOR-MANY = 0x80;
define constant $op-CALL-FOR-SINGLE = 0x90;
define constant $op-PUSH-VALUE = 0xa0;
define constant $op-PUSH-FUNCTION = 0xb0;
define constant $op-POP-VALUE = 0xc0;

define constant $op-PLUS = 0xf0;
define constant $op-MINUS = 0xf1;
define constant $op-LT = 0xf2;
define constant $op-LE = 0xf3;
define constant $op-EQ = 0xf4;
define constant $op-IDP = 0xf5;
define constant $op-NE = 0xf6;
define constant $op-GE = 0xf7;
define constant $op-GT = 0xf8;

