//=========================================================================
//  ANSI C(*) Test Header
//=========================================================================
// This header contains all the strange constructs we want our parser to
// support.
//
// (*) with GCC and MSVC extensions.

#if !defined SPECIFIC_TESTS

#define TEST_BASIC_TYPES
#define TEST_DECLARATORS
#define TEST_TYPEDEFS
#define TEST_ARRAYS
#define TEST_FUNCTIONS
#define TEST_ENUMS
#define TEST_STRUCTS_AND_UNIONS
#define TEST_MACROS

#endif // !defined SPECIFIC_TESTS


//=========================================================================
//  Basic Types
//=========================================================================

#if defined TEST_BASIC_TYPES

// Canonical integer types
char               basic_char;
signed char        basic_signed_char;
unsigned char      basic_unsigned_char;
short              basic_short;
signed short       basic_signed_short;
unsigned short     basic_unsigned_short;
int                basic_int;
signed int         basic_signed_int;
unsigned int       basic_unsigned_int;
long               basic_long;
signed long        basic_signed_long;
unsigned long      basic_unsigned_long;
long long          basic_long_long;
signed long long   basic_signed_long_long;
unsigned long long basic_unsigned_long_long;

// Canonical float types
float              basic_float;
double             basic_double;
long double        basic_long_double;

// Funky (but legal) combinations of specifiers
signed             basic_signed;
unsigned           basic_unsigned;
short int          basic_short_int;
long int           basic_long_int;
long float         basic_long_float;

#endif // TEST_BASIC_TYPES


//=========================================================================
//  Declarators
//=========================================================================

#if defined TEST_DECLARATORS

extern char d1;
extern char * d2;
extern char *** d3;
extern signed long long d4;
extern unsigned int d5, *d6, **d7, ***d8, *****d9;

#endif // TEST_DECLARATORS


//=========================================================================
//  Typedefs
//=========================================================================

#if defined TEST_TYPEDEFS

// Defining typedefs
typedef int typedefs_1;
typedef typedefs_1 *typedefs_2;
typedef int **typedefs_3;

// Using typedefs
typedefs_1 typdefs_1_use;
typedefs_2 typedefs_2_use, *typedefs_2_use_p, **typedefs_2_use_pp;

// Tricky typedef parsing issues--make sure we declare new types before
// we tokenize any part of the next declaration.
typedef int typedef_tokenization;
typedef_tokenization a;

#endif // TEST_TYPEDEFS

//=========================================================================
//  Arrays
//=========================================================================

#if defined TEST_ARRAYS

int array_1[];
int array_2[10];

#endif // TEST_ARRAYS


//=========================================================================
//  Functions
//=========================================================================

#if defined TEST_FUNCTIONS

// Abstract declarators vs. parameters
int function_decls_1(int);
int function_decls_2(int a);
int function_decls_3(int *(*)());
int function_decls_4(int *(*a)());
int function_decls_5(int *(*)[]);
int function_decls_6(int *(*a)[]);
int function_decls_7(int *(*)[10]);
int function_decls_8(int *(*a)[10]);

// Empty argument lists (K&R and ANSI)
int function_empty_1();
int function_empty_2(...);
int function_empty_3(void);

// Multiple arguments
int function_multiple_1(int, int);
int function_multiple_2(int a, int b, int c);
int function_multiple_3(int a, int, int c);

// Varargs
int function_varargs_1(int, int, ...);
int function_varargs_2(int a, int b, ...);

// Fancy return values
int *function_return_1();
int **function_return_2();
int **function_return_3()[];
int *(*function_return_4())[];
int **function_return_5()(int, int);
int *(*function_return_6())(int, int);

// Varargs torture test
// We exercise all grammar rules creating varargs.
int function_varargs_torture_1(int (...));      // abstract-declarator2 (#1)
int function_varargs_torture_2(int (*)(...));   // abstract-declarator2 (#2)
int function_varargs_torture_3(int (*a)(...));  // declarator2
int function_varargs_torture_4(int (*a)(int b, ...)); // parameter-type-list 

// This works, but may not be ANSI C.
//int function_varargs_torture_5(a, b, c);       // parameter-identifier-list

#endif // TEST_FUNCTIONS


//=========================================================================
//  Enums
//=========================================================================

#if defined TEST_ENUMS

// A simple, anonymous enumeration.
enum {
    enum_anonymous_0,
    enum_anonymous_1
};

// A simple, named enumeration.
enum enum_named {
    enum_named_0,
    enum_named_1,
};

// An enumeration with simple assignments
enum enum_assignments {
    enum_assignments_0,
    enum_assignments_1,
    enum_assignments_10 = 10,
    enum_assignments_11
};

// An enumeration with expressions
enum enum_math {
    enum_math_6 = 1 + 5,
    enum_math_7
};

// A comma-terminated enum
enum enum_extra_comma {
    enum_extra_comma_value_0,
};

#endif // TEST_ENUMS


//=========================================================================
//  Structs & Unions
//=========================================================================

#if defined TEST_STRUCTS_AND_UNIONS

struct struct_simple {
    int a;
    int b, *c;
};

struct struct_self_referential {
    struct struct_self_referential *next;
};

typedef int struct_bitfields_int;
struct struct_bitfields {
    int a : 1;
    signed int b : 1;
    unsigned int c : 1;
    int : 1;
    struct_bitfields_int : 1;
};

struct struct_mixed {
    int a, *b, c : 1, :1;
    struct struct_mixed *next;
};

struct /* An anonymous struct */ {
    int a;
};

struct struct_incomplete;

struct struct_circular_1 {
    struct struct_circular_2* next;
};
struct struct_circular_2 {
    struct struct_circular_1* next;
};

typedef int union_mixed_int;
union union_mixed {
    int a, *b;
    union union_mixed *next;
};

#endif // TEST_STRUCTS_AND_UNIONS


//=========================================================================
//  Macros
//=========================================================================

#if defined TEST_MACROS

// Supported macro types.
#define MACRO_EMPTY
#define MACRO_UNKNOWN    { int a; }
#define MACRO_INTEGER_1  1
#define MACRO_INTEGER_2  (1 + 1)
#define MACRO_INTEGER_3  (1 + 1) * 2 - 1
#define MACRO_INTEGER_4  MACRO_INTEGER_1 * 4
#define MACRO_INTEGER_5  MACRO_INTEGER_3 * -1
#define MACRO_STRING_1   "Hi!"
#define MACRO_STRING_2   (("Hello" ", " "world!"))

// Currently unsupported macro types.
typedef int macro_typedef_type;
#define MACRO_TYPE_ALIAS_1 int
#define MACRO_TYPE_ALIAS_2 macro_typedef_type

#endif // TEST_MACROS
