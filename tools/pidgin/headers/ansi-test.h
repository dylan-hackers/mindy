//=========================================================================
//  ANSI C(*) Test Header
//=========================================================================
// This header contains all the strange constructs we want our parser to
// support.
//
// (*) with GCC and MSVC extensions.

#define TEST_BASIC_TYPES
#define TEST_DECLARATORS
#define TEST_TYPEDEFS
#define TEST_ARRAYS
#define TEST_FUNCTIONS


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
int function_varargs_torture_1(int (...));     // abstract-declarator2 (#1)
int function_varargs_torture_2(int (*)(...));  // abstract-declarator2 (#2)
int function_varargs_torture_3(int a(...));    // declarator2
int function_varargs_torture_4(int a(int b, ...)); // parameter-type-list 

// This works, but may not be ANSI C.
//int function_varargs_torture_5(a, b, c);       // parameter-identifier-list

#endif // TEST_FUNCTIONS
