typedef int boolean;
typedef char bool;
#define TRUE 1
#define FALSE 0

#ifdef __APPLE__
   #define __STDBOOL_H__   1
   #define true TRUE
   #define false FALSE
#endif

#if __GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ > 4)
#define GD_NORETURN __attribute__((__noreturn__))
#elif _MSC_VER >= 1200
#define GD_NORETURN __declspec(noreturn)
#else
#define GD_NORETURN
#endif

typedef struct heapobj *heapptr_t;
typedef struct descriptor {
    heapptr_t heapptr;
    union {
        long l;
        float f;
#ifdef GD_DATAWORD_D
	double d;
#endif
#ifdef GD_DATAWORD_X
	long double x;
#endif
        void *ptr;
    } dataword;
} descriptor_t;

#if __GNUC__ > 2
/* utter hack to make (at least) GCC 3.2.2 and 3.3 happy when offset is foo.dataword.l */
#define DEREF(type, ptr, offset) (*(type *)({long t=offset;(char *)ptr + t;}))
#else
#define DEREF(type, ptr, offset) (*(type *)((char *)ptr + offset))
#endif

#define SLOT(ptr, type, offset) DEREF(type, ptr, offset)

typedef descriptor_t *(*entry_t)();

extern heapptr_t allocate(int bytes);
extern descriptor_t *allocate_stack(void);
extern void destroy(void* ptr);
extern heapptr_t make_trampoline(void *func, descriptor_t closure,
				 int nkeys, char *signature);
extern descriptor_t *catch(descriptor_t *(*fn)(descriptor_t *sp, void *state,
                                               heapptr_t thunk),
                           descriptor_t *sp, heapptr_t func);
extern void throw(void *state, descriptor_t *stack_top);

extern descriptor_t *pad_cluster(descriptor_t *start, descriptor_t *end,
				 int min_values);
extern descriptor_t *values_sequence(descriptor_t *sp, heapptr_t vector);

extern heapptr_t make_double_float(double value);
extern double double_float_value(heapptr_t df);

extern heapptr_t make_extended_float(long double value);
extern long double extended_float_value(heapptr_t xf);

#ifdef GD_HAVE_LONG_LONG
extern heapptr_t make_double_integer(long long value);
extern long long double_integer_value(heapptr_t xf);
#else
typedef struct { long hi; long lo; } gd_long_long;
extern heapptr_t make_double_integer(gd_long_long value);
extern gd_long_long double_integer_value(heapptr_t xf);
extern int double_integer_eq(gd_long_long a, gd_long_long b);
extern int double_integer_lt(gd_long_long a, gd_long_long b);
extern gd_long_long double_integer_add(gd_long_long a, gd_long_long b);
extern gd_long_long double_integer_mul(gd_long_long a, gd_long_long b);
extern gd_long_long double_integer_sub(gd_long_long a, gd_long_long b);
extern gd_long_long double_integer_neg(gd_long_long a);
extern gd_long_long double_integer_div(gd_long_long a, gd_long_long b);
extern gd_long_long double_integer_mod(gd_long_long a, gd_long_long b);
extern gd_long_long double_integer_logior(gd_long_long a, gd_long_long b);
extern gd_long_long double_integer_logxor(gd_long_long a, gd_long_long b);
extern gd_long_long double_integer_logand(gd_long_long a, gd_long_long b);
extern gd_long_long double_integer_lognot(gd_long_long a);
extern gd_long_long double_integer_shl(gd_long_long a, long b);
extern gd_long_long double_integer_shr(gd_long_long a, long b);
extern gd_long_long integer_to_double_integer(long b);
extern long double_integer_to_integer(gd_long_long b);
#endif

extern heapptr_t initial_symbols;

extern GD_NORETURN void not_reached(void);
extern void no_core_dumps(void);

float (frexpf)(float x, int *exp);
long double (frexpl)(long double x, int *exp);
float (ldexpf)(float x, int exp);
long double (ldexpl)(long double x, int exp);

long double (logl)(long double x);
long double (log2l)(long double x);
long double (log10l)(long double x);

double (rint)(double x);
double (log2)(double x);

float (fabsf)(float x);
float (sinf)(float x);
float (cosf)(float x);
float (tanf)(float x);
float (asinf)(float x);
float (acosf)(float x);
float (atanf)(float x);
float (atan2f)(float y, float x);
float (expf)(float x);
float (sqrtf)(float x);
float (logf)(float x);
float (log10f)(float x);
float (log10f)(float x);
float (powf)(float b, float x);
float (sinhf)(float x);
float (coshf)(float x);
float (tanhf)(float x);

#include <errno.h>
