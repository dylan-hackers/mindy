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

#define SLOT(ptr, type, offset) (*(type *)((char *)ptr + offset))

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

#ifdef WIN32
   double rint(double x);
#endif

#include <errno.h>
