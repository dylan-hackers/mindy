typedef int boolean;
typedef char bool;
#define TRUE 1
#define FALSE 0

typedef struct heapobj *heapptr_t;
typedef struct descriptor {
    heapptr_t heapptr;
    union {
        long l;
        float f;
        void *ptr;
    } dataword;
} descriptor_t;

#define SLOT(ptr, type, offset) (*(type *)((char *)ptr + offset))

typedef descriptor_t *(*entry_t)();

extern heapptr_t allocate(int bytes);
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

extern heapptr_t initial_symbols;

extern void not_reached(void);

#ifdef WIN32
   double rint(double x);
#endif
