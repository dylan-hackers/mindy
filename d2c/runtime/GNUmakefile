CFLAGS = -g
CPPFLAGS = -I.
CC = gcc
LIBS = c-code/libruntime.a /lib/pa1.1/libm.a

DYLAN_FILES = \
	dylan/exports.dylan \
	dylan/macros.dylan \
	dylan/misc.dylan \
	dylan/object.dylan \
	dylan/boolean.dylan \
	dylan/cmp.dylan \
	dylan/type.dylan \
	dylan/class.dylan \
	dylan/copy.dylan \
	dylan/func.dylan \
	dylan/num.dylan \
	dylan/bignum.dylan \
	dylan/ratio.dylan \
	dylan/rawptr.dylan \
	dylan/symbol.dylan \
	dylan/char.dylan \
	dylan/value-cell.dylan \
	dylan/collection.dylan \
	dylan/array.dylan \
	dylan/vector.dylan \
	dylan/string.dylan \
	dylan/list.dylan \
	dylan/handler.dylan \
	dylan/nlx.dylan \
	dylan/output.dylan \
	dylan/condition.dylan \
	dylan/thread.dylan \
	gobbledygook.dylan

C_FILES = $(patsubst %.dylan,%.c,$(DYLAN_FILES)) unit-init.c

O_FILES = $(patsubst %.c,%.o,$(C_FILES)) inits.o heap.o

foo: ${O_FILES} ${LIBS}
	${CC} ${CFLAGS} -o foo ${O_FILES} ${LIBS}

inits.c: gen-inits
	gen-inits unit

${C_FILES} heap.s: ../compiler/foo.dbc ${DYLAN_FILES}
	time mindy -f ../compiler/foo.dbc ${DYLAN_FILES}

clean:
	rm -f ${C_FILES} inits.c heap.s ${O_FILES} foo core
