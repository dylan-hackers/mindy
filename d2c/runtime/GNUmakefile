CFLAGS = -g
CPPFLAGS = -I.
CC = gcc
LIBS = c-code/libruntime.a /lib/pa1.1/libm.a

DYLAN_FILES = \
	dylan/bootstrap.dylan \
	dylan/macros.dylan \
	dylan/exports.dylan \
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

C_FILES = $(patsubst %.dylan,%.c,$(DYLAN_FILES)) foo-init.c inits.c
S_FILES = heap.s
O_FILES = $(patsubst %.c,%.o,$(C_FILES)) $(patsubst %.s,%.o,$(S_FILES))

foo: foo.lid ${DYLAN_FILES} ../compiler/foo.dbc ${LIBS}
	time mindy -f ../compiler/foo.dbc foo.lid

clean:
	rm -f ${C_FILES} ${S_FILES} ${O_FILES} libfoo.a foo core
