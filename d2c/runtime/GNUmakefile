CFLAGS = -g
CPPFLAGS = -I.
CC = gcc
LIBS = c-code/libruntime.a /lib/pa1.1/libm.a

DYLAN_FILES = \
	dylan/exports.dylan \
	dylan/macros.dylan \
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
	dylan/misc.dylan \
	dylan/handler.dylan \
	dylan/nlx.dylan \
	dylan/output.dylan \
	dylan/condition.dylan \
	dylan/thread.dylan \
	gobbledygook.dylan

foo: output.o heap.o ${LIBS}
	${CC} ${CFLAGS} -o foo output.o heap.o ${LIBS}

output.o: output.c

heap.o: heap.s

output.c heap.s: ../compiler/foo.dbc ${DYLAN_FILES}
	time mindy -f ../compiler/foo.dbc ${DYLAN_FILES}
