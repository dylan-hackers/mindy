
CC = gcc
CPPFLAGS = -I..
CFLAGS = -g

OBJS = allocate.o nlx.o hppa.o main.o

libruntime.a: ${OBJS}
	ar crvu libruntime.a ${OBJS}
