### $Header: /home/housel/work/rcs/gd/src/d2c/compiler/Attic/GNUmakefile,v 1.22 1995/11/12 21:08:03 nkramer Exp $
###
### Copyright (c) 1994 Carnegie Mellon University, all rights reserved.
###

MC = mindycomp
MINDYFLAGS = -lcompiler

%.dbc: %.dylan
	$(MC) ${MINDYFLAGS} -o $@ $$PWD/$<

OBJS = exports.dbc \
	set-module.dbc \
	params.dbc \
	utils.dbc \
	od-format.dbc \
	dylan-dump.dbc \
	ctv.dbc \
	source.dbc \
	header.dbc \
	tokens.dbc \
	names.dbc \
	defns.dbc \
	variables.dbc \
	lexer.dbc \
	fragments.dbc \
	parse-tree.dbc \
	tlf.dbc \
	policy.dbc \
	lexenv.dbc \
	ctype.dbc \
	rep.dbc \
	cclass.dbc \
	type-dump.dbc \
	c-rep.dbc \
	transdef.dbc \
	ctfunc.dbc \
	cteval.dbc \
	expand.dbc \
	deflibmod.dbc \
	parser.dbc \
	macros.dbc \
	data-flow.dbc \
	control-flow.dbc \
	signature.dbc \
	primitives.dbc \
	front.dbc \
	clone.dbc \
	cheese.dbc \
	primopt.dbc \
	trans.dbc \
	builder.dbc \
	fer-builder.dbc \
	fer-convert.dbc \
	fer-dump.dbc \
	deffunc.dbc \
	method-tree.dbc \
	defconstvar.dbc \
	defclass.dbc \
	tlexpr.dbc \
	stackanal.dbc \
	cback.dbc \
	primemit.dbc \
	heap.dbc \
	dump.dbc \
	fer-od.dbc \
	misc-dump.dbc \
	init.dbc \
	autodump.dbc \
	parse-dump.dbc \
	token-dump.dbc \
	main.dbc

foo.dbc: ${OBJS}
	cat ${OBJS} > $@

parser.dylan: parser.input
	cmucl -load do-parsergen


CFLAGS = -g
CC = gcc

test: output.o heap.o
	${CC} ${CFLAGS} -o test output.o heap.o

output.c heap.s: bootstrap.dylan ack.dylan foo.dbc
	mindy -f foo.dbc bootstrap.dylan ack.dylan
