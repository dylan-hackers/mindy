### $Header: /home/housel/work/rcs/gd/src/d2c/compiler/Attic/GNUmakefile,v 1.24 1995/11/13 18:03:46 wlott Exp $
###
### Copyright (c) 1994 Carnegie Mellon University, all rights reserved.
###

MC = mindycomp
MINDYFLAGS = -lcompiler
MINDY = mindy

%.dbc: %.dylan
	$(MC) ${MINDYFLAGS} -o $@ $$PWD/$<

# Autodump divides object files into two types: Normal ones, and 
# ones derived from autodumped Dylan code.
#
NORMAL_OBJS = exports.dbc \
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
	main.dbc

AUTODUMPED_OBJS = parse-dump.dbc token-dump.dbc

OBJS = ${NORMAL_OBJS} ${AUTODUMPED_OBJS}

foo.dbc: ${OBJS}
	cat ${OBJS} > $@

parser.dylan: parser.input
	cmucl -load do-parsergen

parse-dump.dylan: parse-tree.dylan autodump.dylan
	gmake autodumper.dbc
	${MINDY} -f autodumper.dbc -autodump parse-tree

token-dump.dylan: tokens.dylan autodump.dylan
	gmake autodumper.dbc
	${MINDY} -f autodumper.dbc -autodump tokens

autodumper.dbc: ${NORMAL_OBJS}
	cat ${NORMAL_OBJS} > autodumper.dbc
