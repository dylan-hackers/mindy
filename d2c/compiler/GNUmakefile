### $Header: /home/housel/work/rcs/gd/src/d2c/compiler/Attic/GNUmakefile,v 1.4 1995/01/06 21:24:24 ram Exp $
###
### Copyright (c) 1994 Carnegie Mellon University, all rights reserved.
###

MC = /afs/cs/project/gwydion/mindy/bin/mindycomp
MINDYFLAGS = -lcompiler

%.dbc: %.dylan
	$(MC) ${MINDYFLAGS} -o $@ $<

OBJS = exports.dbc \
	set-module.dbc \
	utils.dbc \
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
	ctv.dbc \
	ctype.dbc \
	cteval.dbc \
	expand.dbc \
	deflibmod.dbc \
	parser.dbc \
	macros.dbc \
	control-flow.dbc \
	data-flow.dbc \
	signature.dbc \
	front.dbc \
	builder.dbc \
	fer-builder.dbc \
	fer-convert.dbc \
	fer-dump.dbc \
	deffunc.dbc \
	method-tree.dbc \
	defconstvar.dbc \
	defclass.dbc \
	tlexpr.dbc \
	dump.dbc \
	init.dbc \
	main.dbc

foo.dbc: ${OBJS}
	cat ${OBJS} > $@

parser.dylan: parser.input
	cmucl -load do-parsergen

