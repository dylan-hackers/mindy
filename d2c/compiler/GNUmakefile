### $Header: /home/housel/work/rcs/gd/src/d2c/compiler/Attic/GNUmakefile,v 1.9 1995/04/21 02:51:43 wlott Exp $
###
### Copyright (c) 1994 Carnegie Mellon University, all rights reserved.
###

MC = mindycomp
MINDYFLAGS = -lcompiler

%.dbc: %.dylan
	$(MC) ${MINDYFLAGS} -o $@ $<

OBJS = exports.dbc \
	set-module.dbc \
	params.dbc \
	utils.dbc \
	source.dbc \
	ctv.dbc \
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
	c-rep.dbc \
	cteval.dbc \
	expand.dbc \
	deflibmod.dbc \
	parser.dbc \
	macros.dbc \
	data-flow.dbc \
	control-flow.dbc \
	signature.dbc \
	front.dbc \
	cheese.dbc \
	builder.dbc \
	fer-builder.dbc \
	fer-convert.dbc \
	fer-dump.dbc \
	deffunc.dbc \
	method-tree.dbc \
	defconstvar.dbc \
	defclass.dbc \
	tlexpr.dbc \
	cback.dbc \
	dump.dbc \
	init.dbc \
	main.dbc

foo.dbc: ${OBJS}
	cat ${OBJS} > $@

parser.dylan: parser.input
	cmucl -load do-parsergen

