# This is a gmake makefile for the string-extensions library.

MC = mindycomp
MINDYFLAGS = -lmelange

%.dbc: %.dylan
	$(MC) ${MINDYFLAGS} -o $@ $<

OBJS = exports.dbc c-lexer.dbc c-lexer-cpp.dbc c-parse.dbc c-decl.dbc c-decl-state.dbc c-decl-write.dbc hp-portability.dbc name-map.dbc int-lexer.dbc int-parse.dbc interface.dbc

melange.dbc: ${OBJS}
	cat ${OBJS} > $@ 

