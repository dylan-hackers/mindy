# This is a gmake makefile for the string-extensions library.

MC = mindycomp
MINDYFLAGS = -lmelange

%.dbc: %.dylan
	$(MC) ${MINDYFLAGS} -o $@ $<

OBJS = exports.dbc c-lexer.dbc c-lexer-cpp.dbc hp-portability.dbc \
	c-parse.dbc c-decl.dbc c-decl-state.dbc c-decl-write.dbc \
	name-map.dbc int-lexer.dbc int-parse.dbc interface.dbc

melange.dbc: ${OBJS}
	cat ${OBJS} > $@ 

clean:
	rm -f *.dbc *~ "#"*
