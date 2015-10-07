SET(parsergen_SOURCES
    library.dylan
    lisp-read.dylan
    parsergen.dylan
)

ADD_DYLAN_TOOL(parsergen)
INSTALL_DYLAN_TOOL(parsergen)
