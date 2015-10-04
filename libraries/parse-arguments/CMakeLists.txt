SET(parse-arguments_SOURCES
    library.dylan
    parse-arguments.dylan
    parsers.dylan
    defargparser.dylan
)
ADD_DYLAN_LIBRARY(parse-arguments)
INSTALL_DYLAN_LIBRARY(parse-arguments)
