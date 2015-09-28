PROJECT(mindycomp C)

INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})
INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR})

SET(mindycomp_SOURCES
    compile.c
    dump.c
    dup.c
    envanal.c
    expand.c
    feature.c
    free.c
    header.c
    info.c
    lexenv.c
    literal.c
    lose.c
    mindycomp.c
    print.c
    src.c
    sym.c
    version.c
)

SET(mindycomp_HEADERS
    compile.h
    dump.h
    dup.h
    envanal.h
    expand.h
    feature.h
    free.h
    header.h
    info.h
    lexenv.h
    lexer.h
    literal.h
    lose.h
    mindycomp.h
    parser.h
    print.h
    src.h
    sym.h
    version.h
    ../shared/byteops.h
    ../shared/compiler-support.h
    ../shared/fileops.h
)

FIND_PACKAGE(BISON)
FIND_PACKAGE(FLEX)
FLEX_TARGET(DylanScanner lexer.l ${CMAKE_CURRENT_BINARY_DIR}/lexer-tab.c)
BISON_TARGET(DylanParser parser.y ${CMAKE_CURRENT_BINARY_DIR}/parser-tab.c)
ADD_FLEX_BISON_DEPENDENCY(DylanScanner DylanParser)


ADD_EXECUTABLE(mindycomp
               ${mindycomp_SOURCES}
               ${mindycomp_HEADERS}
               ${BISON_DylanParser_OUTPUTS}
               ${FLEX_DylanScanner_OUTPUTS})
SET_TARGET_PROPERTIES(mindycomp PROPERTIES VERSION ${MINDY_VERSION_STRING})
