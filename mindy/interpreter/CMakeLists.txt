PROJECT(mindy C)

INCLUDE(CheckFunctionExists)

INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})
INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR})

SET(mindy_SOURCES
    bool.c
    brkpt.c
    buf.c
    char.c
    class.c
    coll.c
    debug.c
    def.c
    driver.c
    error.c
    extern.c
    fd.c
    func.c
    gc.c
    handler.c
    init.c
    input.c
    instance.c
    interp.c
    lexer.c
    list.c
    load.c
    lose.c
    mindy.c
    misc.c
    module.c
    nlx.c
    num.c
    obj.c
    print.c
    shl.c
    str.c
    sym.c
    table.c
    thread.c
    type.c
    value.c
    vec.c
    weak.c
    ../../common/time/time-portability.c
    ../shared/byteops.h
    ../shared/compiler-support.h
    ../shared/fileops.h
)

SET(mindy_HEADERS
    bool.h
    brkpt.h
    buf.h
    char.h
    class.h
    coll.h
    debug.h
    def.h
    driver.h
    error.h
    extern.h
    fd.h
    func.h
    gc.h
    handler.h
    init.h
    instance.h
    interp.h
    lexer.h
    list.h
    load.h
    mindy.h
    module.h
    num.h
    obj.h
    parser.h
    print.h
    shl.h
    str.h
    sym.h
    table.h
    thread.h
    type.h
    value.h
    vec.h
    weak.h
    ../shared/compiler-support.h
)

FIND_PACKAGE(BISON)
BISON_TARGET(MindyParser parser.y ${CMAKE_CURRENT_BINARY_DIR}/parser-tab.c)

SET(MINDY_LIBS)

CHECK_FUNCTION_EXISTS(pow HAVE_POW_BY_DEFAULT)
IF(NOT HAVE_POW_BY_DEFAULT)
  LIST(APPEND CMAKE_REQUIRED_LIBRARIES m)
  CHECK_FUNCTION_EXISTS(pow POW_NEEDS_LIBM)
  IF(POW_NEEDS_LIBM)
    LIST(APPEND MINDY_LIBS m)
  ENDIF()
ENDIF()

# Replace with a proper check
LIST(APPEND MINDY_LIBS readline)

ADD_EXECUTABLE(mindy
               ${mindy_SOURCES}
               ${mindy_HEADERS}
               ${BISON_MindyParser_OUTPUTS})
TARGET_LINK_LIBRARIES(mindy ${MINDY_LIBS} ${CMAKE_DL_LIBS})
SET_TARGET_PROPERTIES(mindy PROPERTIES VERSION ${MINDY_VERSION_STRING})
