INCLUDE(AddCCompilerFlag)
INCLUDE(CheckFunctionExists)
INCLUDE(CheckStructHasMember)
INCLUDE(CheckSymbolExists)
INCLUDE(CheckTypeSize)

OPTION(MINDY_SLOW_FUNCTION_POINTERS "(Experimental) Assume function pointers are slow." OFF)
OPTION(MINDY_SLOW_LONGJMP "(Experimental) Assume longjmp is slow." OFF)

CHECK_FUNCTION_EXISTS(_setjmp HAVE__SETJMP)
CHECK_FUNCTION_EXISTS(_longjmp HAVE__LONGJMP)

# Linux provides this.
CHECK_FUNCTION_EXISTS(sincos HAVE_SINCOS)
# Mac OS X provides this.
CHECK_FUNCTION_EXISTS(__sincos HAVE___SINCOS)

CHECK_STRUCT_HAS_MEMBER("struct tm" tm_gmtoff time.h HAVE_TM_GMTOFF)

CHECK_SYMBOL_EXISTS(daylight time.h HAVE_DAYLIGHT)

CHECK_TYPE_SIZE("void*" SIZEOF_VOID_P)

# Even though this is for the interpreter, we do this here
# so that the results can make it into the config.h file.
SET(MINDY_LIBS)

IF(NOT READLINE_INCLUDE_DIR OR NOT READLINE_LIBRARY)
  MESSAGE("-- Looking for readline/readline.h")
  FIND_PATH(READLINE_INCLUDE_DIR readline/readline.h)
  IF(READLINE_INCLUDE_DIR)
    MESSAGE("-- Looking for readline/readline.h - Found (${READLINE_INCLUDE_DIR})")
  ELSE()
    MESSAGE("-- Looking for readline/readline.h - Not found")
  ENDIF()
  MESSAGE("-- Looking for libreadline")
  FIND_LIBRARY(READLINE_LIBRARY NAMES readline)
  IF(READLINE_LIBRARY)
    MESSAGE("-- Looking for libreadline - Found (${READLINE_LIBRARY})")
  ELSE()
    MESSAGE("-- Looking for libreadline - Not found")
  ENDIF()
ENDIF()

IF(READLINE_INCLUDE_DIR AND READLINE_LIBRARY)
  SET(HAVE_LIBREADLINE TRUE)
  SET(HAVE_READLINE_READLINE_H TRUE)
  LIST(APPEND MINDY_LIBS readline)
ENDIF()

ADD_DEFINITIONS(-DLIBDIR="${CMAKE_INSTALL_PREFIX}/${MINDY_LIB_DIR}")

ADD_C_COMPILER_FLAG(-Wall)

# clang doesn't print colored diagnostics when invoked from Ninja
IF (UNIX AND
    CMAKE_C_COMPILER_ID STREQUAL "Clang" AND
    CMAKE_GENERATOR STREQUAL "Ninja")
  SET(CMAKE_C_FLAGS "-fcolor-diagnostics ${CMAKE_C_FLAGS}")
ENDIF()

SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/bin")

CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/config.h.cmake
               ${CMAKE_CURRENT_BINARY_DIR}/config.h)
INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR})

# For shared/*
INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})

ADD_SUBDIRECTORY(compiler)
ADD_SUBDIRECTORY(dbclink)
IF (NOT WIN32)
  ADD_SUBDIRECTORY(exec)
ENDIF()
ADD_SUBDIRECTORY(interpreter)
