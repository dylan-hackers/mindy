# For config.h
INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})

ADD_DEFINITIONS(-DBINDIR="/opt/mindy/bin")
ADD_DEFINITIONS(-DLIBDIR="/opt/mindy/lib/dylan")
ADD_DEFINITIONS(-DTARGET="x86_64-darwin-gcc")

# clang doesn't print colored diagnostics when invoked from Ninja
IF (UNIX AND
    CMAKE_C_COMPILER_ID STREQUAL "Clang" AND
    CMAKE_GENERATOR STREQUAL "Ninja")
  SET(CMAKE_C_FLAGS "-fcolor-diagnostics ${CMAKE_C_FLAGS}")
ENDIF()

SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/bin")

ADD_SUBDIRECTORY(compiler)
ADD_SUBDIRECTORY(exec)
ADD_SUBDIRECTORY(interpreter)
