PROJECT(mindyexec C)

SET(mindyexec_SOURCES
    mindyexec.c
)
ADD_EXECUTABLE(mindyexec
               ${mindyexec_SOURCES})
SET_TARGET_PROPERTIES(mindyexec PROPERTIES VERSION ${MINDY_VERSION})

IF (NOT WIN32)
  INSTALL(TARGETS mindyexec DESTINATION bin)
ENDIF()
