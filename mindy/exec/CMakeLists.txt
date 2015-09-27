PROJECT(mindyexec C)

SET(mindyexec_SOURCES
    mindyexec.c
)
ADD_EXECUTABLE(mindyexec
               ${mindyexec_SOURCES})
SET_TARGET_PROPERTIES(mindyexec PROPERTIES VERSION ${MINDY_VERSION_STRING})
