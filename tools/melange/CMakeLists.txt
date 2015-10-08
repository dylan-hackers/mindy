MACRO(GENERATE_PARSERGEN_PARSER input_file output_variable)
    GET_FILENAME_COMPONENT(output_file_base ${input_file} NAME_WE)
    SET(output_file "${CMAKE_CURRENT_BINARY_DIR}/${output_file_base}.dylan")
    SET(parsergen.dbc ${CMAKE_BINARY_DIR}/bin/parsergen.dbc)
    ADD_CUSTOM_COMMAND(
      OUTPUT ${output_file}
      COMMAND mindy -f ${parsergen.dbc} ${CMAKE_CURRENT_SOURCE_DIR}/${input_file} ${output_file}
      WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/${MINDY_LIB_DIR}
      MAIN_DEPENDENCY ${input_file}
      DEPENDS mindy parsergen
    )
    SET(${output_variable} ${output_file})
ENDMACRO()

# This is temporary until we have a better solution.
# We should rename these files to match MINDY_TARGET_PLATFORM
# and then we can use ${MINDY_TARGET_PLATFORM}-portability.dylan
IF(APPLE)
    SET(PORTABILITY macos-portability.dylan)
ENDIF()

IF(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
    SET(PORTABILITY linux-portability.dylan)
ENDIF()
# End temporary

GENERATE_PARSERGEN_PARSER(c-parse.input C_PARSERGEN)
GENERATE_PARSERGEN_PARSER(int-parse.input INT_PARSERGEN)

SET(melange-c_SOURCES
    c-exports.dylan
    source-location.dylan
    parse-conditions.dylan
    multistring.dylan
    c-lexer.dylan
    c-lexer-cpp.dylan
    ${PORTABILITY}
    ${C_PARSERGEN}
    alignment.dylan
    c-decl-state.dylan
    c-decl-write.dylan
    c-decl.dylan
)
ADD_DYLAN_LIBRARY(melange-c)
INSTALL_DYLAN_LIBRARY(melange-c)

SET(melange_SOURCES
    exports.dylan
    ${INT_PARSERGEN}
    interface.dylan
    name-map.dylan
    int-lexer.dylan
)
ADD_DYLAN_TOOL(melange)
INSTALL_DYLAN_TOOL(melange)
