SET(MINDY_FLAGS)
# mindycomp doesn't print colored diagnostics when invoked from Ninja
IF (UNIX AND CMAKE_GENERATOR STREQUAL "Ninja")
  SET(MINDY_FLAGS "-fcolor-diagnostics")
ENDIF()

MACRO(ADD_DYLAN_PROJECT project_name target_directory suffix)
  SET(DYLAN_${project_name}_OUTPUT "${target_directory}/${project_name}${suffix}.dbc")
  SET(DYLAN_${project_name}_DBC_OUTPUTS)
  FOREACH(_file ${${project_name}_SOURCES})
    GET_FILENAME_COMPONENT(_file_base ${_file} NAME_WE)
    SET(file_dbc "${_file_base}.dbc")
    ADD_CUSTOM_COMMAND(
      OUTPUT ${file_dbc}
      COMMAND mindycomp -l${project_name} ${MINDY_FLAGS} ${CMAKE_CURRENT_SOURCE_DIR}/${_file} -o ${file_dbc}
      MAIN_DEPENDENCY ${_file}
      DEPENDS mindycomp
    )
    LIST(APPEND DYLAN_${project_name}_DBC_OUTPUTS ${file_dbc})
  ENDFOREACH()
  ADD_CUSTOM_COMMAND(
    OUTPUT ${DYLAN_${project_name}_OUTPUT}
    COMMAND mkdir -p ${target_directory}
    COMMAND rm ARGS -f "${DYLAN_${project_name}_OUTPUT}.tmp"
    COMMAND cat ${DYLAN_${project_name}_DBC_OUTPUTS} >> "${DYLAN_${project_name}_OUTPUT}.tmp"
    COMMAND mv "${DYLAN_${project_name}_OUTPUT}.tmp" ${DYLAN_${project_name}_OUTPUT}
    DEPENDS ${DYLAN_${project_name}_DBC_OUTPUTS}
  )
  ADD_CUSTOM_TARGET(${project_name}
                    ALL
                    DEPENDS ${DYLAN_${project_name}_OUTPUT})
ENDMACRO()

MACRO(ADD_DYLAN_LIBRARY library_name)
  ADD_DYLAN_PROJECT(${library_name} "${CMAKE_BINARY_DIR}/${MINDY_LIB_DIR}" "-lib")
ENDMACRO()

MACRO(INSTALL_DYLAN_LIBRARY library_name)
  INSTALL(FILES ${DYLAN_${library_name}_OUTPUT} DESTINATION ${CMAKE_INSTALL_PREFIX}/${MINDY_LIB_DIR})
ENDMACRO()

MACRO(ADD_DYLAN_TEST test_name)
  ADD_DYLAN_PROJECT(${test_name} "${CMAKE_BINARY_DIR}/test-bin/" "")
  ADD_TEST(NAME "test-${test_name}"
           WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/${MINDY_LIB_DIR}"
           COMMAND mindy -f ${DYLAN_${test_name}_OUTPUT})
ENDMACRO()

MACRO(ADD_DYLAN_DEMO demo_name)
  ADD_DYLAN_PROJECT(${demo_name} "${CMAKE_BINARY_DIR}/demo-bin/" "")
ENDMACRO()

MACRO(ADD_DYLAN_TOOL tool_name)
  ADD_DYLAN_PROJECT(${tool_name} "${CMAKE_BINARY_DIR}/bin/" "")
ENDMACRO()
