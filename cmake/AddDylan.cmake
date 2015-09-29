MACRO(ADD_DYLAN_PROJECT project_name target_directory)
  SET(DYLAN_${project_name}_OUTPUT "${target_directory}/${project_name}-lib.dbc")
  SET(DYLAN_${project_name}_DBC_OUTPUTS)
  FOREACH(_file ${${project_name}_SOURCES})
    GET_FILENAME_COMPONENT(_file_base ${_file} NAME_WE)
    SET(file_dbc "${_file_base}.dbc")
    ADD_CUSTOM_COMMAND(
      OUTPUT ${file_dbc}
      COMMAND mindycomp -l${project_name} ${CMAKE_CURRENT_SOURCE_DIR}/${_file} -o ${file_dbc}
      MAIN_DEPENDENCY ${_file}
      DEPENDS mindycomp
    )
    LIST(APPEND DYLAN_${project_name}_DBC_OUTPUTS ${file_dbc})
  ENDFOREACH()
  ADD_CUSTOM_COMMAND(
    OUTPUT ${DYLAN_${project_name}_OUTPUT}
    COMMAND cat ${DYLAN_${project_name}_DBC_OUTPUTS} >> ${DYLAN_${project_name}_OUTPUT}
    DEPENDS ${DYLAN_${project_name}_DBC_OUTPUTS}
  )
  ADD_CUSTOM_TARGET(${project_name}
                    ALL
                    DEPENDS ${DYLAN_${project_name}_OUTPUT})
ENDMACRO()

MACRO(ADD_DYLAN_LIBRARY library_name)
  ADD_DYLAN_PROJECT(${library_name} "${CMAKE_BINARY_DIR}/lib/")
ENDMACRO()
