MACRO(ADD_DYLAN_LIBRARY library_name)
  SET(lib_directory "${CMAKE_BINARY_DIR}/lib/")
  SET(DYLAN_${library_name}_OUTPUT "${lib_directory}/${library_name}-lib.dbc")
  SET(DYLAN_${library_name}_DBC_OUTPUTS)
  FOREACH(_file ${${library_name}_SOURCES})
    GET_FILENAME_COMPONENT(_file_base ${_file} NAME_WE)
    SET(file_dbc "${_file_base}.dbc")
    ADD_CUSTOM_COMMAND(
      OUTPUT ${file_dbc}
      COMMAND mindycomp -l${library_name} ${CMAKE_CURRENT_SOURCE_DIR}/${_file} -o ${file_dbc}
      MAIN_DEPENDENCY ${_file}
      DEPENDS mindycomp
    )
    LIST(APPEND DYLAN_${library_name}_DBC_OUTPUTS ${file_dbc})
  ENDFOREACH()
  ADD_CUSTOM_COMMAND(
    OUTPUT ${DYLAN_${library_name}_OUTPUT}
    COMMAND cat ${DYLAN_${library_name}_DBC_OUTPUTS} >> ${DYLAN_${library_name}_OUTPUT}
    DEPENDS ${DYLAN_${library_name}_DBC_OUTPUTS}
  )
  ADD_CUSTOM_TARGET(${library_name}
                    ALL
                    DEPENDS ${DYLAN_${library_name}_OUTPUT})
ENDMACRO()

ADD_SUBDIRECTORY(dylan)
ADD_SUBDIRECTORY(inspector)
ADD_SUBDIRECTORY(random)
ADD_SUBDIRECTORY(tk)
