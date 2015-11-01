SET(CPACK_PACKAGE_DESCRIPTION_SUMMARY "A bytecode compiler and interpreter for Dylan.")

SET(CPACK_PACKAGE_VENDOR "Project Mindy")
SET(CPACK_PACKAGE_CONTACT "Bruce Mitchener <bruce.mitchener@gmail.com>")

SET(CPACK_PACKAGE_VERSION_MAJOR "${MINDY_VERSION_MAJOR}")
SET(CPACK_PACKAGE_VERSION_MINOR "${MINDY_VERSION_MINOR}")
SET(CPACK_PACKAGE_VERSION_PATCH "${MINDY_VERSION_PATCH}")

SET(CPACK_PACKAGE_FILE_NAME "${CMAKE_PROJECT_NAME}_${MINDY_VERSION}-${MINDY_TARGET_PLATFORM}")
SET(CPACK_SOURCE_PACKAGE_FILE_NAME "${CMAKE_PROJECT_NAME}_${MINDY_VERSION}-sources")

SET(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_SOURCE_DIR}/LICENSE.rst")
SET(CPACK_RESOURCE_FILE_README "${CMAKE_SOURCE_DIR}/README.rst")

SET(CPACK_SOURCE_IGNORE_FILES
    "/build.*"
    "/.git"
)

SET(CPACK_DEBIAN_PACKAGE_HOMEPAGE "http://project-mindy.org/")
SET(CPACK_DEBIAN_PACKAGE_PRIORITY "optional")
SET(CPACK_DEBIAN_PACKAGE_SECTION "interpreters")

INCLUDE(CPack)