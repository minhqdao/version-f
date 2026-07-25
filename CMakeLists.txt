cmake_minimum_required(VERSION 3.20)

file(STRINGS "${CMAKE_CURRENT_SOURCE_DIR}/fpm.toml" VERSION_F_VERSION_LINE
  REGEX "^version[ \t]*=[ \t]*\"[0-9]+\.[0-9]+\.[0-9]+\"$"
  LIMIT_COUNT 1
)
if(NOT VERSION_F_VERSION_LINE)
  message(FATAL_ERROR "Could not read the project version from fpm.toml")
endif()
string(REGEX REPLACE ".*\"([0-9]+\.[0-9]+\.[0-9]+)\".*" "\\1"
  VERSION_F_VERSION "${VERSION_F_VERSION_LINE}"
)

project(version-f
  VERSION "${VERSION_F_VERSION}"
  DESCRIPTION "Fortran implementation of Semantic Versioning 2.0.0"
  LANGUAGES Fortran
)

if(CMAKE_SOURCE_DIR STREQUAL PROJECT_SOURCE_DIR)
  set(VERSION_F_STANDALONE ON)
else()
  set(VERSION_F_STANDALONE OFF)
endif()

option(VERSION_F_BUILD_TESTS "Build version-f tests" "${VERSION_F_STANDALONE}")
option(VERSION_F_BUILD_EXAMPLES "Build version-f examples" "${VERSION_F_STANDALONE}")
option(VERSION_F_INSTALL "Install version-f and its CMake package" "${VERSION_F_STANDALONE}")

set(VERSION_F_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/modules")

add_library(version-f src/version_f.f90)
add_library(version-f::version-f ALIAS version-f)

set_target_properties(version-f PROPERTIES
  Fortran_MODULE_DIRECTORY "${VERSION_F_MODULE_DIRECTORY}"
  Fortran_STANDARD 2008
  Fortran_STANDARD_REQUIRED YES
  Fortran_EXTENSIONS NO
  VERSION "${PROJECT_VERSION}"
  SOVERSION "${PROJECT_VERSION_MAJOR}"
)

target_include_directories(version-f
  PUBLIC "$<BUILD_INTERFACE:${VERSION_F_MODULE_DIRECTORY}>"
)

if(VERSION_F_BUILD_TESTS)
  include(CTest)
  add_executable(version-f-test test/version_f_test.f90)
  target_link_libraries(version-f-test PRIVATE version-f::version-f)
  add_test(NAME version-f-test COMMAND version-f-test)
endif()

if(VERSION_F_BUILD_EXAMPLES)
  foreach(VERSION_F_EXAMPLE IN ITEMS version_f_ex1 version_f_ex2)
    add_executable("${VERSION_F_EXAMPLE}" "example/${VERSION_F_EXAMPLE}.f90")
    target_link_libraries("${VERSION_F_EXAMPLE}" PRIVATE version-f::version-f)
  endforeach()
endif()

if(VERSION_F_INSTALL)
  include(GNUInstallDirs)
  include(CMakePackageConfigHelpers)

  target_include_directories(version-f
    PUBLIC "$<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/version-f>"
  )

  install(TARGETS version-f
    EXPORT version-f-targets
    ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}"
  )
  install(DIRECTORY "${VERSION_F_MODULE_DIRECTORY}/"
    DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/version-f"
    FILES_MATCHING PATTERN "*.mod"
  )

  set(VERSION_F_CMAKE_INSTALL_DIR "${CMAKE_INSTALL_LIBDIR}/cmake/version-f")

  configure_package_config_file(
    cmake/version-f-config.cmake.in
    "${CMAKE_CURRENT_BINARY_DIR}/version-f-config.cmake"
    INSTALL_DESTINATION "${VERSION_F_CMAKE_INSTALL_DIR}"
  )
  write_basic_package_version_file(
    "${CMAKE_CURRENT_BINARY_DIR}/version-f-config-version.cmake"
    VERSION "${PROJECT_VERSION}"
    COMPATIBILITY SameMinorVersion
  )

  install(EXPORT version-f-targets
    FILE version-f-targets.cmake
    NAMESPACE version-f::
    DESTINATION "${VERSION_F_CMAKE_INSTALL_DIR}"
  )
  install(FILES
    "${CMAKE_CURRENT_BINARY_DIR}/version-f-config.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/version-f-config-version.cmake"
    DESTINATION "${VERSION_F_CMAKE_INSTALL_DIR}"
  )
endif()
