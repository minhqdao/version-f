cmake_minimum_required(VERSION 3.20)
project(version-f-find-package-test LANGUAGES Fortran)

file(STRINGS "${CMAKE_CURRENT_LIST_DIR}/../../../fpm.toml"
  VERSION_F_VERSION_LINE
  REGEX "^version[ \t]*=[ \t]*\"[0-9]+\.[0-9]+\.[0-9]+\"$"
  LIMIT_COUNT 1
)
if(NOT VERSION_F_VERSION_LINE)
  message(FATAL_ERROR "Could not read the expected version from fpm.toml")
endif()
string(REGEX REPLACE ".*\"([0-9]+\.[0-9]+\.[0-9]+)\".*" "\\1"
  VERSION_F_EXPECTED_VERSION "${VERSION_F_VERSION_LINE}"
)

find_package(version-f "${VERSION_F_EXPECTED_VERSION}" EXACT CONFIG REQUIRED)

add_executable(consumer ../consumer.f90)
target_link_libraries(consumer PRIVATE version-f::version-f)

enable_testing()
add_test(NAME consumer COMMAND consumer)
