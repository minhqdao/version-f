cmake_minimum_required(VERSION 3.20)
project(version-f-fetch-content-test LANGUAGES Fortran)

include(FetchContent)

if(NOT VERSION_F_SOURCE_DIR)
  message(FATAL_ERROR "VERSION_F_SOURCE_DIR must point to a version-f checkout")
endif()

FetchContent_Declare(version-f SOURCE_DIR "${VERSION_F_SOURCE_DIR}")
FetchContent_MakeAvailable(version-f)

add_executable(consumer ../consumer.f90)
target_link_libraries(consumer PRIVATE version-f::version-f)

enable_testing()
add_test(NAME consumer COMMAND consumer)
