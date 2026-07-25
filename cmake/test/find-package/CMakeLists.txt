cmake_minimum_required(VERSION 3.20)
project(version-f-find-package-test LANGUAGES Fortran)

find_package(version-f 0.4 CONFIG REQUIRED)

add_executable(consumer ../consumer.f90)
target_link_libraries(consumer PRIVATE version-f::version-f)

enable_testing()
add_test(NAME consumer COMMAND consumer)
