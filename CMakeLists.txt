cmake_minimum_required(VERSION 3.24)

set(CMAKE_TOOLCHAIN_FILE ${CMAKE_SOURCE_DIR}/cmake/dputer-toolchain.cmake)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()

project(dputer LANGUAGES ASM)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin)

file(
  GENERATE
  OUTPUT .gitignore
  CONTENT "*")

add_subdirectory(src)
