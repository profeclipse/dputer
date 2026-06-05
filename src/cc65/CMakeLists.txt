cmake_minimum_required(VERSION 3.24)

project(
  hello
  VERSION 1.0
  LANGUAGES ASM)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin)

add_subdirectory(test)
