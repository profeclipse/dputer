cmake_minimum_required(VERSION 3.24)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()

project(dputer)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin)

set(CMAKE_CXX_STANDARD 17)

set(WARNINGS)
add_compile_options(${WARNINGS} $<$<CONFIG:RELEASE>:/O2>)

include(FetchContent)
FetchContent_Declare(
  fmt
  GIT_REPOSITORY https://github.com/fmtlib/fmt.git
  GIT_TAG master)
FetchContent_MakeAvailable(fmt)

file(
  GENERATE
  OUTPUT .gitignore
  CONTENT "*")

add_subdirectory(src)
