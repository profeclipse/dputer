cmake_minimum_required(VERSION 3.24)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()

project(dputer)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin)

set(CMAKE_CXX_STANDARD 17)

set(warnings
	)
add_compile_options(
	${warnings}
	$<$<CONFIG:RELEASE>:/O2>
#	$<$<CONFIG:DEBUG>:-g>
)

include(FetchContent)
FetchContent_Declare(fmt
  GIT_REPOSITORY https://github.com/fmtlib/fmt.git
  GIT_TAG master
)
FetchContent_MakeAvailable(fmt)

file(GENERATE OUTPUT .gitignore CONTENT "*")

#find_package(SDL2 REQUIRED)

#include_directories(${CMAKE_CURRENT_SOURCE_DIR})
add_subdirectory(src)
