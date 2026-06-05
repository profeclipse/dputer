# CMake Toolchin File for CC65

set(CMAKE_SYSTEM_NAME Generic)
set(CMAKE_SYSTEM_PROCESSOR 65c02)

# Add cc65 sdk path to search path
list(APPEND CMAKE_PREFIX_PATH $Env:{CC65_PATH})

# Specify cc65 as the C compiler
find_program(_CL65 cl65)
set(CMAKE_C_COMPILER ${_CL65})
set(CMAKE_C_COMPILER_ID cc65)
set(CMAKE_ASM_COMPILER ${_CL65})
set(CMAKE_ASM_COMPILER_ID ca65)

# Specify ld65 as the linker find_program(_LD65 ld65) set(CMAKE_LINK_LINKER
# ${_LD65}) set(CMAKE_LINK_LLD ld65)

# Specify ar65 as the archiver
find_program(_AR ar65)
set(CMAKE_AR
    "${_AR}"
    CACHE FILEPATH "Archiver path override (prevents issues with cmake)")

# Set up overridable default arguments
set(CC65_TARGET_FLAG
    "-t none"
    CACHE STRING "Target flag for cc65")
set(CC65_DEBUG_FLAG
    "-g -DDEBUG --asm-define DEBUG"
    CACHE STRING "Debug flags for cc65")
set(CC65_OPT_MAX_FLAG
    "-Oisr"
    CACHE STRING "Max optimization flags for cc65")
set(CC65_OPT_MIN_SIZE
    "-O"
    CACHE STRING "Optimization flags for minimum size build for cc65")

set(CA65_TARGET_FLAG
    "-t none"
    CACHE STRING "Target flag for ca65")
# set(LD65_TARGET_FLAG "-t none" CACHE STRING "Target flag for ld65")

set(CMAKE_C_FLAGS_INIT "--no-utf8")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS_INIT} ${CC65_TARGET_FLAG}")
set(CMAKE_C_FLAGS_DEBUG_INIT "${CC65_DEBUG_FLAG}")
set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG_INIT} ${CC65_TARGET_FLAG}")
set(CMAKE_C_FLAGS_RELEASE_INIT "${CC65_C65_OPT_MAX_FLAG}")
set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE_INIT} ${CC65_TARGET_FLAG}")
set(CMAKE_C_FLAGS_MINSIZEREL_INIT "${CC65_C65_OPT_MIN_SIZE}")
set(CMAKE_C_FLAGS_MINSIZEREL
    "${CMAKE_C_FLAGS_MINSIZEREL_INIT} ${CC65_TARGET_FLAG}")

set(CMAKE_ASM_FLAGS_INIT "--no-utf8")
set(CMAKE_ASM_FLAGS "${CMAKE_ASM_FLAGS_INIT} ${CA65_TARGET_FLAG}")

# set(CMAKE_EXE_LINKER_FLAGS_INIT "-t none --no-utf8")
# set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS_INIT}
# ${LD65_TARGET_FLAG}")

set(CMAKE_C_COMPILE_OBJECT
    "<CMAKE_C_COMPILER> <FLAGS> <DEFINES> <INCLUDES> -E -o <OBJECT>.c -S <SOURCE>"
    "<CMAKE_C_COMPILER> <FLAGS> <DEFINES> <INCLUDES> -o <OBJECT>.s -S <SOURCE>"
    "<CMAKE_C_COMPILER> <FLAGS> <DEFINES> <INCLUDES> -l <OBJECT>.lst -o <OBJECT> -c <OBJECT>.s"
)

set(CMAKE_ASM_COMPILE_OBJECT
    "<CMAKE_ASM_COMPILER> <FLAGS> <DEFINES> <INCLUDES> -l <OBJECT>.lst -o <OBJECT> -c <SOURCE>"
)

# set(CMAKE_LINK_EXECUTABLE "<CMAKE_LINK_LINKER> <FLAGS> <OBJECTS> -o <TARGET>
# <LINK_LIBRARIES>")

# HACK: Work around to prevent Cmake from editing some variables
macro(set_readonly VAR)
  # Set the variable itself
  set("${VAR}" "${ARGN}")
  # Store the variable's value for restore it upon modifications.
  set("_${VAR}_readonly_val" "${ARGN}")
  # Register a watcher for a variable
  variable_watch("${VAR}" readonly_guard)
endmacro()

# Watcher for a variable which emulates readonly property.
macro(readonly_guard VAR access value current_list_file stack)
  if("${access}" STREQUAL "MODIFIED_ACCESS")
    # Restore a value of the variable to the initial one.
    set(${VAR} "${_${VAR}_readonly_val}")
  endif()
endmacro()

set_readonly(CMAKE_INCLUDE_FLAG_ASM "--asm-include-dir")

set_readonly(CMAKE_EXECUTABLE_SUFFIX ".bin")

set(CMAKE_DEPFILE_FLAGS_ASM "--create-dep <DEP_FILE>")
set(CMAKE_DEPFILE_FLAGS_C "--create-dep <DEP_FILE>")
