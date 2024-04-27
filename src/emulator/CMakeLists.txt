set(SOURCES
  main.cpp
  dhClock.cpp
  dh65c02.cpp
  dhBus.cpp
  dhTerm.cpp
  dhFileIO.cpp
)

set(INCLUDES
  ../include/dputer.h
  ../include/dhClock.h
  ../include/dh65c02.h
  ../include/dhBus.h
  ../include/dhTerm.h
  ../include/dhFileIO.h
)

add_executable(
  dputer
  ${SOURCES}
  ${INCLUDES}
)

target_include_directories(
  dputer PRIVATE
  ../include
)

target_link_libraries(
  dputer
  pdcurses
  fmt::fmt
)

add_custom_target(run
  COMMAND dputer
)
