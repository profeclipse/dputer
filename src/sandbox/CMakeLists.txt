set(BIOSBINS bios)

set(BIOSINC
    bios.inc
    io.inc
    lcd.inc
    term.inc
    acia.inc
    bios.s
    lcd.s
    acia.s
    term.s
    timer.s
    monitor.s
    jumptable.s
    vectors.s
    zeropage.s
    bios.cfg)

add_custom_command(
  OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/bios.bin
         ${CMAKE_CURRENT_SOURCE_DIR}/bios.lbl
  DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${BIOSINC}
          ${CMAKE_CURRENT_SOURCE_DIR}/bios.cfg
          ${CMAKE_CURRENT_SOURCE_DIR}/bios.s
  COMMAND
    ca65 --no-utf8 ${CMAKE_CURRENT_SOURCE_DIR}/bios.s -o
    ${CMAKE_CURRENT_SOURCE_DIR}/bios.o -l ${CMAKE_CURRENT_SOURCE_DIR}/bios.lst
  COMMAND
    ld65 --no-utf8 -C ${CMAKE_CURRENT_SOURCE_DIR}/bios.cfg
    ${CMAKE_CURRENT_SOURCE_DIR}/bios.o -o ${CMAKE_CURRENT_SOURCE_DIR}/bios.bin
    -Ln ${CMAKE_CURRENT_SOURCE_DIR}/bios.nm
  COMMAND
    python ${PROJECT_SOURCE_DIR}/scripts/label2inc2.py
    ${CMAKE_CURRENT_SOURCE_DIR}/bios.nm ${CMAKE_CURRENT_SOURCE_DIR}/bios.lbl
  COMMAND cmake -E rm ${CMAKE_CURRENT_SOURCE_DIR}/bios.nm)

foreach(ROM ${BIOSBINS})
  list(APPEND ROM_RESULTS ${CMAKE_CURRENT_SOURCE_DIR}/${ROM}.bin)
endforeach()

add_custom_target(sandbox ALL DEPENDS ${ROM_RESULTS})
