set(SOURCES
    bios.s
    monitor.s
    term.s
    lcd.s
    acia.s
    timer.s
    zeropage.s
    jumptable.s
    vectors.s)

add_executable(bios ${SOURCES} ${INCLUDES})

set_property(
    TARGET bios
    APPEND
    PROPERTY LINK_DEPENDS ${PROJECT_SOURCE_DIR}/src/configs/bios.cfg)

target_link_options(
    bios
    PRIVATE
    -C ${PROJECT_SOURCE_DIR}/src/configs/bios.cfg
    -Ln ${CMAKE_CURRENT_SOURCE_DIR}/bios.lbl)

add_custom_command(
    TARGET bios
    POST_BUILD
    COMMAND python
            ${PROJECT_SOURCE_DIR}/scripts/genbiosinc.py
            ${CMAKE_CURRENT_SOURCE_DIR}/bios.lbl
            ${CMAKE_CURRENT_SOURCE_DIR}/../include/bios.inc
    COMMAND cmake -E rm ${CMAKE_CURRENT_SOURCE_DIR}/bios.lbl
    VERBATIM)
