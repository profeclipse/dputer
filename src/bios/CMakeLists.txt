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

set(INCLUDES
    acia.inc
    io.inc
    lcd.inc
    term.inc
    via.inc)

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
    COMMAND
        python
            ${PROJECT_SOURCE_DIR}/scripts/label2inc.py
            ${CMAKE_CURRENT_SOURCE_DIR}/bios.lbl
            ${CMAKE_CURRENT_SOURCE_DIR}/bios.inc
    VERBATIM)
