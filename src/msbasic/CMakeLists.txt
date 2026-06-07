set(SOURCES
    msbasic.s
    )

add_executable(msbasic ${SOURCES} ${INCLUDES})

target_compile_definitions(msbasic PUBLIC dputer)

set_property(
    TARGET msbasic
    APPEND
    #PROPERTY LINK_DEPENDS ${PROJECT_SOURCE_DIR}/src/configs/bios.cfg)
    PROPERTY LINK_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/dputer_msbasic.cfg)

target_link_options(
    msbasic
    PRIVATE
    #-C ${PROJECT_SOURCE_DIR}/src/configs/bios.cfg
    -C ${CMAKE_CURRENT_SOURCE_DIR}/dputer_msbasic.cfg
    -Ln ${CMAKE_CURRENT_SOURCE_DIR}/msbasic.lbl)

