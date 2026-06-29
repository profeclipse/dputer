set(SOURCES
    min_mon.s
	)

set(INCLUDES
    ../include
    )

add_executable(ehbasic ${SOURCES} ${INCLUDES})

target_include_directories(ehbasic PRIVATE ${INCLUDES})

set_property(
    TARGET ehbasic
    APPEND
    PROPERTY LINK_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/ehbasic.cfg)

target_link_options(
    ehbasic
    PRIVATE
    -C ${CMAKE_CURRENT_SOURCE_DIR}/ehbasic.cfg
    -Ln ${CMAKE_CURRENT_SOURCE_DIR}/ehbasic.lbl)
