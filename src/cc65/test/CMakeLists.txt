set(SOURCES hello.s goodbye.s)

set(INCLUDES)

set(LINK_DEPENDS dputer.cfg)

# add_dputer_executable(hello)
add_executable(hello ${SOURCES} ${INCLUDES})

target_link_options(hello PRIVATE -C ${CMAKE_CURRENT_SOURCE_DIR}/dputer.cfg)

# foreach(source ${SOURCES}) add_custom_command( OUTPUT ${source}.o
# ${CMAKE_CURRENT_SOURCE_DIR}/${source}.lst DEPENDS ${INCLUDES}
# ${CMAKE_CURRENT_SOURCE_DIR}/${source} COMMAND ca65
# ${CMAKE_CURRENT_SOURCE_DIR}/${source} -o ${source}.o -l
# ${CMAKE_CURRENT_SOURCE_DIR}/${source}.lst ) list(APPEND OBJS ${source}.o)
# endforeach()

# add_custom_command( OUTPUT hello.bin DEPENDS ${OBJS}
# ${CMAKE_CURRENT_SOURCE_DIR}/dputer.cfg COMMAND ld65 -C
# ${CMAKE_CURRENT_SOURCE_DIR}/dputer.cfg ${OBJS} -o hello.bin )

# add_custom_target(hello ALL DEPENDS hello.bin )
