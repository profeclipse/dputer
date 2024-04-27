include(../dputer.cmake)

set(SOURCES
    hello.s
)

set(INCLUDES
)

add_dputer_executable(hello)

#foreach(source ${SOURCES})
#    add_custom_command(
#        OUTPUT ${source}.o ${CMAKE_CURRENT_SOURCE_DIR}/${source}.lst
#        DEPENDS ${INCLUDES} ${CMAKE_CURRENT_SOURCE_DIR}/${source}
#        COMMAND ca65 ${CMAKE_CURRENT_SOURCE_DIR}/${source} -o ${source}.o -l ${CMAKE_CURRENT_SOURCE_DIR}/${source}.lst
#    )
#    list(APPEND OBJS ${source}.o)
#endforeach()

#add_custom_command(
#    OUTPUT hello.bin
#    DEPENDS ${OBJS} ${CMAKE_CURRENT_SOURCE_DIR}/dputer.cfg
#    COMMAND ld65 -C ${CMAKE_CURRENT_SOURCE_DIR}/dputer.cfg ${OBJS} -o hello.bin
#)

#add_custom_target(hello ALL
#    DEPENDS hello.bin
#)
