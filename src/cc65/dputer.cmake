function (add_dputer_executable exe)
    foreach(source ${SOURCES})
        add_custom_command(
            OUTPUT ${source}.o ${CMAKE_CURRENT_SOURCE_DIR}/${source}.lst
            DEPENDS ${INCLUDES} ${CMAKE_CURRENT_SOURCE_DIR}/${source}
            COMMAND ca65 -I${INCLUDES} ${CMAKE_CURRENT_SOURCE_DIR}/${source} -o ${source}.o -l ${CMAKE_CURRENT_SOURCE_DIR}/${source.lst}
        )
        list(APPEND OBJS ${source}.o)
    endforeach()

    add_custom_command(
        OUTPUT ${exe}.bin
        DEPENDS ${OBJS} ${CMAKE_CURRENT_SOURCE_DIR}/dputer.cfg
        COMMAND ld65 -C ${CMAKE_CURRENT_SOURCE_DIR}/dputer.cfg ${OBJS} -o ${exe}.bin
    )

    add_custom_target(${exe} ALL
        DEPENDS ${exe}.bin
    )
endfunction()
