set (FORTHBINS
    forth
    )

set (FORTHINC
    ../kernel/kernel.lbl
    	forth.inc
    	forthmac.inc
        zeropage.s
    )

add_custom_command(
    OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/forth.bin ${CMAKE_CURRENT_SOURCE_DIR}/forth.lst ${CMAKE_CURRENT_SOURCE_DIR}/forth.o
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${FORTHINC} ${CMAKE_CURRENT_SOURCE_DIR}/forth.s ${CMAKE_CURRENT_SOURCE_DIR}/forth.cfg
    COMMAND ca65 ${CMAKE_CURRENT_SOURCE_DIR}/forth.s -I ${CMAKE_CURRENT_SOURCE_DIR}/../kernel -o ${CMAKE_CURRENT_SOURCE_DIR}/forth.o -l ${CMAKE_CURRENT_SOURCE_DIR}/forth.lst
    COMMAND ld65 -C ${CMAKE_CURRENT_SOURCE_DIR}/forth.cfg ${CMAKE_CURRENT_SOURCE_DIR}/forth.o -o ${CMAKE_CURRENT_SOURCE_DIR}/forth.bin
    )

foreach(BIN ${FORTHBINS})
    list(APPEND FORTH_RESULTS ${CMAKE_CURRENT_SOURCE_DIR}/${BIN}.bin)
endforeach()

add_custom_target(forth ALL
    DEPENDS ${FORTH_RESULTS}
    )

add_dependencies(forth kernel)
