set (KERNELBINS
	kernel
	)

set (KERNELINC
    kernel.inc
    bios.s
    fileio.s
    jumptable.s
    loadaddr.s
    monitor.s
    termio.s
    vectors.s
    zeropage.s
	)

add_custom_command(
	OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/kernel.bin ${CMAKE_CURRENT_SOURCE_DIR}/kernel.lbl
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${KERNELINC} ${CMAKE_CURRENT_SOURCE_DIR}/kernel.cfg ${CMAKE_CURRENT_SOURCE_DIR}/kernel.s
    COMMAND ca65 ${CMAKE_CURRENT_SOURCE_DIR}/kernel.s -o ${CMAKE_CURRENT_SOURCE_DIR}/kernel.o -l ${CMAKE_CURRENT_SOURCE_DIR}/kernel.lst
    COMMAND ld65 -C ${CMAKE_CURRENT_SOURCE_DIR}/kernel.cfg ${CMAKE_CURRENT_SOURCE_DIR}/kernel.o -o ${CMAKE_CURRENT_SOURCE_DIR}/kernel.bin -Ln ${CMAKE_CURRENT_SOURCE_DIR}/kernel.nm
    COMMAND python ${PROJECT_SOURCE_DIR}/scripts/label2inc.py ${CMAKE_CURRENT_SOURCE_DIR}/kernel.nm ${CMAKE_CURRENT_SOURCE_DIR}/kernel.lbl
    COMMAND cmake -E rm ${CMAKE_CURRENT_SOURCE_DIR}/kernel.nm
	)

foreach(ROM ${KERNELBINS})
	list(APPEND ROM_RESULTS ${CMAKE_CURRENT_SOURCE_DIR}/${ROM}.bin)
endforeach()

add_custom_target(kernel ALL
    DEPENDS ${ROM_RESULTS}
    )
