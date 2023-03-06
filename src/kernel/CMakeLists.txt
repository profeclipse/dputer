set (KERNELBINS
	kernel
	)

set (KERNELINC
	macros.inc
	kernel.inc
	termio.inc
	termio.asm
	fileio.inc
	monitor.asm
	)

add_custom_command(
	OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/kernel.bin ${CMAKE_CURRENT_SOURCE_DIR}/kernel.lbl
	DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${KERNELINC} ${CMAKE_CURRENT_SOURCE_DIR}/kernel.asm
	COMMAND 64tass -q --tab-size=4 --verbose-list -l ${CMAKE_CURRENT_SOURCE_DIR}/kernel.lbl -L ${CMAKE_CURRENT_SOURCE_DIR}/kernel.lst ${CMAKE_CURRENT_SOURCE_DIR}/kernel.asm -o ${CMAKE_CURRENT_SOURCE_DIR}/kernel.bin
	)

foreach(ROM ${KERNELBINS})
	list(APPEND ROM_RESULTS ${CMAKE_CURRENT_SOURCE_DIR}/${ROM}.bin)
endforeach()

add_custom_target(kernel ALL
    DEPENDS ${ROM_RESULTS}
    )
