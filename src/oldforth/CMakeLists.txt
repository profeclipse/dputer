set (FORTHBINS
	forth
	)

set (FORTHINC
	../kernel/kernel.lbl
	forth.inc
	forth.mac
	)

add_custom_command(
	OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/forth.bin
	DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${FORTHINC} ${CMAKE_CURRENT_SOURCE_DIR}/forth.s
	COMMAND 64tass -q -Wmacro-prefix -Wshadow --tab-size=4 -L ${CMAKE_CURRENT_SOURCE_DIR}/forth.lst --no-monitor -I ${CMAKE_CURRENT_SOURCE_DIR}/../kernel ${CMAKE_CURRENT_SOURCE_DIR}/forth.s -o ${CMAKE_CURRENT_SOURCE_DIR}/forth.bin
	)

foreach(BIN ${FORTHBINS})
	list(APPEND FORTH_RESULTS ${CMAKE_CURRENT_SOURCE_DIR}/${BIN}.bin)
endforeach()

add_custom_target(forth ALL
	DEPENDS ${FORTH_RESULTS}
	)

add_dependencies(forth kernel)
