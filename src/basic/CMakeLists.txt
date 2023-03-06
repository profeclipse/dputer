set (BASICBINS
	basic
	)

set (BASICINC
	basic.asm
	../kernel/kernel.lbl
	)

add_custom_command(
	OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/basic.bin
	DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${BASICINC} ${CMAKE_CURRENT_SOURCE_DIR}/min_mon.asm
	COMMAND 64tass -q -Wmacro-prefix --tab-size=4 -l ${CMAKE_CURRENT_SOURCE_DIR}/basic.lbl -L ${CMAKE_CURRENT_SOURCE_DIR}/basic.lst -I ${CMAKE_CURRENT_SOURCE_DIR}/../kernel ${CMAKE_CURRENT_SOURCE_DIR}/min_mon.asm -o ${CMAKE_CURRENT_SOURCE_DIR}/basic.bin
	)

foreach(BIN ${BASICBINS})
	list(APPEND BASIC_RESULTS ${CMAKE_CURRENT_SOURCE_DIR}/${BIN}.bin)
endforeach()

add_custom_target(basic ALL
	DEPENDS ${BASIC_RESULTS}
	)

