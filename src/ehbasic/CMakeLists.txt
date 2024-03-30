set (BASICBINS
	basic
	)

set (BASICINC
	../kernel/kernel.lbl
	basic.s
    min_mon.s
    zeropage.s
	)

add_custom_command(
    OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/basic.bin ${CMAKE_CURRENT_SOURCE_DIR}/basic.o ${CMAKE_CURRENT_SOURCE_DIR}/basic.lbl
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${BASICINC} ${CMAKE_CURRENT_SOURCE_DIR}/basic.cfg
    COMMAND ca65 ${CMAKE_CURRENT_SOURCE_DIR}/min_mon.s -I ${CMAKE_CURRENT_SOURCE_DIR}/../kernel -o ${CMAKE_CURRENT_SOURCE_DIR}/basic.o -l ${CMAKE_CURRENT_SOURCE_DIR}/basic.lst
    COMMAND ld65 -C ${CMAKE_CURRENT_SOURCE_DIR}/basic.cfg ${CMAKE_CURRENT_SOURCE_DIR}/basic.o -o ${CMAKE_CURRENT_SOURCE_DIR}/basic.bin -Ln ${CMAKE_CURRENT_SOURCE_DIR}/basic.nm
    COMMAND python ${PROJECT_SOURCE_DIR}/scripts/label2inc.py ${CMAKE_CURRENT_SOURCE_DIR}/basic.nm ${CMAKE_CURRENT_SOURCE_DIR}/basic.lbl
    COMMAND cmake -E rm ${CMAKE_CURRENT_SOURCE_DIR}/basic.nm
	)

foreach(BIN ${BASICBINS})
	list(APPEND BASIC_RESULTS ${CMAKE_CURRENT_SOURCE_DIR}/${BIN}.bin)
endforeach()

add_custom_target(basic ALL
	DEPENDS ${BASIC_RESULTS}
	)

add_dependencies(basic kernel)
