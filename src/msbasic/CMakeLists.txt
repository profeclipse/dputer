set (MACHINES
    #    aim65
    #    applesoft
    #    cbmbasic1
    #    cbmbasic2
    #    kb9
    #    kbdbasic
    #    microtan
    #    osi
    #    sym1                                                                                   
    dputer
	)

set (MSBASICINC
    dputer_extra.s
    dputer_iscntc.s
    dputer_loadsave.s
    array.s
    chrget.s
    defines_dputer.s
    defines.s
    error.s
    eval.s
    extra.s
    float.s
    flow1.s
    flow2.s
    header.s
    init.s
    inline.s
    input.s
    iscntc.s
    loadsave.s
    macros.s
    memory.s
    message.s
    misc1.s
    misc2.s
    misc3.s
    msbasic.s
    poke.s
    print.s
    program.s
    rnd.s
    string.s
    token.s
    trig.s
    var.s
    zeropage.s
	)

foreach(MACHINE ${MACHINES})
add_custom_command(
    OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/tmp/${MACHINE}.bin ${CMAKE_CURRENT_SOURCE_DIR}/tmp/${MACHINE}.lst ${CMAKE_CURRENT_SOURCE_DIR}/tmp/${MACHINE}.nm
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${MSBASICINC} ${CMAKE_CURRENT_SOURCE_DIR}/${MACHINE}.cfg
    COMMAND ca65 -D ${MACHINE} ${CMAKE_CURRENT_SOURCE_DIR}/msbasic.s -o ${CMAKE_CURRENT_SOURCE_DIR}/tmp/${MACHINE}.o -l ${CMAKE_CURRENT_SOURCE_DIR}/tmp/${MACHINE}.lst
    COMMAND ld65 -C ${CMAKE_CURRENT_SOURCE_DIR}/${MACHINE}.cfg ${CMAKE_CURRENT_SOURCE_DIR}/tmp/${MACHINE}.o -o ${CMAKE_CURRENT_SOURCE_DIR}/tmp/${MACHINE}.bin -Ln ${CMAKE_CURRENT_SOURCE_DIR}/tmp/${MACHINE}.nm
	)
endforeach()

foreach(ROM ${MACHINES})
	list(APPEND ROM_RESULTS ${CMAKE_CURRENT_SOURCE_DIR}/tmp/${ROM}.bin)
endforeach()

add_custom_target(msbasic ALL
    DEPENDS ${ROM_RESULTS}
)
