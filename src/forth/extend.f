\ ************************************************************************** /
\ Load Forth Extensions                                                      /
\ ************************************************************************** /

cr .( Extending Forth Kernel...)

fload coreext.f
fload util.f
fload progtools.f
fload order.f
fload module.f
fload words.f
fload asm65c02.f
fload see.f
fload forget.f

\+ FENCE HERE FENCE !

cr .( Saving Extended Forth ...)
fsave newforth.bin

