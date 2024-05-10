//============================================================================
// dh65c02.cpp
//
// Implements the 65c02 CPU emulation.
//============================================================================
#include "dh65c02.h"
#include "dhBus.h"
#include <fmt/format.h>
#include <iostream>

namespace dputer {
dh65c02::dh65c02() {
  irqLine = 0;
  halted = false;
  waiting = false;

  optable[0x00] = {"BRK", &dh65c02::modeIMP, &dh65c02::opBRK, 1, 7};
  optable[0x01] = {"ORA", &dh65c02::modeXII, &dh65c02::opORA, 2, 6};
  optable[0x02] = {"BAD", &dh65c02::modeIMM, &dh65c02::opBAD, 2, 2};
  optable[0x03] = {"BAD", &dh65c02::modeIMP, &dh65c02::opBAD, 1, 1};
  optable[0x04] = {"TSB", &dh65c02::modeZPG, &dh65c02::opTSB, 2, 5};
  optable[0x05] = {"ORA", &dh65c02::modeZPG, &dh65c02::opORA, 2, 3};
  optable[0x06] = {"ASL", &dh65c02::modeZPG, &dh65c02::opASL, 2, 5};
  optable[0x07] = {"RMB", &dh65c02::modeZPG, &dh65c02::opRMB, 2, 5};
  optable[0x08] = {"PHP", &dh65c02::modeIMP, &dh65c02::opPHP, 1, 3};
  optable[0x09] = {"ORA", &dh65c02::modeIMM, &dh65c02::opORA, 2, 2};
  optable[0x0a] = {"ASL", &dh65c02::modeACC, &dh65c02::opASL_ACC, 2, 2};
  optable[0x0b] = {"BAD", &dh65c02::modeIMP, &dh65c02::opBAD, 1, 1};
  optable[0x0c] = {"TSB", &dh65c02::modeABS, &dh65c02::opTSB, 3, 6};
  optable[0x0d] = {"ORA", &dh65c02::modeABS, &dh65c02::opORA, 3, 4};
  optable[0x0e] = {"ASL", &dh65c02::modeABS, &dh65c02::opASL, 3, 6};
  optable[0x0f] = {"BBR", &dh65c02::modeZPR, &dh65c02::opBBR, 3, 5};
  optable[0x10] = {"BPL", &dh65c02::modeREL, &dh65c02::opBPL, 2, 2};
  optable[0x11] = {"ORA", &dh65c02::modeIYI, &dh65c02::opORA, 2, 5};
  optable[0x12] = {"ORA", &dh65c02::modeZPI, &dh65c02::opORA, 2, 5};
  optable[0x13] = {"BAD", &dh65c02::modeIMP, &dh65c02::opBAD, 1, 1};
  optable[0x14] = {"TRB", &dh65c02::modeZPG, &dh65c02::opTRB, 2, 5};
  optable[0x15] = {"ORA", &dh65c02::modeZPX, &dh65c02::opORA, 2, 4};
  optable[0x16] = {"ASL", &dh65c02::modeZPX, &dh65c02::opASL, 2, 6};
  optable[0x17] = {"RMB", &dh65c02::modeZPG, &dh65c02::opRMB, 2, 5};
  optable[0x18] = {"CLC", &dh65c02::modeIMP, &dh65c02::opCLC, 1, 2};
  optable[0x19] = {"ORA", &dh65c02::modeABY, &dh65c02::opORA, 3, 4};
  optable[0x1a] = {"INC", &dh65c02::modeIMP, &dh65c02::opINC_ACC, 2, 1};
  optable[0x1b] = {"BAD", &dh65c02::modeIMP, &dh65c02::opBAD, 1, 1};
  optable[0x1c] = {"TRB", &dh65c02::modeABS, &dh65c02::opTRB, 3, 6};
  optable[0x1d] = {"ORA", &dh65c02::modeABX, &dh65c02::opORA, 3, 4};
  optable[0x1e] = {"ASL", &dh65c02::modeABX, &dh65c02::opASL, 3, 6};
  optable[0x1f] = {"BBR", &dh65c02::modeZPR, &dh65c02::opBBR, 3, 5};
  optable[0x20] = {"JSR", &dh65c02::modeABS, &dh65c02::opJSR, 3, 6};
  optable[0x21] = {"AND", &dh65c02::modeXII, &dh65c02::opAND, 2, 6};
  optable[0x22] = {"BAD", &dh65c02::modeIMM, &dh65c02::opBAD, 2, 2};
  optable[0x23] = {"BAD", &dh65c02::modeIMP, &dh65c02::opBAD, 1, 1};
  optable[0x24] = {"BIT", &dh65c02::modeZPG, &dh65c02::opBIT, 2, 3};
  optable[0x25] = {"AND", &dh65c02::modeZPG, &dh65c02::opAND, 2, 3};
  optable[0x26] = {"ROL", &dh65c02::modeZPG, &dh65c02::opROL, 2, 5};
  optable[0x27] = {"RMB", &dh65c02::modeZPG, &dh65c02::opRMB, 2, 5};
  optable[0x28] = {"PLP", &dh65c02::modeIMP, &dh65c02::opPLP, 1, 4};
  optable[0x29] = {"AND", &dh65c02::modeIMM, &dh65c02::opAND, 2, 2};
  optable[0x2a] = {"ROL", &dh65c02::modeACC, &dh65c02::opROL_ACC, 1, 2};
  optable[0x2b] = {"BAD", &dh65c02::modeIMP, &dh65c02::opBAD, 1, 1};
  optable[0x2c] = {"BIT", &dh65c02::modeABS, &dh65c02::opBIT, 3, 4};
  optable[0x2d] = {"AND", &dh65c02::modeABS, &dh65c02::opAND, 3, 4};
  optable[0x2e] = {"ROL", &dh65c02::modeABS, &dh65c02::opROL, 3, 6};
  optable[0x2f] = {"BBR", &dh65c02::modeZPR, &dh65c02::opBBR, 3, 5};
  optable[0x30] = {"BMI", &dh65c02::modeREL, &dh65c02::opBMI, 2, 2};
  optable[0x31] = {"AND", &dh65c02::modeIYI, &dh65c02::opAND, 2, 5};
  optable[0x32] = {"AND", &dh65c02::modeZPI, &dh65c02::opAND, 2, 5};
  optable[0x33] = {"BAD", &dh65c02::modeIMP, &dh65c02::opBAD, 1, 1};
  optable[0x34] = {"BIT", &dh65c02::modeZPX, &dh65c02::opBIT, 2, 3};
  optable[0x35] = {"AND", &dh65c02::modeZPX, &dh65c02::opAND, 2, 4};
  optable[0x36] = {"ROL", &dh65c02::modeZPX, &dh65c02::opROL, 2, 6};
  optable[0x37] = {"RMB", &dh65c02::modeZPG, &dh65c02::opRMB, 2, 5};
  optable[0x38] = {"SEC", &dh65c02::modeIMP, &dh65c02::opSEC, 1, 2};
  optable[0x39] = {"AND", &dh65c02::modeABY, &dh65c02::opAND, 3, 4};
  optable[0x3a] = {"DEC", &dh65c02::modeIMP, &dh65c02::opDEC_ACC, 1, 2};
  optable[0x3b] = {"BAD", &dh65c02::modeIMP, &dh65c02::opBAD, 1, 1};
  optable[0x3c] = {"BIT", &dh65c02::modeABX, &dh65c02::opBIT, 3, 4};
  optable[0x3d] = {"AND", &dh65c02::modeABX, &dh65c02::opAND, 3, 4};
  optable[0x3e] = {"ROL", &dh65c02::modeABX, &dh65c02::opROL, 3, 6};
  optable[0x3f] = {"BBR", &dh65c02::modeZPR, &dh65c02::opBBR, 3, 5};
  optable[0x40] = {"RTI", &dh65c02::modeIMP, &dh65c02::opRTI, 1, 6};
  optable[0x41] = {"EOR", &dh65c02::modeXII, &dh65c02::opEOR, 2, 6};
  optable[0x42] = {"BAD", &dh65c02::modeIMM, &dh65c02::opBAD, 2, 2};
  optable[0x43] = {"BAD", &dh65c02::modeIMP, &dh65c02::opBAD, 1, 1};
  optable[0x44] = {"BAD", &dh65c02::modeZPG, &dh65c02::opBAD, 2, 3};
  optable[0x45] = {"EOR", &dh65c02::modeZPG, &dh65c02::opEOR, 2, 3};
  optable[0x46] = {"LSR", &dh65c02::modeZPG, &dh65c02::opLSR, 2, 5};
  optable[0x47] = {"RMB", &dh65c02::modeZPG, &dh65c02::opRMB, 2, 5};
  optable[0x48] = {"PHA", &dh65c02::modeIMP, &dh65c02::opPHA, 1, 3};
  optable[0x49] = {"EOR", &dh65c02::modeIMM, &dh65c02::opEOR, 2, 2};
  optable[0x4a] = {"LSR", &dh65c02::modeACC, &dh65c02::opLSR_ACC, 1, 2};
  optable[0x4b] = {"BAD", &dh65c02::modeIMP, &dh65c02::opBAD, 1, 1};
  optable[0x4c] = {"JMP", &dh65c02::modeABS, &dh65c02::opJMP, 3, 3};
  optable[0x4d] = {"EOR", &dh65c02::modeABS, &dh65c02::opEOR, 3, 4};
  optable[0x4e] = {"LSR", &dh65c02::modeABS, &dh65c02::opLSR, 3, 6};
  optable[0x4f] = {"BBR", &dh65c02::modeZPR, &dh65c02::opBBR, 3, 5};
  optable[0x50] = {"BVC", &dh65c02::modeREL, &dh65c02::opBVC, 2, 2};
  optable[0x51] = {"EOR", &dh65c02::modeIYI, &dh65c02::opEOR, 2, 5};
  optable[0x52] = {"EOR", &dh65c02::modeZPI, &dh65c02::opEOR, 2, 5};
  optable[0x53] = {"BAD", &dh65c02::modeIMP, &dh65c02::opBAD, 1, 1};
  optable[0x54] = {"BAD", &dh65c02::modeZPX, &dh65c02::opBAD, 2, 4};
  optable[0x55] = {"EOR", &dh65c02::modeZPX, &dh65c02::opEOR, 2, 4};
  optable[0x56] = {"LSR", &dh65c02::modeZPX, &dh65c02::opLSR, 2, 6};
  optable[0x57] = {"rmb", &dh65c02::modeZPG, &dh65c02::opRMB, 2, 5};
  optable[0x58] = {"CLI", &dh65c02::modeIMP, &dh65c02::opCLI, 1, 2};
  optable[0x59] = {"EOR", &dh65c02::modeABY, &dh65c02::opEOR, 3, 4};
  optable[0x5a] = {"PHY", &dh65c02::modeIMP, &dh65c02::opPHY, 1, 3};
  optable[0x5b] = {"BAD", &dh65c02::modeIMP, &dh65c02::opBAD, 1, 1};
  optable[0x5c] = {"BAD", &dh65c02::modeABS, &dh65c02::opBAD, 3, 8};
  optable[0x5d] = {"EOR", &dh65c02::modeABX, &dh65c02::opEOR, 3, 4};
  optable[0x5e] = {"LSR", &dh65c02::modeABX, &dh65c02::opLSR, 3, 6};
  optable[0x5f] = {"BBR", &dh65c02::modeZPR, &dh65c02::opBBR, 3, 5};
  optable[0x60] = {"RTS", &dh65c02::modeIMP, &dh65c02::opRTS, 1, 6};
  optable[0x61] = {"ADC", &dh65c02::modeXII, &dh65c02::opADC, 2, 6};
  optable[0x62] = {"BAD", &dh65c02::modeIMM, &dh65c02::opBAD, 2, 2};
  optable[0x63] = {"BAD", &dh65c02::modeIMP, &dh65c02::opBAD, 1, 1};
  optable[0x64] = {"STZ", &dh65c02::modeZPG, &dh65c02::opSTZ, 2, 3};
  optable[0x65] = {"ADC", &dh65c02::modeZPG, &dh65c02::opADC, 2, 3};
  optable[0x66] = {"ROR", &dh65c02::modeZPG, &dh65c02::opROR, 2, 5};
  optable[0x67] = {"RMB", &dh65c02::modeZPG, &dh65c02::opRMB, 2, 5};
  optable[0x68] = {"PLA", &dh65c02::modeIMP, &dh65c02::opPLA, 1, 4};
  optable[0x69] = {"ADC", &dh65c02::modeIMM, &dh65c02::opADC, 2, 2};
  optable[0x6a] = {"ROR", &dh65c02::modeACC, &dh65c02::opROR_ACC, 1, 2};
  optable[0x6b] = {"BAD", &dh65c02::modeIMP, &dh65c02::opBAD, 1, 1};
  optable[0x6c] = {"JMP", &dh65c02::modeIND, &dh65c02::opJMP, 3, 6};
  optable[0x6d] = {"ADC", &dh65c02::modeABS, &dh65c02::opADC, 3, 4};
  optable[0x6e] = {"ROR", &dh65c02::modeABS, &dh65c02::opROR, 3, 6};
  optable[0x6f] = {"BBR", &dh65c02::modeZPR, &dh65c02::opBBR, 3, 5};
  optable[0x70] = {"BVS", &dh65c02::modeREL, &dh65c02::opBVS, 2, 2};
  optable[0x71] = {"ADC", &dh65c02::modeIYI, &dh65c02::opADC, 2, 5};
  optable[0x72] = {"ADC", &dh65c02::modeZPI, &dh65c02::opADC, 2, 5};
  optable[0x73] = {"BAD", &dh65c02::modeIMP, &dh65c02::opBAD, 1, 1};
  optable[0x74] = {"STZ", &dh65c02::modeZPX, &dh65c02::opSTZ, 2, 4};
  optable[0x75] = {"ADC", &dh65c02::modeZPX, &dh65c02::opADC, 2, 4};
  optable[0x76] = {"ROR", &dh65c02::modeZPX, &dh65c02::opROR, 2, 6};
  optable[0x77] = {"RMB", &dh65c02::modeZPG, &dh65c02::opRMB, 2, 5};
  optable[0x78] = {"SEI", &dh65c02::modeIMP, &dh65c02::opSEI, 1, 2};
  optable[0x79] = {"ADC", &dh65c02::modeABY, &dh65c02::opADC, 3, 4};
  optable[0x7a] = {"PLY", &dh65c02::modeIMP, &dh65c02::opPLY, 1, 3};
  optable[0x7b] = {"BAD", &dh65c02::modeIMP, &dh65c02::opBAD, 1, 1};
  optable[0x7c] = {"JMP", &dh65c02::modeAII, &dh65c02::opJMP, 3, 6};
  optable[0x7d] = {"ADC", &dh65c02::modeABX, &dh65c02::opADC, 3, 4};
  optable[0x7e] = {"ROR", &dh65c02::modeABX, &dh65c02::opROR, 3, 6};
  optable[0x7f] = {"BBR", &dh65c02::modeZPR, &dh65c02::opBBR, 3, 5};
  optable[0x80] = {"BRA", &dh65c02::modeREL, &dh65c02::opBRA, 2, 3};
  optable[0x81] = {"STA", &dh65c02::modeXII, &dh65c02::opSTA, 2, 6};
  optable[0x82] = {"BAD", &dh65c02::modeIMM, &dh65c02::opBAD, 2, 2};
  optable[0x83] = {"BAD", &dh65c02::modeIMP, &dh65c02::opBAD, 1, 1};
  optable[0x84] = {"STY", &dh65c02::modeZPG, &dh65c02::opSTY, 2, 3};
  optable[0x85] = {"STA", &dh65c02::modeZPG, &dh65c02::opSTA, 2, 3};
  optable[0x86] = {"STX", &dh65c02::modeZPG, &dh65c02::opSTX, 2, 3};
  optable[0x87] = {"SMB", &dh65c02::modeZPG, &dh65c02::opSMB, 2, 5};
  optable[0x88] = {"DEY", &dh65c02::modeIMP, &dh65c02::opDEY, 1, 2};
  optable[0x89] = {"BIT", &dh65c02::modeIMM, &dh65c02::opBIT_IMM, 2, 3};
  optable[0x8a] = {"TXA", &dh65c02::modeIMP, &dh65c02::opTXA, 1, 2};
  optable[0x8b] = {"BAD", &dh65c02::modeIMP, &dh65c02::opBAD, 1, 1};
  optable[0x8c] = {"STY", &dh65c02::modeABS, &dh65c02::opSTY, 3, 4};
  optable[0x8d] = {"STA", &dh65c02::modeABS, &dh65c02::opSTA, 3, 4};
  optable[0x8e] = {"STX", &dh65c02::modeABS, &dh65c02::opSTX, 3, 4};
  optable[0x8f] = {"BBS", &dh65c02::modeZPR, &dh65c02::opBBS, 3, 5};
  optable[0x90] = {"BCC", &dh65c02::modeREL, &dh65c02::opBCC, 2, 2};
  optable[0x91] = {"STA", &dh65c02::modeIYI, &dh65c02::opSTA, 2, 6};
  optable[0x92] = {"STA", &dh65c02::modeZPI, &dh65c02::opSTA, 2, 5};
  optable[0x93] = {"BAD", &dh65c02::modeIMP, &dh65c02::opBAD, 1, 1};
  optable[0x94] = {"STY", &dh65c02::modeZPX, &dh65c02::opSTY, 2, 4};
  optable[0x95] = {"STA", &dh65c02::modeZPX, &dh65c02::opSTA, 2, 4};
  optable[0x96] = {"STX", &dh65c02::modeZPY, &dh65c02::opSTX, 2, 4};
  optable[0x97] = {"SMB", &dh65c02::modeZPG, &dh65c02::opSMB, 2, 5};
  optable[0x98] = {"TYA", &dh65c02::modeIMP, &dh65c02::opTYA, 1, 2};
  optable[0x99] = {"STA", &dh65c02::modeABY, &dh65c02::opSTA, 3, 5};
  optable[0x9a] = {"TXS", &dh65c02::modeIMP, &dh65c02::opTXS, 1, 2};
  optable[0x9b] = {"BAD", &dh65c02::modeIMP, &dh65c02::opBAD, 1, 1};
  optable[0x9c] = {"STZ", &dh65c02::modeABS, &dh65c02::opSTZ, 3, 4};
  optable[0x9d] = {"STA", &dh65c02::modeABX, &dh65c02::opSTA, 3, 5};
  optable[0x9e] = {"STZ", &dh65c02::modeABX, &dh65c02::opSTZ, 3, 5};
  optable[0x9f] = {"BBS", &dh65c02::modeZPR, &dh65c02::opBBS, 3, 5};
  optable[0xa0] = {"LDY", &dh65c02::modeIMM, &dh65c02::opLDY, 2, 2};
  optable[0xa1] = {"LDA", &dh65c02::modeXII, &dh65c02::opLDA, 2, 6};
  optable[0xa2] = {"LDX", &dh65c02::modeIMM, &dh65c02::opLDX, 2, 2};
  optable[0xa3] = {"BAD", &dh65c02::modeIMP, &dh65c02::opBAD, 1, 1};
  optable[0xa4] = {"LDY", &dh65c02::modeZPG, &dh65c02::opLDY, 2, 3};
  optable[0xa5] = {"LDA", &dh65c02::modeZPG, &dh65c02::opLDA, 2, 3};
  optable[0xa6] = {"LDX", &dh65c02::modeZPG, &dh65c02::opLDX, 2, 3};
  optable[0xa7] = {"SMB", &dh65c02::modeZPG, &dh65c02::opSMB, 2, 5};
  optable[0xa8] = {"TAY", &dh65c02::modeIMP, &dh65c02::opTAY, 1, 2};
  optable[0xa9] = {"LDA", &dh65c02::modeIMM, &dh65c02::opLDA, 2, 2};
  optable[0xaa] = {"TAX", &dh65c02::modeIMP, &dh65c02::opTAX, 1, 2};
  optable[0xab] = {"BAD", &dh65c02::modeIMP, &dh65c02::opBAD, 1, 1};
  optable[0xac] = {"LDY", &dh65c02::modeABS, &dh65c02::opLDY, 3, 4};
  optable[0xad] = {"LDA", &dh65c02::modeABS, &dh65c02::opLDA, 3, 4};
  optable[0xae] = {"LDX", &dh65c02::modeABS, &dh65c02::opLDX, 3, 4};
  optable[0xaf] = {"BBS", &dh65c02::modeZPR, &dh65c02::opBBS, 3, 5};
  optable[0xb0] = {"BCS", &dh65c02::modeREL, &dh65c02::opBCS, 2, 2};
  optable[0xb1] = {"LDA", &dh65c02::modeIYI, &dh65c02::opLDA, 2, 5};
  optable[0xb2] = {"LDA", &dh65c02::modeZPI, &dh65c02::opLDA, 2, 5};
  optable[0xb3] = {"BAD", &dh65c02::modeIMP, &dh65c02::opBAD, 1, 1};
  optable[0xb4] = {"LDY", &dh65c02::modeZPX, &dh65c02::opLDY, 2, 4};
  optable[0xb5] = {"LDA", &dh65c02::modeZPX, &dh65c02::opLDA, 2, 4};
  optable[0xb6] = {"LDX", &dh65c02::modeZPY, &dh65c02::opLDX, 2, 4};
  optable[0xb7] = {"SMB", &dh65c02::modeZPG, &dh65c02::opSMB, 2, 5};
  optable[0xb8] = {"CLV", &dh65c02::modeIMP, &dh65c02::opCLV, 1, 2};
  optable[0xb9] = {"LDA", &dh65c02::modeABY, &dh65c02::opLDA, 3, 4};
  optable[0xba] = {"TSX", &dh65c02::modeIMP, &dh65c02::opTSX, 1, 2};
  optable[0xbb] = {"BAD", &dh65c02::modeIMP, &dh65c02::opBAD, 1, 1};
  optable[0xbc] = {"LDY", &dh65c02::modeABX, &dh65c02::opLDY, 3, 4};
  optable[0xbd] = {"LDA", &dh65c02::modeABX, &dh65c02::opLDA, 3, 4};
  optable[0xbe] = {"LDX", &dh65c02::modeABY, &dh65c02::opLDX, 3, 4};
  optable[0xbf] = {"BBS", &dh65c02::modeZPR, &dh65c02::opBBS, 2, 5};
  optable[0xc0] = {"CPY", &dh65c02::modeIMM, &dh65c02::opCPY, 2, 3};
  optable[0xc1] = {"CMP", &dh65c02::modeXII, &dh65c02::opCMP, 2, 6};
  optable[0xc2] = {"BAD", &dh65c02::modeIMM, &dh65c02::opBAD, 2, 2};
  optable[0xc3] = {"BAD", &dh65c02::modeIMP, &dh65c02::opBAD, 1, 1};
  optable[0xc4] = {"CPY", &dh65c02::modeZPG, &dh65c02::opCPY, 2, 3};
  optable[0xc5] = {"CMP", &dh65c02::modeZPG, &dh65c02::opCMP, 2, 3};
  optable[0xc6] = {"DEC", &dh65c02::modeZPG, &dh65c02::opDEC, 2, 5};
  optable[0xc7] = {"SMB", &dh65c02::modeZPG, &dh65c02::opSMB, 2, 5};
  optable[0xc8] = {"INY", &dh65c02::modeIMP, &dh65c02::opINY, 1, 2};
  optable[0xc9] = {"CMP", &dh65c02::modeIMM, &dh65c02::opCMP, 2, 2};
  optable[0xca] = {"DEX", &dh65c02::modeIMP, &dh65c02::opDEX, 1, 2};
  optable[0xcb] = {"WAI", &dh65c02::modeIMP, &dh65c02::opWAI, 1, 1};
  optable[0xcc] = {"CPY", &dh65c02::modeABS, &dh65c02::opCPY, 3, 4};
  optable[0xcd] = {"CMP", &dh65c02::modeABS, &dh65c02::opCMP, 3, 4};
  optable[0xce] = {"DEC", &dh65c02::modeABS, &dh65c02::opDEC, 3, 6};
  optable[0xcf] = {"BBS", &dh65c02::modeZPR, &dh65c02::opBBS, 3, 5};
  optable[0xd0] = {"BNE", &dh65c02::modeREL, &dh65c02::opBNE, 2, 2};
  optable[0xd1] = {"CMP", &dh65c02::modeIYI, &dh65c02::opCMP, 2, 5};
  optable[0xd2] = {"CMP", &dh65c02::modeZPI, &dh65c02::opCMP, 2, 5};
  optable[0xd3] = {"BAD", &dh65c02::modeIMP, &dh65c02::opBAD, 1, 1};
  optable[0xd4] = {"BAD", &dh65c02::modeZPX, &dh65c02::opBAD, 2, 4};
  optable[0xd5] = {"CMP", &dh65c02::modeZPX, &dh65c02::opCMP, 2, 4};
  optable[0xd6] = {"DEC", &dh65c02::modeZPX, &dh65c02::opDEC, 2, 6};
  optable[0xd7] = {"SMB", &dh65c02::modeZPG, &dh65c02::opSMB, 2, 5};
  optable[0xd8] = {"CLD", &dh65c02::modeIMP, &dh65c02::opCLD, 1, 2};
  optable[0xd9] = {"CMP", &dh65c02::modeABY, &dh65c02::opCMP, 3, 4};
  optable[0xda] = {"PHX", &dh65c02::modeIMP, &dh65c02::opPHX, 1, 3};
  optable[0xdb] = {"STP", &dh65c02::modeIMP, &dh65c02::opSTP, 1, 1};
  optable[0xdc] = {"BAD", &dh65c02::modeABS, &dh65c02::opBAD, 3, 4};
  optable[0xdd] = {"CMP", &dh65c02::modeABX, &dh65c02::opCMP, 3, 4};
  optable[0xde] = {"DEC", &dh65c02::modeABX, &dh65c02::opDEC, 3, 7};
  optable[0xdf] = {"BBS", &dh65c02::modeZPR, &dh65c02::opBBS, 3, 5};
  optable[0xe0] = {"CPX", &dh65c02::modeIMM, &dh65c02::opCPX, 2, 2};
  optable[0xe1] = {"SBC", &dh65c02::modeXII, &dh65c02::opSBC, 2, 6};
  optable[0xe2] = {"BAD", &dh65c02::modeIMM, &dh65c02::opBAD, 2, 2};
  optable[0xe3] = {"BAD", &dh65c02::modeIMP, &dh65c02::opBAD, 1, 1};
  optable[0xe4] = {"CPX", &dh65c02::modeZPG, &dh65c02::opCPX, 2, 3};
  optable[0xe5] = {"SBC", &dh65c02::modeZPG, &dh65c02::opSBC, 2, 3};
  optable[0xe6] = {"INC", &dh65c02::modeZPG, &dh65c02::opINC, 2, 5};
  optable[0xe7] = {"SMB", &dh65c02::modeZPG, &dh65c02::opSMB, 2, 5};
  optable[0xe8] = {"INX", &dh65c02::modeIMP, &dh65c02::opINX, 1, 2};
  optable[0xe9] = {"SBC", &dh65c02::modeIMM, &dh65c02::opSBC, 2, 2};
  optable[0xea] = {"NOP", &dh65c02::modeIMP, &dh65c02::opNOP, 1, 2};
  optable[0xeb] = {"BAD", &dh65c02::modeIMP, &dh65c02::opBAD, 1, 1};
  optable[0xec] = {"CPX", &dh65c02::modeABS, &dh65c02::opCPX, 3, 4};
  optable[0xed] = {"SBC", &dh65c02::modeABS, &dh65c02::opSBC, 3, 4};
  optable[0xee] = {"INC", &dh65c02::modeABS, &dh65c02::opINC, 3, 6};
  optable[0xef] = {"BBS", &dh65c02::modeZPR, &dh65c02::opBBS, 3, 5};
  optable[0xf0] = {"BEQ", &dh65c02::modeREL, &dh65c02::opBEQ, 2, 2};
  optable[0xf1] = {"SBC", &dh65c02::modeIYI, &dh65c02::opSBC, 2, 5};
  optable[0xf2] = {"SBC", &dh65c02::modeZPI, &dh65c02::opSBC, 2, 5};
  optable[0xf3] = {"BAD", &dh65c02::modeIMP, &dh65c02::opBAD, 1, 1};
  optable[0xf4] = {"BAD", &dh65c02::modeZPX, &dh65c02::opBAD, 2, 4};
  optable[0xf5] = {"SBC", &dh65c02::modeZPX, &dh65c02::opSBC, 2, 4};
  optable[0xf6] = {"INC", &dh65c02::modeZPX, &dh65c02::opINC, 2, 6};
  optable[0xf7] = {"SMB", &dh65c02::modeZPG, &dh65c02::opSMB, 2, 5};
  optable[0xf8] = {"SED", &dh65c02::modeIMP, &dh65c02::opSED, 1, 2};
  optable[0xf9] = {"SBC", &dh65c02::modeABY, &dh65c02::opSBC, 3, 4};
  optable[0xfa] = {"PLX", &dh65c02::modeIMP, &dh65c02::opPLX, 1, 3};
  optable[0xfb] = {"BAD", &dh65c02::modeIMP, &dh65c02::opBAD, 1, 1};
  optable[0xfc] = {"BAD", &dh65c02::modeABS, &dh65c02::opBAD, 3, 4};
  optable[0xfd] = {"SBC", &dh65c02::modeABX, &dh65c02::opSBC, 3, 4};
  optable[0xfe] = {"INC", &dh65c02::modeABX, &dh65c02::opINC, 3, 7};
  optable[0xff] = {"BBS", &dh65c02::modeZPR, &dh65c02::opBBS, 3, 5};
}

uint8_t dh65c02::reset() {
  irqLine = 0;
  halted = false;
  waiting = false;
  resetStatus();
  setFlag(IF, true);
  PC = readWord(ADDR_RESET);

  return 9;
}

uint8_t dh65c02::nmi() {
  waiting = false;
  pushWord(PC);
  push(PS);
  setFlag(IF, true);
  setFlag(DF, false);
  PC = readWord(ADDR_NMI);

  return 5;
}

uint8_t dh65c02::irq() {
  uint8_t cycles = 0;

  if (!getFlag(IF)) {
    pushWord(PC);
    push(PS);
    setFlag(IF, true);
    setFlag(DF, false);
    PC = readWord(ADDR_IRQ);
    cycles = 5;
  }

  return cycles;
}

uint8_t dh65c02::tick() {
  cyclesUsed = 0;

  if (irqLine) {
    waiting = false;
    cyclesUsed += irq();
  }

  if (!waiting) {
    opcode = read(PC++);
    struct OPCODE_ENTRY *entry = &optable[opcode];
    cyclesUsed += entry->cycles;
    uint16_t addr = (this->*(entry->mode))();
    (this->*(entry->op))(addr);
  }

  return cyclesUsed;
}

void dh65c02::disassemble() {
  uint8_t op = read(PC);
  struct OPCODE_ENTRY entry = optable[op];
  std::cerr << fmt::format("{:04X}:", PC);
  int i, j;
  for (i = 0; i < entry.length; ++i) {
    std::cerr << fmt::format(" {:02X}", read(PC + i));
  }
  for (j = i; j < 3; ++j) {
    std::cerr << "   ";
  }

  std::string inst = fmt::format("{}", entry.mnemonic);

  if (entry.mode == &dh65c02::modeACC)
    inst += fmt::format(" A");
  else if (entry.mode == &dh65c02::modeABS)
    inst += fmt::format(" ${:04X}", readWord(PC + 1));
  else if (entry.mode == &dh65c02::modeABX)
    inst += fmt::format(" ${:04X},X", readWord(PC + 1));
  else if (entry.mode == &dh65c02::modeABY)
    inst += fmt::format(" ${:04X},Y", readWord(PC + 1));
  else if (entry.mode == &dh65c02::modeIMM)
    inst += fmt::format(" #${:02X}", read(PC + 1));
  else if (entry.mode == &dh65c02::modeIMP)
    inst += fmt::format(" ");
  else if (entry.mode == &dh65c02::modeIND)
    inst += fmt::format(" (${:04X})", readWord(PC + 1));
  else if (entry.mode == &dh65c02::modeXII)
    inst += fmt::format(" (${:02X},X)", read(PC + 1));
  else if (entry.mode == &dh65c02::modeIYI)
    inst += fmt::format(" (${:02X}),Y", read(PC + 1));
  else if (entry.mode == &dh65c02::modeREL) {
    uint16_t addr = read(PC + 1);
    if (addr & 0x80)
      addr |= 0xFF00;
    addr += PC + 2;
    inst += fmt::format(" ${:04X}", addr);
  } else if (entry.mode == &dh65c02::modeZPG)
    inst += fmt::format(" ${:02X}", read(PC + 1));
  else if (entry.mode == &dh65c02::modeZPX)
    inst += fmt::format(" ${:02X},X", read(PC + 1));
  else if (entry.mode == &dh65c02::modeZPY)
    inst += fmt::format(" ${:02X},Y", read(PC + 1));
  else if (entry.mode == &dh65c02::modeAII)
    inst += fmt::format(" (${:04X},X)", readWord(PC + 1));
  else if (entry.mode == &dh65c02::modeZPI)
    inst += fmt::format(" (${:02X})", read(PC + 1));
  else if (entry.mode == &dh65c02::modeZPR) {
    uint8_t zp = read(PC + 1);
    uint16_t rel = read(PC + 2);
    uint16_t addr = rel;
    if (rel & 0x80)
      addr |= 0xFF00;
    addr += PC + 3;
    inst += fmt::format(" n,${:02X},${:04X}", zp, addr);
  }
  std::cerr << fmt::format(" {:<14}", inst);

  char flags[9];
  flags[0] = (PS & NF) ? 'N' : '-';
  flags[1] = (PS & VF) ? 'V' : '-';
  flags[2] = (PS & uF) ? '-' : '-';
  flags[3] = (PS & BF) ? 'B' : '-';
  flags[4] = (PS & DF) ? 'D' : '-';
  flags[5] = (PS & IF) ? 'I' : '-';
  flags[6] = (PS & ZF) ? 'Z' : '-';
  flags[7] = (PS & CF) ? 'C' : '-';
  flags[8] = 0;

  std::cerr << fmt::format("    {:02X} {:02X} {:02X} {:02X} {} {:3d}\n", A, X,
                           Y, SP, flags, irqLine);
}

void dh65c02::push(uint8_t value) {
  bus->write(STACK_BASE + SP, value);
  --SP;
}

void dh65c02::pushWord(uint16_t value) {
  push((value >> 8) & 0xFF);
  push(value & 0xFF);
}

uint8_t dh65c02::pop() {
  ++SP;
  return bus->read(STACK_BASE + SP);
}

uint16_t dh65c02::popWord() {
  uint16_t lo = pop();
  uint16_t hi = pop();

  return ((hi << 8) | lo);
}

uint16_t dh65c02::modeACC() { return 0; }

uint16_t dh65c02::modeABS() {
  uint16_t addr = readWord(PC);
  PC += 2;

  return addr;
}

uint16_t dh65c02::modeABX() {
  uint16_t addr = readWord(PC);
  uint16_t effective = addr + X;
  PC += 2;

  if ((effective & 0xFF00) != (addr & 0xFF00))
    ++cyclesUsed;

  return effective;
}

uint16_t dh65c02::modeABY() {
  uint16_t addr = readWord(PC);
  uint16_t effective = addr + Y;
  PC += 2;

  if ((effective & 0xFF00) != (addr & 0xFF00))
    ++cyclesUsed;

  return effective;
}

uint16_t dh65c02::modeIMM() { return PC++; }

uint16_t dh65c02::modeIMP() { return 0; }

uint16_t dh65c02::modeIND() {
  uint16_t addr = readWord(PC);
  PC += 2;

  return readWord(addr);
}

uint16_t dh65c02::modeXII() {
  uint16_t addr = read(PC++);

  uint16_t lo = (addr + X) & 0xFF;
  uint16_t hi = (lo + 1) & 0xFF;
  addr = (uint16_t)read(lo) | ((uint16_t)read(hi) << 8);

  return addr;
}

uint16_t dh65c02::modeIYI() {
  uint16_t lo = read(PC++);
  uint16_t hi = (lo + 1) & 0xFF;

  uint16_t effective = (uint16_t)read(lo) | ((uint16_t)read(hi) << 8);

  if (((effective + Y) & 0xFF00) != (effective & 0xFF00))
    ++cyclesUsed;

  return effective + Y;
}

uint16_t dh65c02::modeREL() {
  uint16_t value = read(PC++);
  if (value & 0x80)
    value |= 0xFF00;

  return PC + value;
}

uint16_t dh65c02::modeZPG() { return read(PC++); }

uint16_t dh65c02::modeZPX() { return (read(PC++) + X) & 0xFF; }

uint16_t dh65c02::modeZPY() { return (read(PC++) + Y) & 0xFF; }

uint16_t dh65c02::modeAII() {
  uint16_t addr = readWord(PC);
  PC += 2;

  return readWord(addr + X);
}

uint16_t dh65c02::modeZPI() {
  uint16_t lo = read(PC++);
  uint16_t hi = (lo + 1) & 0xFF;
  uint16_t addr = (uint16_t)read(lo) | ((uint16_t)read(hi) << 8);

  return addr;
}

uint16_t dh65c02::modeZPR() {
  uint16_t addr = read(PC++);

  return addr;
}

void dh65c02::opNOP(uint16_t src) {}

void dh65c02::opSTP(uint16_t src) { halted = true; }

void dh65c02::opLDA(uint16_t src) {
  A = read(src);

  setFlag(ZF, A == 0);
  setFlag(NF, (A & 0x80) != 0);
}

void dh65c02::opLDX(uint16_t src) {
  X = read(src);

  setFlag(ZF, X == 0);
  setFlag(NF, (X & 0x80) != 0);
}

void dh65c02::opLDY(uint16_t src) {
  Y = read(src);

  setFlag(ZF, Y == 0);
  setFlag(NF, (Y & 0x80) != 0);
}

void dh65c02::opSTA(uint16_t src) { write(src, A); }

void dh65c02::opSTX(uint16_t src) { write(src, X); }

void dh65c02::opSTY(uint16_t src) { write(src, Y); }

void dh65c02::opTXA(uint16_t src) {
  A = X;

  setFlag(ZF, A == 0);
  setFlag(NF, (A & 0x80) != 0);
}

void dh65c02::opTYA(uint16_t src) {
  A = Y;

  setFlag(ZF, A == 0);
  setFlag(NF, (A & 0x80) != 0);
}

void dh65c02::opTXS(uint16_t src) { SP = X; }

void dh65c02::opTAY(uint16_t src) {
  Y = A;

  setFlag(ZF, Y == 0);
  setFlag(NF, (Y & 0x80) != 0);
}

void dh65c02::opTAX(uint16_t src) {
  X = A;

  setFlag(ZF, X == 0);
  setFlag(NF, (X & 0x80) != 0);
}

void dh65c02::opTSX(uint16_t src) {
  X = SP;

  setFlag(ZF, X == 0);
  setFlag(NF, (X & 0x80) != 0);
}

void dh65c02::opPHA(uint16_t src) { push(A); }

void dh65c02::opPHP(uint16_t src) { push(PS | uF | BF); }

void dh65c02::opPLA(uint16_t src) {
  A = pop();

  setFlag(ZF, A == 0);
  setFlag(NF, (A & 0x80) != 0);
}

void dh65c02::opPLP(uint16_t src) {
  PS = pop();

  setFlag(BF, false);
  setFlag(uF, true);
}

void dh65c02::opDEC(uint16_t src) {
  uint8_t value = read(src);
  --value;
  write(src, value);

  setFlag(ZF, value == 0);
  setFlag(NF, (value & 0x80) != 0);
}

void dh65c02::opDEC_ACC(uint16_t src) {
  --A;

  setFlag(ZF, A == 0);
  setFlag(NF, (A & 0x80) != 0);
}

void dh65c02::opDEX(uint16_t src) {
  --X;

  setFlag(ZF, X == 0);
  setFlag(NF, (X & 0x80) != 0);
}

void dh65c02::opDEY(uint16_t src) {
  --Y;

  setFlag(ZF, Y == 0);
  setFlag(NF, (Y & 0x80) != 0);
}

void dh65c02::opINC(uint16_t src) {
  uint8_t value = read(src);
  ++value;
  write(src, value);

  setFlag(ZF, value == 0);
  setFlag(NF, (value & 0x80) != 0);
}

void dh65c02::opINC_ACC(uint16_t src) {
  ++A;

  setFlag(ZF, A == 0);
  setFlag(NF, (A & 0x80) != 0);
}

void dh65c02::opINX(uint16_t src) {
  ++X;

  setFlag(ZF, X == 0);
  setFlag(NF, (X & 0x80) != 0);
}

void dh65c02::opINY(uint16_t src) {
  ++Y;

  setFlag(ZF, Y == 0);
  setFlag(NF, (Y & 0x80) != 0);
}

void dh65c02::opADC(uint16_t src) {
  uint8_t m = read(src);
  int16_t tmp = m + A + (getFlag(CF) ? 1 : 0);

  setFlag(ZF, !(tmp & 0xFF));
  if (getFlag(DF)) {
    if (((A & 0xF) + (m & 0xF) + (getFlag(CF) ? 1 : 0)) > 9)
      tmp += 6;
    setFlag(NF, (tmp & 0x80));
    setFlag(VF, !((A ^ m) & 0x80) && ((A ^ tmp) & 0x80));
    if (tmp > 0x99) {
      tmp += 96;
    }
    setFlag(CF, tmp > 0x99);
    ++cyclesUsed;
  } else {
    setFlag(NF, tmp & 0x80);
    setFlag(VF, !((A ^ m) & 0x80) && ((A ^ tmp) & 0x80));
    setFlag(CF, (tmp > 0xFF));
  }

  A = tmp & 0xFF;
}

void dh65c02::opSBC(uint16_t src) {
  uint8_t m = read(src);
  uint16_t tmp = A - m - (getFlag(CF) ? 0 : 1);
  setFlag(NF, tmp & 0x80);
  setFlag(ZF, !(tmp & 0xFF));
  setFlag(VF, ((A ^ tmp) & 0x80) && ((A ^ m) & 0x80));

  if (getFlag(DF)) {
    if (((A & 0x0F) - (getFlag(CF) ? 0 : 1)) < (m & 0x0F))
      tmp -= 6;
    if (tmp > 0x99)
      tmp -= 0x60;
    ++cyclesUsed;
  }
  setFlag(CF, tmp < 0x100);
  A = (tmp & 0xFF);
}

void dh65c02::opAND(uint16_t src) {
  A &= read(src);

  setFlag(ZF, A == 0);
  setFlag(NF, (A & 0x80) != 0);
}

void dh65c02::opEOR(uint16_t src) {
  A ^= read(src);

  setFlag(ZF, A == 0);
  setFlag(NF, (A & 0x80) != 0);
}

void dh65c02::opORA(uint16_t src) {
  A |= read(src);

  setFlag(ZF, A == 0);
  setFlag(NF, (A & 0x80) != 0);
}

void dh65c02::opASL(uint16_t src) {
  uint8_t value = read(src);

  setFlag(CF, value & 0x80);

  value <<= 1;
  write(src, value);

  setFlag(ZF, value == 0);
  setFlag(NF, (value & 0x80) != 0);
}

void dh65c02::opASL_ACC(uint16_t src) {
  setFlag(CF, A & 0x80);

  A <<= 1;

  setFlag(ZF, A == 0);
  setFlag(NF, (A & 0x80) != 0);
}

void dh65c02::opLSR(uint16_t src) {
  uint8_t value = read(src);

  setFlag(CF, value & 0x01);

  value >>= 1;
  write(src, value);

  setFlag(ZF, value == 0);
  setFlag(NF, (value & 0x80) != 0);
}

void dh65c02::opLSR_ACC(uint16_t src) {
  setFlag(CF, A & 0x01);

  A >>= 1;

  setFlag(ZF, A == 0);
  setFlag(NF, (A & 0x80) != 0);
}

void dh65c02::opROL(uint16_t src) {
  uint8_t value = read(src);
  uint8_t cf = value & 0x80;

  value <<= 1;
  value |= (getFlag(CF) ? 1 : 0);
  write(src, value);

  setFlag(CF, cf);
  setFlag(ZF, value == 0);
  setFlag(NF, (value & 0x80) != 0);
}

void dh65c02::opROL_ACC(uint16_t src) {
  uint8_t cf = A & 0x80;

  A <<= 1;
  A |= (getFlag(CF) ? 1 : 0);

  setFlag(CF, cf);
  setFlag(ZF, A == 0);
  setFlag(NF, (A & 0x80) != 0);
}

void dh65c02::opROR(uint16_t src) {
  uint8_t value = read(src);
  uint8_t cf = value & 0x01;

  value >>= 1;
  value |= (getFlag(CF) ? 0x80 : 0);
  write(src, value);

  setFlag(CF, cf);
  setFlag(ZF, value == 0);
  setFlag(NF, (value & 0x80) != 0);
}

void dh65c02::opROR_ACC(uint16_t src) {
  uint8_t cf = A & 0x01;

  A >>= 1;
  A |= (getFlag(CF) ? 0x80 : 0);

  setFlag(CF, cf);
  setFlag(ZF, A == 0);
  setFlag(NF, (A & 0x80) != 0);
}

void dh65c02::opCLC(uint16_t src) { setFlag(CF, false); }

void dh65c02::opCLD(uint16_t src) { setFlag(DF, false); }

void dh65c02::opCLI(uint16_t src) { setFlag(IF, false); }

void dh65c02::opCLV(uint16_t src) { setFlag(VF, false); }

void dh65c02::opSEC(uint16_t src) { setFlag(CF, true); }

void dh65c02::opSED(uint16_t src) { setFlag(DF, true); }

void dh65c02::opSEI(uint16_t src) { setFlag(IF, true); }

void dh65c02::opCMP(uint16_t src) {
  uint8_t value = read(src);

  setFlag(CF, A >= value);
  setFlag(ZF, A == value);
  setFlag(NF, A < value);
}

void dh65c02::opCPX(uint16_t src) {
  uint8_t value = read(src);

  setFlag(CF, X >= value);
  setFlag(ZF, X == value);
  setFlag(NF, X < value);
}

void dh65c02::opCPY(uint16_t src) {
  uint8_t value = read(src);

  setFlag(CF, Y >= value);
  setFlag(ZF, Y == value);
  setFlag(NF, Y < value);
}

void dh65c02::opBRK(uint16_t src) {
  pushWord(PC + 1);
  push(PS | BF | uF);

  setFlag(IF, true);
  setFlag(DF, false);

  PC = readWord(ADDR_IRQ);
}

void dh65c02::opJMP(uint16_t src) { PC = src; }

void dh65c02::opJSR(uint16_t src) {
  pushWord(PC - 1);

  PC = src;
}

void dh65c02::opRTI(uint16_t src) {
  PS = pop();
  setFlag(BF, 0);
  setFlag(uF, 0);

  PC = popWord();
}

void dh65c02::opRTS(uint16_t src) { PC = popWord() + 1; }

void dh65c02::opBCC(uint16_t src) {
  if (!getFlag(CF)) {
    PC = src;
    ++cyclesUsed;
  }
}

void dh65c02::opBCS(uint16_t src) {
  if (getFlag(CF)) {
    PC = src;
    ++cyclesUsed;
  }
}

void dh65c02::opBEQ(uint16_t src) {
  if (getFlag(ZF)) {
    PC = src;
    ++cyclesUsed;
  }
}

void dh65c02::opBMI(uint16_t src) {
  if (getFlag(NF)) {
    PC = src;
    ++cyclesUsed;
  }
}

void dh65c02::opBNE(uint16_t src) {
  if (!getFlag(ZF)) {
    PC = src;
    ++cyclesUsed;
  }
}

void dh65c02::opBPL(uint16_t src) {
  if (!getFlag(NF)) {
    PC = src;
    ++cyclesUsed;
  }
}

void dh65c02::opBVS(uint16_t src) {
  if (getFlag(VF)) {
    PC = src;
    ++cyclesUsed;
  }
}

void dh65c02::opBVC(uint16_t src) {
  if (!getFlag(VF)) {
    PC = src;
    ++cyclesUsed;
  }
}

void dh65c02::opBIT(uint16_t src) {
  uint8_t value = read(src);
  uint8_t res = A & value;

  setFlag(NF, value & 0b10000000);
  setFlag(VF, value & 0b01000000);
  setFlag(ZF, !res);
}

void dh65c02::opBIT_IMM(uint16_t src) {
  uint8_t value = read(src);
  uint8_t res = A & value;

  setFlag(ZF, !res);
}

void dh65c02::opPHX(uint16_t src) { push(X); }

void dh65c02::opPHY(uint16_t src) { push(Y); }

void dh65c02::opPLX(uint16_t src) {
  X = pop();

  setFlag(ZF, X == 0);
  setFlag(NF, (X & 0x80) != 0);
}

void dh65c02::opPLY(uint16_t src) {
  Y = pop();

  setFlag(ZF, Y == 0);
  setFlag(NF, (Y & 0x80) != 0);
}

void dh65c02::opSTZ(uint16_t src) { write(src, 0); }

void dh65c02::opRMB(uint16_t src) {
  uint8_t value = read(src);
  uint8_t bit = opcode >> 4;

  value &= ~(1 << bit);
  write(src, value);
}

void dh65c02::opSMB(uint16_t src) {
  uint8_t value = read(src);
  uint8_t bit = (opcode >> 4) - 8;

  value |= (1 << bit);
  write(src, value);
}

void dh65c02::opBRA(uint16_t src) {
  PC = src;
  ++cyclesUsed;
}

void dh65c02::opTRB(uint16_t src) {
  uint8_t value = read(src);
  write(src, value & ~A);

  setFlag(ZF, !(value & A));
}

void dh65c02::opTSB(uint16_t src) {
  uint8_t value = read(src);
  write(src, value | A);

  setFlag(ZF, !(value & A));
}

void dh65c02::opBBR(uint16_t src) {
  uint8_t value = read(src);
  uint8_t bit = opcode >> 4;
  uint16_t addr = read(PC++);
  if (addr & 0x80)
    addr |= 0xFF00;

  addr = PC + addr;

  if (!(value & bit)) {
    PC = addr;
    ++cyclesUsed;
  }
}

void dh65c02::opBBS(uint16_t src) {
  uint8_t value = read(src);
  uint8_t bit = opcode >> 4;
  uint16_t addr = read(PC++);
  if (addr & 0x80)
    addr |= 0xFF00;

  addr = PC + addr;

  if (value & bit) {
    PC = addr;
    ++cyclesUsed;
  }
}

void dh65c02::opWAI(uint16_t src) { waiting = true; }

void dh65c02::opBAD(uint16_t src) {}
} // namespace dputer
