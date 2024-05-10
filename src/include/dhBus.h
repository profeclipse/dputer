//============================================================================
// dhBus.h
//
// Implements the virtual computer bus.
//============================================================================
#pragma once
#include "dputer.h"
#include <cstdint>

namespace dputer {
class dh65c02;
class dhTerm;
class dhFileIO;

class dhBus {
public:
  dhBus(dh65c02 *cpu, dhTerm *term, dhFileIO *file);
  ~dhBus();

  uint8_t reset();
  void requestIRQ();
  void releaseIRQ();
  uint8_t nmi();

  void write(uint16_t addr, uint8_t value);
  void writeWord(uint16_t addr, uint16_t value);
  uint8_t read(uint16_t addr);
  uint16_t readWord(uint16_t addr);

  uint8_t tick();

private:
  dh65c02 *cpu;
  dhTerm *term;
  dhFileIO *file;
  uint8_t memory[RAMSIZE];
};
} // namespace dputer
