//============================================================================
// dhClock.h
//
// Implements a high-precision CPU clock.
//============================================================================
#pragma once
#include "dputer.h"
#include <cstdint>

namespace dputer {
uint64_t ns();

class dhClock {
public:
  dhClock(uint64_t freq = CLOCKFREQ);

  void start();
  void wait();
  void wait(uint8_t cycles);
  uint64_t getStart();
  void setFrequency(uint64_t freq);
  uint64_t getFrequency();
  uint64_t getCycleTime();

private:
  uint64_t cycleTime;
  uint64_t startTime;
};
} // namespace dputer
