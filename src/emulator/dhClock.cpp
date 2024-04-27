//============================================================================
// dhClock.cpp
// 
// Implements a high-precision CPU clock.
//============================================================================
#include <chrono>
#include <iostream>
#include "dputer.h"
#include "dhClock.h"

namespace dputer {
uint64_t ns() {
  auto now = std::chrono::steady_clock::now().time_since_epoch();
  auto nanos = std::chrono::duration_cast<std::chrono::nanoseconds>(now).count();
  return nanos;
}

dhClock::dhClock(uint64_t freq) {
  setFrequency(freq);
}

void dhClock::start() {
  startTime = ns();
}

void dhClock::wait() {
  uint64_t target = startTime + cycleTime;
  while (ns() < target) {
  }
}

void dhClock::wait(uint8_t cycles) {
  uint64_t target = startTime + (cycles * cycleTime);
  while (ns() < target) {
  }
}

uint64_t dhClock::getStart() {
  return startTime;
}

void dhClock::setFrequency(uint64_t freq) {
  cycleTime = 1000000000 / freq;
}

uint64_t dhClock::getFrequency() {
  return 1000000000 / cycleTime;
}

uint64_t dhClock::getCycleTime() {
  return cycleTime;
}
}
