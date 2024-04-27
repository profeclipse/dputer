#include <iostream>
#include <fstream>
#include <fmt/format.h>
#include "dputer.h"
#include "dhClock.h"
#include "dh65c02.h"
#include "dhTerm.h"
#include "dhFileIO.h"
#include <windows.h>
#include <process.h>
#include <chrono>
#include <thread>

dputer::dhClock cpuClock;
dputer::dh65c02 cpu;
dputer::dhTerm term;
dputer::dhFileIO file;
dputer::dhBus bus(&cpu,&term,&file);

bool debug = false;
bool noclock = false;
bool profile = false;

static std::string debugopt		= "--debug";
static std::string freqopt		= "--freq";
static std::string kernelopt	= "--kernel";
static std::string loadopt		= "--load";
static std::string noclockopt	= "--noclock";
static std::string profileopt   = "--profile";

HANDLE hRunMutex;
HANDLE hTerminalThread;

void setup() {
  hRunMutex = CreateMutexW(NULL,TRUE,NULL);
  hTerminalThread = (HANDLE)_beginthread(dputer::dhTerm::terminalThread,0,(void*)&term);
}

void shutdown() {
  ReleaseMutex(hRunMutex);
  CloseHandle(hRunMutex);
}

void loadROM(const char *fn,bool setResetVector) {
  char buffer[1024];
  uint16_t addr = 0;
  uint16_t bytes = 0;

  std::ifstream f(fn,std::ios::in | std::ios::binary);
  if (!f) {
    std::cout << "Failed to load rom: " << fn << std::endl;
    std::cout << "could not open file" << std::endl;
  }
  f.read((char *)&addr,2);
  if (f.eof()) {
    std::cout << "Skipping empty rom: " << fn << std::endl;
  }
  else if (f.good()) {
    std::cout << fmt::format("{:04x} - {}\n",addr,fn);
    if (setResetVector) {
      bus.write(cpu.ADDR_RESET,addr&0xFF);
      bus.write(cpu.ADDR_RESET+1,(addr>>8)&0xFF);
    }
    do {
      f.read(buffer,1024);
      bytes = f.gcount();
      for (uint16_t i=0 ; i<bytes ; ++i) {
        bus.write(addr++,(uint8_t)buffer[i]);
      }
    } while (f.good());
  }
  if (f.fail() && !f.eof()) {
    std::cout << "Failed to load rom: " << fn << std::endl;
    exit(1);
  }

  f.close();
}

void doDebug() {
  static bool firstTime = true;

  if (firstTime) {
    firstTime = false;
    std::cerr <<
      fmt::format("ADDR  OP D1 D2 INST              A  X  Y  SP NVuBDIZC IRQ\n");
    std::cerr <<
      fmt::format("----  -- -- -- ---------------   -- -- -- -- -------- ---\n");
  }

  cpu.disassemble();
}

std::ofstream profileStream;
void startProfiler() {
  profileStream.open("profile.out",std::ios::out);
}

void stopProfiler() {
  profileStream.close();
}

void doProfile(uint8_t cycles) {
  uint64_t start = cpuClock.getStart();
  uint64_t end = dputer::ns();
  uint64_t expected = cycles * cpuClock.getCycleTime();
  uint64_t actual = end - start;

  if (profile)
    profileStream << fmt::format("{:04x} - {:2d} - Expected: {:5d} Actual: {:5d}\n",
                                 cpu.getPC(),cycles,expected,actual);
}

int main(int argc, char* argv[]) {
  for (int i=1 ; i<argc ; ++i) {
    if (debugopt == argv[i]) {
      debug = true;
    }
    else if (freqopt == argv[i]) {
      ++i;
      std::string freqstr = argv[i];
      uint64_t freq = std::stoul(freqstr,nullptr,10);
      cpuClock.setFrequency(freq);
    }
    else if (kernelopt == argv[i]) {
      ++i;
      loadROM(argv[i],true);
    }
    else if (loadopt == argv[i]) {
      ++i;
      loadROM(argv[i],false);
    }
    else if (noclockopt == argv[i]) {
      noclock = true;
    }
    else if (profileopt == argv[i]) {
      profile = true;
    }
  }

  if (profile)
    startProfiler();

  setup();

  cpuClock.start();

  uint8_t cycles = bus.reset();

  while (true) {
    if (debug) {
      doDebug();
    }

    if (!noclock) {
      cpuClock.wait(cycles);
    }

    if (profile)
      doProfile(cycles);

    cpuClock.start();

    cycles = bus.tick();

    if (cpu.isHalted()) {
      break;
    }
  }

  shutdown();

  if (profile) {
    stopProfiler();
  }

  std::cout << fmt::format("PC: {:04X}\n",cpu.getPC());

  return 0;
}
