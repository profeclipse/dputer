//============================================================================
// dhBus.cpp
// 
// Implements the virtual computer bus.
//============================================================================
#include "dputer.h"
#include "dhBus.h"
#include "dh65c02.h"
#include "dhTerm.h"
#include "dhFileIO.h"

namespace dputer {
	dhBus::dhBus(dh65c02* cpu,dhTerm* term,dhFileIO* file) {
		this->cpu = cpu;
		this->term = term;
		this->file = file;
		cpu->attachBus(this);
		term->attachBus(this);
		file->attachBus(this);

		for (uint32_t addr=0 ; addr<RAMSIZE ; ++addr) {
			memory[addr] = 0;
		}
	}

	dhBus::~dhBus() {
	}

	void dhBus::write(uint16_t addr,uint8_t value) {
		memory[addr] = value;
	}

	void dhBus::writeWord(uint16_t addr,uint16_t value) {
		memory[addr] = value & 0xFF;
		memory[addr+1] = (value >> 8) & 0xFF;
	}

	uint8_t dhBus::read(uint16_t addr) {
		return memory[addr];
	}

	uint16_t dhBus::readWord(uint16_t addr) {
		return ((uint16_t)memory[addr] | ((uint16_t)memory[addr+1] << 8));
	}

	uint8_t dhBus::reset() {
		term->reset();
		uint8_t cycles = cpu->reset();

		return cycles;
	}

	void dhBus::requestIRQ() {
		cpu->requestIRQ();
	}

	void dhBus::releaseIRQ() {
		cpu->releaseIRQ();
	}

	uint8_t dhBus::nmi() {
		uint8_t cycles = cpu->nmi();

		return cycles;
	}

	uint8_t dhBus::tick() {
		term->tick();
		file->tick();
		uint8_t cycles = cpu->tick();

		return cycles;
	}
}
