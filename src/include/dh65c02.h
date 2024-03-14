//============================================================================
// dh65c02.h
//
// Implements the 65c02 CPU emulation.
//============================================================================
#pragma once
#include <cstdint>
#include <cstddef>
#include <string>
#include "dputer.h"
#include "dhBus.h"

namespace dputer {
	class dh65c02 {
		public:
			dh65c02();

		public:
			dhBus* bus;

			uint8_t A;
			uint8_t X;
			uint8_t Y;
			uint8_t SP;
			uint16_t PC;
			uint8_t PS;
			uint8_t irqLine;
			bool halted;
			bool waiting;

			uint8_t opcode;
			uint8_t cyclesUsed;

			static inline uint16_t const ADDR_NMI	= 0xFFFA;
			static inline uint16_t const ADDR_RESET	= 0xFFFC;
			static inline uint16_t const ADDR_IRQ	= 0xFFFE;
			static inline uint16_t const STACK_BASE	= 0x0100;

			enum FLAGS: uint8_t {
				CF = 0b00000001,
				ZF = 0b00000010,
				IF = 0b00000100,
				DF = 0b00001000,
				BF = 0b00010000,
				uF = 0b00100000,
				VF = 0b01000000,
				NF = 0b10000000 
			};

			inline void resetStatus() {
				PS = uF;
			}

			inline void setFlag(uint8_t flag,bool value) {
				PS = value ? PS | flag : PS & ~flag;
			}

			inline bool getFlag(uint8_t flag) {
				return PS & flag;
			}

			void attachBus(dhBus* bus) {
				this->bus = bus;
			}

			uint8_t reset();
			uint8_t nmi();
			uint8_t irq();

			void requestIRQ() {
				++irqLine;
			}

			void releaseIRQ() {
				if (irqLine)
					--irqLine;
			}

			uint8_t tick();

			void disassemble();

            uint16_t getPC() {
                return PC;
            }

			inline bool isHalted() {
				return halted;
			}

			inline uint8_t read(uint16_t addr) {
				return bus->read(addr);
			}

			inline uint16_t readWord(uint16_t addr) {
				return bus->readWord(addr);
			}

			inline void write(uint16_t addr,uint8_t value) {
				bus->write(addr,value);
			}

			inline void writeWord(uint16_t addr,uint16_t value) {
				bus->writeWord(addr,value);
			}

			inline void push(uint8_t value);
			inline void pushWord(uint16_t value);
			inline uint8_t pop();
			inline uint16_t popWord();

			typedef uint16_t (dh65c02::*modeFunc)();
			typedef void (dh65c02::*opFunc)(uint16_t);

			struct OPCODE_ENTRY {
				std::string		mnemonic;
				modeFunc		mode;
				opFunc       	op;
				uint8_t         length;
				uint8_t         cycles;
			};

			struct OPCODE_ENTRY	optable[256];

			uint16_t modeACC();	// Accumulator			ROL A
			uint16_t modeABS();	// Absolute				LDA $FFFF
			uint16_t modeABX();	// Absolute-X			LDA $F000,X
			uint16_t modeABY();	// Absolute-Y			LDA $F000,Y
			uint16_t modeIMM();	// Immediate			LDA #0
			uint16_t modeIMP();	// Implied				PHX
			uint16_t modeIND();	// Absolute Indirect	JMP ($F000)
			uint16_t modeXII();	// Indexed Indirect		LDA ($00,X)
			uint16_t modeIYI();	// Indirect Indexed		LDA ($00),Y
			uint16_t modeREL();	// Relative				BCC $F000
			uint16_t modeZPG();	// Zero page			LDA $00
			uint16_t modeZPX(); // Zero page X			LDA $00,X
			uint16_t modeZPY();	// Zero page Y			LDA $00,Y
			uint16_t modeAII();	// Absolute Indexed Ind	JMP ($F000,X)
			uint16_t modeZPI();	// Zero page Indexed	LDA ($00)
			uint16_t modeZPR();	// Zero page Relative	BBS 7,$00,$F000

			void opNOP(uint16_t src);
			void opSTP(uint16_t src);
			void opLDA(uint16_t src);
			void opLDX(uint16_t src);
			void opLDY(uint16_t src);
			void opSTA(uint16_t src);
			void opSTX(uint16_t src);
			void opSTY(uint16_t src);
			void opTXA(uint16_t src);
			void opTYA(uint16_t src);
			void opTXS(uint16_t src);
			void opTAY(uint16_t src);
			void opTAX(uint16_t src);
			void opTSX(uint16_t src);
			void opPHA(uint16_t src);
			void opPHP(uint16_t src);
			void opPLA(uint16_t src);
			void opPLP(uint16_t src);
			void opDEC(uint16_t src);
			void opDEC_ACC(uint16_t src);
			void opDEX(uint16_t src);
			void opDEY(uint16_t src);
			void opINC(uint16_t src);
			void opINC_ACC(uint16_t src);
			void opINX(uint16_t src);
			void opINY(uint16_t src);
			void opADC(uint16_t src);
			void opSBC(uint16_t src);
			void opAND(uint16_t src);
			void opEOR(uint16_t src);
			void opORA(uint16_t src);
			void opASL(uint16_t src);
			void opASL_ACC(uint16_t src);
			void opLSR(uint16_t src);
			void opLSR_ACC(uint16_t src);
			void opROL(uint16_t src);
			void opROL_ACC(uint16_t src);
			void opROR(uint16_t src);
			void opROR_ACC(uint16_t src);
			void opCLC(uint16_t src);
			void opCLD(uint16_t src);
			void opCLI(uint16_t src);
			void opCLV(uint16_t src);
			void opSEC(uint16_t src);
			void opSED(uint16_t src);
			void opSEI(uint16_t src);
			void opCMP(uint16_t src);
			void opCPX(uint16_t src);
			void opCPY(uint16_t src);
			void opBRK(uint16_t src);
			void opJMP(uint16_t src);
			void opJSR(uint16_t src);
			void opRTI(uint16_t src);
			void opRTS(uint16_t src);
			void opBCC(uint16_t src);
			void opBCS(uint16_t src);
			void opBEQ(uint16_t src);
			void opBMI(uint16_t src);
			void opBNE(uint16_t src);
			void opBPL(uint16_t src);
			void opBVS(uint16_t src);
			void opBVC(uint16_t src);
			void opBIT(uint16_t src);
			void opBIT_IMM(uint16_t src);
			void opPHX(uint16_t src);
			void opPHY(uint16_t src);
			void opPLX(uint16_t src);
			void opPLY(uint16_t src);
			void opSTZ(uint16_t src);
			void opRMB(uint16_t src);
			void opSMB(uint16_t src);
			void opBRA(uint16_t src);
			void opTRB(uint16_t src);
			void opTSB(uint16_t src);
			void opBBR(uint16_t src);
			void opBBS(uint16_t src);
			void opWAI(uint16_t src);
			void opBAD(uint16_t src);
	};
}
