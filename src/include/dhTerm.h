//////////////////////////////////////////////////////////////////////////////
// dhTerm.h
//////////////////////////////////////////////////////////////////////////////
#pragma once
#include <cstdint>
#include <string>

namespace dputer {
	inline const uint16_t TERMIO_BASE		= 0xDC00;
	inline const uint16_t TERMIO_IREADY		= TERMIO_BASE+0;
	inline const uint16_t TERMIO_IDATA		= TERMIO_BASE+1;
	inline const uint16_t TERMIO_OREADY		= TERMIO_BASE+2;
	inline const uint16_t TERMIO_ODATA		= TERMIO_BASE+3;
	inline const uint16_t TERMIO_CREADY		= TERMIO_BASE+4;
	inline const uint16_t TERMIO_CCMD		= TERMIO_BASE+5;
	inline const uint16_t TERMIO_CDATA		= TERMIO_BASE+6;

	class dhBus;

	class dhTerm {
		public:
			enum TERMIO_CFLAGS: uint8_t {
				CLS 		= 1,
				HOME		= 2,
				CURSOR_X 	= 3,
				CURSOR_Y 	= 4,
				SCREEN_W	= 5,
				SCREEN_H	= 6,
				GETCURSOR_X	= 7,
				GETCURSOR_Y	= 8,
                CHROUT      = 9,
                RESET       = 10
			};

			dhTerm();
			~dhTerm();

			void attachBus(dhBus* bus) {
				this->bus = bus;
			}

			void init();
			void reset();
            void doReset();
			void shutdown();

			void tick();

			void cls();
			void emit(uint8_t c);
			uint8_t key();
			bool keyq();

            static void terminalThread(void* pterm);

		private:
			dhBus* bus;

			void handleControl(uint8_t cmd,uint8_t data);
	};
}
