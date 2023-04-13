//////////////////////////////////////////////////////////////////////////////
// dhTerm.cpp
//////////////////////////////////////////////////////////////////////////////
#include <cstdint>
#include <string>
#include <curses.h>
#include "dputer.h"
#include "dhBus.h"
#include "dhTerm.h"

extern bool profile;

namespace dputer {
	dhTerm::dhTerm() {
		init();
	}

	dhTerm::~dhTerm() {
		shutdown();
	}

	void dhTerm::init() {
#if 1
		initscr();
#endif
	}

	void dhTerm::reset() {
#if 1
		cbreak();
		noecho();
		nonl();
		intrflush(stdscr,FALSE);
		scrollok(stdscr,TRUE);
		keypad(stdscr,TRUE);
		cls();
#endif
	}

	void dhTerm::shutdown() {
#if 1
		endwin();
#endif
	}

	const char *cmdBufferIdx = "\n\nj 0200\nfload extend.f\nhalt\n";

	void dhTerm::tick() {
		if (bus->read(TERMIO_IREADY) == 0) {
			bus->releaseIRQ();
		}
		if (bus->read(TERMIO_CREADY) != 0) {
			uint8_t cmd = bus->read(TERMIO_CCMD);
			uint8_t data = bus->read(TERMIO_CDATA);
			handleControl(cmd,data);
			bus->write(TERMIO_CREADY,0);
		}

		if (bus->read(TERMIO_IREADY) == 0)
		{
			if (profile && *cmdBufferIdx) {
				bus->write(TERMIO_IDATA,*cmdBufferIdx++);
				bus->write(TERMIO_IREADY,0xff);
				bus->requestIRQ();
				}
			else if (keyq()) {
				bus->write(TERMIO_IDATA,key());
				bus->write(TERMIO_IREADY,0xff);
				bus->requestIRQ();
			}
		}

		if (bus->read(TERMIO_OREADY) != 0) {
			uint8_t data = bus->read(TERMIO_ODATA);
			emit(data);
			bus->write(TERMIO_OREADY,0);
			refresh();
		}
	}

	void dhTerm::cls() {
		clear();
		refresh();
	}

	void dhTerm::emit(uint8_t c) {
		switch (c) {
			case '\n':
			case '\r':
				echochar('\n');
				echochar('\r');
				break;
			case '\a':
				beep();
				break;
			case 8:
			case 127:
			case KEY_BACKSPACE:
				echochar('\b');
				echochar(' ');
				echochar('\b');
				break;
			default:
				echochar(c);
				break;
		}
	}

	uint8_t dhTerm::key() {
		int c;

		while ((c = getch()) == ERR) {
			;
		}

		return (uint8_t)c;
	}

	bool dhTerm::keyq() {
		nodelay(stdscr,TRUE);
		int32_t c = getch();
		nodelay(stdscr,FALSE);

		if (c != ERR)
			ungetch(c);

		return (c != ERR);
	}

	void dhTerm::handleControl(uint8_t cmd,uint8_t data) {
		int x,y;
		switch (cmd) {
			case CLS:
				cls();
				break;
			case HOME:
				move(0,0);
				break;
			case CURSOR_X:
				getyx(stdscr,y,x);
				move(y,data);
				break;
			case CURSOR_Y:
				getyx(stdscr,y,x);
				move(data,x);
				break;
			case SCREEN_W:
				getmaxyx(stdscr,y,x);
				bus->write(TERMIO_CDATA,(uint8_t)x);
				break;
			case SCREEN_H:
				getmaxyx(stdscr,y,x);
				bus->write(TERMIO_CDATA,(uint8_t)y);
				break;
			case GETCURSOR_X:
				getyx(stdscr,y,x);
				bus->write(TERMIO_CDATA,(uint8_t)x);
				break;
			case GETCURSOR_Y:
				getyx(stdscr,y,x);
				bus->write(TERMIO_CDATA,(uint8_t)y);
				break;
		}
	}
}
