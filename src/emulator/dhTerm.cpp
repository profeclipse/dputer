//////////////////////////////////////////////////////////////////////////////
// dhTerm.cpp
//////////////////////////////////////////////////////////////////////////////
#include <cstdint>
#include <string>
#include <windows.h>
#include <process.h>
#include <curses.h>
#include <iostream>
#include <deque>
#include "dputer.h"
#include "dhBus.h"
#include "dhTerm.h"

extern HANDLE hRunMutex;

namespace dputer {
    static HANDLE hOutQueueMutex;
    static HANDLE hInQueueMutex;

    enum IN_QUEUE_ENTRY_TYPE {
        KEY_ENTRY,
        CURSOR_ENTRY
    };

    struct IN_QUEUE_ENTRY {
        IN_QUEUE_ENTRY_TYPE type;
        uint8_t value;
    };

    struct OUT_QUEUE_ENTRY {
        uint8_t type;
        uint8_t value;
    };

    static std::deque<IN_QUEUE_ENTRY> inQueue;
    static std::deque<OUT_QUEUE_ENTRY> outQueue;

	dhTerm::dhTerm() {
	}

	dhTerm::~dhTerm() {
	}

	void dhTerm::init() {
		initscr();
	}

	void dhTerm::doReset() {
        raw();
		noecho();
		nonl();
		intrflush(stdscr,FALSE);
		scrollok(stdscr,TRUE);
		keypad(stdscr,TRUE);
		cls();
	}

    void dhTerm::reset() {
        WaitForSingleObject(hOutQueueMutex,INFINITE);
        outQueue.push_front({RESET,0});
        ReleaseMutex(hOutQueueMutex);
    }

	void dhTerm::shutdown() {
		endwin();
	}

	void dhTerm::tick() {
		if (bus->read(TERMIO_IREADY) == 0) {
			bus->releaseIRQ();
		}
        if (bus->read(TERMIO_OREADY) & 0x80) {
			uint8_t data = bus->read(TERMIO_ODATA);
            WaitForSingleObject(hOutQueueMutex,INFINITE);
            outQueue.push_front({CHROUT,data});
            ReleaseMutex(hOutQueueMutex);
            bus->write(TERMIO_OREADY,0);
        }
		if (bus->read(TERMIO_CREADY) & 0x80) {
			uint8_t cmd = bus->read(TERMIO_CCMD);
			uint8_t data = bus->read(TERMIO_CDATA);
            WaitForSingleObject(hOutQueueMutex,INFINITE);
            outQueue.push_front({cmd,data});
            ReleaseMutex(hOutQueueMutex);
            if ((cmd != SCREEN_W) && (cmd != SCREEN_H) &&
                    (cmd != GETCURSOR_X) && (cmd != GETCURSOR_Y))
            {
			    bus->write(TERMIO_CREADY,0);
            }
		}
        WaitForSingleObject(hInQueueMutex,INFINITE);
        if (inQueue.size()) {
            IN_QUEUE_ENTRY i = inQueue.back();
            if ((i.type == KEY_ENTRY) && (bus->read(TERMIO_IREADY) == 0)) {
                inQueue.pop_back();
                bus->write(TERMIO_IDATA,i.value);
                bus->write(TERMIO_IREADY,0xff);
                bus->requestIRQ();
            }
            else if (i.type == CURSOR_ENTRY) {
                inQueue.pop_back();
				bus->write(TERMIO_CDATA,i.value);
                bus->write(TERMIO_CREADY,0);
            }
        }
        ReleaseMutex(hInQueueMutex);
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
            case 12:
                cls();
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

    void dhTerm::terminalThread(void* pterm)
    {
        dhTerm *term = (dhTerm*)pterm;
        int x,y;

        term->init();

        //std::cerr << "Thread started" << std::endl;

        hInQueueMutex = CreateMutexW(NULL,FALSE,NULL);
        hOutQueueMutex = CreateMutexW(NULL,FALSE,NULL);

        while (WaitForSingleObject(hRunMutex,0L) == WAIT_TIMEOUT)
        {
            bool hadOutput = false;
            WaitForSingleObject(hOutQueueMutex,INFINITE);
            while (outQueue.size())
            {
                OUT_QUEUE_ENTRY o = outQueue.back();
                outQueue.pop_back();
                switch (o.type)
                {
                    case CHROUT:
                        term->emit(o.value);
                        hadOutput = true;
                        break;
                    case CLS:
                        term->cls();
                        hadOutput = true;
                        break;
                    case HOME:
                        move(0,0);
                        hadOutput = true;
                        break;
                    case CURSOR_X:
                        getyx(stdscr,y,x);
                        move(y,o.value);
                        hadOutput = true;
                        break;
                    case CURSOR_Y:
                        getyx(stdscr,y,x);
                        move(o.value,x);
                        hadOutput = true;
                        break;
                    case SCREEN_W:
                        getmaxyx(stdscr,y,x);
                        WaitForSingleObject(hInQueueMutex,INFINITE);
                        inQueue.push_front({CURSOR_ENTRY,(uint8_t)x});
                        ReleaseMutex(hInQueueMutex);
                        break;
                    case SCREEN_H:
                        getmaxyx(stdscr,y,x);
                        WaitForSingleObject(hInQueueMutex,INFINITE);
                        inQueue.push_front({CURSOR_ENTRY,(uint8_t)y});
                        ReleaseMutex(hInQueueMutex);
                        break;
                    case GETCURSOR_X:
                        getyx(stdscr,y,x);
                        WaitForSingleObject(hInQueueMutex,INFINITE);
                        inQueue.push_front({CURSOR_ENTRY,(uint8_t)x});
                        ReleaseMutex(hInQueueMutex);
                        break;
                    case GETCURSOR_Y:
                        getyx(stdscr,y,x);
                        WaitForSingleObject(hInQueueMutex,INFINITE);
                        inQueue.push_front({CURSOR_ENTRY,(uint8_t)y});
                        ReleaseMutex(hInQueueMutex);
                        break;
                    case RESET:
                        term->doReset();
                        break;
                }
            }
            ReleaseMutex(hOutQueueMutex);
            if (hadOutput)
                refresh();
            if (term->keyq())
            {
                WaitForSingleObject(hInQueueMutex,INFINITE);
                inQueue.push_front({KEY_ENTRY,term->key()});
                ReleaseMutex(hInQueueMutex);
            }
        }

        term->shutdown();
    }
}
