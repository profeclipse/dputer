#include <cstdint>
#include <iostream>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <fmt/format.h>
#include "dputer.h"
#include "dhBus.h"
#include "dhFileIO.h"

namespace dputer {
	static FILE *fileBuffers[128];

	static FILE *fdToFile(uint8_t fd) {
		if (fd < 3 || fd > 127) {
			return nullptr;
		}

		return fileBuffers[fd];
	};

	static void setFileFromFD(uint8_t fd,FILE* f) {
		if (fd >= 3 && fd <= 127) {
			fileBuffers[fd] = f;
		}
	}

	static void clearFD(uint8_t fd) {
		if (fd >= 3 && fd <= 127) {
			fileBuffers[fd] = nullptr;
		}
	}

	dhFileIO::dhFileIO() {
	}

	dhFileIO::~dhFileIO() {
	}

	void dhFileIO::init() {
	}

	void dhFileIO::reset() {
	}

	void dhFileIO::shutdown() {
	}

	void dhFileIO::tick() {
		if (bus->read(FILEIO_CREADY) != 0) {
			uint8_t cmd = bus->read(FILEIO_CCMD);
			uint8_t datalo = bus->read(FILEIO_CDATA_LO);
			uint8_t datahi = bus->read(FILEIO_CDATA_HI);
			uint8_t datalo2 = bus->read(FILEIO_CDATA_LO2);
			uint8_t datahi2 = bus->read(FILEIO_CDATA_HI2);
			uint8_t fd = bus->read(FILEIO_CFD);
			uint8_t mode = bus->read(FILEIO_CMODE);
			handleControl(cmd,datalo,datahi,datalo2,datahi2,fd,mode);
			bus->write(FILEIO_CREADY,0);
		}
	}

	void dhFileIO::handleControl(uint8_t cmd,
			uint8_t datalo,uint8_t datahi,
			uint8_t datalo2,uint8_t datahi2,
			uint8_t fd,uint8_t mode) {
		switch (cmd) {
			case OPEN:
				doOpen(mode);
				break;
			case CLOSE:
				doClose(fd);
				break;
			case READ:
				doRead(fd);
				break;
			case WRITE:
				doWrite(fd,datalo);
				break;
			case SEEK:
				doSeek(fd,datalo,datahi,datalo2,datahi2);
				break;
			case DELETE:
				doDelete();
				break;
			case FILEPOS:
				doPosition(fd);
				break;
			case FILESIZE:
				doSize(fd);
				break;
			case FLUSH:
				doFlush(fd);
				break;
			case RESIZE:
				doResize(fd,datalo,datahi,datalo2,datahi2);
				break;
		}
	}

	void dhFileIO::doOpen(uint8_t mode) {
		std::string flags;
		int perm = 0;
		char fn[2048];

		switch (mode) {
			case MODE_READ:
				flags = "r";
				break;
			case MODE_WRITE:
				flags = "w";
				perm = S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH;
				break;
			case MODE_RW:
				flags = "r+";
				perm = S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH;
				break;
			default:
				bus->write(FILEIO_CSTATUS,STATUS_ERR);
				return;
		}

		for (int i=0 ; i<2047 ; ++i) {
			fn[i] = bus->read(FILEIO_FILENAME+i);
			if (fn[i] == 0) {
				break;
			}
		}

		int fd;
		FILE *f = fopen(fn,flags.c_str());
		if (f == nullptr) {
			fd = -1;
		}
		else {
			fd = fileno(f);
			setFileFromFD(fd,f);
		}

		if (fd < 0) {
			bus->write(FILEIO_CSTATUS,STATUS_ERR);
		}
		else {
			bus->write(FILEIO_CSTATUS,STATUS_OK);
			bus->write(FILEIO_CFD,(uint8_t)fd);
		}
	}

	void dhFileIO::doClose(uint8_t fd) {
		FILE* f = fdToFile(fd);
		if (f == nullptr || fclose(f) < 0) {
			bus->write(FILEIO_CSTATUS,STATUS_ERR);
		}
		else {
			clearFD(fd);
			bus->write(FILEIO_CSTATUS,STATUS_OK);
		}
	}

	void dhFileIO::doRead(uint8_t fd) {
		char c;
		size_t r;
		FILE* f = fdToFile(fd);

		if (f != nullptr) {
			r = fread(&c,1,1,f);
		}
		else {
			r = -1;
		}
		if (r == 0) {
			if (feof(f)) {
				bus->write(FILEIO_CDATA_LO,0);
				bus->write(FILEIO_CSTATUS,STATUS_EOF);
			}
			else {
				bus->write(FILEIO_CDATA_LO,0);
				bus->write(FILEIO_CSTATUS,STATUS_ERR);
			}
		}
		else {
			bus->write(FILEIO_CDATA_LO,c);
			bus->write(FILEIO_CSTATUS,STATUS_OK);
		}
	}

	void dhFileIO::doWrite(uint8_t fd,uint8_t data) {
		size_t w;

		char c = (char)data;
		FILE* f = fdToFile(fd);
		if (f != nullptr) {
			w = fwrite(&c,1,1,f);
		}
		else {
			w = -1;
		}
		if (w != 1) {
			bus->write(FILEIO_CSTATUS,STATUS_ERR);
		}
		else {
			bus->write(FILEIO_CSTATUS,STATUS_OK);
		}
	}

	void dhFileIO::doSeek(uint8_t fd,
			uint8_t datalo,uint8_t datahi,uint8_t datalo2,uint8_t datahi2) {
		FILE* f = fdToFile(fd);
		off_t pos = 0;
		int r = 0;

		if (f == nullptr) {
			r = -1;
		}
		else {
			pos = ((off_t)datahi2 << 24) | ((off_t)datalo2 << 16)
				| ((off_t)datahi << 8) | ((off_t)datalo);
			r = fseek(f,pos,SEEK_SET);
		}

		if (r == 0) {
			bus->write(FILEIO_CSTATUS,STATUS_OK);
		}
		else {
			bus->write(FILEIO_CSTATUS,STATUS_ERR);
		}
	}

	void dhFileIO::doDelete() {
		char fn[2048];
		for (int i=0 ; i<2047 ; ++i) {
			fn[i] = bus->read(FILEIO_FILENAME+i);
			if (fn[i] == 0) {
				break;
			}
		}
		if (unlink(fn)) {
			bus->write(FILEIO_CSTATUS,STATUS_ERR);
		}
		else {
			bus->write(FILEIO_CSTATUS,STATUS_OK);
		}
	}

	void dhFileIO::doPosition(uint8_t fd) {
		FILE* f = fdToFile(fd);
		long pos = 0;
		if (f != nullptr) {
			pos = ftell(f);
		}

		if (pos != -1) {
			bus->write(FILEIO_CSTATUS,STATUS_OK);
		}
		else {
			bus->write(FILEIO_CSTATUS,STATUS_ERR);
			pos = 0;
		}

		bus->write(FILEIO_CDATA_LO,((uint32_t)pos) & 0xff);
		bus->write(FILEIO_CDATA_HI,((uint32_t)pos >> 8) & 0xff);
		bus->write(FILEIO_CDATA_LO2,((uint32_t)pos >> 16) & 0xff);
		bus->write(FILEIO_CDATA_HI2,((uint32_t)pos >> 24) & 0xff);
	}

	void dhFileIO::doSize(uint8_t fd) {
		struct stat buf;
		off_t size;

		if (fstat(fd,&buf) != 0) {
			bus->write(FILEIO_CSTATUS,STATUS_ERR);
			size = 0;
		}
		else {
			bus->write(FILEIO_CSTATUS,STATUS_OK);
			size = buf.st_size;
		}

		bus->write(FILEIO_CDATA_LO,((uint32_t)size) & 0xff);
		bus->write(FILEIO_CDATA_HI,((uint32_t)size >> 8) & 0xff);
		bus->write(FILEIO_CDATA_LO2,((uint32_t)size >> 16) & 0xff);
		bus->write(FILEIO_CDATA_HI2,((uint32_t)size >> 24) & 0xff);
	}

	void dhFileIO::doFlush(uint8_t fd) {
		FILE* f = fdToFile(fd);
		int r;

		if (f != nullptr) {
			r = fflush(f);
		}
		else {
			r = -1;
		}

		if (r != 0) {
			bus->write(FILEIO_CSTATUS,STATUS_ERR);
		}
		else {
			bus->write(FILEIO_CSTATUS,STATUS_OK);
		}
	}

	void dhFileIO::doResize(uint8_t fd,
			uint8_t datalo,uint8_t datahi,uint8_t datalo2,uint8_t datahi2) {
		FILE* f = fdToFile(fd);
		off_t pos = 0;
		int r = 0;

		if (f == nullptr) {
			r = -1;
		}
		else {
			pos = ((off_t)datahi2 << 24) | ((off_t)datalo2 << 16)
				| ((off_t)datahi << 8) | ((off_t)datalo);
			r = ftruncate(fd,pos);
		}

		if (r == 0) {
			bus->write(FILEIO_CSTATUS,STATUS_OK);
		}
		else {
			bus->write(FILEIO_CSTATUS,STATUS_ERR);
		}
	}
}
