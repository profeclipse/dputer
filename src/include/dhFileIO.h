//////////////////////////////////////////////////////////////////////////////
// dhFileIO.h
//////////////////////////////////////////////////////////////////////////////
#pragma once
#include <cstdint>

namespace dputer {
inline const uint16_t FILEIO_BASE = 0x0210;
inline const uint16_t FILEIO_CREADY = FILEIO_BASE + 0;
inline const uint16_t FILEIO_CCMD = FILEIO_BASE + 1;
inline const uint16_t FILEIO_CFD = FILEIO_BASE + 2;
inline const uint16_t FILEIO_CMODE = FILEIO_BASE + 3;
inline const uint16_t FILEIO_CDATA_LO = FILEIO_BASE + 4;
inline const uint16_t FILEIO_CDATA_HI = FILEIO_BASE + 5;
inline const uint16_t FILEIO_CDATA_LO2 = FILEIO_BASE + 6;
inline const uint16_t FILEIO_CDATA_HI2 = FILEIO_BASE + 7;
inline const uint16_t FILEIO_CSTATUS = FILEIO_BASE + 8;

inline const uint16_t FILEIO_FILENAME = FILEIO_BASE + 10;

class dhBus;

class dhFileIO {
public:
  enum FILEIO_CFLAGS : uint8_t {
    OPEN = 1,
    CLOSE = 2,
    READ = 3,
    WRITE = 4,
    SEEK = 5,
    DELETE = 6,
    FILEPOS = 7,
    FILESIZE = 8,
    FLUSH = 9,
    RESIZE = 10
  };

  enum FILEIO_MODES : uint8_t {
    MODE_READ = 1,
    MODE_WRITE = 2,
    MODE_RW = 3,
    MODE_BIN = 128
  };

  enum FILEIO_STATUS : uint8_t {
    STATUS_OK = 0x00,
    STATUS_EOF = 0x01,
    STATUS_ERR = 0xff
  };

  dhFileIO();
  ~dhFileIO();

  void attachBus(dhBus *bus) { this->bus = bus; }

  void init();
  void reset();
  void shutdown();

  void tick();

private:
  dhBus *bus;

  void handleControl(uint8_t cmd, uint8_t datalo, uint8_t datahi,
                     uint8_t datalo2, uint8_t datahi2, uint8_t fd,
                     uint8_t mode);
  void doOpen(uint8_t mode);
  void doClose(uint8_t fd);
  void doRead(uint8_t fd);
  void doWrite(uint8_t fd, uint8_t datalo);
  void doSeek(uint8_t fd, uint8_t datalo, uint8_t datahi, uint8_t datalo2,
              uint8_t datahi2);
  void doDelete();
  void doPosition(uint8_t fd);
  void doSize(uint8_t fd);
  void doFlush(uint8_t fd);
  void doResize(uint8_t fd, uint8_t datalo, uint8_t datahi, uint8_t datalo2,
                uint8_t datahi2);
};
} // namespace dputer
