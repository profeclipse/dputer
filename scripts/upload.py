import sys

import serial


def waitForPrompt(ser):
    s = ser.read_until(b"==> ")
    print(s.decode(), end="")


def doUpload(iname):
    ser = serial.Serial(port="COM5", baudrate=19200, timeout=60, parity=serial.PARITY_NONE, stopbits=1)
    with open(iname, "rb") as infile:
        addrlo = infile.read(1)
        addrhi = infile.read(1)
        addr = (int.from_bytes(addrhi) * 256) + int.from_bytes(addrlo)
        data = infile.read()
        length = len(data)
        count = 0
        col = 0

        print("Address: %04X  Size: %d" % (addr, length))
        waitForPrompt(ser)
        for value in data:
            if col == 16:
                ser.write(b"\n")
                col = 0
                waitForPrompt(ser)
            if col == 0:
                ser.write(b"w %04X" % addr)
            ser.write(b" %02X" % value)
            col += 1
            addr += 1
        if col > 0:
            ser.write(b"\n")
            waitForPrompt(ser)
    ser.close()


if __name__ == "__main__":
    doUpload(sys.argv[1])
