# bin2hd - convert binary file to hex dump suitable for
# use with DPuter monitor

import sys


def doDump(iname, oname):
    with open(oname, "w") as outfile:
        with open(iname, "rb") as infile:
            addrlo = infile.read(1)
            addrhi = infile.read(1)
            addr = (int.from_bytes(addrhi) * 256) + int.from_bytes(addrlo)
            data = infile.read()
            length = len(data)
            count = 0
            col = 0

            print("Address: %04X  Size: %d" % (addr, length))
            for value in data:
                if col == 16:
                    print()
                    outfile.write("\n")
                    col = 0
                if col == 0:
                    print("w %04X" % addr, end="")
                    outfile.write("w %04X" % addr)
                print(" %02X" % value, end="")
                outfile.write(" %02X" % value)
                col += 1
                addr += 1
            if col > 0:
                print()
                outfile.write("\n")


if __name__ == "__main__":
    doDump(sys.argv[1], sys.argv[2])
