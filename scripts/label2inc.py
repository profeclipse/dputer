# label2inc - convert ld65 lable file to ca65 include file

import sys

def doConvert(iname,oname):
    labels = []
    with open(oname,'w') as outfile:
        with open(iname,'r') as infile:
            for line in infile:
                (junk,addr,name) = line.rstrip('\n').split(' ')
                name = name.removeprefix('.')
                if name[0].isupper(): #name.startswith('__') and not name.startswith('@')
                    addr = addr.removeprefix('00')
                    labels.append([name,addr])
            labels.sort(key=lambda value: value[1])
            for (name,addr) in labels:
                outfile.write('{:<30} = ${}\n'.format(name,addr))

if __name__ == '__main__':
    doConvert(sys.argv[1],sys.argv[2])
