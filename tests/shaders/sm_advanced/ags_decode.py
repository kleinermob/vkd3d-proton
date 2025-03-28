import sys

MagicCodeShift = 28
OpcodePhaseShift = 24
DataShift = 8
OpcodeShift = 0

for i in range(1, len(sys.argv)):
    v = int(sys.argv[i])

    magic = (v >> MagicCodeShift) & 0xf
    phase = (v >> OpcodePhaseShift) & 0xf
    data = (v >> DataShift) & 0xffff
    opcode = (v >> OpcodeShift) & 0xff

    print('Code({}, {}, {})'.format(hex(opcode), hex(phase), hex(data)))
