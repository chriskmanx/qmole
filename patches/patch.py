#!/usr/bin/env python3
"""Patch QMole: neutralize beta expiry + start-button grey-out."""
import shutil
from capstone import Cs, CS_ARCH_ARM, CS_MODE_THUMB

SRC = '/tmp/qmole/QMole'
DST = '/tmp/qmole/QMole.patched'
shutil.copyfile(SRC, DST)
data = bytearray(open(DST, 'rb').read())
md = Cs(CS_ARCH_ARM, CS_MODE_THUMB)

def off(va): return va - 0x4000

def show(va, label, n=3):
    o = off(va)
    out = []
    for i in md.disasm(bytes(data[o:o+n*4+4]), va):
        out.append('%06x %s %s' % (i.address, i.mnemonic, i.op_str))
        if len(out) >= n: break
    print('[%s]\n  %s' % (label, '\n  '.join(out)))

print('=== BEFORE ===')
show(0xa884, 'gate1 branch', 2)
show(0xa922, 'gate2 tst', 2)
show(0x9abc, 'IsMarkedExpired', 3)
show(0x9a48, 'MarkExpired', 3)

# 1) 0xa884: bpl #0xa89e (D50B) -> b #0xa89e (E00B): change cond byte
assert data[off(0xa884):off(0xa884)+2] == b'\x0b\xd5', data[off(0xa884):off(0xa884)+2].hex()
data[off(0xa884)+1] = 0xE0

# 2) 0xa922: tst.w r0,#0xff (10 f0 ff 0f) -> cmp r0,r0 (80 42) + nop (00 bf)
assert data[off(0xa922):off(0xa922)+4] == bytes.fromhex('10f0ff0f')
data[off(0xa922):off(0xa922)+4] = bytes.fromhex('8042' + '00bf')

# 3) IsMarkedExpired @0x9abc: -> movs r0,#0; bx lr; nop*
assert data[off(0x9abc):off(0x9abc)+2] == b'\xf0\xb5', data[off(0x9abc):off(0x9abc)+2].hex()
data[off(0x9abc):off(0x9abc)+12] = bytes.fromhex('0020' + '7047' + '00bf'*4)

# 4) MarkExpired @0x9a48: -> bx lr; nop*
assert data[off(0x9a48):off(0x9a48)+8] == bytes.fromhex('b0b502af82b00920')
data[off(0x9a48):off(0x9a48)+8] = bytes.fromhex('7047' + '00bf'*3)

# 5) 0xa89e: ldr r0,[sp,#0x4c] (13 98) -> movs r0,#0 (00 20): date comparison always reads 0
assert data[off(0xa89e):off(0xa89e)+2] == b'\x13\x98', data[off(0xa89e):off(0xa89e)+2].hex()
data[off(0xa89e):off(0xa89e)+2] = bytes.fromhex('0020')

# 6) 0xacbc: ldr r0,[sp,#0x4c] (13 98) -> movs r0,#0 (00 20): same for block 2
assert data[off(0xacbc):off(0xacbc)+2] == b'\x13\x98', data[off(0xacbc):off(0xacbc)+2].hex()
data[off(0xacbc):off(0xacbc)+2] = bytes.fromhex('0020')

open(DST, 'wb').write(bytes(data))

print('=== AFTER ===')
data2 = open(DST, 'rb').read()
md2 = Cs(CS_ARCH_ARM, CS_MODE_THUMB)
for va, n in ((0xa884, 2), (0xa89e, 2), (0xa922, 3), (0x9abc, 3), (0x9a48, 3), (0xacbc, 3)):
    o = va - 0x4000
    out = []
    for i in md2.disasm(data2[o:o+n*4+4], va):
        out.append('%06x %s %s' % (i.address, i.mnemonic, i.op_str))
        if len(out) >= n: break
    print('  %s' % '\n  '.join(out))
print('OK ->', DST)
