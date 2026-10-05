#!/usr/bin/env python3
"""Patch autostart: neutralize the 2016-08-31 beta date gate. (corrected)"""
import shutil
shutil.copyfile('/tmp/qmole/autostart', '/tmp/qmole/autostart.patched')
data = bytearray(open('/tmp/qmole/autostart.patched', 'rb').read())

# VA 0x3f78 -> file offset 0x2f78 (__text VA 0x2000 <-> file 0x1000)
c = bytes(data[0x2f78:0x2f80])
print('gate bytes @file 0x2f78:', c.hex(' '))
assert c == bytes.fromhex('0300a0e33f2b00eb'), c.hex()
nop = bytes.fromhex('0000a0e1')  # ARM "mov r0,r0" = word 0xE1A00000, little-endian bytes 00 00 A0 E1
assert len(nop) == 4
data[0x2f78:0x2f80] = nop + nop
open('/tmp/qmole/autostart.patched', 'wb').write(bytes(data))
assert len(data) == len(open('/tmp/qmole/autostart','rb').read()), 'size changed!'
print('patched OK, size preserved:', len(data))

import subprocess
r = subprocess.run(['objdump', '-d', '--start-address=0x3f6c', '--stop-address=0x3f88',
                    '/tmp/qmole/autostart.patched'], capture_output=True, text=True)
print(r.stdout[-420:])
