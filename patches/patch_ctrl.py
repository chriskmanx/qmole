#!/usr/bin/env python3
"""Patch qmole-ctrl: neutralize the 2016-09-01 beta date gate."""
import shutil
shutil.copyfile('/tmp/qmole/qmole-ctrl', '/tmp/qmole/qmole-ctrl.patched')
data = bytearray(open('/tmp/qmole/qmole-ctrl.patched', 'rb').read())

def off(va): return va - 0x1000  # __text: VA 0x2000 <-> file 0x1000

# Gate 1 @ VA 0x3ba0: mov r0,#3 (OCaml int 1); bl Pervasives.exit
c = bytes(data[off(0x3ba0):off(0x3ba0)+8])
print('gate1 bytes @0x3ba0:', c.hex(' '))
assert c == bytes.fromhex('0300a0e3f02b00eb'), c.hex()
data[off(0x3ba0):off(0x3ba0)+8] = bytes.fromhex('e1a00000' + 'e1a00000')  # nop; nop

open('/tmp/qmole/qmole-ctrl.patched', 'wb').write(bytes(data))
print('patched OK')

# verify by disassembly
import subprocess
r = subprocess.run(['objdump', '-d', '--start-address=0x3b90', '--stop-address=0x3bb0',
                    '/tmp/qmole/qmole-ctrl.patched'], capture_output=True, text=True)
print(r.stdout[-600:])
