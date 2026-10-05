#!/usr/bin/env python3
"""Parse 32-bit arm Mach-O: sections + ObjC metadata, map imp addresses."""
import struct, sys, bisect, re

PATH = '/tmp/qmole/QMole'
data = open(PATH, 'rb').read()
magic = struct.unpack_from('<I', data, 0)[0]
endian = '<' if magic in (0xfeedface, 0xfeedfacf) else '>'
is64 = magic in (0xfeedfacf, 0xcffaedfe)

ncmds = struct.unpack_from(endian+'I', data, 16)[0]
segs = []
p = 32 if is64 else 28
for _ in range(ncmds):
    cmd, size = struct.unpack_from(endian+'II', data, p)
    if cmd == 0x1 and not is64:
        segname = data[p+8:p+24].rstrip(b'\0').decode(errors='replace')
        nsects = struct.unpack_from(endian+'I', data, p+48)[0]
        sects = []
        for i in range(nsects):
            so = p + 56 + i*68
            if so + 68 > len(data): break
            sname = data[so:so+16].rstrip(b'\0').decode(errors='replace')
            saddr, ssize = struct.unpack_from(endian+'II', data, so+32)
            soff = struct.unpack_from(endian+'I', data, so+40)[0]
            sects.append((sname, saddr, ssize, soff))
        segs.append((segname, sects))
    p += size

def va2off(va):
    for segname, sects in segs:
        for sname, addr, size, foff in sects:
            if size and addr and addr <= va < addr + size:
                return foff + (va - addr)
    return None

def cstr(va):
    o = va2off(va)
    if o is None: return None
    e = data.find(b'\0', o)
    if e < 0: return None
    return data[o:e].decode(errors='replace')

classes = {}  # imp -> (cls, sel)
for segname, sects in segs:
    for sname, addr, size, foff in sects:
        if sname == '__objc_classlist' and size:
            for i in range(size // 4):
                cptr = struct.unpack_from(endian+'I', data, foff + i*4)[0]
                co = va2off(cptr)
                if co is None: continue
                # 32-bit class (this binary): isa@0 super@4 cachemask@8 cachebkt@12 data@16; metaclass obj @+20, its data @+36
                cand = [struct.unpack_from(endian+'I', data, co+16)[0]]
                if co+40 <= len(data):
                    cand.append(struct.unpack_from(endian+'I', data, co+36)[0])
                for dvoff in cand:
                    do = va2off(dvoff)
                    if do is None: continue
                    # non-fragile ro: name@20 methods@24; fragile: name@16 methods@20
                    for nameoff, mlistoff in ((20, 24), (16, 20)):
                        name_va = struct.unpack_from(endian+'I', data, do+nameoff)[0]
                        mlist_va = struct.unpack_from(endian+'I', data, do+mlistoff)[0]
                        mo = va2off(mlist_va)
                        if mo is None: continue
                        entsize, count = struct.unpack_from(endian+'II', data, mo)
                        if entsize < 12 or entsize > 100 or count == 0 or count > 100000: continue
                        sname_obj = cstr(name_va)
                        if not sname_obj or not sname_obj.replace('_','').isalnum(): continue
                        esz = entsize & ~3
                        for j in range(count):
                            eo = mo + 8 + j*esz
                            if eo+12 > len(data): break
                            namep, typesp, impp = struct.unpack_from(endian+'III', data, eo)
                            s = cstr(namep)
                            classes[impp] = (sname_obj, s or '??')
                        break

imps = sorted(classes)
print('parsed %d method imps' % len(imps))

def report(k):
    i = bisect.bisect_left(imps, k)
    if i < len(imps):
        v = imps[i]
        print('0x%x -> next imp 0x%x  %s[%s]' % (k, v, classes[v][0], classes[v][1]))
    else:
        print('no imp after 0x%x' % k)

for arg in sys.argv[1:]:
    if arg.startswith('grep:'):
        rx = re.compile(arg[5:])
        for v, (c, s) in sorted(classes.items()):
            if rx.search(s) or rx.search(c):
                print('0x%06x %s[%s]' % (v, c, s))
    else:
        report(int(arg, 0))
