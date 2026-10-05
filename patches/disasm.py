#!/usr/bin/env python3
"""Disassemble QMole (armv7) with PIC literal tracking and stub symbol resolution."""
import struct, sys, re
from capstone import Cs, CS_ARCH_ARM, CS_MODE_THUMB, CS_MODE_ARM

PATH = '/tmp/qmole/QMole'
data = open(PATH, 'rb').read()

ncmds = struct.unpack_from('<I', data, 16)[0]
p = 28
segs = []
symtab_off = symtab_n = stroff = None
dysym = None
for _ in range(ncmds):
    cmd, size = struct.unpack_from('<II', data, p)
    if cmd == 0x1:
        segname = data[p+8:p+24].rstrip(b'\0').decode(errors='replace')
        nsects = struct.unpack_from('<I', data, p+48)[0]
        sects = []
        for i in range(nsects):
            so = p+56+i*68
            if so+68 > len(data): break
            sname = data[so:so+16].rstrip(b'\0').decode(errors='replace')
            saddr, ssize = struct.unpack_from('<II', data, so+32)
            soff = struct.unpack_from('<I', data, so+40)[0]
            sects.append((sname, saddr, ssize, soff))
        segs.append((segname, sects))
    elif cmd == 0x2:
        symtab_off = struct.unpack_from('<I', data, p+8)[0]
        symtab_n = struct.unpack_from('<I', data, p+12)[0]
        stroff = struct.unpack_from('<I', data, p+16)[0]
    elif cmd == 0xb:
        indsym_off = struct.unpack_from('<I', data, p+60)[0]
        indsym_n = struct.unpack_from('<I', data, p+64)[0]
        dysym = (indsym_off, indsym_n)
    p += size

def va2off(va):
    for segname, sects in segs:
        for sname, addr, size, foff in sects:
            if size and addr and addr <= va < addr + size:
                return foff + (va - addr)
    return None

def cstr_at_off(o, maxlen=500):
    if o is None or o < 0 or o >= len(data): return None
    e = data.find(b'\0', o)
    if e < 0 or e-o > maxlen or e == o: return None
    try:
        s = data[o:e].decode('utf-8')
        return s if all(31 < ord(c) < 0x3000 for c in s) else None
    except Exception: return None

# symbols
symname_by_value = {}
symname_by_index = []
if symtab_off is not None:
    for i in range(symtab_n):
        o = symtab_off + i*12
        n_strx = struct.unpack_from('<I', data, o)[0]
        n_value = struct.unpack_from('<I', data, o+8)[0]
        no = stroff + n_strx
        e = data.find(b'\0', no)
        nm = data[no:e].decode(errors='replace') if e > no else '?'
        symname_by_index.append(nm)
        if n_value: symname_by_value.setdefault(n_value, nm)

# indirect symbol table -> pointer sections
ind_by_slot = {}
if dysym and dysym[0]:
    indoff, indn = dysym
    inds = [struct.unpack_from('<I', data, indoff + i*4)[0] for i in range(indn)]
    idx = 0
    for segname, sects in segs:
        for sname, addr, size, foff in sects:
            if sname in ('__la_symbol_ptr', '__lazy_symbol', '__nl_symbol_ptr', '__got'):
                n = size // 4
                for k in range(n):
                    if idx + k < len(inds):
                        si = inds[idx+k]
                        if si < len(symname_by_index):
                            ind_by_slot[addr + k*4] = symname_by_index[si]
                idx += n

# __symbolstub1: ARM stubs `ldr ip,[pc,#imm]; bx ip; <literal->lazy slot>`
stubname = {}
for segname, sects in segs:
    for sname, addr, size, foff in sects:
        if sname == '__symbolstub1':
            n = size // 12
            for k in range(n):
                sva = addr + k*12
                o = foff + k*12
                w0 = struct.unpack_from('<I', data, o)[0]
                # ARM LDR pc, [pc, #imm12]: 0xe59ff000 | imm
                if (w0 & 0xFFFFF000) == 0xE59FF000:
                    imm = w0 & 0xFFF
                    lit_addr = sva + 8 + imm
                    lo = va2off(lit_addr)
                    if lo is not None:
                        slot = struct.unpack_from('<I', data, lo)[0]
                        nm = ind_by_slot.get(slot) or ind_by_slot.get(slot & ~3)
                        if nm: stubname[sva] = nm.lstrip('_')

# refs
selref, cfstr, clsref = {}, {}, {}
methname_range = None
for segname, sects in segs:
    for sname, addr, size, foff in sects:
        if sname == '__objc_selrefs':
            for i in range(size // 4):
                pva = struct.unpack_from('<I', data, foff+i*4)[0]
                s = cstr_at_off(va2off(pva))
                if s: selref[addr+i*4] = s
        elif sname == '__cfstring':
            for i in range(size // 16):
                o = foff + i*16
                ptr = struct.unpack_from('<I', data, o+8)[0]
                s = cstr_at_off(va2off(ptr))
                if s: cfstr[addr+i*16] = s
        elif sname == '__objc_classrefs':
            for i in range(size // 4):
                pva = struct.unpack_from('<I', data, foff+i*4)[0]
                o = va2off(pva)
                if o is None: continue
                try:
                    dva = struct.unpack_from('<I', data, o+16)[0]
                    do = va2off(dva)
                    nva = struct.unpack_from('<I', data, do+20)[0]
                    nm = cstr_at_off(va2off(nva))
                    if nm: clsref[addr+i*4] = nm
                except Exception: pass
        elif sname == '__objc_methname':
            methname_range = (addr, size)

def resolve(val):
    if val in selref: return "@sel '%s'" % selref[val]
    if val in cfstr: return '@"%s"' % cfstr[val]
    if val in clsref: return 'class %s' % clsref[val]
    if val in stubname: return 'stub %s' % stubname[val]
    if val in symname_by_value: return '_%s' % symname_by_value[val].lstrip('_')
    if methname_range and methname_range[0] <= val < methname_range[0]+methname_range[1]:
        s = cstr_at_off(va2off(val))
        if s: return "@sel '%s'" % s
    s = cstr_at_off(va2off(val))
    if s: return '"%s"' % s
    return None

syms = {}
try:
    for line in open('/tmp/qmole/syms.txt'):
        parts = line.split(None, 1)
        if parts: syms[int(parts[0], 0)] = parts[1].strip() if len(parts) > 1 else '?'
except FileNotFoundError: pass

md_t = Cs(CS_ARCH_ARM, CS_MODE_THUMB)
md_a = Cs(CS_ARCH_ARM, CS_MODE_ARM)

def disasm(va, count=400):
    base = va & ~1
    md = md_t if (va & 1) else md_a
    off = va2off(base)
    if off is None: return 'bad va 0x%x' % va
    out = []
    regs = {}
    for insn in md.disasm(data[off:off+count*8], base):
        line = '0x%06x  %-8s %s' % (insn.address, insn.mnemonic, insn.op_str)
        mn = insn.mnemonic
        try:
            if mn == 'movw':
                mm = re.match(r'(\w+), #(0x[0-9a-f]+|\d+)', insn.op_str)
                if mm: regs[mm.group(1)] = int(mm.group(2), 0) & 0xFFFF
            elif mn == 'movt':
                mm = re.match(r'(\w+), #(0x[0-9a-f]+|\d+)', insn.op_str)
                if mm:
                    lo16 = regs.get(mm.group(1), 0) & 0xFFFF
                    regs[mm.group(1)] = lo16 | (int(mm.group(2), 0) << 16)
            elif mn == 'add' and ', pc' in insn.op_str:
                mm = re.match(r'(\w+), pc', insn.op_str)
                if mm and mm.group(1) in regs:
                    lit = insn.address + 4 + regs[mm.group(1)]
                    lo = va2off(lit)
                    val = struct.unpack_from('<I', data, lo)[0] if lo is not None and lo+4 <= len(data) else None
                    if lit in cfstr: note = '@"%s"' % cfstr[lit]
                    elif lit in clsref: note = 'classref %s' % clsref[lit]
                    elif lit in selref: note = "@selref '%s'" % selref[lit]
                    else: note = resolve(val) or ('[0x%x]=0x%x' % (lit, val) if val is not None else 'bss')
                    line += '   ; ->0x%x %s' % (lit, note)
                    regs[mm.group(1)] = lit
            elif mn == 'ldr' and '[pc,' in insn.op_str:
                mm = re.search(r'\[pc, #(-?\d+|0x[0-9a-f]+)\]', insn.op_str)
                if mm:
                    lit = ((insn.address + 4) & ~3) + int(mm.group(1), 0)
                    lo = va2off(lit)
                    val = struct.unpack_from('<I', data, lo)[0] if lo is not None and lo+4 <= len(data) else None
                    note = resolve(val) or ('[0x%x]=0x%x' % (lit, val) if val is not None else 'bss')
                    line += '   ; ->0x%x %s' % (lit, note)
            elif mn == 'ldr' and re.match(r'\w+, \[\w+\]$', insn.op_str):
                mm = re.match(r'(\w+), \[(\w+)\]$', insn.op_str)
                if mm and mm.group(2) in regs:
                    lit = regs[mm.group(2)]
                    lo = va2off(lit)
                    val = struct.unpack_from('<I', data, lo)[0] if lo is not None and lo+4 <= len(data) else None
                    note = resolve(val)
                    if note: line += '   ; %s' % note
                    regs[mm.group(1)] = ('DEREF', lit)
            elif mn in ('bl','blx','b','bx','bne','beq','blt','bgt','ble','bge','blez','bgez','cbz','cbnz','bls','bhi','bcs','bcc','blgt','blle'):
                mm = re.search(r'#?(0x[0-9a-f]+)$', insn.op_str)
                if mm:
                    tgt = int(mm.group(1), 16)
                    if tgt in syms: line += '   ; %s' % syms[tgt]
                    elif tgt in stubname: line += '   ; -> %s' % stubname[tgt]
        except Exception:
            pass
        out.append(line)
        if len(out) >= count: break
    return '\n'.join(out)

if __name__ == '__main__':
    print(disasm(int(sys.argv[1], 0), int(sys.argv[2]) if len(sys.argv) > 2 else 300))
