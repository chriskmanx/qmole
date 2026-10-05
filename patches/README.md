# QMole Revival Patch — root cause & patch details (2026)

The last shipped QMole beta ("beta2") hard-fails on any modern date. This is **not**
a license-server check — it is a set of **baked-in, offline date gates** compiled
into three separate binaries. QMole was patched to run permanently on a jailbroken
**iPhone 4S, iOS 7.1.2 (11D257)** and this directory contains everything needed to
reproduce it: original binaries, patched binaries, byte-exact patch scripts, and
analysis tooling.

## Root cause: three independent date gates

All three gates compare the current time against early-September-2016 cutoffs and
silently disable or kill QMole when it is in the past:

| # | File | Gate | Behavior when expired |
|---|------|------|----------------------|
| 1 | `/Applications/QMole.app/QMole` (armv7 Mach-O, cryptid 0) | `-[AuthViewController viewDidAppear:]` — two cutoff comparisons at VA `0xa89e` and `0xacbc` (cutoffs `0x57C61DFF` / `0x57C61E00`, i.e. Sept 2 2016); helper functions `IsMarkedExpired` (VA `0x9abc`) and `MarkExpired` (VA `0x9a48`) | Start button stays greyed out; `MarkExpired` also writes the marker file `/var/mobile/Documents/beta2_now.txt` |
| 2 | `/usr/bin/qmole-ctrl` (OCaml, arm Mach-O, `__text` VA `0x2000` ↔ file offset `0x1000`) | `_camlMain__entry` at VA `0x3ba0` → file offset `0x2b00`: `mov r0,#3; bl Pervasives__exit` (`0300a0e3 f02b00eb`) | process exits with code 1, **no output at all** — Xvnc and herbstluftwm never start |
| 3 | `/var/mobile/.config/herbstluftwm/autostart` (OCaml, same VA↔file mapping) | gate at VA `0x3f78` → file offset `0x2f78`: `mov r0,#3; bl Pervasives__exit` (`0300a0e3 3f2b00eb`) | desktop starts but the autostart script exits before launching **bmpanel2** (the panel/taskbar), dzen2 and the wallpaper |

Gate 3 is why the "ribbons & toolbars" (bmpanel2 panel) never appeared: the window
manager itself ran fine, but the autostart binary — which launches the panel,
dzen2, wallpaper (`Esetroot`) and keybinds — was killing itself at boot.

The OCaml binaries compare against the double `1472601600.0` (Sept 1 2016 00:00 UTC,
bits `0x41d5f187 0x80000000`); the app UI gates use `0x57C61DFF`/`0x57C61E00`.

## The patches

ARM/Thumb notes: an ARM `nop` is the word `0xE1A00000` (`mov r0,r0`) =
little-endian bytes `00 00 a0 e1`. In the app binary, file offset = VA − 0x4000.

### 1. `/Applications/QMole.app/QMole` — 6 byte patches (`patches/patch.py`)

| File offset | VA | Original bytes | Patched bytes | Effect |
|---|---|---|---|---|
| `0xa884` | | Thumb-2 `bpl` (`f0 b5 … D5`) | byte `0xD5 → 0xE0` (`b`) | always take the non-expiry branch |
| `0xa922` | | `tst.w` `10 f0 ff 0f` | `movs r0,#0; nop` (`80 42 00 bf`) | skip expiry comparison |
| `0x9abc` | `IsMarkedExpired` prologue `f0 b5` | `movs r0,#0; bx lr; nops` | function always returns false |
| `0x9a48` | `MarkExpired` prologue `b0 b5 02 af 82 b0 09 20` | `bx lr; nops` | never writes the expiry marker |
| `0xa89e` | | `ldr r0,[sp,#0x4c]` (`13 98`) | `movs r0,#0` (`00 20`) | first cutoff check passes |
| `0xacbc` | | `ldr r0,[sp,#0x4c]` (`13 98`) | `movs r0,#0` (`00 20`) | second cutoff check passes |

After patching, re-sign: `ldid -S QMole.patched`.
**Warning:** do *not* retarget the branch at `0xa89c` — `0xad6c` branches back
through `0xa898`, which creates an infinite loop. Only the two `0xa89e`/`0xacbc`
load-compare sites plus the helpers above are safe.

### 2. `/usr/bin/qmole-ctrl` — NOP the exit gate (`patches/patch_ctrl.py`)

File offset `0x2b00`: replace `0300a0e3 f02b00eb` (`mov r0,#3; bl Pervasives__exit`)
with two ARM nops. `qmole-ctrl` now proceeds to start Xvnc (768x1024, depth 16,
VncAuth on :5900) and herbstluftwm.

### 3. `autostart` — NOP the exit gate (`patches/patch_autostart.py`)

File offset `0x2f78`: replace `0300a0e3 3f2b00eb` with two ARM nops
(`00 00 a0 e1` × 2). File size is preserved exactly (340864 bytes); verified by
disassembly (`mov r0,r0`). The autostart now launches bmpanel2, dzen2 and the
wallpaper — the desktop ribbons/toolbars come back.

## Checksums (verify after copying!)

| File | MD5 |
|---|---|
| `QMole` (original) | `de12cc489aaf76b2e0d3eaf21483c530` |
| `QMole.patched` | `ba1d210cd8044324a975a6d634a8dea0` |
| `qmole-ctrl` (original) | `94a7d98775c0989f5002b1e7e488d5da` |
| `qmole-ctrl.patched` | `298f2305d380147186fa73610c0a6039` |
| `autostart` (original) | `9e299ff3ea08171d23563d6446e874ca` |
| `autostart.patched` | `ea55bfc6bbbb6af6f0b34716b7405155` |

## Tooling

- `analyze.py` — minimal Mach-O/ObjC metadata parser (class/method list dump)
- `disasm.py` — capstone-based annotated disassembler (uses `syms.txt`, `QMole`)
- `syms.txt`, `viewDidAppear.txt` — extracted ObjC symbols and the annotated
  `viewDidAppear:` disassembly that located the gates
- `apt-wrapper` — modern `apt` front-end for the device's apt 0.7.25 (see SETUP.md)
- `apt_su_test.exp` — expect script that regression-tests the full
  mobile-user → `su` → `apt install` flow over SSH

## Result (verified on device)

- App launches to the Linux desktop (no more greyed-out Start button)
- Xvnc + herbstluftwm start via `qmole-ctrl`
- bmpanel2 panel running; `/tmp/herbstluft.log` shows the full
  `HERBSTLUFT AUTOSTART START → … → END` sequence
- `apt-get update` fetches from the QMole package repo; `apt-get install` works
