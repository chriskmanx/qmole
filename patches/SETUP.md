# QMole Setup Guide — jailbroken iPhone 4S, iOS 7.1.2 (2026 revival)

End-to-end guide to get the patched QMole running on a jailbroken iPhone 4S over
USB, including the desktop panels and the package manager. All steps were
verified on: iPhone 4S (N94AP), iOS 7.1.2 (11D257), OpenSSH + Cydia toolchain.

## Prerequisites

On the device (via Cydia):
- OpenSSH
- Core utilities (`su`, `dpkg`, `apt`, `ldid` if you re-patch)

On your Mac (Homebrew):
```bash
brew install libimobiledevice sshpass
```

## 1. SSH over USB (iproxy)

The USB tunnel **must be restarted in every new session** — it does not survive
disconnects. Modern libimobiledevice uses the `-l` listen syntax (the old
positional syntax `iproxy 2222 22` fails with `Invalid listen port`):

```bash
iproxy -l 2222:22 -l 5900:5900 &      # 2222 -> ssh, 5900 -> VNC
```

Connect (password is the classic `alpine`; on these old builds the RSA host key
algorithm must be requested explicitly):

```bash
sshpass -p alpine ssh -p 2222 \
  -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null \
  -o HostKeyAlgorithms=ssh-rsa root@localhost
```

## 2. Deploy the patched binaries

Pre-patched binaries live in this directory (`*.patched` + the `apt-wrapper`).
Copy them to the device and fix ownership/perms — the app controller must be
`root:admin 755`, the herbstluftwm autostart must be `mobile:mobile 755` or the
desktop cannot execute it:

```bash
scp -P 2222 patches/QMole.patched       root@localhost:/Applications/QMole.app/QMole
scp -P 2222 patches/qmole-ctrl.patched  root@localhost:/usr/bin/qmole-ctrl
scp -P 2222 patches/autostart.patched   root@localhost:/var/mobile/.config/herbstluftwm/autostart
scp -P 2222 patches/apt-wrapper         root@localhost:/usr/bin/apt

ssh -p 2222 root@localhost '
  chown root:admin   /Applications/QMole.app/QMole /usr/bin/qmole-ctrl
  chmod 755          /Applications/QMole.app/QMole /usr/bin/qmole-ctrl
  chown mobile:mobile /var/mobile/.config/herbstluftwm/autostart
  chmod 755          /var/mobile/.config/herbstluftwm/autostart
  chown root:wheel   /usr/bin/apt && chmod 755 /usr/bin/apt
  rm -f /var/mobile/Documents/beta2_now.txt
  md5 /Applications/QMole.app/QMole /usr/bin/qmole-ctrl \
      /var/mobile/.config/herbstluftwm/autostart
'
```

Verify the three md5s against the table in [README.md](README.md).
`beta2_now.txt` is the expiry marker written by the original `MarkExpired` —
the patched binary never writes it again, but delete any leftover copy.
Keep `.orig.bak` copies of the original files if you still have them.

Re-patching from scratch instead? Run `patch.py`, `patch_ctrl.py` and
`patch_autostart.py` against the originals (they assert original bytes before
patching and verify after), then re-sign the app binary with `ldid -S`.

## 3. Launch

**Launch from SpringBoard** (tap the icon, or `uiopen 'qmole://'`). Do *not*
launch the app binary directly as the mobile user
(`su mobile -c /Applications/QMole.app/QMole`) — that path SIGABRTs; it is not
an app bug, the direct-launch method is just unsupported.

Sanity check over SSH after ~15 s:

```bash
ssh -p 2222 root@localhost '
  ps aux | grep -E "QMole|Xvnc|herbstluftwm|bmpanel2|dzen2" | grep -v grep
  cat /tmp/qmole.log /tmp/herbstluft.log | tail -30
'
```

Expected: `QMole`, `Xvnc -geometry 768x1024 -depth 16`, `herbstluftwm`,
`bmpanel2` running; herbstluft log shows
`HERBSTLUFT AUTOSTART START … bmpanel theme parsing … HERBSTLUFT AUTOSTART END`.
The bmpanel2 panel (the desktop "ribbons & toolbars") is visible on screen.
VNC: `localhost:5900` through the same iproxy tunnel (password file
`/var/mobile/.vnc/passwd`, set with `vncpasswd`).

## 4. Package management (apt / dpkg)

The userland is managed by Cydia's dpkg/apt. Inside QMole's terminal you are the
`mobile` user, so raw `apt-get` fails with
`E: Could not open lock file /var/lib/apt/lists/lock - open (13: Permission denied)`.
Two ways to install packages:

**Option A — the `apt` wrapper (recommended).** This repo's `apt-wrapper` is
installed as `/usr/bin/apt` in step 2 and gives you modern syntax plus automatic
root escalation (it prompts once for the root password, `alpine`):

```bash
apt update
apt install nano          # any package
apt search <term>
```

**Option B — classic.** Become root first, then use apt-get (the device ships
apt 0.7.25 — there is no unified `apt` binary without the wrapper):

```bash
su                        # password: alpine
apt-get update
apt-get install <pkg>
```

Notes:
- QMole's own package repo is mirrored at `http://qmole.pj568.top/` (the ~97
  `uk.qmole.*` packages that make up the Linux userland). `dpkg -l | grep qmole`
  lists what is installed; original `.deb` files live in `/var/mobile/Documents`.
- Harmless update noise: GPG `NO_PUBKEY` warnings on `cydia.preloading.dev` and
  `repo.tihmstar.net`, and a 404 from the long-dead `repo666.ultrasn0w.com`.
  Neither blocks installs.

## Troubleshooting

| Symptom | Cause / fix |
|---|---|
| `iproxy` connection refused | tunnel died between sessions — restart it (step 1) |
| ssh `Permission denied, please try again` spam | transient auth flake; retry the same command |
| Start button greyed out | app binary not patched / wrong md5 — recheck checksums; make sure `beta2_now.txt` is gone |
| `qmole-ctrl` exits silently, no Xvnc | gate 2 not patched (exit 1 with no output is its signature) |
| Desktop starts but no panels | gate 3 (autostart) not patched; check `/tmp/herbstluft.log` stops after `AUTOSTART START` |
| `apt`/`apt-get` says Permission denied | you are `mobile` — use the wrapper or `su` first (step 4) |
| App SIGABRT on launch | you launched the binary directly as mobile; use SpringBoard/uiopen |
