# Windows VM Cheat Sheet

Commands for the `windows11` guest — starting it, the three ways to look at it,
and stopping it.

The domain runs under the **system** libvirt instance, so every `virsh` call
needs `--connect qemu:///system`. Worth an alias:

```bash
alias vsh='virsh --connect qemu:///system'
```

---

## 1. Three ways in

Each solves a different problem. **Looking Glass and winapp take turns** —
Windows 11 Pro allows one interactive session, and connecting with winapp takes
it away from the console that Looking Glass is showing.

| | Command | What it is | Use it for |
|---|---|---|---|
| **Looking Glass** | `looking-glass` | The whole desktop, rendered by the passed-through iGPU and moved over shared memory. Nothing is encoded, so there is no compression latency. | Installers, Windows Update, anything needing a real machine. |
| **winapp** | `winapp --list` | Individual programs as ordinary Linux windows, via RDP RemoteApp. Each Windows window becomes a real X11 window that tiles and alt-tabs normally. | Excel, Word, PDF-XChange — day-to-day work. |
| **virt-viewer** | `virt-viewer …` | The SPICE console, independent of the guest's GPU. Survives a guest with no working display driver. | Emergency fallback. Currently blank — see §5. |

---

## 2. Start & stop

You rarely need to start it by hand: both `looking-glass` and `winapp` bring the
VM up themselves if it is off.

```bash
virsh --connect qemu:///system start windows11        # boot the VM
virsh --connect qemu:///system domstate windows11     # running, or shut off?
virsh --connect qemu:///system list --all             # all domains and their state
virsh --connect qemu:///system shutdown windows11     # ask Windows to shut down cleanly
virsh --connect qemu:///system destroy windows11      # pull the plug (only if unresponsive)

# the guest's IP, straight from the guest agent
virsh --connect qemu:///system domifaddr windows11 --source agent
```

> ### ⚠️ Leave the VM running
>
> The iGPU **cannot be reset** once Windows has driven it — its Platform Security
> Processor holds firmware state that only a real power cycle clears. After *any*
> shutdown of the VM, **reboot the host** before starting it again, or Windows
> comes up with error 43 on the AMD adapter and Looking Glass stays black.
>
> No bus reset, driver rebind, or `vendor-reset` avoids this. Proven directly:
> binding the card to `amdgpu` on the host fails at
> `psp_v13_0_ring_create: Failed to wait for trust OS ready`.

---

## 3. Looking Glass

The launcher starts the VM if needed, waits for the shared-memory file, asks
libvirt which SPICE port it landed on, and pins the client to the NVIDIA EGL
vendor — without that last part libglvnd hands the display to Mesa and the whole
desktop renders on the CPU.

```bash
looking-glass                       # connect, starting the VM first if off
looking-glass -- -F                 # start full screen
looking-glass -m KEY_RIGHTALT       # make right Alt the escape key
looking-glass -- win:showFPS=yes    # any client option passes through after --
looking-glass-client --help         # every option the client accepts
```

> ### ⚠️ If looking-glass client quits randomly
>
> This issue could be resolved by setting the environment variable `export
> __NV_DISABLE_EXPLICIT_SYNC=1`. See [GitHub
> issue](https://github.com/gnif/LookingGlass/issues/1151).

### Key bindings

`ScrLk` is the escape key. You can make another key the escape key using
`looking-glass -m KEY_...` (for example, `KEY_RIGHTALT`). Hold it ~200 ms for
the on-screen help menu.

| Key | Action | Key | Action |
|---|---|---|---|
| `ScrLk` | Toggle capture mode | `ScrLk`+`O` | Overlay mode |
| `ScrLk`+`F` | Full screen | `ScrLk`+`I` | SPICE input toggle |
| `ScrLk`+`Q` | Quit | `ScrLk`+`N` | Night vision |
| `ScrLk`+`D` | FPS display | `ScrLk`+`R` | Rotate 90° |
| `ScrLk`+`T` | Frame timing | `ScrLk`+`E` | Audio recording |
| `ScrLk`+`V` | Video stream toggle | `ScrLk`+`F1` | Send Ctrl+Alt+F1 |

**It shows the lock screen after using winapp.** That is the session hand-off,
not a fault — RemoteApp took the console session. Sign in again inside the
Looking Glass window.

---

## 4. winapp — apps as Linux windows

Every discovered program also has an entry in your application menu, with its
real icon extracted from the Windows binary. These are the same thing from a
terminal.

```bash
winapp --list                       # programs that have a launcher entry
winapp --desktop                    # the whole desktop over RDP, in one window
winapp 'C:\Program Files\PDF-XChange\PDF Editor\PXCEditor.exe'
winapp 'C:\…\PXCEditor.exe' ~/Documents/report.pdf    # open a Linux file in it

./make_windows_vm.sh --only 5       # re-scan the guest after installing software
```

**Files.** Anything in `~/Shared` is permanently visible to Windows as drive
`Z:`. A file passed on the command line from anywhere else is shared just for
that session and arrives as `\\tsclient\winapp\…`.

**To close:** close the app's windows — that ends the RDP session and frees the
console again.

---

## 5. virt-viewer

The SPICE console. It talks to the emulated display adapter rather than the
passed-through GPU, so it works even when the guest has no working graphics
driver.

```bash
virt-viewer --connect qemu:///system --domain-name windows11

./make_windows_vm.sh --enable-console   # switch the emulated adapter back on
./make_windows_vm.sh --only 6           # switch it off again
```

> **Expect a blank window today.** Phase 6 disables the emulated adapter on
> purpose, so the guest has exactly one display: with two attached, SPICE's
> pointer coordinates span both screens while Looking Glass shows only one, and
> every click lands somewhere you cannot see.
>
> Run `--enable-console` first if you genuinely need this view — and turn it back
> off afterwards, or the mouse stops lining up in Looking Glass.

---

## 6. Setup script

`~/sw/dotfiles/make_windows_vm.sh`. Every phase is safe to re-run — each checks
for what it creates and skips it.

```bash
./make_windows_vm.sh --status           # which phases are done
./make_windows_vm.sh --only 5           # re-scan installed programs, rebuild launchers
./make_windows_vm.sh --only 6           # re-apply Looking Glass / virtual display setup
./make_windows_vm.sh --enable-console   # bring back the emulated display adapter
./make_windows_vm.sh --help             # all options
```

**Installing Office:** install it through Looking Glass, then run `--only 5`;
Word and Excel pick up launcher entries automatically.

---

## 7. Where things live

| Path | What |
|---|---|
| `~/Shared` | Shared folder — drive `Z:` in Windows |
| `~/Shared/.setup` | Installers staged for the guest; safe to ignore |
| `~/.local/bin/looking-glass` | Launcher — resolves SPICE, fixes EGL vendor |
| `~/.local/bin/winapp` | RemoteApp launcher |
| `~/.local/state/windows-vm/` | Phase markers and the RDP credential (mode 600) |
| `/dev/shm/looking-glass` | The 128 MiB frame buffer, created by QEMU at start |
| `~/sw/dotfiles/make_windows_vm.sh` | The setup script; templates in `.windows/` |

---

## 8. When it breaks

**Device Manager: error 43 on AMD Radeon Graphics**
The iGPU was wedged by a VM restart. Reboot the host — nothing inside the guest
or on the host can recover it.

**`ERRCONNECT_LOGON_FAILURE`**
The stored RDP credential is stale. Most likely the Windows account password
changed — linking it to a Microsoft account does exactly that. Save the current
password to `~/.local/state/windows-vm/windows-password`. Note that a Windows
Hello **PIN is not the password**; RDP will not accept it.

**Looking Glass window is black**
Usually nobody is signed in and you are looking at an unlit logon screen — sign
in inside the window. If it stays black, check the AMD adapter for error 43.

**Mouse hovers but clicks land elsewhere**
The guest has two displays again. Run `--only 6` to disable the emulated adapter.

**Looking Glass feels sluggish**
It fell back to software rendering. Check its log for `llvmpipe`; the launcher
should be pinning `__EGL_VENDOR_LIBRARY_FILENAMES` to the NVIDIA vendor file.

---

Domain `windows11` · 32 GiB · 16 vCPU · AMD iGPU passthrough · Looking Glass B7
