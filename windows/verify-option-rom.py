"""Check that a file really is a PCI option ROM of the expected kind.

    verify-option-rom.py <file> [expected-code-type]

Code types are the ones the PCI firmware specification defines: 0 for a legacy
x86 BIOS image and 3 for a UEFI driver. Passing one makes this exit non-zero
unless some image in the ROM has that type.

Worth doing because the failure it catches is silent. A download that returned
an HTML error page, or stopped half way, is still a file; hand it to QEMU as a
romfile and the guest simply comes up with a graphics device that does not
work, which looks exactly like every other cause of error code 43.

Prints a short description of each image found.

Written by make_windows_vm.sh; edit that instead of this.
"""

import struct
import sys

CODE_TYPES = {
    0: "legacy x86 BIOS",
    1: "Open Firmware",
    2: "HP PA-RISC",
    3: "UEFI driver",
}

EFI_SUBSYSTEMS = {
    10: "EFI application",
    11: "EFI boot service driver",
    12: "EFI runtime driver",
}

EFI_MACHINES = {0x014C: "i386", 0x8664: "x86_64", 0xAA64: "aarch64"}


def images(data):
    """Walk the chain of images a PCI option ROM is made of."""
    offset = 0
    while offset + 0x20 <= len(data):
        if data[offset:offset + 2] != b"\x55\xaa":
            raise ValueError(f"no 0x55AA signature at {offset:#x}")

        # An EFI image keeps the pointer to its PCI data structure at 0x18,
        # same as a legacy one, but also carries an EFI header before it. The
        # pointer is relative to the image, not to the file, which matters
        # from the second image of a chain onwards.
        pcir_offset = struct.unpack_from("<H", data, offset + 0x18)[0]
        pcir = offset + pcir_offset
        if data[pcir:pcir + 4] != b"PCIR":
            raise ValueError(f"no PCIR structure at {pcir:#x}")

        vendor, device = struct.unpack_from("<HH", data, pcir + 4)
        length = struct.unpack_from("<H", data, pcir + 0x10)[0] * 512
        code_type = data[pcir + 0x14]
        last = bool(data[pcir + 0x15] & 0x80)

        info = {
            "offset": offset,
            "length": length,
            "vendor": vendor,
            "device": device,
            "code_type": code_type,
            "last": last,
        }

        if code_type == 3:
            efi_sig, = struct.unpack_from("<I", data, offset + 4)
            subsystem, machine, compressed = struct.unpack_from(
                "<HHH", data, offset + 8)
            info.update(efi_signature=efi_sig, subsystem=subsystem,
                        machine=machine, compressed=compressed)

        yield info

        if last or length == 0:
            return
        offset += length


def main():
    if len(sys.argv) < 2:
        sys.exit(__doc__)

    path = sys.argv[1]
    want = int(sys.argv[2]) if len(sys.argv) > 2 else None

    try:
        data = open(path, "rb").read()
    except OSError as exc:
        print(f"cannot read {path}: {exc}", file=sys.stderr)
        return 1

    if len(data) < 512:
        print(f"{path} is only {len(data)} bytes; not an option ROM",
              file=sys.stderr)
        return 1

    try:
        found = list(images(data))
    except (ValueError, struct.error) as exc:
        print(f"{path} is not a valid option ROM: {exc}", file=sys.stderr)
        return 1

    for image in found:
        kind = CODE_TYPES.get(image["code_type"], "unknown")
        line = (f"   image at {image['offset']:#x}: {image['length']} bytes, "
                f"{image['vendor']:04x}:{image['device']:04x}, {kind}")
        if image["code_type"] == 3:
            ok = "ok" if image.get("efi_signature") == 0x0EF1 else "BAD"
            line += (f" [{EFI_SUBSYSTEMS.get(image['subsystem'], '?')}, "
                     f"{EFI_MACHINES.get(image['machine'], '?')}, "
                     f"signature {ok}]")
        print(line)

    if want is not None and not any(i["code_type"] == want for i in found):
        print(f"{path} has no {CODE_TYPES.get(want, want)} image",
              file=sys.stderr)
        return 1

    return 0


if __name__ == "__main__":
    sys.exit(main())
