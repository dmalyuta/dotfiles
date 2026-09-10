"""Join several PCI option ROMs into one chained ROM.

    combine-option-roms.py <out> <vendor:device> <rom> [<rom> ...]

A PCI expansion ROM is a chain of images, each introduced by a 0x55AA header
and described by a PCIR structure that says how long it is and whether it is
the last one. Firmware walks the chain and runs whichever image it understands:
a UEFI machine wants a code type 3 image, a legacy BIOS wants type 0.

That is why this exists. An integrated Radeon's VBIOS, as it appears in the
ACPI VFCT table, holds only the legacy image. Under OVMF nothing in it can be
run, the card is never posted, and Windows reports error code 43 on a device
that is otherwise passed through correctly. Chaining a separate GOP driver
behind the VBIOS gives each firmware the image it is looking for.

Two edits are needed to make a chain out of files that were each written to
stand alone:

  * every image but the last must have its end-of-chain flag cleared, or the
    walk stops at the first one;
  * each image's PCIR vendor and device ID is rewritten to the card the ROM is
    being built for, because a GOP driver salvaged from someone else's
    firmware carries whatever IDs that firmware used.

Written by make_windows_vm.sh; edit that instead of this.
"""

import struct
import sys


def image_span(data):
    """Offset and length of the first image in data, with its PCIR offset."""
    if data[:2] != b"\x55\xaa":
        raise ValueError("no 0x55AA signature")
    pcir = struct.unpack_from("<H", data, 0x18)[0]
    if data[pcir:pcir + 4] != b"PCIR":
        raise ValueError(f"no PCIR structure at {pcir:#x}")
    length = struct.unpack_from("<H", data, pcir + 0x10)[0] * 512
    if not length:
        raise ValueError("PCIR declares a zero-length image")
    return pcir, length


def main():
    if len(sys.argv) < 4:
        sys.exit(__doc__)

    out, ident, paths = sys.argv[1], sys.argv[2], sys.argv[3:]
    try:
        vendor, device = (int(x, 16) for x in ident.split(":"))
    except ValueError:
        print(f"{ident} is not a vendor:device pair", file=sys.stderr)
        return 1

    chain = bytearray()
    for index, path in enumerate(paths):
        try:
            data = bytearray(open(path, "rb").read())
        except OSError as exc:
            print(f"cannot read {path}: {exc}", file=sys.stderr)
            return 1

        try:
            pcir, length = image_span(data)
        except (ValueError, struct.error) as exc:
            print(f"{path} is not a usable option ROM: {exc}", file=sys.stderr)
            return 1

        # The declared length is what the next image's offset is computed
        # from, so a file with anything after it would push the chain out of
        # step. Trim to what the header says, and refuse to invent bytes the
        # file does not have.
        if len(data) < length:
            print(f"{path} declares {length} bytes but holds {len(data)}",
                  file=sys.stderr)
            return 1
        data = data[:length]

        struct.pack_into("<HH", data, pcir + 4, vendor, device)

        last = index == len(paths) - 1
        if last:
            data[pcir + 0x15] |= 0x80
        else:
            data[pcir + 0x15] &= ~0x80

        chain += data

    try:
        with open(out, "wb") as fh:
            fh.write(chain)
    except OSError as exc:
        print(f"cannot write {out}: {exc}", file=sys.stderr)
        return 1

    print(f"   {len(paths)} images, {len(chain)} bytes -> {out}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
