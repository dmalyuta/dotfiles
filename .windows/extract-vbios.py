import struct, sys

table, out, pci, ids = sys.argv[1:5]
bus, slot, func = int(pci[5:7], 16), int(pci[8:10], 16), int(pci[11], 16)
want_vid, want_did = (int(x, 16) for x in ids.split(":"))

d = open(table, "rb").read()
if d[:4] != b"VFCT":
    sys.exit("VFCT table has the wrong signature")

tbl_len = struct.unpack_from("<I", d, 4)[0]
# ACPI header is 36 bytes, then TableUUID[16], then VBIOSImageOffset.
off = struct.unpack_from("<I", d, 52)[0]

image = None
while 0 < off < tbl_len and off + 28 <= len(d):
    b, s, f = struct.unpack_from("<III", d, off)
    vid, did = struct.unpack_from("<HH", d, off + 12)
    length = struct.unpack_from("<I", d, off + 24)[0]
    if length == 0:
        break
    if (b, s, f, vid, did) == (bus, slot, func, want_vid, want_did):
        image = d[off + 28 : off + 28 + length]
        break
    off += 28 + length

if image is None:
    sys.exit(f"no VBIOS for {pci} ({ids}) in the VFCT table")

# Sanity-check it really is a PCI option ROM before trusting it: the
# 0x55AA magic, and a PCIR block whose IDs match the card.
if image[:2] != b"\x55\xaa":
    sys.exit("extracted image is not a PCI option ROM")
pcir = struct.unpack_from("<H", image, 0x18)[0]
if image[pcir : pcir + 4] != b"PCIR":
    sys.exit("extracted image has no PCIR structure")
rvid, rdid = struct.unpack_from("<HH", image, pcir + 4)
if (rvid, rdid) != (want_vid, want_did):
    sys.exit(f"PCIR says {rvid:04x}:{rdid:04x}, wanted {ids}")

open(out, "wb").write(image)
print(f"   {len(image)} bytes, PCIR {rvid:04x}:{rdid:04x}")
