import re, struct, sys

with open(sys.argv[1], "rb") as f:
    # The XML resource header sits at 0x48 in the WIM header:
    # a packed size whose top byte is flags, then the offset
    # of the blob within the file.
    f.seek(0x48)
    packed, offset, _ = struct.unpack("<QQQ", f.read(24))
    f.seek(offset)
    blob = f.read(packed & 0xFFFFFFFFFFFF)

text = blob.decode("utf-16-le", errors="replace")
for name in re.findall(r"<NAME>(.*?)</NAME>", text, re.S):
    print(name)
