// SPDX-License-Identifier: GPL-2.0-only
//
// Brings back screen auto-rotation on AMD laptops with the Sensor Fusion Hub (amd_sfh), like the
// ASUS ProArt PX13.
//
// The hub's accelerometer HID report descriptor ends with a "motion state" field (usage 0x0451,
// one byte) that the hub never sends, so each 14-byte report is one byte shorter than the 15 the
// descriptor declares. hid-core used to pad such reports with zeros, but since kernel 7.1 ("HID:
// core: Mitigate potential OOB by removing bogus memset()") it drops them, logging "Event data for
// report 1 was too short (15 vs 14)". The accelerometer then never updates, and iio-sensor-proxy
// reads noise.
//
// This removes the field from the descriptor, like the kernel fix "HID: amd_sfh: Remove unsupported
// motion-state field from accelerometer report" (mainline 6ff86122906, for 7.4, tagged for stable).
// Once a kernel carries that fix, the descriptor no longer ends with the field, and the fixup leaves
// it as it is. Installed by scripts/configure_hardware.
//
// The hub's sensors share the IDs 0020:1022:0001 (bus AMD SFH, vendor AMD), so the program is
// matched against all of them, and probe() attaches it to the accelerometer only. The fixup itself
// changes the descriptor only if it still ends with the field.
//
// Author: Danylo Malyuta, 2026.

#include "vmlinux.h"
#include <bpf/bpf_helpers.h>
#include <bpf/bpf_tracing.h>

#define BUS_AMD_SFH 0x20
#define VID_AMD 0x1022
#define PID_SFH 0x0001
#define HID_MAX_DESCRIPTOR_SIZE 4096
#define EINVAL 22

extern __u8 *hid_bpf_get_data(struct hid_bpf_ctx *ctx, unsigned int offset,
			      const size_t __sz) __ksym;

// The devices udev-hid-bpf loads this for, in the layout it reads (HID_BPF_CONFIG(HID_DEVICE(...))
// in udev-hid-bpf's hid_bpf_helpers.h).
union {
	struct {
		__uint(name, 0);
		__uint(bus, BUS_AMD_SFH);
		__uint(group, 0); // Any.
		__uint(vid, VID_AMD);
		__uint(pid, PID_SFH);
	} entry;
} _device_ids SEC(".hid_bpf_config");

// What udev-hid-bpf passes to probe() (struct hid_bpf_probe_args in its hid_bpf.h).
struct hid_bpf_probe_args {
	unsigned int hid;
	unsigned int rdesc_size;
	unsigned char rdesc[HID_MAX_DESCRIPTOR_SIZE];
	int retval;
};

// The end of the accelerometer descriptor: the motion-state field, then End Collection.
static const __u8 motion_state_tail[] = {
	0x0a, 0x51, 0x04, // Usage (Motion State)
	0x15, 0x00,       // Logical Minimum (0)
	0x25, 0x01,       // Logical Maximum (1)
	0x75, 0x08,       // Report Size (8)
	0x95, 0x01,       // Report Count (1)
	0x81, 0x02,       // Input (Data,Var,Abs)
	0xc0,             // End Collection
};
#define TAIL_SIZE sizeof(motion_state_tail)

// How the accelerometer's descriptor starts: Usage Page (Sensor), Usage (Accelerometer 3D). The
// light sensor's has Usage (Ambient Light), 0x41, instead.
static const __u8 accel_head[] = { 0x05, 0x20, 0x09, 0x73 };

// Whether the descriptor of this size ends with the motion-state field. The masks keep the
// verifier sure that each index is within the buffer (size is at most its length).
static __always_inline int has_motion_state(const __u8 *rdesc, __u32 size)
{
	if (size < TAIL_SIZE || size > HID_MAX_DESCRIPTOR_SIZE)
		return 0;
	__u32 start = size - TAIL_SIZE;
	for (__u32 i = 0; i < TAIL_SIZE; i++) {
		if (rdesc[(start + i) & (HID_MAX_DESCRIPTOR_SIZE - 1)] != motion_state_tail[i])
			return 0;
	}
	return 1;
}

SEC("struct_ops/hid_rdesc_fixup")
int BPF_PROG(amd_sfh_accel_rdesc_fixup, struct hid_bpf_ctx *hctx)
{
	__u8 *data = hid_bpf_get_data(hctx, 0, HID_MAX_DESCRIPTOR_SIZE);
	__u32 size = hctx->size;

	if (!data || !has_motion_state(data, size))
		return 0; // Unchanged.

	// Drop the field: End Collection moves to where it started.
	__u32 start = size - TAIL_SIZE;
	data[start & (HID_MAX_DESCRIPTOR_SIZE - 1)] = 0xc0;
	return start + 1;
}

SEC(".struct_ops.link")
struct hid_bpf_ops amd_sfh_accel = {
	.hid_rdesc_fixup = (void *)amd_sfh_accel_rdesc_fixup,
};

// Only fixed offsets here: the verifier refuses a variable one into a syscall program's context.
SEC("syscall")
int probe(struct hid_bpf_probe_args *ctx)
{
	ctx->retval = ctx->rdesc_size >= sizeof(accel_head) &&
		!__builtin_memcmp(ctx->rdesc, accel_head, sizeof(accel_head)) ? 0 : -EINVAL;
	return 0;
}

char _license[] SEC("license") = "GPL";
