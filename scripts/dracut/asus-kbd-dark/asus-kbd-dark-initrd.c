/*
 * Keep the ASUS laptop's keyboard backlight from pulsing while it starts: scripts/bin/asus-kbd-dark
 * for the initramfs, which has no Python. The same packets, see there for what each is.
 *
 * Why here too: the keyboard controller (N-KEY, 0b05:19b6) forgets "no animation at power-on"
 * whenever it loses power, so only a restart stays dark by itself. After a power-off or a
 * hibernation it pulses until told again, which asus-kbd-dark does 4 s into a boot, and after a
 * hibernation only once the image is loaded, some 20 s in. From the initramfs it is told as soon as
 * hid_asus has the controller, and before a hibernation's image is loaded.
 *
 * Run by asus-kbd-dark-initrd.service. Built and installed by scripts/configure_hardware.
 *
 * Author: Danylo Malyuta, 2026.
 */

#include <fcntl.h>
#include <glob.h>
#include <libgen.h>
#include <limits.h>
#include <linux/hidraw.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <time.h>
#include <unistd.h>

#define REPORT_SIZE 64
#define KBD 0x5A /* the report for keyboard functions */
#define LED 0x5D /* the report for lighting */

/* Whether the controller's hidraw node at this sysfs path has both reports. */
static int has_both_reports(const char *node)
{
	char path[PATH_MAX];
	unsigned char d[4096];
	int kbd = 0, led = 0;

	snprintf(path, sizeof(path), "%s/device/report_descriptor", node);
	int fd = open(path, O_RDONLY);
	if (fd < 0)
		return 0;
	ssize_t n = read(fd, d, sizeof(d));
	close(fd);
	for (ssize_t i = 0; i + 1 < n; i++) {
		if (d[i] == 0x85 && d[i + 1] == KBD)
			kbd = 1;
		if (d[i] == 0x85 && d[i + 1] == LED)
			led = 1;
	}
	return kbd && led;
}

/*
 * The controller's hidraw node that has both reports, as /dev/hidrawN in dev, and whether hid_asus
 * is its driver. Returns 0 if there is none.
 */
static int find_hidraw(char *dev, size_t size, int *is_asus)
{
	glob_t g;
	int found = 0;

	if (glob("/sys/class/hidraw/hidraw*", 0, NULL, &g) != 0)
		return 0;
	for (size_t i = 0; i < g.gl_pathc && !found; i++) {
		char link[PATH_MAX], real[PATH_MAX];

		snprintf(link, sizeof(link), "%s/device", g.gl_pathv[i]);
		if (!realpath(link, real) || !strstr(real, ":0B05:19B6.") ||
		    !has_both_reports(g.gl_pathv[i]))
			continue;
		snprintf(dev, size, "/dev/%s", basename(g.gl_pathv[i]));
		snprintf(link, sizeof(link), "%s/device/driver", g.gl_pathv[i]);
		*is_asus = realpath(link, real) && strcmp(basename(real), "asus") == 0;
		found = 1;
	}
	globfree(&g);
	return found;
}

static void pause_ms(long ms)
{
	struct timespec t = { ms / 1000, ms % 1000 * 1000000L };

	nanosleep(&t, NULL);
}

/* Send a feature report, as hid_asus does: as long as given, or padded to the report's size. */
static int feature(int fd, const unsigned char *packet, size_t len, int pad)
{
	unsigned char buf[REPORT_SIZE] = { 0 };

	memcpy(buf, packet, len);
	return ioctl(fd, HIDIOCSFEATURE(pad ? REPORT_SIZE : len), buf) < 0 ? -1 : 0;
}

/* Send an output report, as asusd does, but in full length. */
static int output(int fd, const unsigned char *packet, size_t len)
{
	unsigned char buf[REPORT_SIZE] = { 0 };

	memcpy(buf, packet, len);
	return write(fd, buf, sizeof(buf)) < 0 ? -1 : 0;
}

int main(void)
{
	/* The same steps, in the same order, as asus-kbd-dark at brightness 0. */
	static const struct {
		const char *name;
		int kind; /* 0: feature as is, 1: feature padded, 2: output */
		size_t len;
		unsigned char packet[16];
	} steps[] = {
		{ "handshake", 0, 16, { KBD, 'A', 'S', 'U', 'S', ' ', 'T', 'e', 'c', 'h', '.', 'I', 'n', 'c', '.', 0 } },
		{ "handshake", 0, 16, { LED, 'A', 'S', 'U', 'S', ' ', 'T', 'e', 'c', 'h', '.', 'I', 'n', 'c', '.', 0 } },
		{ "out-of-box mode", 0, 6, { KBD, 0x05, 0x20, 0x31, 0x00, 0x08 } },
		{ "out-of-box mode", 0, 6, { KBD, 0xBA, 0xC5, 0xC4, 0x00, 0x00 } },
		{ "out-of-box mode", 0, 6, { KBD, 0xD0, 0x8F, 0x01, 0x00, 0x00 } },
		{ "out-of-box mode", 0, 6, { KBD, 0xD0, 0x85, 0xFF, 0x00, 0x00 } },
		{ "animations", 2, 8, { LED, 0xBD, 0x01, 0x08, 0x00, 0x00, 0x00, 0xFF } },
		{ "brightness", 1, 5, { KBD, 0xBA, 0xC5, 0xC4, 0 } },
	};
	char dev[64], failed[256] = "", message[512];
	int is_asus = 0, found = 0;
	struct timespec now;

	/*
	 * Wait for hid_asus to have the controller (it takes it over from hid-generic 0.3 s later), then
	 * for it to be done with it: talking to the controller meanwhile makes its own setup fail, see
	 * asus-kbd-rebind. Without hid_asus after 2 s, go ahead with the driver that is there.
	 */
	for (int i = 0; i < 20; i++) {
		found = find_hidraw(dev, sizeof(dev), &is_asus);
		if (found && is_asus)
			break;
		pause_ms(100);
	}
	int log = open("/dev/kmsg", O_WRONLY);
	if (!found) {
		if (log >= 0)
			dprintf(log, "asus-kbd-dark: no ASUS keyboard controller found in the initramfs\n");
		return 0;
	}
	if (is_asus)
		pause_ms(300);

	int fd = open(dev, O_RDWR);
	if (fd < 0) {
		if (log >= 0)
			dprintf(log, "asus-kbd-dark: can't open %s in the initramfs\n", dev);
		return 0;
	}
	for (size_t i = 0; i < sizeof(steps) / sizeof(steps[0]); i++) {
		int error = steps[i].kind == 2
			? output(fd, steps[i].packet, steps[i].len)
			: feature(fd, steps[i].packet, steps[i].len, steps[i].kind);

		if (error && strlen(failed) + strlen(steps[i].name) + 3 < sizeof(failed)) {
			strcat(failed, failed[0] ? ", " : "");
			strcat(failed, steps[i].name);
		}
		pause_ms(5);
	}
	close(fd);

	clock_gettime(CLOCK_MONOTONIC, &now);
	snprintf(message, sizeof(message),
		 "asus-kbd-dark: keyboard backlight set to 0 on %s in the initramfs%s, %.1f s after boot%s%s\n",
		 dev, is_asus ? "" : " (without hid_asus)", now.tv_sec + now.tv_nsec / 1e9,
		 failed[0] ? ", failed: " : "", failed);
	if (log >= 0)
		dprintf(log, "%s", message);
	return 0;
}
