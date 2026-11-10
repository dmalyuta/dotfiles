/* Replay recorded touchpad events through libinput and print what it makes of them.
 *
 * Needs no root and touches nothing: libinput gets a pipe instead of the touchpad's event node,
 * and this program answers the ioctls that libinput (libevdev) asks a real node, from the
 * recording's description of the device. The device's udev properties are the real touchpad's,
 * whose node is given, so the library (the installed one, or another by LD_LIBRARY_PATH) treats
 * the events exactly as it does in KWin, timers included: the replay runs at the recorded pace.
 *
 * Usage: replay NODE [OPTION=VALUE...] < EVENTS
 *   NODE     the touchpad's node, e.g. /dev/input/event15 (only looked up, never opened)
 *   OPTIONS  tap=1 drag=1 draglock=0 natural=1 dwt=1 speed=0.0 profile=adaptive|flat
 *            verbose=1 (libinput's debug log, as L lines)
 *   EVENTS   lines, as written by `touchpad-trace convert`:
 *              N name / I bustype vendor product version / P property...
 *              B type code... / A code min max fuzz flat resolution
 *              E usec type code value    (usec: since the recording's start)
 *
 * Prints one line per libinput event, with the recording's time in usec:
 *   M usec dx dy udx udy          pointer motion, and unaccelerated
 *   S usec v h source             scroll (finger), vertical and horizontal; source f(inger)/c(ontinuous)
 *   B usec button state           button
 *   G usec type fingers dx dy     gesture (swipe/pinch/hold begin, update, end)
 *   L usec text                   libinput's debug log (with verbose=1): its state changes
 *
 * Built and run by scripts/touchpad/touchpad-trace.
 */
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <libinput.h>
#include <linux/input.h>
#include <poll.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <time.h>
#include <unistd.h>

#define BITS_PER_LONG (8 * sizeof(unsigned long))
#define NLONGS(n) (((n) + BITS_PER_LONG - 1) / BITS_PER_LONG)

static struct {
	int fd; /* the read end of the pipe, libinput's "device" */
	const char *node;
	char name[256];
	struct input_id id;
	unsigned long props[NLONGS(INPUT_PROP_CNT)];
	unsigned long types[NLONGS(EV_CNT)];
	unsigned long codes[EV_CNT][NLONGS(KEY_CNT)];
	struct input_absinfo abs[ABS_CNT];
} dev = { .fd = -1 };

static void
set_bit(unsigned long *bits, unsigned bit)
{
	bits[bit / BITS_PER_LONG] |= 1UL << (bit % BITS_PER_LONG);
}

static int
copy_out(void *arg, size_t arg_size, const void *data, size_t data_size)
{
	size_t n = arg_size < data_size ? arg_size : data_size;

	memset(arg, 0, arg_size);
	memcpy(arg, data, n);
	return (int)n;
}

/* The evdev ioctls that libevdev and libinput use, for our pipe only. */
int
ioctl(int fd, unsigned long request, ...)
{
	va_list args;
	void *arg;
	unsigned nr = _IOC_NR(request), size = _IOC_SIZE(request);

	va_start(args, request);
	arg = va_arg(args, void *);
	va_end(args);

	if (fd != dev.fd || dev.fd < 0 || _IOC_TYPE(request) != 'E')
		return (int)syscall(SYS_ioctl, fd, request, arg);

	if (request == EVIOCGVERSION) {
		*(int *)arg = EV_VERSION;
		return 0;
	}
	if (request == EVIOCGID) {
		*(struct input_id *)arg = dev.id;
		return 0;
	}
	if (request == EVIOCSCLOCKID || request == EVIOCGRAB)
		return 0;
	if (request == EVIOCGREP) {
		((unsigned *)arg)[0] = 250;
		((unsigned *)arg)[1] = 33;
		return 0;
	}
	if (_IOC_DIR(request) == _IOC_READ) {
		if (nr == _IOC_NR(EVIOCGNAME(0)))
			return copy_out(arg, size, dev.name, strlen(dev.name) + 1);
		if (nr == _IOC_NR(EVIOCGPHYS(0)) || nr == _IOC_NR(EVIOCGUNIQ(0))) {
			errno = ENOENT;
			return -1;
		}
		if (nr == _IOC_NR(EVIOCGPROP(0)))
			return copy_out(arg, size, dev.props, sizeof(dev.props));
		/* The state of keys, LEDs, sounds and switches: all off */
		if (nr >= _IOC_NR(EVIOCGKEY(0)) && nr <= _IOC_NR(EVIOCGSW(0))) {
			memset(arg, 0, size);
			return (int)size;
		}
		if (nr == _IOC_NR(EVIOCGMTSLOTS(0))) {
			/* [code, value of each slot...]: no touches */
			int code = *(int *)arg;
			int *values = (int *)arg + 1;
			size_t i, n = size / sizeof(int) - 1;

			for (i = 0; i < n; i++)
				values[i] = code == ABS_MT_TRACKING_ID ? -1 : 0;
			return 0;
		}
		if (nr == _IOC_NR(EVIOCGBIT(0, 0)))
			return copy_out(arg, size, dev.types, sizeof(dev.types));
		if (nr > _IOC_NR(EVIOCGBIT(0, 0)) && nr < _IOC_NR(EVIOCGBIT(EV_CNT, 0)))
			return copy_out(arg,
					size,
					dev.codes[nr - _IOC_NR(EVIOCGBIT(0, 0))],
					sizeof(dev.codes[0]));
		if (nr >= _IOC_NR(EVIOCGABS(0)) && nr < _IOC_NR(EVIOCGABS(ABS_CNT))) {
			*(struct input_absinfo *)arg = dev.abs[nr - _IOC_NR(EVIOCGABS(0))];
			return 0;
		}
	}
	errno = EINVAL;
	return -1;
}

/* libinput checks that what it opened is the device it asked for. */
int
fstat(int fd, struct stat *st)
{
	if (fd == dev.fd && dev.fd >= 0)
		return stat(dev.node, st);
	return (int)syscall(SYS_fstat, fd, st);
}

int
fstat64(int fd, struct stat64 *st)
{
	return fstat(fd, (struct stat *)st);
}

static int
open_restricted(const char *path, int flags, void *user_data)
{
	return dev.fd;
}

static void
close_restricted(int fd, void *user_data)
{
}

static const struct libinput_interface interface = {
	.open_restricted = open_restricted,
	.close_restricted = close_restricted,
};

static int64_t
now_usec(void)
{
	struct timespec ts;

	clock_gettime(CLOCK_MONOTONIC, &ts);
	return (int64_t)ts.tv_sec * 1000000 + ts.tv_nsec / 1000;
}

struct event {
	int64_t usec;
	uint16_t type, code;
	int32_t value;
};

static int64_t start; /* now_usec() of the recording's time 0 */

static void
print_events(struct libinput *li)
{
	struct libinput_event *event;

	libinput_dispatch(li);
	while ((event = libinput_get_event(li))) {
		enum libinput_event_type type = libinput_event_get_type(event);
		struct libinput_event_pointer *p;
		struct libinput_event_gesture *g;

		switch (type) {
		case LIBINPUT_EVENT_POINTER_MOTION:
			p = libinput_event_get_pointer_event(event);
			printf("M %lld %.4f %.4f %.4f %.4f\n",
			       (long long)libinput_event_pointer_get_time_usec(p) - start,
			       libinput_event_pointer_get_dx(p),
			       libinput_event_pointer_get_dy(p),
			       libinput_event_pointer_get_dx_unaccelerated(p),
			       libinput_event_pointer_get_dy_unaccelerated(p));
			break;
		case LIBINPUT_EVENT_POINTER_SCROLL_FINGER:
		case LIBINPUT_EVENT_POINTER_SCROLL_CONTINUOUS:
			p = libinput_event_get_pointer_event(event);
			printf("S %lld %.4f %.4f %c\n",
			       (long long)libinput_event_pointer_get_time_usec(p) - start,
			       libinput_event_pointer_has_axis(p, LIBINPUT_POINTER_AXIS_SCROLL_VERTICAL)
				       ? libinput_event_pointer_get_scroll_value(
						 p,
						 LIBINPUT_POINTER_AXIS_SCROLL_VERTICAL)
				       : 0.0,
			       libinput_event_pointer_has_axis(p, LIBINPUT_POINTER_AXIS_SCROLL_HORIZONTAL)
				       ? libinput_event_pointer_get_scroll_value(
						 p,
						 LIBINPUT_POINTER_AXIS_SCROLL_HORIZONTAL)
				       : 0.0,
			       type == LIBINPUT_EVENT_POINTER_SCROLL_FINGER ? 'f' : 'c');
			break;
		case LIBINPUT_EVENT_POINTER_BUTTON:
			p = libinput_event_get_pointer_event(event);
			printf("B %lld %u %d\n",
			       (long long)libinput_event_pointer_get_time_usec(p) - start,
			       libinput_event_pointer_get_button(p),
			       libinput_event_pointer_get_button_state(p));
			break;
		case LIBINPUT_EVENT_GESTURE_SWIPE_BEGIN:
		case LIBINPUT_EVENT_GESTURE_SWIPE_UPDATE:
		case LIBINPUT_EVENT_GESTURE_SWIPE_END:
		case LIBINPUT_EVENT_GESTURE_PINCH_BEGIN:
		case LIBINPUT_EVENT_GESTURE_PINCH_UPDATE:
		case LIBINPUT_EVENT_GESTURE_PINCH_END:
		case LIBINPUT_EVENT_GESTURE_HOLD_BEGIN:
		case LIBINPUT_EVENT_GESTURE_HOLD_END: {
			static const char *names[] = {
				"swipe-begin", "swipe-update", "swipe-end", "pinch-begin",
				"pinch-update", "pinch-end", "hold-begin", "hold-end",
			};
			bool moves = type == LIBINPUT_EVENT_GESTURE_SWIPE_UPDATE ||
				     type == LIBINPUT_EVENT_GESTURE_PINCH_UPDATE;

			g = libinput_event_get_gesture_event(event);
			printf("G %lld %s %d %.4f %.4f\n",
			       (long long)libinput_event_gesture_get_time_usec(g) - start,
			       names[type - LIBINPUT_EVENT_GESTURE_SWIPE_BEGIN],
			       libinput_event_gesture_get_finger_count(g),
			       moves ? libinput_event_gesture_get_dx(g) : 0.0,
			       moves ? libinput_event_gesture_get_dy(g) : 0.0);
			break;
		}
		default:
			break;
		}
		libinput_event_destroy(event);
	}
}

/* Let libinput work (its timers too) until the time in usec, or for as long as it has to. */
static void
run_until(struct libinput *li, int64_t until)
{
	struct pollfd pfd = { .fd = libinput_get_fd(li), .events = POLLIN };

	for (;;) {
		int64_t left = until - now_usec();

		if (left <= 0)
			break;
		if (left > 2000) {
			/* Sleep for most of it, then spin: poll() alone is late by up to a ms */
			if (poll(&pfd, 1, (int)((left - 1000) / 1000)) > 0)
				print_events(li);
		}
	}
	print_events(li);
}

static void
log_line(struct libinput *li, enum libinput_log_priority priority, const char *format, va_list args)
{
	char text[1024];

	vsnprintf(text, sizeof(text), format, args);
	text[strcspn(text, "\n")] = '\0';
	printf("L %lld %s\n", start ? (long long)(now_usec() - start) : 0LL, text);
}

static const char *
option(int argc, char **argv, const char *name, const char *fallback)
{
	size_t len = strlen(name);

	for (int i = 2; i < argc; i++)
		if (strncmp(argv[i], name, len) == 0 && argv[i][len] == '=')
			return argv[i] + len + 1;
	return fallback;
}

int
main(int argc, char **argv)
{
	char *line = NULL;
	size_t line_size = 0, nevents = 0, events_size = 0;
	struct event *events = NULL;
	struct libinput *li;
	struct libinput_device *device;
	int fds[2];

	if (argc < 2) {
		fprintf(stderr, "usage: %s NODE [OPTION=VALUE...] < EVENTS\n", argv[0]);
		return 2;
	}

	while (getline(&line, &line_size, stdin) > 0) {
		char *rest = line + 2;

		switch (line[0]) {
		case 'N':
			rest[strcspn(rest, "\n")] = '\0';
			snprintf(dev.name, sizeof(dev.name), "%s", rest);
			break;
		case 'I':
			sscanf(rest, "%hu %hu %hu %hu", &dev.id.bustype, &dev.id.vendor, &dev.id.product,
			       &dev.id.version);
			break;
		case 'P':
			for (char *end; *rest && *rest != '\n'; rest = end) {
				unsigned long prop = strtoul(rest, &end, 10);
				if (end == rest)
					break;
				set_bit(dev.props, prop);
			}
			break;
		case 'B': {
			char *end;
			unsigned long type = strtoul(rest, &end, 10);

			if (type >= EV_CNT)
				break;
			set_bit(dev.types, type);
			for (rest = end; *rest && *rest != '\n'; rest = end) {
				unsigned long code = strtoul(rest, &end, 10);
				if (end == rest)
					break;
				if (code < KEY_CNT)
					set_bit(dev.codes[type], code);
			}
			break;
		}
		case 'A': {
			unsigned code;
			struct input_absinfo abs = { 0 };

			if (sscanf(rest, "%u %d %d %d %d %d", &code, &abs.minimum, &abs.maximum, &abs.fuzz,
				   &abs.flat, &abs.resolution) == 6 &&
			    code < ABS_CNT)
				dev.abs[code] = abs;
			break;
		}
		case 'E': {
			struct event e;
			long long usec;

			if (sscanf(rest, "%lld %hu %hu %d", &usec, &e.type, &e.code, &e.value) != 4)
				break;
			e.usec = usec;
			if (nevents == events_size) {
				events_size = events_size ? events_size * 2 : 4096;
				events = realloc(events, events_size * sizeof(*events));
			}
			events[nevents++] = e;
			break;
		}
		default:
			break;
		}
	}

	if (pipe2(fds, O_NONBLOCK | O_CLOEXEC) != 0) {
		perror("pipe");
		return 1;
	}
	fcntl(fds[1], F_SETFL, fcntl(fds[1], F_GETFL) & ~O_NONBLOCK);
	dev.fd = fds[0];
	dev.node = argv[1];

	li = libinput_path_create_context(&interface, NULL);
	if (atoi(option(argc, argv, "verbose", "0"))) {
		libinput_log_set_priority(li, LIBINPUT_LOG_PRIORITY_DEBUG);
		libinput_log_set_handler(li, log_line);
	}
	device = libinput_path_add_device(li, argv[1]);
	if (!device) {
		fprintf(stderr, "libinput didn't take %s\n", argv[1]);
		return 1;
	}
	libinput_device_config_tap_set_enabled(device, atoi(option(argc, argv, "tap", "1")));
	libinput_device_config_tap_set_drag_enabled(device, atoi(option(argc, argv, "drag", "1")));
	libinput_device_config_tap_set_drag_lock_enabled(device,
							  atoi(option(argc, argv, "draglock", "0")));
	libinput_device_config_scroll_set_natural_scroll_enabled(
		device,
		atoi(option(argc, argv, "natural", "1")));
	libinput_device_config_dwt_set_enabled(device, atoi(option(argc, argv, "dwt", "1")));
	libinput_device_config_accel_set_speed(device, atof(option(argc, argv, "speed", "0")));
	libinput_device_config_accel_set_profile(
		device,
		strcmp(option(argc, argv, "profile", "adaptive"), "flat") == 0
			? LIBINPUT_CONFIG_ACCEL_PROFILE_FLAT
			: LIBINPUT_CONFIG_ACCEL_PROFILE_ADAPTIVE);
	libinput_device_config_scroll_set_method(device, LIBINPUT_CONFIG_SCROLL_2FG);
	print_events(li);

	start = now_usec() + 100000;
	for (size_t i = 0; i < nevents;) {
		struct input_event frame[64];
		size_t n = 0;

		/* A frame: up to its SYN_REPORT, written at once as the kernel does */
		while (i < nevents && n < 64) {
			int64_t usec = start + events[i].usec;
			bool last = events[i].type == EV_SYN && events[i].code == SYN_REPORT;

			frame[n++] = (struct input_event){
				.input_event_sec = usec / 1000000,
				.input_event_usec = usec % 1000000,
				.type = events[i].type,
				.code = events[i].code,
				.value = events[i].value,
			};
			i++;
			if (last)
				break;
		}
		run_until(li, start + events[i - 1].usec);
		if (write(fds[1], frame, n * sizeof(frame[0])) < 0)
			perror("write");
		print_events(li);
	}
	/* What's left on timers: tap timeouts, scroll stops */
	run_until(li, now_usec() + 1000000);
	fflush(stdout);
	libinput_unref(li);
	return 0;
}
