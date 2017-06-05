/* af_unix.c
 * (c) 2002 Mikulas Patocka
 * This file is a part of the Links program, released under GPL
 */

#include "links.h"

#ifdef DONT_USE_AF_UNIX

int bind_to_af_unix(void)
{
	return -1;
}

void af_unix_close(void)
{
}

#else

#ifdef USE_AF_UNIX
#include <sys/un.h>
#endif

static void af_unix_connection(void *);

#define ADDR_SIZE	4096

union address {
	struct sockaddr s;
#ifdef USE_AF_UNIX
	struct sockaddr_un sun;
#endif
	struct sockaddr_in sin;
	unsigned char buffer[ADDR_SIZE];
};

static union address s_unix;
static union address s_unix_acc;

static socklen_t s_unix_l;
static int s_unix_fd = -1;
static int s_unix_master = 0;


#define S2C1_HANDSHAKE_LENGTH	6
#define C2S2_HANDSHAKE_LENGTH	sizeof(struct links_handshake)
#define S2C3_HANDSHAKE_LENGTH	sizeof(struct links_handshake)

static struct links_handshake {
	unsigned char version[30];
	unsigned char system_name[32];
	unsigned char system_id;
	unsigned char sizeof_long;
} links_handshake;

#define HANDSHAKE_WRITE(hndl, sz)					\
	if ((r = hard_write(hndl, (unsigned char *)&links_handshake, sz)) != (sz))
#define HANDSHAKE_READ(hndl, sz)					\
	if ((r = hard_read(hndl, (unsigned char *)&received_handshake, sz)) != (sz) || memcmp(&received_handshake, &links_handshake, sz))


#ifdef USE_AF_UNIX

static int get_address(void)
{
	unsigned char *path;
	if (!links_home) return -1;
	path = stracpy(links_home);
	add_to_strn(&path, LINKS_SOCK_NAME);
	s_unix_l = (unsigned char *)&s_unix.sun.sun_path - (unsigned char *)&s_unix.sun + strlen(path) + 1;
	if (strlen(path) > sizeof(union address) || (size_t)s_unix_l > sizeof(union address)) {
		mem_free(path);
		return -1;
	}
	memset(&s_unix, 0, sizeof s_unix);
	s_unix.sun.sun_family = AF_UNIX;
	strcpy(s_unix.sun.sun_path, path);
	mem_free(path);
	return PF_UNIX;
}

static void unlink_unix(void)
{
	/*debug("unlink: %s", s_unix.sun.sun_path);*/
	if (unlink(s_unix.sun.sun_path)) {
		/*perror("unlink");*/
	}
}

#else

static int get_address(void)
{
	memset(&s_unix, 0, sizeof s_unix);
	s_unix.sin.sin_family = AF_INET;
	s_unix.sin.sin_port = htons(LINKS_PORT);
	s_unix.sin.sin_addr.s_addr = htonl(0x7f000001);
	s_unix_l = sizeof(struct sockaddr_in);
	return PF_INET;
}

static void unlink_unix(void)
{
}

#endif

static void sleep_a_little_bit(void)
{
	struct timeval tv = { 0, 100000 };
	fd_set dummy;
	FD_ZERO(&dummy);
	select(0, &dummy, &dummy, &dummy, &tv);
}

int bind_to_af_unix(void)
{
	int u = 0;
	int a1 = 1;
	int cnt = 0;
	int af;
	int r;
	struct links_handshake received_handshake;
	memset(&links_handshake, 0, sizeof links_handshake);
	safe_strncpy(links_handshake.version, "Links " VERSION_STRING, sizeof links_handshake.version);
	safe_strncpy(links_handshake.system_name, system_name, sizeof links_handshake.system_name);
	links_handshake.system_id = SYSTEM_ID;
	links_handshake.sizeof_long = sizeof(long);
	if ((af = get_address()) == -1) return -1;
	again:
	if ((s_unix_fd = socket(af, SOCK_STREAM, 0)) == -1) return -1;
#if defined(SOL_SOCKET) && defined(SO_REUSEADDR)
	setsockopt(s_unix_fd, SOL_SOCKET, SO_REUSEADDR, (void *)&a1, sizeof a1);
#endif
	if (bind(s_unix_fd, &s_unix.s, s_unix_l)) {
		/*perror("");
		debug("bind: %d", errno);*/
		if (af == PF_INET && errno == EADDRNOTAVAIL) {
			/* do not try to connect if the user has not configured loopback interface */
			close(s_unix_fd);
			return -1;
		}
		close(s_unix_fd);
		if ((s_unix_fd = socket(af, SOCK_STREAM, 0)) == -1) return -1;
#if defined(SOL_SOCKET) && defined(SO_REUSEADDR)
		setsockopt(s_unix_fd, SOL_SOCKET, SO_REUSEADDR, (void *)&a1, sizeof a1);
#endif
		if (connect(s_unix_fd, &s_unix.s, s_unix_l)) {
retry:
			/*perror("");
			debug("connect: %d", errno);*/
			if (++cnt < MAX_BIND_TRIES) {
				sleep_a_little_bit();
				close(s_unix_fd), s_unix_fd = -1;
				goto again;
			}
			close(s_unix_fd), s_unix_fd = -1;
			if (!u) {
				unlink_unix();
				u = 1;
				goto again;
			}
			return -1;
		}
		HANDSHAKE_READ(s_unix_fd, S2C1_HANDSHAKE_LENGTH) {
			if (r != S2C1_HANDSHAKE_LENGTH) goto retry;
			goto close_and_fail;
		}
		HANDSHAKE_WRITE(s_unix_fd, C2S2_HANDSHAKE_LENGTH)
			goto close_and_fail;
		HANDSHAKE_READ(s_unix_fd, S2C3_HANDSHAKE_LENGTH)
			goto close_and_fail;
		return s_unix_fd;
	}
	if (listen(s_unix_fd, 100)) {
		error("ERROR: listen failed: %d", errno);
		close_and_fail:
		close(s_unix_fd), s_unix_fd = -1;
		return -1;
	}
	s_unix_master = 1;
	set_handlers(s_unix_fd, af_unix_connection, NULL, NULL, NULL);
	return -1;
}

static void af_unix_connection(void *xxx)
{
	socklen_t l = s_unix_l;
	int ns;
	int r;
	struct links_handshake received_handshake;
	memset(&s_unix_acc, 0, sizeof s_unix_acc);
	ns = accept(s_unix_fd, &s_unix_acc.s, &l);
	if (ns == -1) return;
	HANDSHAKE_WRITE(ns, S2C1_HANDSHAKE_LENGTH) {
		close(ns);
		return;
	}
	HANDSHAKE_READ(ns, C2S2_HANDSHAKE_LENGTH) {
		sleep_a_little_bit();	/* workaround a race in previous Links version */
		close(ns);
		return;
	}
	HANDSHAKE_WRITE(ns, S2C3_HANDSHAKE_LENGTH) {
		close(ns);
		return;
	}
	init_term(ns, ns, win_func);
	set_highpri();
}

void af_unix_close(void)
{
	if (s_unix_master) {
		set_handlers(s_unix_fd, NULL, NULL, NULL, NULL);
	}
	if (s_unix_fd != -1) {
		close(s_unix_fd);
		s_unix_fd = -1;
	}
	if (s_unix_master) {
		unlink_unix();
		s_unix_master = 0;
	}
}

#endif
