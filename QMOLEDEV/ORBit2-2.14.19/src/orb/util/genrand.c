#include <config.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <fcntl.h>
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif
#ifdef _WIN32
#  include <process.h>
#endif
#ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
#endif
#include <errno.h>

#include <glib.h>
#include <linc/linc.h>
#include <orbit/util/orbit-genrand.h>

#include "orbit-purify.h"

#ifdef G_OS_WIN32
#  define pid_t     DWORD
#include <wincrypt.h>
#endif

static ORBitGenUidType  genuid_type = ORBIT_GENUID_STRONG;
#ifndef G_OS_WIN32
static int              random_fd = -1;
#else
static HCRYPTPROV	hprov = 0;
#endif
static GRand           *glib_prng = NULL;
static pid_t            genuid_pid;
#ifndef G_OS_WIN32
static uid_t            genuid_uid;
#else
static int		genuid_uid = 0;
#endif

/* This is quite possibly a complete waste of cycles */
static GMutex          *inc_lock = NULL;
#define INC_LOCK()      LINK_MUTEX_LOCK (inc_lock)
#define INC_UNLOCK()    LINK_MUTEX_UNLOCK (inc_lock)

/**
 * ORBit_genuid_init:
 * @type: how strong / weak we want to be
 * 
 * initializes randomness bits
 * 
 * Return value: TRUE if we achieve the strength desired
 **/
gboolean
ORBit_genuid_init (ORBitGenUidType type)
{
	GTimeVal time;
	gboolean hit_strength;
	
	genuid_pid = getpid ();
#ifndef G_OS_WIN32
	genuid_uid = getuid ();
#endif
	inc_lock = link_mutex_new();

	glib_prng = g_rand_new ();
	g_get_current_time (&time);
	g_rand_set_seed (glib_prng, (time.tv_sec << 20) ^ time.tv_usec);

	genuid_type = type;

	switch (genuid_type) {
	case ORBIT_GENUID_STRONG:
#ifndef G_OS_WIN32
		random_fd = open ("/dev/urandom", O_RDONLY);

		if (random_fd < 0)
			random_fd = open ("/dev/random", O_RDONLY);

		hit_strength = (random_fd >= 0);
#else
		if (CryptAcquireContext (&hprov, NULL, NULL, PROV_RSA_FULL,
					 CRYPT_VERIFYCONTEXT))
			hit_strength = TRUE;
		else
			hit_strength = FALSE;
#endif
#ifdef LINK_SSL_SUPPORT
		hit_strength = TRUE; /* foolishly trust OpenSSL */
#endif
		break;
	default:
		hit_strength = TRUE;
		break;
	}

	return hit_strength;
}

void
ORBit_genuid_fini (void)
{
#ifndef G_OS_WIN32
	if (random_fd >= 0) {
		close (random_fd);
		random_fd = -1;
	}
#else
	if (hprov) {
		CryptReleaseContext (hprov, 0);
		hprov = 0;
	}
#endif
	if (glib_prng) {
		g_rand_free (glib_prng);
		glib_prng = NULL;
	}

	if (inc_lock) {
		g_mutex_free (inc_lock);
		inc_lock = NULL;
	}
}

#ifndef G_OS_WIN32

static gboolean
genuid_rand_device (guchar *buffer, int length)
{
	int n;

	for (; length > 0; ) {
		n = read (random_fd, buffer, length);

		if (n < 0) {
			if (errno == EINTR || errno == EAGAIN)
				continue;
			else {
				close (random_fd);
				random_fd = -1;

				return FALSE;
			}  
		}

		length -= n;
		buffer += n;
	}

	return TRUE;
}

#endif

#ifdef LINK_SSL_SUPPORT
#include <openssl/rand.h>

static gboolean
genuid_rand_openssl (guchar *buffer, int length)
{
	static RAND_METHOD *rm = NULL;

	INC_LOCK();
	if (!rm)
		rm = RAND_get_rand_method ();
	INC_UNLOCK();

	RAND_bytes (buffer, length);

	return TRUE;
}
#endif

static void
xor_buffer (guchar *buffer, int length)
{
	static glong   s = 0x6b842128;
	glong          i, t;
	GTimeVal       time;

	g_get_current_time (&time);

	t = time.tv_sec ^ time.tv_usec;

	for (i = 0; i < length; i++)
		buffer [i] ^= (guchar) (s ^ (t << i));

	s ^= t;
}

static void
genuid_simple (guchar *buffer, int length)
{
	static guint32 inc = 0;

	g_assert (length >= 4);

	p_memzero (buffer, length);

	if (length > 4)
		memcpy (buffer + 4, &genuid_pid, 4);

	if (length > 8)
		memcpy (buffer + 8, &genuid_uid, 4);

	INC_LOCK ();

	inc++;
	memcpy (buffer, &inc, 4);

	xor_buffer (buffer, length);

	INC_UNLOCK ();
}

static void
genuid_glib_pseudo (guchar *buffer, int length)
{
	static guint32 inc = 0;
	int            i;

	INC_LOCK ();
	inc++;

	for (i = 0; i < length; i++) {
		buffer [i] = g_rand_int_range (glib_prng, 0, 255);

		if (i < sizeof (guint32))
			buffer [i] ^= ((guchar *) &inc) [i];
	}

	xor_buffer (buffer, length);
	INC_UNLOCK ();
}

void
ORBit_genuid_buffer (guint8         *buffer,
		     int             length,
		     ORBitGenUidRole role)
{
	ORBitGenUidType type = genuid_type;

	if (role == ORBIT_GENUID_OBJECT_ID)
		type = ORBIT_GENUID_SIMPLE;

	switch (type) {

	case ORBIT_GENUID_STRONG:
#ifndef G_OS_WIN32
		if (random_fd >= 0 &&
		    genuid_rand_device (buffer, length))
			return;
#else
		if (hprov &&
		    CryptGenRandom (hprov, length, buffer))
			return;
#endif
#if LINK_SSL_SUPPORT
		else if (genuid_rand_openssl (buffer, length))
			return;
#endif
		genuid_glib_pseudo (buffer, length);
		break;

	case ORBIT_GENUID_SIMPLE:
		genuid_simple (buffer, length);
		break;

	default:
		g_error ("serious randomness failure");
		break;
	}
}
