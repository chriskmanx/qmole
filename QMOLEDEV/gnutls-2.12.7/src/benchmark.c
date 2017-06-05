/*
 * Copyright (C) 2009, 2010  Free Software Foundation, Inc.
 *
 * This file is part of GnuTLS.
 *
 * GnuTLS is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * GnuTLS is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 *
 * Written by Nikos Mavrogiannopoulos <nmav@gnutls.org>.
 */

#include <config.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <gnutls/gnutls.h>
#include <gnutls/crypto.h>
#include <time.h>
#include <signal.h>
#include "timespec.h"           /* gnulib gettime */

static unsigned char data[64 * 1024];

static int must_finish = 0;

#if !defined(_WIN32)
static void
alarm_handler (int signo)
{
  must_finish = 1;
}
#else
#include <windows.h>
DWORD WINAPI alarm_handler (LPVOID lpParameter);
DWORD WINAPI
alarm_handler (LPVOID lpParameter)
{
  HANDLE wtimer = *((HANDLE *) lpParameter);
  WaitForSingleObject (wtimer, INFINITE);
  must_finish = 1;
  return 0;
}

#define W32_ALARM_VARIABLES HANDLE wtimer = NULL, wthread = NULL; \
  LARGE_INTEGER alarm_timeout
#define W32_ALARM_TRIGGER(timeout, leave) { \
  wtimer = CreateWaitableTimer (NULL, TRUE, NULL); \
  if (wtimer == NULL) \
    { \
      fprintf (stderr, "error: CreateWaitableTimer %u\n", GetLastError ()); \
      leave; \
    } \
  wthread = CreateThread (NULL, 0, alarm_handler, &wtimer, 0, NULL); \
  if (wthread == NULL) \
    { \
      fprintf (stderr, "error: CreateThread %u\n", GetLastError ()); \
      leave; \
    } \
  alarm_timeout.QuadPart = timeout * 10000000; \
  if (SetWaitableTimer (wtimer, &alarm_timeout, 0, NULL, NULL, FALSE) == 0) \
    { \
      fprintf (stderr, "error: SetWaitableTimer %u\n", GetLastError ()); \
      leave; \
    } \
  }
#define W32_ALARM_CLEANUP { \
  if (wtimer != NULL) \
    CloseHandle (wtimer); \
  if (wthread != NULL) \
    CloseHandle (wthread);}
#endif

static void
tls_log_func (int level, const char *str)
{
  fprintf (stderr, "|<%d>| %s", level, str);
}

static void
value2human (double bytes, double time, double *data, double *speed,
             char *metric)
{
  if (bytes > 1000 && bytes < 1000 * 1000)
    {
      *data = ((double) bytes) / 1000;
      *speed = *data / time;
      strcpy (metric, "Kb");
      return;
    }
  else if (bytes >= 1000 * 1000 && bytes < 1000 * 1000 * 1000)
    {
      *data = ((double) bytes) / (1000 * 1000);
      *speed = *data / time;
      strcpy (metric, "Mb");
      return;
    }
  else if (bytes >= 1000 * 1000 * 1000)
    {
      *data = ((double) bytes) / (1000 * 1000 * 1000);
      *speed = *data / time;
      strcpy (metric, "Gb");
      return;
    }
  else
    {
      *data = (double) bytes;
      *speed = *data / time;
      strcpy (metric, "bytes");
      return;
    }
}

static void
cipher_bench (int algo, int size)
{
  int ret;
  gnutls_cipher_hd_t ctx;
  void *_key, *_iv;
  gnutls_datum_t key, iv;
  struct timespec start, stop;
  double secs;
  double data_size = 0;
  double dspeed, ddata;
  int blocksize = gnutls_cipher_get_block_size (algo);
  int keysize = gnutls_cipher_get_key_size (algo);
  char metric[16];
#if defined(_WIN32)
  W32_ALARM_VARIABLES;
#endif

  _key = malloc (keysize);
  if (_key == NULL)
    return;
  memset (_key, 0xf0, keysize);

  _iv = malloc (blocksize);
  if (_iv == NULL)
    return;
  memset (_iv, 0xf0, blocksize);

  iv.data = _iv;
  iv.size = blocksize;

  key.data = _key;
  key.size = keysize;

  printf ("Checking %s (%dkb payload)... ", gnutls_cipher_get_name (algo),
          size);
  fflush (stdout);

  must_finish = 0;
#if !defined(_WIN32)
  alarm (5);
#else
  W32_ALARM_TRIGGER(5, goto leave);
#endif

  gettime (&start);

  ret = gnutls_cipher_init (&ctx, algo, &key, &iv);
  if (ret < 0)
    {
      fprintf (stderr, "error: %s\n", gnutls_strerror (ret));
      goto leave;
    }

  do
    {
      gnutls_cipher_encrypt (ctx, data, size * 1024);
      data_size += size * 1024;
    }
  while (must_finish == 0);

  gnutls_cipher_deinit (ctx);

  gettime (&stop);

  secs = (stop.tv_sec * 1000 + stop.tv_nsec / (1000 * 1000) -
          (start.tv_sec * 1000 + start.tv_nsec / (1000 * 1000)));
  secs /= 1000;

  value2human (data_size, secs, &ddata, &dspeed, metric);
  printf ("Encrypted %.2f %s in %.2f secs: ", ddata, metric, secs);
  printf ("%.2f %s/sec\n", dspeed, metric);

leave:
  free (_key);
  free (_iv);
#if defined(_WIN32)
  W32_ALARM_CLEANUP;
#endif
}

static void
mac_bench (int algo, int size)
{
  void *_key;
  struct timespec start, stop;
  double secs;
  double data_size = 0;
  double ddata, dspeed;
  int blocksize = gnutls_hmac_get_len (algo);
  char metric[16];
#if defined(_WIN32)
  W32_ALARM_VARIABLES;
#endif

  _key = malloc (blocksize);
  if (_key == NULL)
    return;
  memset (_key, 0xf0, blocksize);

  printf ("Checking %s (%dkb payload)... ", gnutls_mac_get_name (algo), size);
  fflush (stdout);

  must_finish = 0;
#if !defined(_WIN32)
  alarm (5);
#else
  W32_ALARM_TRIGGER(5, goto leave);
#endif

  gettime (&start);

  do
    {
      gnutls_hmac_fast (algo, _key, blocksize, data, size * 1024, _key);
      data_size += size * 1024;
    }
  while (must_finish == 0);

  gettime (&stop);

  secs =
    (stop.tv_sec * 1000 + stop.tv_nsec / (1000 * 1000) -
     (start.tv_sec * 1000 + start.tv_nsec / (1000 * 1000)));
  secs /= 1000;

  value2human (data_size, secs, &ddata, &dspeed, metric);

  printf ("Hashed %.2f %s in %.2f secs: ", ddata, metric, secs);
  printf ("%.2f %s/sec\n", dspeed, metric);
#if defined(_WIN32)
leave:
  W32_ALARM_CLEANUP;
#endif
  free (_key);
}

int
main (int argc, char **argv)
{
  int debug_level = 0;

  if (argc > 1)
    debug_level = 2;

#if !defined(_WIN32)
  signal (SIGALRM, alarm_handler);
#endif

  gnutls_global_set_log_function (tls_log_func);
  gnutls_global_set_log_level (debug_level);
  gnutls_global_init ();

  mac_bench (GNUTLS_MAC_SHA1, 4);
  mac_bench (GNUTLS_MAC_SHA1, 8);
  mac_bench (GNUTLS_MAC_SHA1, 16);

  mac_bench (GNUTLS_MAC_SHA256, 4);
  mac_bench (GNUTLS_MAC_SHA256, 8);
  mac_bench (GNUTLS_MAC_SHA256, 16);

  cipher_bench (GNUTLS_CIPHER_3DES_CBC, 4);
  cipher_bench (GNUTLS_CIPHER_3DES_CBC, 8);
  cipher_bench (GNUTLS_CIPHER_3DES_CBC, 16);

  cipher_bench (GNUTLS_CIPHER_AES_128_CBC, 4);
  cipher_bench (GNUTLS_CIPHER_AES_128_CBC, 8);
  cipher_bench (GNUTLS_CIPHER_AES_128_CBC, 16);

  cipher_bench (GNUTLS_CIPHER_ARCFOUR, 4);
  cipher_bench (GNUTLS_CIPHER_ARCFOUR, 8);
  cipher_bench (GNUTLS_CIPHER_ARCFOUR, 16);

  return 0;
}
