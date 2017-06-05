/*
 * Copyright (C) 2008, 2010 Free Software Foundation, Inc.
 *
 * Author: Nikos Mavrogiannopoulos
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
 * along with GnuTLS.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#if !defined(_WIN32)
#include <sys/wait.h>
#endif

#include "utils.h"
#include <gnutls/gnutls.h>
#include <gnutls/crypto.h>
#include "../lib/random.h"

#if !defined(_WIN32)
static void dump(const char* name, unsigned char* buf, int buf_size)
{
int i;
  printf("%s: ", name);
  for(i=0;i<buf_size;i++)
    printf("%.2x:", buf[i]);
  printf("\n");
}
  
#define FILENAME "./rng-test"
   
void
doit (void)
{
  unsigned char buf1[32];
  unsigned char buf2[32];
  pid_t pid;
  int ret;
  FILE* fp;

  gnutls_global_init ();
  pid = fork();
  if (pid == 0)
    {
      fp = fopen(FILENAME, "w");
      if (fp == NULL)
        fail("cannot open file");
      
      _gnutls_rnd (GNUTLS_RND_RANDOM, buf1, sizeof (buf1));
      if (debug) dump("buf1", buf1, sizeof(buf1));
      
      fwrite(buf1, 1, sizeof(buf1), fp);
      fclose(fp);
    }
  else
    {
      /* daddy */
      _gnutls_rnd (GNUTLS_RND_RANDOM, buf2, sizeof (buf2));
      if (debug) dump("buf2", buf2, sizeof(buf2));
      waitpid(pid, NULL, 0);
      
      fp = fopen(FILENAME, "r");
      if (fp == NULL)
        fail("cannot open file");
        
      ret = fread(buf1, 1, sizeof(buf1), fp);
      
      fclose(fp);
      remove(FILENAME);
      
      if (ret != sizeof(buf1))
        {
          fail("error testing the random generator.");
          return;
        }

      if (memcmp(buf1, buf2, sizeof(buf1))==0)
        {
          fail("error in the random generator. Produces same valus after fork()");
          return;
        }
      
      success("success");
    }
}
#else
void
doit (void)
{
  exit (77);
}
#endif
