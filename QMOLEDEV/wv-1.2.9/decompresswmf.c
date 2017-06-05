/* wvWare
 * Copyright (C) Caolan McNamara, Dom Lachowicz, and others
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include "wv.h"

#if defined WIN32
#define HAVE_MMAP 1 /* have mmap replacement in winmmap.[ch] */
#endif

#if defined(HAVE_ZLIB) && defined(HAVE_MMAP)
#include <zlib.h>
#if defined WIN32 /*lvm007@aha.ru fix for mmap realization in Win*/
#include "winmmap.h"
#include <io.h>
#else
#include <sys/mman.h>
#endif
#endif 

/*lvm007@aha.ru fix */
#ifndef WIN32
#include <sys/types.h>
#endif
#include <sys/stat.h>
#include <fcntl.h>

/*
 * written by thisguy@somewhere.com who doesnt want his name in the source
 */

/*
 * there's some notes in the notes dir on compression
*/

int
setdecom (void)
{
#ifdef HAVE_ZLIB
    return (1);
#else
    wvError (
	     ("libwv was not compiled against zlib, so wmf files cannot be decompressed\n"));
    return (0);
#endif
}

int
decompress (FILE * inputfile, FILE * outputfile, U32 inlen, U32 outlen)
{
#if defined(HAVE_ZLIB) && defined(HAVE_MMAP)
    unsigned char *compr;
    unsigned char *uncompr;
    int err;
    unsigned long uncomprLen, comprLen;


    unsigned char *input, *output;
    int out;
    int in;



    if (inputfile == NULL)
      {
	  wvError (("danger, file to decompress is NULL\n"));
	  return (-1);
      }

    in = fileno (inputfile);

    input = mmap (0, inlen, PROT_READ | PROT_WRITE, MAP_SHARED, in, 0);

    if (input == (unsigned char *) -1)
      {
	  wvError (("unable to mmap inputfile\n"));
	  return (-1);
      }

    out = fileno (outputfile);
    lseek (out, outlen, SEEK_SET);
    if (out == -1)
      {
	  wvError (("unable to create outputfile\n"));
	  munmap (input, inlen);
	  return (-1);
      }
    if (-1 == write (out, "0", 1))
      {
	  wvError (("unable to write to outputfile\n"));
	  munmap (input, inlen);
	  close (out);
	  return (-1);
      }
    lseek (out, 0, SEEK_SET);

    output = mmap (0, outlen, PROT_READ | PROT_WRITE, MAP_SHARED, out, 0);

    if (output == (unsigned char *) -1)
      {
	  wvError (("map out failed\n"));
	  munmap (input, inlen);
	  close (out);
	  return (-1);
      }

    /* set the size of the file */
    comprLen = inlen;

    /* Read in the file contents */
    compr = input;
    uncompr = output;
    if (compr == NULL)
      {
	  wvError (("no mem to decompress wmf files\n"));
	  return (-1);
      }
    if (uncompr == NULL)
      {
	  wvError (("no mem to decompress wmf files\n"));
	  return (-1);
      }

    uncomprLen = outlen;	/* This was the trick :( */

    wvTrace (("len is %d %d\n", uncomprLen, comprLen));

    err = uncompress (uncompr, &uncomprLen, compr, comprLen);

    munmap (input, inlen);
    munmap (output, outlen);

    if (err != Z_OK)
      {
	  wvError (("decompress error: %d\n", err));
	  return (-1);
      }
#else
    wvError (
	     ("System does not have mmap, if you are so inclined rewrite decompresswmf to not require this\n"));
#endif
    return 0;
}
