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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "wv.h"

#include <gsf/gsf-output-memory.h>

/* ROTATE_LEFT rotates x left n bits , with a bitlen of b*/
#define ROTATE_LEFT(x, n,b) (((x) << (n)) | ((x) >> (b-(n))))

int
wvDecrypt95 (wvParseStruct * ps)
{
    GsfOutput *mainfd;
    unsigned char pw[16], z, g;
    unsigned char key[16];
    U8 pwkey[2];
    int i, c, len, ret = 1;
    U32 j = 0;
    unsigned long end;
    unsigned char test[0x10];
    U16 hash, h;

    if (ps->password[0] == 0)
	return (ret);

    hash = (U16) ps->fib.lKey & 0xffff;

    pwkey[0] = (U8) ((ps->fib.lKey >> 16) & 0xFF);
    pwkey[1] = (U8) ((ps->fib.lKey >> 24) & 0xFF);

    /* 
       currently I do not know what the story is with the actual
       input of non ascii password for word 95
     */
    for (i = 0; i < 16; i++)
	pw[i] = (U8) ps->password[i];

    len = strlen ((char *) pw);
    i = len;
    z = 0xbb;
    for (; i < 16; i++)
      {
	  switch (j)
	    {
	    case 0:
		pw[i] = 0xbb;
		break;
	    case 1:
		pw[i] = 0xff;
		break;
	    case 2:
		pw[i] = 0xff;
		break;
	    case 3:
		pw[i] = 0xba;
		break;
	    case 4:
		pw[i] = 0xff;
		break;
	    case 5:
		pw[i] = 0xff;
		break;
	    case 6:
		pw[i] = 0xb9;
		break;
	    case 7:
		pw[i] = 0x80;
		break;
	    case 8:
		pw[i] = 0x0;
		break;
	    case 9:
		pw[i] = 0xbe;
		break;
	    case 10:
		pw[i] = 0xf;
		break;
	    case 11:
		pw[i] = 0x0;
		break;
	    case 12:
		pw[i] = 0xbf;
		break;
	    case 13:
		pw[i] = 0xf;
		break;
	    case 14:
		pw[i] = 0;
		break;
		j++;
	    }
	  j++;
      }

    h = 0xce4b;
    wvTrace (("hash is now %x\n", hash));
    for (i = 0; i < 16; i++)
      {
	  g = (pw[i] ^ pwkey[i & 1]);
	  g = ROTATE_LEFT (g, 7, 8);
	  h ^= (ROTATE_LEFT (pw[i], i + 1, 15) ^ (i + 1) ^ i);
	  wvTrace (("h is now %x\n", h));
	  if (i == len - 1)
	      if (h == hash)
		  ret = 0;
	  key[i] = g;
      }

    if (ret)
	return (ret);

    wvStream_offset_from_end (ps->mainfd, 0);
    end = wvStream_tell (ps->mainfd);

    j = 0;
    wvStream_goto (ps->mainfd, j);

    mainfd = gsf_output_memory_new ();

    while (j < 0x30)
      {
	  c = read_8ubit (ps->mainfd);
	  gsf_output_write (mainfd, 1, (guint8 *)&c);
	  j++;
      }

    while (j < end)
      {
	  for (i = 0; i < 16; i++)
	      test[i] = read_8ubit (ps->mainfd);
	  for (i = 0; i < 16; i++)
	    {
		if (test[i] != 0)
		    c = key[i] ^ test[i];
		else
		    c = 0;
		gsf_output_write (mainfd, 1, (guint8 *)&c);
	    }
	  j += 16;
      }


    if (ps->tablefd0)
	wvStream_close (ps->tablefd0);
    if (ps->tablefd1)
	wvStream_close (ps->tablefd1);
    wvStream_close (ps->mainfd);

    gsf_output_close (mainfd);

    wvStream_memory_create(&ps->mainfd, 
			   g_memdup (gsf_output_memory_get_bytes (GSF_OUTPUT_MEMORY (mainfd)), gsf_output_size (mainfd)),
			   gsf_output_size (mainfd));

    g_object_unref (G_OBJECT (mainfd));

    ps->tablefd = ps->mainfd;
    ps->tablefd0 = ps->mainfd;
    ps->tablefd1 = ps->mainfd;

    wvStream_rewind (ps->mainfd);
    ps->fib.fEncrypted = 0;
    wvGetFIB (&ps->fib, ps->mainfd);
    ps->fib.fEncrypted = 0;
    return (ret);
}

#if 0
int
wvCrack95 (wvParseStruct * ps)
{
    /* 
       is is quite possible to crack password 95 files fairly easily,
       there is standard frequency analysis on the cipher text whiich
       is simply stamped with a 16byte xor key from 0x28 onwards,
       but there are quite a few known bytes in the fib header with
       is also encrypted, for instance unencrypted is the beginning
       and end of the text stream, encrypted is the len of the
       text stream, so already you have a few bytes of the xor key,
       and so on, the fcStshOrig is nearly always the same as the fcStsh
       so theres another byte or two, and so forth. If a struct has
       an lcb of 0, it remains zero despite what should happen in a
       xor and thus you know that the fc before and after the 0 lcb
       are the same, and so forth, so you could derive a lot of 
       information about the xor key from the header, I might
       complete this some day, there have been a load of other programs
       that do similiar things, so i don't know that my adding to that
       pile will help
     */


    int i;
    U8 block1[4];
    U8 block2[4];
    U32 reallen;
    wvError (("begin is %x\n", ps->fib.fcMin));
    wvError (("end is %x\n", ps->fib.fcMac));
    wvError (("len is %x\n", ps->fib.fcMac - ps->fib.fcMin));

    reallen = ps->fib.fcMac - ps->fib.fcMin;

    wvError (("encrypted len is %x\n", ps->fib.ccpText));
    block1[0] = (U8) (ps->fib.ccpText & 0xFF);
    block1[1] = (U8) ((ps->fib.ccpText >> 8) & 0xFF);
    block1[2] = (U8) ((ps->fib.ccpText >> 16) & 0xFF);
    block1[3] = (U8) ((ps->fib.ccpText >> 24) & 0xFF);

    block2[0] = (U8) (reallen & 0xFF);
    block2[1] = (U8) ((reallen >> 8) & 0xFF);
    block2[2] = (U8) ((reallen >> 16) & 0xFF);
    block2[3] = (U8) ((reallen >> 24) & 0xFF);

    key[4] = block1[0] ^ block2[0];
    key[5] = block1[1] ^ block2[1];
    key[6] = block1[2] ^ block2[2];
    key[7] = block1[3] ^ block2[3];


    block1[0] = (U8) (ps->fib.lcbStshf & 0xFF);
    block1[1] = (U8) ((ps->fib.lcbStshf >> 8) & 0xFF);
    block1[2] = (U8) ((ps->fib.lcbStshf >> 16) & 0xFF);
    block1[3] = (U8) ((ps->fib.lcbStshf >> 24) & 0xFF);

    block2[0] = block1[0] ^ key[4];
    block2[1] = block1[1] ^ key[5];
    block2[2] = block1[2] ^ key[6];
    block2[3] = block1[3] ^ key[7];

    reallen = sread_32ubit (block2);
    fprintf (stderr, "reallen is %x\n", reallen);

    block2[0] = (U8) (reallen & 0xFF);
    block2[1] = (U8) ((reallen >> 8) & 0xFF);
    block2[2] = (U8) ((reallen >> 16) & 0xFF);
    block2[3] = (U8) ((reallen >> 24) & 0xFF);

    block1[0] = (U8) (ps->fib.lcbStshfOrig & 0xFF);
    block1[1] = (U8) ((ps->fib.lcbStshfOrig >> 8) & 0xFF);
    block1[2] = (U8) ((ps->fib.lcbStshfOrig >> 16) & 0xFF);
    block1[3] = (U8) ((ps->fib.lcbStshfOrig >> 24) & 0xFF);

    key[12] = block1[0] ^ block2[0];
    key[13] = block1[1] ^ block2[1];
    key[14] = block1[2] ^ block2[2];
    key[15] = block1[3] ^ block2[3];


    block1[0] = (U8) (ps->fib.fcStshf & 0xFF);
    block1[1] = (U8) ((ps->fib.fcStshf >> 8) & 0xFF);
    block1[2] = (U8) ((ps->fib.fcStshf >> 16) & 0xFF);
    block1[3] = (U8) ((ps->fib.fcStshf >> 24) & 0xFF);

    block2[0] = (U8) (ps->fib.fcPlcffndRef & 0xFF);
    block2[1] = (U8) ((ps->fib.fcPlcffndRef >> 8) & 0xFF);
    block2[2] = (U8) ((ps->fib.fcPlcffndRef >> 16) & 0xFF);
    block2[3] = (U8) ((ps->fib.fcPlcffndRef >> 24) & 0xFF);

    if ((block1[0] == 0) && (block2[0]) && (reallen < 0xff))
      {
	  fprintf (stderr, "good\n");
	  key[8] = reallen ^ block2[0];
      }
    else
	fprintf (stderr, "crap\n");

    block2[0] = (U8) (ps->fib.lcbPlcffndRef & 0xFF);
    block2[1] = (U8) ((ps->fib.lcbPlcffndRef >> 8) & 0xFF);
    block2[2] = (U8) ((ps->fib.lcbPlcffndRef >> 16) & 0xFF);
    block2[3] = (U8) ((ps->fib.lcbPlcffndRef >> 24) & 0xFF);

    fprintf (stderr, "%x %x %x %x\n", block2[0], block2[1], block2[1],
	     block2[0]);

    if ((block2[0] == 0) && (block2[1] == 0) && (block2[2] == 0)
	&& (block2[3] == 0))
      {
	  fprintf (stderr, "good\n");
	  block1[0] = (U8) (ps->fib.fcPlcffndRef & 0xFF);
	  block2[0] = (U8) (ps->fib.fcPlcffndTxt & 0xFF);
	  reallen = key[8] ^ block2[0];
	  key[0] = reallen ^ block1[0];
      }

    for (i = 0; i < 16; i++)
	fprintf (stderr, "key %d is %x\n", i, key[i]);
    return (0);
}
#endif
