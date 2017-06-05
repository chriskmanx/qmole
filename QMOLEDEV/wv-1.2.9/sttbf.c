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

/*
STTBF (STring TaBle stored in File)

Word has many tables of strings that are stored as Pascal type strings.
STTBFs consist of an optional short containing 0xFFFF, indicating that the
strings are extended character strings, a short indicating how many strings
are included in the string table, another short indicating the size in bytes
of the extra data stored with each string and each string followed by the
extra data. Non-extended charater Pascal strings begin with a single byte
length count which describes how many characters follow the length byte in
the string. If pst is a pointer to an array of characters storing a Pascal
style string then the length of the string is *pst+1. In an STTBF Pascal
style strings are concatenated one after another until the length of the
STTBF recorded in the FIB is exhausted. Extra data associated with a string
may also be stored in an sttbf. When extra data is stored for an STTBF, it
is written at the end of each string. For example: The extra data for an
STTBF consists of a short. If the string "Cat" were stored, the actual entry
in the string table would consist of a length byte containing 3 (3 for
"Cat") followed by the bytes 'C' 'a' 't', followed by the 2 bytes containing
the short. Extended character strings are stored just the same, except they
have a double byte length count and each extended character occupies two
bytes.
*/


void
wvGetSTTBF (STTBF * anS, U32 offset, U32 len, wvStream * fd)
{
    int i, j;
    U16 slen;

    anS->s8strings = NULL;
    anS->u16strings = NULL;
    anS->extradata = NULL;

    wvTrace (("sttbf offset is %x,len %d\n", offset, len));
    if (len == 0)
      {
	  anS->nostrings = 0;
	  return;
      }
    wvStream_goto (fd, offset);
    anS->extendedflag = read_16ubit (fd);
    if (anS->extendedflag != 0xFFFF)
      {
	  /*old U8 strings */
	  anS->nostrings = anS->extendedflag;
      }
    else
      {
	  /*U16 chars */
	  anS->nostrings = read_16ubit (fd);
      }
    anS->extradatalen = read_16ubit (fd);

    if (anS->extendedflag == 0xFFFF)
	anS->u16strings = (U16 **) wvMalloc (sizeof (U16 *) * anS->nostrings);
    else
	anS->s8strings = (S8 **) wvMalloc (sizeof (S8 *) * anS->nostrings);

    if (anS->extradatalen)
      {
	  anS->extradata = (U8 **) wvMalloc (sizeof (U8 *) * anS->nostrings);
	  for (i = 0; i < anS->nostrings; i++)
	      anS->extradata[i] = (U8 *) wvMalloc (anS->extradatalen);
      }

    if (anS->extendedflag == 0xFFFF)
      {
	  for (i = 0; i < anS->nostrings; i++)
	    {
		slen = read_16ubit (fd);
		if (slen == 0)
		    anS->u16strings[i] = NULL;
		else
		  {
		      anS->u16strings[i] =
			  (U16 *) wvMalloc (sizeof (U16) * (slen + 1));
		      for (j = 0; j < slen; j++)
			  anS->u16strings[i][j] = read_16ubit (fd);
		      anS->u16strings[i][j] = 0;
		  }
		if (anS->extradatalen)
		    for (j = 0; j < anS->extradatalen; j++)
			anS->extradata[i][j] = read_8ubit (fd);
	    }
      }
    else
      {
	  for (i = 0; i < anS->nostrings; i++)
	    {
		slen = read_8ubit (fd);
		if (slen == 0)
		    anS->s8strings[i] = NULL;
		else
		  {
		      anS->s8strings[i] = (S8 *) wvMalloc (slen + 1);
		      for (j = 0; j < slen; j++)
			  anS->s8strings[i][j] = read_8ubit (fd);
		      anS->s8strings[i][j] = 0;
		  }
		if (anS->extradatalen)
		    for (j = 0; j < anS->extradatalen; j++)
			anS->extradata[i][j] = read_8ubit (fd);
	    }
      }
}

void
wvReleaseSTTBF (STTBF * item)
{
    int i;

    if (item->s8strings != NULL)
      {
	  for (i = 0; i < item->nostrings; i++)
	      wvFree (item->s8strings[i]);
	  wvFree (item->s8strings);
      }
    if (item->u16strings != NULL)
      {
	  for (i = 0; i < item->nostrings; i++)
	      wvFree (item->u16strings[i]);
	  wvFree (item->u16strings);
      }
    if (item->extradata != NULL)
      {
	  for (i = 0; i < item->nostrings; i++)
	      wvFree (item->extradata[i]);
	  wvFree (item->extradata);
      }
}


void
wvListSTTBF (STTBF * item)
{
    int i, j;
    U16 *letter;

    if (item->s8strings != NULL)
      {
	  for (i = 0; i < item->nostrings; i++)
	      fprintf (stderr, "string is %s\n", item->s8strings[i]);
      }
    else if (item->u16strings != NULL)
      {
	  for (i = 0; i < item->nostrings; i++)
	    {
		fprintf (stderr, "string is ");
		letter = item->u16strings[i];
		while ((letter != NULL) && (*letter != 0))
		    fprintf (stderr, "%c", *letter++);
		fprintf (stderr, "\n");
	    }
      }

    if (item->extradata != NULL)
      {
	  for (i = 0; i < item->nostrings; i++)
	      for (j = 0; j < item->extradatalen; j++)
		  fprintf (stderr, " %x ", item->extradata[i][j]);
	  fprintf (stderr, "\n");
      }
}


void
wvPrintTitle (wvParseStruct * ps, STTBF * item)
{
    int i = 0;
    CHP achp;
    wvInitCHP (&achp);

    if ((item) && (item->nostrings >= 3))
      {
	  if (item->extendedflag == 0xFFFF)
	    {
		if (item->u16strings[ibstAssocTitle] != NULL)
		  {
		      while (item->u16strings[ibstAssocTitle][i])
			{
			    wvTrace (
				     ("title char is %c\n",
				      (item->u16strings[ibstAssocTitle][i])));
			    wvOutputTextChar (item->u16strings[ibstAssocTitle]
					      [i++], 0, ps, &achp);
			}
		      return;
		  }
	    }
	  else
	    {
		if (item->s8strings[ibstAssocTitle] != NULL)
		  {
		      while (item->s8strings[ibstAssocTitle][i])
			  wvOutputTextChar (item->s8strings[ibstAssocTitle]
					    [i++], 1, ps, &achp);
		      return;
		  }
	    }
      }
    printf ("Untitled");
}


void
wvGetSTTBF6 (STTBF * anS, U32 offset, U32 len, wvStream * fd)
{
    int i, j;
    U16 slen;
    U32 table_len = 0;

    anS->s8strings = NULL;
    anS->u16strings = NULL;
    anS->extradata = NULL;
    anS->nostrings = 0;

    wvTrace (("word 6 sttbf offset is %x,len %d\n", offset, len));
    if (len == 0)
      {
	  anS->nostrings = 0;
	  return;
      }
    wvStream_goto (fd, offset);
    /* this is very bad, since there can be many more strings in a table than
       17 !!! We have to do two passes over the string table
    anS->nostrings = ibstAssocMaxWord6; */
#ifdef DEBUG
    /* the table len is a 16 bit, not 8-bit value */
    if (len != (U32) read_16ubit (fd))
	wvTrace (("word 6 sttbf len does not match up correctly, strange\n"));
#else
    /* whether debugging or not, have to get over the table length value !!! */
    read_16ubit (fd);
#endif

    while(table_len < len)
    {
       slen = read_8ubit (fd);
       table_len++;
       if(slen > 0)
       {
           anS->nostrings++;
           for(i = 0; i < slen; i++)
               read_8ubit(fd);
           table_len += slen;
       }
    }

    anS->extendedflag = ibstAssocMaxWord6;  /*just for the sake of it */
    anS->extradatalen = 0;
    anS->s8strings = (S8 **) wvMalloc (sizeof (S8 *) * anS->nostrings);

    /* now go back to the strings table */
    wvStream_goto (fd, offset + 2);

    for (i = 0; i < anS->nostrings; i++)
      {
	  slen = read_8ubit (fd);
	  if (slen == 0)
	      anS->s8strings[i] = NULL;
	  else
	    {
		anS->s8strings[i] = (S8 *) wvMalloc (slen + 1);
		for (j = 0; j < slen; j++)
		    anS->s8strings[i][j] = read_8ubit (fd);
		anS->s8strings[i][j] = 0;
	    }
      }
}


U16 *
UssrStrBegin (STTBF * sttbf, int no)
{
    if (no >= sttbf->nostrings)
	return (NULL);

    return (sttbf->u16strings[no] + 11);
}


void
wvGetGrpXst (STTBF * anS, U32 offset, U32 len, wvStream * fd)
{
    U16 slen, i;
    U32 pos = 0;

    anS->extendedflag = 1;
    anS->nostrings = 0;
    anS->extradatalen = 0;
    anS->s8strings = NULL;
    anS->u16strings = NULL;
    anS->extradata = NULL;
    if (len == 0)
	return;
    wvStream_goto (fd, offset);

    while (pos < len)
      {
	  slen = read_16ubit (fd);
	  pos += 2;
	  anS->nostrings++;
	  anS->u16strings =
	      (U16 **) realloc (anS->u16strings,
				sizeof (U16 *) * anS->nostrings);
	  anS->u16strings[anS->nostrings - 1] =
	      (U16 *) wvMalloc (sizeof (U16) * (slen + 1));
	  for (i = 0; i < slen; i++)
	      anS->u16strings[anS->nostrings - 1][i] = read_16ubit (fd);
	  anS->u16strings[anS->nostrings - 1][i] = 0;
	  pos += (i * 2);
      }

}
