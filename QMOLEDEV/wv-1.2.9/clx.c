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

void
wvReleaseCLX (CLX * clx)
{
    U16 i;
    for (i = 0; i < clx->grpprl_count; i++)
	wvFree (clx->grpprl[i]);
    wvFree (clx->grpprl);
    wvFree (clx->cbGrpprl);
    wvReleasePCD_PLCF (clx->pcd, clx->pos);
}

void
wvBuildCLXForSimple6 (CLX * clx, FIB * fib)
{
    wvInitCLX (clx);
    clx->nopcd = 1;;

    clx->pcd = (PCD *) wvMalloc (clx->nopcd * sizeof (PCD));
    clx->pos = (U32 *) wvMalloc ((clx->nopcd + 1) * sizeof (U32));

    clx->pos[0] = 0;
    clx->pos[1] = fib->ccpText;

    wvInitPCD (&(clx->pcd[0]));
    clx->pcd[0].fc = fib->fcMin;

    /* reverse the special encoding thing they do for word97 
       if we are using the usual 8 bit chars */

    if (fib->fExtChar == 0)
      {
	  clx->pcd[0].fc *= 2;
	  clx->pcd[0].fc |= 0x40000000UL;
      }

    clx->pcd[0].prm.fComplex = 0;
    clx->pcd[0].prm.para.var1.isprm = 0;
    /*
       these set the ones that *I* use correctly, but may break for other wv
       users, though i doubt it, im just marking a possible firepoint for the
       future
     */
}

/*
The complex part of a file (CLX) is composed of a number of variable-sized
blocks of data. Recorded first are any grpprls that may be referenced by the
plcfpcd (if the plcfpcd has no grpprl references, no grpprls will be
recorded) followed by the plcfpcd. Each block in the complex part is
prefaced by a clxt (clx type), which is a 1-byte code, either 1 (meaning the
block contains a grpprl) or 2 (meaning this is the plcfpcd). A clxtGrpprl
(1) is followed by a 2-byte cb which is the count of bytes of the grpprl. A
clxtPlcfpcd (2) is followed by a 4-byte lcb which is the count of bytes of
the piece table. A full saved file will have no clxtGrpprl's.
*/
void
wvGetCLX (wvVersion ver, CLX * clx, U32 offset, U32 len, U8 fExtChar,
	  wvStream * fd)
{
    U8 clxt;
    U16 cb;
    U32 lcb, i, j = 0;

    wvTrace (("offset %x len %d\n", offset, len));
    wvStream_goto (fd, offset);

    wvInitCLX (clx);

    while (j < len)
      {
	  clxt = read_8ubit (fd);
	  j++;
	  if (clxt == 1)
	    {
		cb = read_16ubit (fd);
		j += 2;
		clx->grpprl_count++;
		clx->cbGrpprl =
		    (U16 *) realloc (clx->cbGrpprl,
				     sizeof (U16) * clx->grpprl_count);
		clx->cbGrpprl[clx->grpprl_count - 1] = cb;
		clx->grpprl =
		    (U8 **) realloc (clx->grpprl,
				     sizeof (U8 *) * (clx->grpprl_count));
		clx->grpprl[clx->grpprl_count - 1] = (U8 *) wvMalloc (cb);
		for (i = 0; i < cb; i++)
		    clx->grpprl[clx->grpprl_count - 1][i] = read_8ubit (fd);
		j += i;
	    }
	  else if (clxt == 2)
	    {
		if (ver == WORD8)
		  {
		      lcb = read_32ubit (fd);
		      j += 4;
		  }
		else
		  {
		      wvTrace (("Here so far\n"));
#if 0
		      lcb = read_16ubit (fd);	/* word 6 only has two bytes here */
		      j += 2;
#endif

		      lcb = read_32ubit (fd);	/* word 6 specs appeared to have lied ! */
		      j += 4;
		  }
		wvGetPCD_PLCF (&clx->pcd, &clx->pos, &clx->nopcd,
			       wvStream_tell (fd), lcb, fd);
		j += lcb;

		if (ver <= WORD7)	/* MV 28.8.2000 Appears to be valid */
		  {
#if 0
		      /* DANGER !!, this is a completely mad attempt to differenciate 
		         between word 95 files that use 16 and 8 bit characters. It may
		         not work, it attempt to err on the side of 8 bit characters.
		       */
		      if (!(wvGuess16bit (clx->pcd, clx->pos, clx->nopcd)))
#else
		      /* I think that this is the correct reason for this behaviour */
		      if (fExtChar == 0)
#endif
			  for (i = 0; i < clx->nopcd; i++)
			    {
				clx->pcd[i].fc *= 2;
				clx->pcd[i].fc |= 0x40000000UL;
			    }
		  }
	    }
	  else
	    {
		wvError (("clxt is not 1 or 2, it is %d\n", clxt));
		return;
	    }
      }
}


void
wvInitCLX (CLX * item)
{
    item->pcd = NULL;
    item->pos = NULL;
    item->nopcd = 0;

    item->grpprl_count = 0;
    item->cbGrpprl = NULL;
    item->grpprl = NULL;
}


int
wvGetPieceBoundsFC (U32 * begin, U32 * end, CLX * clx, U32 piececount)
{
    int type;
    if ((piececount + 1) > clx->nopcd)
      {
	  wvTrace (
		   ("piececount is > nopcd, i.e.%d > %d\n", piececount + 1,
		    clx->nopcd));
	  return (-1);
      }
    *begin = wvNormFC (clx->pcd[piececount].fc, &type);

    if (type)
	*end = *begin + (clx->pos[piececount + 1] - clx->pos[piececount]);
    else
	*end = *begin + ((clx->pos[piececount + 1] - clx->pos[piececount]) * 2);

    return (type);
}

int
wvGetPieceBoundsCP (U32 * begin, U32 * end, CLX * clx, U32 piececount)
{
    if ((piececount + 1) > clx->nopcd)
	return (-1);
    *begin = clx->pos[piececount];
    *end = clx->pos[piececount + 1];
    return (0);
}


char *
wvAutoCharset (wvParseStruct * ps)
{
    U16 i = 0;
    int flag;
    char *ret;
    ret = "iso-8859-15";

    /* 
       If any of the pieces use unicode then we have to assume the
       worst and use utf-8
     */
    while (i < ps->clx.nopcd)
      {
	  wvNormFC (ps->clx.pcd[i].fc, &flag);
	  if (flag == 0)
	    {
		ret = "UTF-8";
		break;
	    }
	  i++;
      }

    /* 
       Also if the document fib is not codepage 1252 we also have to 
       assume the worst 
     */
    if (strcmp (ret, "UTF-8"))
      {
	  if (
	      (ps->fib.lid != 0x407) &&
	      (ps->fib.lid != 0x807) &&
	      (ps->fib.lid != 0x409) &&
	      (ps->fib.lid != 0x807) && (ps->fib.lid != 0xC09))
	      ret = "UTF-8";
      }
    return (ret);
}



int
wvQuerySamePiece (U32 fcTest, CLX * clx, U32 piece)
{
    /*
       wvTrace(("Same Piece, %x %x %x\n",fcTest,wvNormFC(clx->pcd[piece].fc,NULL),wvNormFC(clx->pcd[piece+1].fc,NULL)));
       if ( (fcTest >= wvNormFC(clx->pcd[piece].fc,NULL)) && (fcTest < wvNormFC(clx->pcd[piece+1].fc,NULL)) )
     */
    wvTrace (
	     ("Same Piece, %x %x %x\n", fcTest, clx->pcd[piece].fc,
	      wvGetEndFCPiece (piece, clx)));
    if ((fcTest >= wvNormFC (clx->pcd[piece].fc, NULL))
	&& (fcTest < wvGetEndFCPiece (piece, clx)))
	return (1);
    return (0);
}


U32
wvGetPieceFromCP (U32 currentcp, CLX * clx)
{
    U32 i = 0;
    while (i < clx->nopcd)
      {
	  wvTrace (
		   ("i %d: currentcp is %d, clx->pos[i] is %d, clx->pos[i+1] is %d\n",
		    i, currentcp, clx->pos[i], clx->pos[i + 1]));
	  if ((currentcp >= clx->pos[i]) && (currentcp < clx->pos[i + 1]))
	      return (i);
	  i++;
      }
    wvTrace (("cp was not in any piece ! \n", currentcp));
    return (0xffffffffL);
}

U32
wvGetEndFCPiece (U32 piece, CLX * clx)
{
    int flag;
    U32 fc;
    U32 offset = clx->pos[piece + 1] - clx->pos[piece];

    wvTrace (("offset is %x, befc is %x\n", offset, clx->pcd[piece].fc));
    fc = wvNormFC (clx->pcd[piece].fc, &flag);
    wvTrace (("fc is %x, flag %d\n", fc, flag));
    if (flag)
	fc += offset;
    else
	fc += offset * 2;
    wvTrace (("fc is finally %x\n", fc));
    return (fc);
}

/*
1) search for the piece containing the character in the piece table.

2) Then calculate the FC in the file that stores the character from the piece
    table information.
*/
U32
wvConvertCPToFC (U32 currentcp, CLX * clx)
{
    U32 currentfc = 0xffffffffL;
    U32 i = 0;
    int flag;

    while (i < clx->nopcd)
      {
	  if ((currentcp >= clx->pos[i]) && (currentcp < clx->pos[i + 1]))
	    {
		currentfc = wvNormFC (clx->pcd[i].fc, &flag);
		if (flag)
		    currentfc += (currentcp - clx->pos[i]);
		else
		    currentfc += ((currentcp - clx->pos[i]) * 2);
		break;
	    }
	  i++;
      }

    if (currentfc == 0xffffffffL)
      {
	  i--;
	  currentfc = wvNormFC (clx->pcd[i].fc, &flag);
	  if (flag)
	      currentfc += (currentcp - clx->pos[i]);
	  else
	      currentfc += ((currentcp - clx->pos[i]) * 2);
	  wvTrace (("flaky cp to fc conversion underway\n"));
      }

    return (currentfc);
}

struct test {
    U32 fc;
    U32 offset;
};

int
compar (const void *a, const void *b)
{
    struct test *one, *two;
    one = (struct test *) a;
    two = (struct test *) b;

    if (one->fc < two->fc)
	return (-1);
    else if (one->fc == two->fc)
	return (0);
    return (1);
}

/* 
In word 95 files there is no flag attached to each
offset as there is in word 97 to tell you that we are
talking about 16 bit chars, so I attempt here to make
an educated guess based on overlapping offsets to
figure it out, If I had some actual information as
the how word 95 actually stores it it would help.
*/

int
wvGuess16bit (PCD * pcd, U32 * pos, U32 nopcd)
{
    struct test *fcs;
    U32 i;
    int ret = 1;
    fcs = (struct test *) wvMalloc (sizeof (struct test) * nopcd);
    for (i = 0; i < nopcd; i++)
      {
	  fcs[i].fc = pcd[i].fc;
	  fcs[i].offset = (pos[i + 1] - pos[i]) * 2;
      }

    qsort (fcs, nopcd, sizeof (struct test), compar);

    for (i = 0; i < nopcd - 1; i++)
      {
	  if (fcs[i].fc + fcs[i].offset > fcs[i + 1].fc)
	    {
		wvTrace (("overlap, my guess is 8 bit\n"));
		ret = 0;
		break;
	    }
      }

    wvFree (fcs);
    return (ret);
}
