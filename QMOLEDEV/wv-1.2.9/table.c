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
#include "wv.h"
#include "bintree.h"

/*
allow figures within 3 units of each other to
be considered the same
*/
int
cellCompLT (void *a, void *b)
{
    S16 *a2, *b2;

    if (cellCompEQ (a, b))
	return (0);

    a2 = (S16 *) a;
    b2 = (S16 *) b;
    return (*a2 < (*b2) + 3);
}

int
cellCompEQ (void *a, void *b)
{
    int ret;
    S16 *a2, *b2;
    a2 = (S16 *) a;
    b2 = (S16 *) b;
    ret = abs (*a2 - *b2);
    if (ret <= 3)
	ret = 1;
    else
	ret = 0;
    return (ret);
}

void
wvGetRowTap (wvParseStruct * ps, PAP * dpap, U32 para_intervals,
	     BTE * btePapx, U32 * posPapx)
{
    PAPX_FKP para_fkp;
	
    U32 para_fcFirst, para_fcLim = 0xffffffffL;
    PAP apap;
    U32 i;
    S32 j = 0;
    wvVersion ver = wvQuerySupported (&ps->fib, NULL);
    wvCopyPAP (&apap, dpap);

    wvInitPAPX_FKP (&para_fkp);

    i = wvStream_tell (ps->mainfd);
    wvTrace (("RowTab begin\n"));
    do
      {
	  wvReleasePAPX_FKP (&para_fkp);
	  wvGetSimpleParaBounds (ver, &para_fkp,
				 &para_fcFirst, &para_fcLim, i, btePapx,
				 posPapx, para_intervals, ps->mainfd);
	  wvTrace (("2: para from %x to %x\n", para_fcFirst, para_fcLim));
	  wvAssembleSimplePAP (ver, &apap, para_fcLim, &para_fkp, ps);
	  i = para_fcLim;
      }
    while ((apap.fTtp == 0) && apap.fInTable); /* placing '&& apap.fInTable' here fixes #11433. I can't find any regressions */

    wvTrace (("fTtp is %d\n", apap.fTtp));

    wvReleasePAPX_FKP (&para_fkp);
    wvCopyTAP (&(dpap->ptap), &apap.ptap);

    for (j = 0; j < apap.ptap.itcMac + 1; j++)
	wvTrace (("This Row-->%d\n", apap.ptap.rgdxaCenter[j]));
}

void
wvGetFullTableInit (wvParseStruct * ps, U32 para_intervals, BTE * btePapx,
		    U32 * posPapx)
{
    PAPX_FKP para_fkp;
	U32 para_fcFirst, para_fcLim = 0xffffffffL;
    PAP apap;
    U32 i, j = 0;
    TAP *test = NULL;
    wvVersion ver = wvQuerySupported (&ps->fib, NULL);
    if (ps->intable)
	return;

    wvInitPAPX_FKP (&para_fkp);

    i = wvStream_tell (ps->mainfd);
    wvTrace (("TOP\n"));
    do
      {
	  wvReleasePAPX_FKP (&para_fkp);
	  wvGetSimpleParaBounds (ver, &para_fkp,
				 &para_fcFirst, &para_fcLim, i, btePapx,
				 posPapx, para_intervals, ps->mainfd);
	  wvAssembleSimplePAP (ver, &apap, para_fcLim, &para_fkp, ps);
	  wvTrace (("para from %x to %x\n", para_fcFirst, para_fcLim));
	  i = para_fcLim;

	  /* ignore the row end markers */
	  /*if (apap.ptap.itcMac)*/
	  /* we ascertain the number of rows by counting the end of row
		 markers. NB: a row marker can have a 0 itcMac*/
	  if (apap.fTtp)
	    {
		test = (TAP *) realloc (test, sizeof (TAP) * (j + 1));
		wvCopyTAP (&(test[j]), &apap.ptap);
		wvTrace (("Row %d\n", j));
		j++;
	    }

      }
    while (apap.fInTable);
    wvTrace (("BOTTOM\n"));

    wvReleasePAPX_FKP (&para_fkp);

    wvSetTableInfo (ps, test, j);
    ps->intable = 1;
    ps->norows = j;
    wvFree (test);
}


/*
-------------------------
|          |            |
-------------------------
|   | | |    |     |    |
-------------------------
|                       |
-------------------------
|     |         |       |
-------------------------

==>

|   | | |  | |  |  |    |

As in this example we create a list of cell begin
positions which is a superset of all begin
positions in all rows, once we have this list we
restart at the top of the table and figure out
how many spans each cell has to achieve to match
back up to its original boundaries.

We will have to match boundaries that are with in
3 units of eachother to be the same boundary as
that occurs frequently in word tables, (gagh!)
*/
void
wvSetTableInfo (wvParseStruct * ps, TAP * ptap, int no)
{
    BintreeInfo tree;
    Node *testn, *testp;
    int i, j, k;

    if (ps->vmerges)
      {
	  wvTrace (("vmerges is not NULL\n"));
	  for (i = 0; i < ps->norows; i++)
	      wvFree (ps->vmerges[i]);
	  wvFree (ps->vmerges);
	  ps->vmerges = NULL;
      }

    if (no == 0)
      {
	  wvWarning ("Broken tables, continuing and hoping for the best\n");
	  ps->nocellbounds = 0;
	  return;
      }

    InitBintree (&tree, cellCompLT, cellCompEQ);

    wvTrace (("we still ok, no is %d\n", no));

    for (i = 0; i < no; i++)
      {
		  for (j = 0; j < ptap[i].itcMac + 1; j++)
	    {
		wvTrace (("%d\n", ptap[i].rgdxaCenter[j]));
		InsertNode (&tree, (void *) &(ptap[i].rgdxaCenter[j]));
	    }
      }
    wvTrace (("end of in\n"));

    testn = NextNode (&tree, NULL);

    ps->nocellbounds = tree.no_in_tree;
    wvFree (ps->cellbounds);
    if (tree.no_in_tree)
	ps->cellbounds = (S16 *) wvMalloc (sizeof (S16) * tree.no_in_tree);
    else
	ps->cellbounds = NULL;

    i = 0;
    wvTrace (("No in tree is %d\n", tree.no_in_tree));
    while (testn != NULL)
      {
	  ps->cellbounds[i++] = *((S16 *) testn->Data);
	  wvTrace (("cellbound are %d\n", ps->cellbounds[i - 1]));
	  testp = NextNode (&tree, testn);
	  wvDeleteNode (&tree, testn);
	  testn = testp;
      }
    wvTrace (("No in tree according to i is %d\n", i));

    wvTrace (("end of out\n"));

    ps->vmerges = (S16 **) wvMalloc (sizeof (S16 *) * no);
    wvTrace (("no of rows is %d\n", no));
    for (i = 0; i < no; i++)
      {
	  ps->vmerges[i] = (S16 *) wvMalloc (sizeof (S16) * ptap[i].itcMac);
	  wvTrace (("no of cells is %d\n", ptap[i].itcMac));
	  for (j = 0; j < ptap[i].itcMac; j++)
	      ps->vmerges[i][j] = 1;
      }

    for (i = no - 1; i > 0; i--)
      {
	  for (j = 0; j < ptap[i].itcMac; j++)
	    {
		wvTrace (
			 ("Vertical merge is %d\n",
			  ptap[i].rgtc[j].fVertMerge));
		if (ptap[i].rgtc[j].fVertMerge)
		  {
		      wvTrace (
			       ("Vertical merge found, row %d, cell %d\n", i,
				j));
		      /* 
		         find a cell above me with the same boundaries
		         if it is also merged increment it, and set myself to 0
		         else leave me alone
		       */
		      for (k = 0; k < ptap[i - 1].itcMac; k++)	/* the row above */
			{
			    wvTrace (
				     ("cell begins are %d %d\n",
				      ptap[i - 1].rgdxaCenter[k],
				      ptap[i].rgdxaCenter[j]));
			    wvTrace (
				     ("cell ends are %d %d\n",
				      ptap[i - 1].rgdxaCenter[k + 1],
				      ptap[i].rgdxaCenter[j + 1]));

			    if (
				(cellCompEQ
				 ((void *) &(ptap[i - 1].rgdxaCenter[k]),
				  (void *) &(ptap[i].rgdxaCenter[j])))
				&&
				(cellCompEQ
				 ((void *) &(ptap[i - 1].rgdxaCenter[k + 1]),
				  (void *) &(ptap[i].rgdxaCenter[j + 1]))))
			      {
				  wvTrace (("found a cell above me, yippee\n"));
				  if (ptap[i - 1].rgtc[k].fVertMerge)
				    {
					ps->vmerges[i - 1][k] +=
					    ps->vmerges[i][j];
					ps->vmerges[i][j] = 0;
				    }
			      }

			}
		  }
	    }
      }


    for (i = 0; i < no; i++)
	for (j = 0; j < ptap[i].itcMac; j++)
	    wvTrace (("rowspan numbers are %d\n", ps->vmerges[i][j]));
}





/*	table backgrounds */
/*
 0  1  2  3		first row 	
 4  5  6  7		even row 	
 8  9 10 11		odd row 	
12 13 14 15 	last row 	

 F  E  O  L 
 i  v  d  a
 r  e  d  s
 s  n     t
 t     C 
    C  o  C
 C  o  l  o
 o  l  u  l
 l  u  m  u
 u  m  n  m
 m  n     n
 n
*/

static int cellbgcolors[40][4][4] = {

    /*0 */
    {
     {8, 8, 8, 8},
     {8, 8, 8, 8},
     {8, 8, 8, 8},
     {8, 8, 8, 8}
     },

    /*1 */
    {
     {8, 8, 8, 8},
     {8, 8, 8, 8},
     {8, 8, 8, 8},
     {8, 8, 8, 8}
     },

    /*2 */
    {
     {8, 8, 8, 8},
     {8, 8, 8, 8},
     {8, 8, 8, 8},
     {8, 8, 8, 8}
     },

    /*3 */
    {
     {1, 1, 1, 1},
     {8, 8, 8, 8},
     {8, 8, 8, 8},
     {8, 8, 8, 8}
     },

    /*4 */
    {
     {8, 8, 8, 8},
     {8, 8, 8, 8},
     {8, 8, 8, 8},
     {8, 8, 8, 8}
     },

    /*5 */
    {
     {12, 12, 12, 12},
     {16, 8, 8, 8},
     {16, 8, 8, 8},
     {16, 8, 8, 8}
     },

    /*6 */
    {
     {9, 9, 9, 9},
     {16, 16, 16, 16},
     {16, 16, 16, 16},
     {8, 8, 8, 8}
     },

    /*7 */
    {
     {2, 2, 2, 2},
     {8, 8, 8, 8},
     {8, 8, 8, 8},
     {15, 15, 15, 15}
     },

    /*8 */
    {
     {1, 1, 1, 1},
     {9, 11, 11, 11},
     {9, 11, 11, 11},
     {9, 11, 11, 11}
     },

    /*9 */
    {
     {6, 6, 6, 6},
     {7, 7, 7, 16},
     {7, 7, 7, 16},
     {7, 7, 7, 16}
     },

    /*10 */
    {
     {1, 11, 11, 11},
     {11, 3, 3, 3},
     {11, 3, 3, 3},
     {11, 3, 3, 3}
     },

    /*11 */
    {
     {16, 16, 7, 16},
     {16, 16, 7, 16},
     {16, 16, 7, 16},
     {16, 16, 7, 16}
     },

    /*12 */
    {
     {9, 9, 9, 9},
     {16, 16, 4, 16},
     {16, 16, 4, 16},
     {16, 16, 4, 16}
     },

    /*13 */
    {
     {9, 9, 9, 9},
     {15, 15, 16, 15},
     {15, 15, 16, 15},
     {15, 15, 16, 15}
     },

    /*14 */
    {
     {1, 1, 1, 1},
     {3, 3, 16, 3},
     {3, 3, 16, 3},
     {3, 3, 16, 3}
     },

    /*15 */
    {
     {15, 15, 8, 15},
     {15, 15, 8, 15},
     {15, 15, 8, 15},
     {15, 15, 8, 15}
     },

    /*16 */
    {
     {8, 8, 8, 8},
     {8, 8, 8, 8},
     {8, 8, 8, 8},
     {8, 8, 8, 8}
     },

    /*17 */
    {
     {8, 8, 8, 8},
     {8, 8, 8, 8},
     {8, 8, 8, 8},
     {8, 8, 8, 8}
     },

    /*18 */
    {
     {7, 7, 7, 7},
     {8, 8, 8, 8},
     {8, 8, 8, 8},
     {8, 8, 8, 8}
     },

    /*19 */
    {
     {7, 7, 7, 7},
     {8, 8, 8, 8},
     {8, 8, 8, 8},
     {7, 7, 7, 7}
     },

    /*20 */
    {
     {8, 8, 8, 8},
     {8, 8, 8, 8},
     {8, 8, 8, 8},
     {8, 8, 8, 8}
     },

    /*21 */
    {
     {8, 8, 8, 8},
     {8, 8, 8, 8},
     {8, 8, 8, 8},
     {8, 8, 8, 8}
     },

    /*22 */
    {
     {8, 8, 8, 8},
     {8, 8, 8, 8},
     {8, 8, 8, 8},
     {8, 8, 8, 8}
     },

    /*23 */
    {
     {9, 9, 9, 9},
     {8, 8, 8, 8},
     {8, 8, 8, 8},
     {8, 8, 8, 8}
     },

    /*24 */
    {
     {16, 16, 16, 16},
     {8, 8, 8, 8},
     {16, 16, 16, 16},
     {8, 8, 8, 8}
     },

    /*25 */
    {
     {11, 11, 11, 11},
     {8, 8, 8, 8},
     {4, 4, 4, 4},
     {8, 8, 8, 8}
     },

    /*26 */
    {
     {8, 8, 8, 8},
     {8, 8, 8, 8},
     {8, 8, 8, 8},
     {8, 8, 8, 8}
     },

    /*27 */
    {
     {15, 15, 15, 15},
     {8, 8, 8, 8},
     {8, 8, 8, 8},
     {8, 8, 8, 8}
     },

    /*28 */
    {
     {8, 8, 8, 8},
     {8, 8, 8, 8},
     {8, 8, 8, 8},
     {8, 8, 8, 8}
     },

    /*29 */
    {
     {15, 15, 15, 15},
     {15, 15, 15, 15},
     {16, 16, 16, 16},
     {15, 15, 15, 15}
     },

    /*30 */
    {
     {16, 16, 16, 16},
     {7, 7, 7, 7},
     {16, 16, 16, 16},
     {8, 8, 8, 8}
     },

    /*31 */
    {
     {7, 7, 7, 7},
     {6, 6, 6, 6},
     {14, 14, 14, 14},
     {6, 6, 6, 6}
     },

    /*32 */
    {
     {15, 15, 15, 15},
     {15, 15, 15, 15},
     {15, 15, 15, 15},
     {15, 15, 15, 15}
     },

    /*33 */
    {
     {15, 15, 15, 15},
     {15, 15, 15, 15},
     {15, 15, 15, 15},
     {15, 15, 15, 15}
     },

    /*34 */
    {
     {15, 15, 16, 15},
     {15, 15, 16, 15},
     {15, 15, 16, 15},
     {15, 15, 16, 15}
     },

    /*35 */
    {
     {15, 15, 15, 15},
     {15, 15, 15, 15},
     {16, 16, 16, 16},
     {15, 15, 15, 15}
     },

    /*36 */
    {
     {8, 8, 8, 8},
     {8, 8, 8, 8},
     {8, 8, 8, 8},
     {8, 8, 8, 8}
     },

    /*37 */
    {
     {1, 1, 1, 1},
     {8, 8, 8, 8},
     {8, 8, 8, 8},
     {8, 8, 8, 8}
     },

    /*38 */
    {
     {8, 8, 8, 8},
     {8, 8, 8, 8},
     {10, 10, 10, 10},
     {5, 5, 5, 5}
     },

    /*39 */
    {
     {8, 8, 8, 8},
     {4, 8, 8, 10},
     {4, 8, 8, 10},
     {4, 8, 8, 10}
     }

};


/* 
determine where we should be in the grid, 
if we do not want the special end cases, and we are
one of them, then shift into a new location in
the grid, and output the color sitting there
*/

int
wvCellBgColor (int whichrow, int whichcell, int nocells, int norows, TLP * tlp)
{
    if (whichrow == norows - 1)
	whichrow = 3;
    else if (whichrow > 0)
      {
	  if (isodd (whichrow))
	      whichrow = 2;
	  else
	      whichrow = 1;
      }

    if (whichcell == nocells - 1)
	whichcell = 3;
    else if (whichcell > 0)
      {
	  if (isodd (whichcell))
	      whichcell = 2;
	  else
	      whichcell = 1;
      }

    wvTrace (
	     ("the cell index and bgcolor is %d %d %d,%d (last cell and row are %d %d)\n",
	      tlp->itl, whichrow, whichcell,
	      cellbgcolors[tlp->itl][whichrow][whichcell], nocells, norows));
    if (tlp->itl >= 40)
      {
	  wvWarning
	      ("Table Look %d requested, but theres only %d in the list\n",
	       tlp->itl + 1, 40);
	  return (8);
      }
    return (cellbgcolors[tlp->itl][whichrow][whichcell]);
}

/*
get the end cp of the piece

i as a cp
wvGetComplexParaBounds as fc's
go to the end as a cp
*/
void
TheTest (wvParseStruct * ps, U32 piece, BTE * btePapx, U32 * posPapx,
	 U32 para_intervals)
{
    U32 piececount;
    int ichartype;
    U8  chartype;
    U32 begincp, endcp;
    U32 beginfc, endfc;
    U32 i, j, k = 0;
    U32 para_fcFirst, para_fcLim;
    PAPX_FKP para_fkp;
	PAP apap;
    int cpiece = 0;
    wvVersion ver = wvQuerySupported (&ps->fib, NULL);
    long pos = wvStream_tell (ps->mainfd);
    wvInitPAPX_FKP (&para_fkp);

    para_fcFirst = wvConvertCPToFC (ps->currentcp, &ps->clx);

    for (piececount = piece; piececount < ps->clx.nopcd; piececount++)
      {
	  ichartype =
	      wvGetPieceBoundsFC (&beginfc, &endfc, &ps->clx, piececount);
	  if(ichartype==-1)
		  break;
	  chartype = (U8) ichartype;
	  wvStream_goto (ps->mainfd, beginfc);
	  wvGetPieceBoundsCP (&begincp, &endcp, &ps->clx, piececount);
	  if (k == 0)
	    {
		wvTrace (
			 ("cp distance is %d, cp is %d\n",
			  ps->currentcp - begincp, ps->currentcp));
		wvTrace (
			 ("no of pieces is %d, this one is %d\n",
			  ps->clx.nopcd, piece));
		k++;
		begincp = ps->currentcp;
		beginfc = wvConvertCPToFC (ps->currentcp, &ps->clx);
	    }
	  wvTrace (
		   ("begin and end are %d %d (%x %x)\n", begincp, endcp,
		    beginfc, endfc));
	  para_fcLim = 0xffffffffL;
	  for (i = begincp, j = beginfc; (i < endcp && i < ps->fib.ccpText);
	       i++, j += wvIncFC (chartype))
	    {
		if ((para_fcLim == 0xffffffffL) || (para_fcLim == j))
		  {
		      wvReleasePAPX_FKP (&para_fkp);
		      wvTrace (
			       ("cp and fc are %x(%d) %x\n", i, i,
				wvConvertCPToFC (i, &ps->clx)));
		      cpiece =
			  wvGetComplexParaBounds (ver, &para_fkp,
						  &para_fcFirst, &para_fcLim,
						  wvConvertCPToFC (i, &ps->clx),
						  &ps->clx, btePapx, posPapx,
						  para_intervals, piececount,
						  ps->mainfd);
		      wvTrace (
			       ("para begin and end is %x %x, pieceend is %x\n",
				para_fcFirst, para_fcLim,
				wvConvertCPToFC (endcp, &ps->clx)));
		  }
		if (j == para_fcFirst)
		  {
		      wvAssembleSimplePAP (ver, &apap, para_fcLim, &para_fkp, ps);
		      wvAssembleComplexPAP (ver, &apap, cpiece, ps);
		      wvTrace (
			       ("table ttp are %d %d\n", apap.fInTable,
				apap.fTtp));
		  }
	    }
      }

    wvStream_goto (ps->mainfd, pos);
}

void
wvGetComplexFullTableInit (wvParseStruct * ps, U32 para_intervals,
			   BTE * btePapx, U32 * posPapx, U32 piece)
{
    PAPX_FKP para_fkp;
	U32 para_fcFirst, para_fcLim = 0xffffffffL;
    PAP apap;
    U32 i, j = 0, k = 0;
    S32 l;
    TAP *test = NULL;
    wvVersion ver = wvQuerySupported (&ps->fib, NULL);
    if (ps->intable)
	return;

#if 0
    /* some testing code */
    wvTrace (("before test\n"));
    TheTest (ps, piece, btePapx, posPapx, para_intervals);
    wvTrace (("after test\n"));
#endif

    wvInitPAPX_FKP (&para_fkp);

    i = wvStream_tell (ps->mainfd);
    wvTrace (("TOP\n"));
    do
      {
	  wvTrace (("cycle again\n"));
	  wvReleasePAPX_FKP (&para_fkp);

	  wvTrace (
		   ("2: cp and fc are %x(%d) %x\n", i, i,
		    wvConvertCPToFC (i, &ps->clx)));
	  piece =
	      wvGetComplexParaBounds (ver,
				      &para_fkp, &para_fcFirst, &para_fcLim,
				      i, &ps->clx, btePapx, posPapx,
				      para_intervals, piece, ps->mainfd);


	  if (piece == 0xffffffffL)
	      break;
	  wvAssembleSimplePAP (ver, &apap,
			       para_fcLim, &para_fkp, ps);
	  wvTrace (("para from %x to %x\n", para_fcFirst, para_fcLim));
	  wvAssembleComplexPAP (ver, &apap,
				piece, ps);

	  wvTrace (("para from %x to %x\n", para_fcFirst, para_fcLim));
	  i = para_fcLim;

	  /* ignore the row end markers */
	  /*  if ((apap.ptap.itcMac) (apap.fTtp))*/
	  /* we ascertain the number of rows by counting the end of row
		 markers. NB: a row marker can have a 0 itcMac*/
	  if (apap.fTtp)
	    {
		test = (TAP *) realloc (test, sizeof (TAP) * (j + 1));
		wvCopyTAP (&(test[j]), &apap.ptap);
		for (l = 0; l < apap.ptap.itcMac + 1; l++)
		    wvTrace (("In This Row-->%d\n", apap.ptap.rgdxaCenter[l]));
		j++;
	    }
	  if (apap.fTtp)
	      k++;
      }
    while (apap.fInTable);
    wvTrace (("BOTTOM\n"));
#ifdef DEBUG
    if (piece == 0xffffffffL)
	wvTrace (("broken on line %d\n", j));
#endif
    wvTrace (("no of lines is %d %d\n", j, k));

    wvReleasePAPX_FKP (&para_fkp);

    wvSetTableInfo (ps, test, j);
    ps->intable = 1;
    ps->norows = j;
    wvFree (test);
}

void
wvGetComplexRowTap (wvParseStruct * ps, PAP * dpap, U32 para_intervals,
		    BTE * btePapx, U32 * posPapx, U32 piece)
{
    PAPX_FKP para_fkp;
	U32 para_fcFirst, para_fcLim = 0xffffffffL;
    PAP apap;
    U32 i;
    S32 j = 0;
    wvVersion ver = wvQuerySupported (&ps->fib, NULL);
    wvCopyPAP (&apap, dpap);

    wvInitPAPX_FKP (&para_fkp);

    i = wvStream_tell (ps->mainfd);

    do
      {
	  wvReleasePAPX_FKP (&para_fkp);

	  wvTrace (
		   ("3: cp and fc are %x(%d) %x\n", i, i,
		    wvConvertCPToFC (i, &ps->clx)));
	  piece =
	      wvGetComplexParaBounds (ver, &para_fkp, &para_fcFirst, &para_fcLim,
				      i, &ps->clx, btePapx, posPapx,
				      para_intervals, piece, ps->mainfd);
	  if (piece == 0xffffffffL)
	      break;
	  wvAssembleSimplePAP (ver, &apap, para_fcLim, &para_fkp, ps);
	  wvAssembleComplexPAP (ver, &apap, piece, ps);
	  wvTrace (
		   ("para from %x to %x, table is %d\n", para_fcFirst,
		    para_fcLim, apap.fInTable));
	  i = para_fcLim;
      }
    while (apap.fTtp == 0);

    wvReleasePAPX_FKP (&para_fkp);
    wvCopyTAP (&(dpap->ptap), &apap.ptap);

    for (j = 0; j < apap.ptap.itcMac + 1; j++)
	wvTrace (("This Row-->%d\n", apap.ptap.rgdxaCenter[j]));
}
