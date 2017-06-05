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

#include <sys/types.h>
#include <string.h>
#include <stdio.h>
#include "wv.h"

void
wvGetDOPTYPOGRAPHY (DOPTYPOGRAPHY * dopt, wvStream * fd)
{
    int i;
    U16 temp16 = read_16ubit (fd);

    dopt->fKerningPunct = temp16 & 0x0001;
    dopt->iJustification = (temp16 & 0x0006) >> 1;
    dopt->iLevelOfKinsoku = (temp16 & 0x0018) >> 3;
    dopt->f2on1 = (temp16 & 0x0020) >> 5;
    dopt->reserved = (temp16 & 0xFFC0) >> 6;

    dopt->cchFollowingPunct = read_16ubit (fd);
    dopt->cchLeadingPunct = read_16ubit (fd);

    for (i = 0; i < 101; i++)
	dopt->rgxchFPunct[i] = read_16ubit (fd);

    for (i = 0; i < 51; i++)
	dopt->rgxchLPunct[i] = read_16ubit (fd);
}

void
wvInitDOPTYPOGRAPHY (DOPTYPOGRAPHY * dopt)
{
    int i;
    dopt->fKerningPunct = 0;
    dopt->iJustification = 0;
    dopt->iLevelOfKinsoku = 0;
    dopt->f2on1 = 0;
    dopt->reserved = 0;
    dopt->cchFollowingPunct = 0;
    dopt->cchLeadingPunct = 0;
    for (i = 0; i < 101; i++)
	dopt->rgxchFPunct[i] = 0;
    for (i = 0; i < 51; i++)
	dopt->rgxchLPunct[i] = 0;
}
