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

void
wvGetRS (RS * item, wvStream * fd)
{
    item->fRouted = (S16) read_16ubit (fd);
    item->fReturnOrig = (S16) read_16ubit (fd);
    item->fTrackStatus = (S16) read_16ubit (fd);
    item->fDirty = (S16) read_16ubit (fd);
    item->nProtect = (S16) read_16ubit (fd);
    item->iStage = (S16) read_16ubit (fd);
    item->delOption = (S16) read_16ubit (fd);
    item->cRecip = (S16) read_16ubit (fd);
}
