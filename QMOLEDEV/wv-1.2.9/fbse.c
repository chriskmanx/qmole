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
#include <string.h>
#include "wv.h"
#include "wvinternal.h"

void
wvCopyBlip (Blip * dest, Blip * src)
{
    int i;
    wvCopyFBSE (&dest->fbse, &src->fbse);
    dest->type = src->type;

    if (src->name)
      {
	  dest->name = (U16 *) wvMalloc (src->fbse.cbName * sizeof (U16));
	  for (i = 0; i < src->fbse.cbName; i++)
	      dest->name[i] = src->name[i];
      }
    else
	dest->name = NULL;
    switch (dest->type)
      {
      case msoblipWMF:
      case msoblipEMF:
      case msoblipPICT:
	  wvCopyMetafile (&dest->blip.metafile, &(src->blip.metafile));
	  break;
      case msoblipJPEG:
      case msoblipPNG:
      case msoblipDIB:
	  wvCopyBitmap (&dest->blip.bitmap, &(src->blip.bitmap));
	  break;
      }
}

void
wvReleaseBlip (Blip * blip)
{
    wvFree (blip->name);
}

U32
wvGetBlip (Blip * blip, wvStream * fd, wvStream * delay)
{
    U32 i, count, count2;
    MSOFBH amsofbh;
    long pos = 0;
    count = wvGetFBSE (&blip->fbse, fd);
    wvTrace (("count is %d\n", count));
    if (blip->fbse.cbName == 0)
	blip->name = NULL;
    else
	blip->name = (U16 *) wvMalloc (sizeof (U16) * blip->fbse.cbName);
    for (i = 0; i < blip->fbse.cbName; i++)
	blip->name[i] = read_16ubit (fd);
    count += blip->fbse.cbName * 2;
    wvTrace (("count is %d\n", count));
    wvTrace (("offset %x\n", blip->fbse.foDelay));

    if (delay)
      {
	  pos = wvStream_tell (delay);
	  if(blip->fbse.foDelay!=-1)
		wvStream_goto (delay, blip->fbse.foDelay);
	  wvTrace (("offset %x\n", blip->fbse.foDelay));
	  fd = delay;
      }

    count2 = wvGetMSOFBH (&amsofbh, fd);
    wvTrace (("count is %d\n", count2));
    wvTrace (
	     ("HERE is %x %x (%d)\n", wvStream_tell (fd), amsofbh.fbt,
	      amsofbh.fbt - msofbtBlipFirst));
    wvTrace (("type is %x\n", amsofbh.fbt));
    switch (amsofbh.fbt - msofbtBlipFirst)
      {
      case msoblipWMF:
      case msoblipEMF:
      case msoblipPICT:
	  count2 += wvGetMetafile (&blip->blip.metafile, &amsofbh, fd);
	  break;
      case msoblipJPEG:
      case msoblipPNG:
      case msoblipDIB:
	  count2 += wvGetBitmap (&blip->blip.bitmap, &amsofbh, fd);
	  break;
      }
    wvTrace (("count is %d\n", count2));
    blip->type = amsofbh.fbt - msofbtBlipFirst;

    if (delay)
      {
	  wvStream_goto (delay, pos);
	  return (count);
      }

    return (count + count2);
}

U32
wvGetFBSE (FBSE * afbse, wvStream * fd)
{
    int i;
    afbse->btWin32 = read_8ubit (fd);
    afbse->btMacOS = read_8ubit (fd);
    for (i = 0; i < 16; i++)
	afbse->rgbUid[i] = read_8ubit (fd);
    afbse->tag = read_16ubit (fd);
    afbse->size = read_32ubit (fd);
    afbse->cRef = read_32ubit (fd);
    afbse->foDelay = read_32ubit (fd);
    wvTrace (("location is %x, size is %d\n", afbse->foDelay, afbse->size));
    afbse->usage = read_8ubit (fd);
    afbse->cbName = read_8ubit (fd);
    wvTrace (("name len is %d\n", afbse->cbName));
    afbse->unused2 = read_8ubit (fd);
    afbse->unused3 = read_8ubit (fd);
    return (36);
}

void
wvCopyFBSE (FBSE * dest, FBSE * src)
{
    memcpy (dest, src, sizeof (FBSE));
}


U32
wvGetBitmap (BitmapBlip * abm, MSOFBH * amsofbh, wvStream * fd)
{
    U32 i, count;
    char extra = 0;
    wvStream * stm = NULL;
    wvTrace (("starting bitmap at %x\n", wvStream_tell (fd)));
    for (i = 0; i < 16; i++)
	abm->m_rgbUid[i] = read_8ubit (fd);
    count = 16;

    abm->m_rgbUidPrimary[0] = 0;

    switch (amsofbh->fbt - msofbtBlipFirst)
      {
      case msoblipPNG:
	  wvTrace (("msoblipPNG\n"));
	  /*  sprintf(buffer,"%s-wv-%d.png",aimage,no++); */
	  if (amsofbh->inst ^ msobiPNG)
	      extra = 1;
	  break;
      case msoblipJPEG:
	  wvTrace (("msoblipJPEG\n"));
	  /*  sprintf(buffer,"%s-wv-%d.jpg",aimage,no++); */
	  if (amsofbh->inst ^ msobiJFIF)
	      extra = 1;
	  break;
      case msoblipDIB:
	  wvTrace (("msoblipDIB\n"));
	  /*  sprintf(buffer,"%s-wv-%d.dib",aimage,no++); */
	  if (amsofbh->inst ^ msobiDIB)
	      extra = 1;
	  break;
      }

    if (extra)
      {
	  for (i = 0; i < 16; i++)
	      abm->m_rgbUidPrimary[i] = read_8ubit (fd);
	  count += 16;
      }

    abm->m_bTag = read_8ubit (fd);
    abm->m_pvBits = NULL;

    count++;
    stm = wvStream_TMP_create (amsofbh->cbLength);

    if (!stm) {
      abm->m_pvBits = NULL;
      return 0;
    }

    char *tmp = wvMalloc( amsofbh->cbLength - count);
    if (!tmp) {
      abm->m_pvBits = NULL;
      return 0;
    }
    wvStream_read(tmp,1,amsofbh->cbLength - count,fd);
    wvStream_write(tmp,1,amsofbh->cbLength - count,stm);
    wvFree(tmp);
    
    wvStream_rewind (stm);
    
    abm->m_pvBits = stm;

    count += i;
    return count;
}

void
wvCopyBitmap (BitmapBlip * dest, BitmapBlip * src)
{
    U8 i;
    for (i = 0; i < 16; i++)
      {
	  dest->m_rgbUid[i] = src->m_rgbUid[i];
	  dest->m_rgbUidPrimary[i] = src->m_rgbUidPrimary[i];
      }

    dest->m_bTag = src->m_bTag;
    dest->m_pvBits = src->m_pvBits;
}


U32
wvGetMetafile (MetaFileBlip * amf, MSOFBH * amsofbh, wvStream * fd)
{
    char extra = 0;
    U32 i, count;
    wvStream * stm = 0;
    char *buf, *p;

    for (i = 0; i < 16; i++)
	amf->m_rgbUid[i] = read_8ubit (fd);
    count = 16;

    amf->m_rgbUidPrimary[0] = 0;

    switch (amsofbh->fbt - msofbtBlipFirst)
      {
      case msoblipEMF:
	  wvTrace (("msoblipEMF\n"));
	  /*
	     sprintf(buffer,"%s-wv-%d.emf",aimage,no++);
	   */
	  if (amsofbh->inst ^ msobiEMF)
	      extra = 1;
	  break;
      case msoblipWMF:
	  wvTrace (("msoblipWMF\n"));
	  /*
	     sprintf(buffer,"%s-wv-%d.wmf",aimage,no++);
	   */
	  if (amsofbh->inst ^ msobiWMF)
	      extra = 1;
	  break;
      case msoblipPICT:
	  wvTrace (("msoblipPICT\n"));
	  /*
	     sprintf(buffer,"%s-wv-%d.pict",aimage,no++);
	   */
	  if (amsofbh->inst ^ msobiPICT)
	      extra = 1;
	  break;
      }

    if (extra)
      {
	  for (i = 0; i < 16; i++)
	      amf->m_rgbUidPrimary[i] = read_8ubit (fd);
	  count += 16;
      }


    amf->m_cb = read_32ubit (fd);
    amf->m_rcBounds.bottom = read_32ubit (fd);
    amf->m_rcBounds.top = read_32ubit (fd);
    amf->m_rcBounds.right = read_32ubit (fd);
    amf->m_rcBounds.left = read_32ubit (fd);
    amf->m_ptSize.y = read_32ubit (fd);
    amf->m_ptSize.x = read_32ubit (fd);
    amf->m_cbSave = read_32ubit (fd);
    amf->m_fCompression = read_8ubit (fd);
    amf->m_fFilter = read_8ubit (fd);
    amf->m_pvBits = NULL;
    count += 34;

    buf = wvMalloc(amsofbh->cbLength);
    p = buf;

    for (i = count; i < amsofbh->cbLength; i++)
	*p++ = read_8ubit (fd);
    count += i;

    wvStream_memory_create (&stm, buf, amsofbh->cbLength);

    amf->m_pvBits = stm; 

    return (count);
}


void wvCopyMetafile (MetaFileBlip * dest,
		     MetaFileBlip * src)
{
  U8 i; for (i = 0; i < 16; i++)
    {
      dest->m_rgbUid[i] = src->m_rgbUid[i];
      dest->m_rgbUidPrimary[i] = src->m_rgbUidPrimary[i];}
  dest->m_cb = src->m_cb;
  dest->m_rcBounds.bottom = src->m_rcBounds.bottom;
  dest->m_rcBounds.top = src->m_rcBounds.top;
  dest->m_rcBounds.right = src->m_rcBounds.right;
  dest->m_rcBounds.left = src->m_rcBounds.left;
  dest->m_ptSize.y = src->m_ptSize.y;
  dest->m_ptSize.x = src->m_ptSize.x;
  dest->m_cbSave = src->m_cbSave;
  dest->m_fCompression = src->m_fCompression;
  dest->m_fFilter = src->m_fFilter;
  dest->m_pvBits = src->m_pvBits;
}

/* TODO: code wvPutBlip(), wvPutMetafile() */

void
wvPutFBSE (FBSE * item, wvStream * fd)
{
    int i;

    write_8ubit (fd, item->btWin32);
    write_8ubit (fd, item->btMacOS);

    for (i = 0; i < 16; i++)
	write_8ubit (fd, item->rgbUid[i]);

    write_16ubit (fd, item->tag);
    write_32ubit (fd, item->size);
    write_32ubit (fd, item->cRef);
    write_32ubit (fd, item->foDelay);
    write_8ubit (fd, item->usage);
    write_8ubit (fd, item->cbName);
    write_8ubit (fd, item->unused2);
    write_8ubit (fd, item->unused3);
}
