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
#include <errno.h>

#include "wv.h"
#include "wvinternal.h"

U32 PutWord8Structs(MSOFBH *bse_pic_amsofbh, U8* p, size_t size);

U32 PutWord8MetafileHeader(PICF * apicf, U8* p);

U32 PutWord8BitmapHeader(PICF * apicf, U8* p, long len, U32 header_len);

/* return value: 1 == success 0 == failure */
int
wvGetPICF (wvVersion ver, PICF * apicf, wvStream * fd)
{
    U8 temp;
    U32 i;
    U8 *buf,*p;
    size_t size;

    long pos = wvStream_tell (fd);

    apicf->lcb = read_32ubit (fd);
    apicf->cbHeader = read_16ubit (fd);
    wvTrace (("size of pic is %x (%d)\n", apicf->cbHeader, apicf->cbHeader));
    apicf->mfp_mm = (S16) read_16ubit (fd);
    wvTrace (("mm type is %d\n", apicf->mfp_mm));
    apicf->mfp_xExt = (S16) read_16ubit (fd);
    apicf->mfp_yExt = (S16) read_16ubit (fd);
    apicf->mfp_hMF = (S16) read_16ubit (fd);
    if (apicf->mfp_mm == 99)
	wvGetBITMAP (&(apicf->obj.bitmap), fd);
    else
	wvGetrc (&(apicf->obj.arc), fd);
    apicf->dxaGoal = (S16) read_16ubit (fd);
    apicf->dyaGoal = (S16) read_16ubit (fd);
    apicf->mx = read_16ubit (fd);
    apicf->my = read_16ubit (fd);
    apicf->dxaCropLeft = (S16) read_16ubit (fd);
    apicf->dyaCropTop = (S16) read_16ubit (fd);
    apicf->dxaCropRight = (S16) read_16ubit (fd);
    apicf->dyaCropBottom = (S16) read_16ubit (fd);
    temp = read_8ubit (fd);

    apicf->brcl = temp & 0x0F;
    apicf->fFrameEmpty = (temp & 0x10) >> 4;
    apicf->fBitmap = (temp & 0x20) >> 5;
    wvTrace (("bitmap is %d\n", apicf->fBitmap));
    apicf->fDrawHatch = (temp & 0x40) >> 6;
    apicf->fError = (temp & 0x80) >> 7;

    apicf->bpp = read_8ubit (fd);
    wvGetBRC (ver, &(apicf->brcTop), fd);
    wvGetBRC (ver, &(apicf->brcLeft), fd);
    wvGetBRC (ver, &(apicf->brcBottom), fd);
    wvGetBRC (ver, &(apicf->brcRight), fd);
    apicf->dxaOrigin = (S16) read_16ubit (fd);
    apicf->dyaOrigin = (S16) read_16ubit (fd);
    if (ver == WORD8)
	apicf->cProps = (S16) read_16ubit (fd);
    else
	apicf->cProps = 0;
    pos = wvStream_tell (fd) - pos;
    for (i = pos; i < apicf->cbHeader; i++)
	read_8ubit (fd);
    wvTrace (("pos is finally %x\n", wvStream_tell (fd)));
    wvTrace (("len of data is %d\n", apicf->lcb - apicf->cbHeader));
    wvTrace (
	     ("ends at %x\n",
	      wvStream_tell (fd) + apicf->lcb - apicf->cbHeader));

    i = 0;

    if (apicf->mfp_mm < 90){
	  MSOFBH bse_pic_amsofbh;
	  size_t lHeaderSize;
	  size_t lWordStructsSize;
	  U8 *pHeader;
	  U8 *pWordStructs;

	  U32 len;
	  U32 j;
	  U8 bmp_header[40];
	  U32 header_len;
	  U32 colors_used;
	  U16 bpp;

	  pWordStructs = pHeader = 0;

	  wvTrace (("test\n"));
	  len = apicf->lcb - apicf->cbHeader;
	  
	  /*lvm007@aha.ru store*/
	  pos = wvStream_tell (fd);

	  i = wvEatOldGraphicHeader (fd, len);
  	  if(i!=-1)/*Found BMP */
	  {
		wvTrace (("len is %d, header len guess is %d\n", len, i));
		if (i + 2 >= len)
		{
			wvTrace (("all read ok methinks\n"));
			apicf->rgb = NULL;
			return 1;
		}
	    len -= i;
		
		pos = wvStream_tell (fd);

		for(j=0;j< sizeof(bmp_header);j++)
			bmp_header[j] = read_8ubit (fd);

		bpp = bmp_header[14] + (bmp_header[15] << 8);

		if ( bpp < 9)
		{
		    colors_used = bmp_header[32] 
			+ (bmp_header[33] << 8)
			+ (bmp_header[34] << 16)
			+ (bmp_header[35] << 24);
		}
		else
		{
		colors_used = 0;
		}
	
		wvStream_goto(fd,pos);  
		
		header_len = 14 + 40 + 4 * colors_used;
		

		/*Get Blip Header Size*/
		lHeaderSize = PutWord8BitmapHeader(apicf, 0, len + i-14, header_len);
		pHeader = wvMalloc(lHeaderSize);
		/*Write Blip Header*/
		PutWord8BitmapHeader(apicf, pHeader, len, header_len);
		
		/*Set Type of FBSE Header*/
		bse_pic_amsofbh.ver  = 0;
		bse_pic_amsofbh.inst = msobiDIB;
		bse_pic_amsofbh.fbt  = msofbtBlipFirst + msoblipDIB;/*msoblipJPEG msoblipPNG*/
		bse_pic_amsofbh.cbLength = lHeaderSize+len;
	  }else{/*must be WMF*/ 

		/*Get Blip Header Size*/
		lHeaderSize = PutWord8MetafileHeader(apicf, 0);
		pHeader = wvMalloc(lHeaderSize);
		/*Write Blip Header*/
		PutWord8MetafileHeader(apicf, pHeader);

		/*Set Type of FBSE Header*/
		bse_pic_amsofbh.ver  = 0;
		bse_pic_amsofbh.inst = msobiWMF; 
		bse_pic_amsofbh.fbt  = msofbtBlipFirst + msoblipWMF; 
		bse_pic_amsofbh.cbLength = lHeaderSize+len;

		i = 0;
  	    wvStream_goto(fd,pos); 
	  }
	  
	  lWordStructsSize = PutWord8Structs(&bse_pic_amsofbh, 0,0);
	  pWordStructs = wvMalloc(lWordStructsSize);	  
	  PutWord8Structs(&bse_pic_amsofbh, pWordStructs, lWordStructsSize);

 	  size = lHeaderSize + lWordStructsSize + apicf->lcb - apicf->cbHeader;
	  p = buf = wvMalloc(size);

	  if(!p)
	  {
		wvFree(pWordStructs);
		wvFree(pHeader);
		return 0;
	  }

	  memcpy(p, pWordStructs, lWordStructsSize);
	  p+= lWordStructsSize;
	  memcpy(p, pHeader, lHeaderSize);
	  p+=lHeaderSize;

	  wvFree(pWordStructs);
	  wvFree(pHeader);
	}
	else{
 		size = apicf->lcb - apicf->cbHeader;
		p = buf = wvMalloc(size);
	}
	
	for (; i < apicf->lcb - apicf->cbHeader; i++)
	  *p++ = read_8ubit (fd);
	
/*	f = fopen("test.dat","wb");
	fwrite(buf, 1,size, f);
	fclose(f);
*/	

    wvStream_memory_create(&apicf->rgb, (char*)buf, size); 
    return 1;
}

U32
wvEatOldGraphicHeader (wvStream * fd, U32 len)
{
    U32 X, entry, count = 0, test;
    U16 pad;
    test = read_32ubit (fd);	/*0x00090001 */
    if (test != 0x00090001L){
	 wvError (("Old Graphic\n"));
	 return -1;
	}
    count += 4;
    test = read_16ubit (fd);	/*0x0300 */
    if (test != 0x0300){
		wvError (("Old Graphic\n"));
 	    return -1;
	}

    count += 2;

    read_32ubit (fd);		/*changes */
    count += 4;
    test = read_16ubit (fd);	/*0x0000 */
    if (test != 0x00000000L){
  		wvError (("Old Graphic\n"));
		return -1;
	}
    count += 2;
    X = read_32ubit (fd);	/*changes, lets call this X */
    wvError (("X is %x\n", X));
    count += 4;
    test = read_16ubit (fd);		/*0x0000 */
    if (test != 0x00000000L){
		wvError (("Old Graphic\n"));
		return -1;
	}
    count += 2;

    /*
       while ( entry != X)
     */
    do
      {
	  entry = read_32ubit (fd);
	  count += 4;
	  wvTrace (
		   ("Entry is %x, %x, count is %d\n", entry,
		    wvStream_tell (fd), count));
	  switch (entry)
	    {
	    case 3:
		read_16ubit (fd);
		count += 2;
		wvTrace (
			 ("suspect that we are finished, count %d, len %d\n",
			  count, len));
		break;
	    default:
		{
		    U32 lene2 = entry - 2;
		    U32 i;
		    wvTrace (
			     ("lene2 is %d, predict end of %x\n", len,
			      wvStream_tell (fd) + (entry - 2) * 2));
		    /* RIES (rvt@dds.nl)
		       prolly a dirty patch because I check count
		       everytime it's incremnented against lene2.
		       This seems twork very well I tried it on around 15.000
		       word documents and it seems to work! */
		    for (i = 0; i < lene2; i++)
		      {
		          if ( (count + 1) >= len) return (count);
			  test = read_16ubit (fd);
			  if ((i == 0)
			      && ((test == 0x0f43) || (test == 0x0b41)))
			    {
				wvTrace (
					 ("Found a Bitmap, Will strip header and return with bitmap data\n"));
				count += 2;
    		        	if ( (count + 1) >= len) return (count);
				pad = test;
				test = read_32ubit (fd);	/*0x00cc0020 */
				if (test != 0x00cc0020)
				    wvTrace (("Old Graphic\n"));
				count += 4;
    		        	if ( (count + 1) >= len) return (count);

				if (pad == 0x0f43)
				  {
				      test = read_16ubit (fd);	/*0x0000 */
				      if (test != 0x0000)
					  wvTrace (("Old Graphic\n"));
				      count += 2;
			              if ( (count + 1) >= len) return (count);
				  }

				read_16ubit (fd);	/*width */
				count += 2;
    		        	if ( (count + 1) >= len) return (count);
				read_16ubit (fd);	/*height */
				count += 2;
    		        	if ( (count + 1) >= len) return (count);
				test = read_32ubit (fd);	/*0x00000000L */
				if (test != 0x00000000L)
				    wvTrace (("Old Graphic\n"));
				count += 4;
    		        	if ( (count + 1) >= len) return (count);
				read_16ubit (fd);	/*width */
				count += 2;
    		        	if ( (count + 1) >= len) return (count);
				read_16ubit (fd);	/*height */
				count += 2;
    		        	if ( (count + 1) >= len) return (count);
				test = read_32ubit (fd);	/*0x00000000L */
				if (test != 0x00000000L)
				    wvTrace (("Old Graphic\n"));
				count += 4;
				return (count);
			    }
			  count += 2;
	        	  if ( (count + 1) >= len) return (count);
		      }
		}
		break;
	    }
      }
    while (count + 1 < len);
    wvTrace (("Entry is %x %x, %d\n", entry, wvStream_tell (fd), count));
    return (count);
}

U32 wvPutMSOFBH(MSOFBH * amsofbh, wvStream * fd)
{
	if(fd){
		write_16ubit (fd,(U16)((amsofbh->ver & 0x000F)|(amsofbh->inst<<4)));
		write_16ubit (fd,(U16)amsofbh->fbt);
		write_32ubit (fd,(U32)amsofbh->cbLength);
	}
    return (8);
}

U32 PutWord8Structs(MSOFBH *bse_pic_amsofbh, U8* buf, size_t size)
{
	long count;
	MSOFBH amsofbh,opt_amsofbh,bse_amsofbh;
	FBSE afbse;
	FOPTE *fopte;
	
	wvStream OutStream;
	wvStream *fd;
	U8* int_buf;
	int i;

	if(!bse_pic_amsofbh)
		 return 0;

	count = 0;

	
    if(buf){
		fd = &OutStream;
		int_buf = wvMalloc(size);
		wvStream_memory_create(&fd, (char*)int_buf, size); 
	}
	else
		fd = 0;
	/*Container Data*/
	
	/*Init Data
	*OPT amsofbh*/
	opt_amsofbh.ver=0;
	opt_amsofbh.inst=0;
	opt_amsofbh.fbt = msofbtOPT;
	opt_amsofbh.cbLength = 12; /* 2*6 see wvPutFOPTEArray */
	
	/*OPT*/
	fopte = (FOPTE *) wvMalloc (sizeof (FOPTE) * 2);
	for(i=0;i<2;i++){
	 (fopte)[i].pid=0;
	 (fopte)[i].op=1;/*-i;*/
	 (fopte)[i].fComplex=0;
	 (fopte)[i].fBid=1;/*+i*(79999);*/
	 (fopte)[i].entry=0;
	}

	/*Container amsofbh*/
	amsofbh.ver=0;
	amsofbh.inst=0;
	amsofbh.fbt = msofbtSpContainer;
	amsofbh.cbLength = sizeof(opt_amsofbh) + opt_amsofbh.cbLength;
	
	/*Write Data*/
	/*Container amsofbh*/
	count+=wvPutMSOFBH(&amsofbh, fd);
	
	/*OPT amsofbh*/
	count+=wvPutMSOFBH(&opt_amsofbh, fd);
	
	/*OPT */
	if(buf)
		wvPutFOPTEArray(&fopte,&opt_amsofbh,fd);
	count+=opt_amsofbh.cbLength;
	wvFree(fopte);

	/*Set Blip Data */
	memset(&afbse,0,sizeof(afbse));
	afbse.btMacOS = 3;
	afbse.btWin32 = 4;
	afbse.cRef    = 1;
	afbse.tag     = 0xff;
	afbse.size = sizeof(*bse_pic_amsofbh) + bse_pic_amsofbh->cbLength;
	
	/*msofbtBSE amsofbh*/
	bse_amsofbh.ver=0;
	bse_amsofbh.inst=0;
	bse_amsofbh.fbt = msofbtBSE;
	bse_amsofbh.cbLength = sizeof(afbse) + afbse.size;

	count+=wvPutMSOFBH(&bse_amsofbh, fd);
	
	if(buf)
	 wvPutFBSE (&afbse, fd);
	count+=sizeof(afbse);
	
	count+=wvPutMSOFBH(bse_pic_amsofbh, fd);
	
	if(buf){
		memcpy(buf, int_buf, size);
	}
	return count;
}

/*Put Metafile Blip*/
U32 PutWord8MetafileHeader(PICF * apicf, U8* buf)
{
 int i;
 MetaFileBlip amf;
 size_t count;
 size_t chunk_size;
 U8 *p;

 memset(amf.m_rgbUidPrimary, 0, sizeof(amf.m_rgbUidPrimary));
 p = buf;
 count=0;
 chunk_size = sizeof(amf.m_rgbUid);
 if(buf){
	for(i=0;i<16;i++)
		amf.m_rgbUid[i] = i; /*I Dont know what to put here*/
	memcpy(p, &amf.m_rgbUid, chunk_size);
	/* count = fwrite( &amf.m_rgbUid,1,sizeof(amf.m_rgbUid),f);*/
	p+=chunk_size;
 }	
 count += chunk_size;

 /* dont save pointer amf.m_pvBits; */
 chunk_size = sizeof(amf.m_cb) + sizeof(amf.m_fCompression) + sizeof(amf.m_fFilter)+ sizeof(amf.m_ptSize) + sizeof(amf.m_rcBounds) + sizeof(amf.m_cbSave);
 if(buf){
	amf.m_cb = amf.m_cbSave = apicf->lcb - apicf->cbHeader;
	amf.m_fCompression= msocompressionNone;
	amf.m_fFilter = msofilterNone;
	
	amf.m_ptSize.x = apicf->dxaGoal;
	amf.m_ptSize.y = apicf->dyaGoal;
	
	amf.m_rcBounds.bottom = apicf->dyaCropBottom;
	amf.m_rcBounds.top	   = apicf->dyaCropTop;
	amf.m_rcBounds.left   = apicf->dxaCropRight;
	amf.m_rcBounds.right  = apicf->dxaCropLeft;
	amf.m_pvBits = 0;
 /*count += fwrite( &amf.m_cb,1,len,f);*/
	memcpy(p, &amf.m_cb, chunk_size);
	p+=chunk_size;
 }
 count += chunk_size;
 return count;
}

/*Put Metafile Blip*/
U32 PutWord8BitmapHeader(PICF * apicf, U8* buf, long len, U32 header_len)
{
 int i;
 BitmapBlip abm;
 size_t count;
 size_t chunk_size;
 U8 *p;
 
 p = buf;
 count=0;
 
 chunk_size = sizeof(abm.m_rgbUid);
 if(buf){
	for(i=0;i<16;i++)
		abm.m_rgbUid[i] = i; /*I Dont know what to put here*/
	/*fwrite( &abm,1,sizeof(abm.m_rgbUid),f);*/
	memcpy(p, &abm, chunk_size);
	p+=chunk_size;
 }
 count += chunk_size;

 chunk_size = sizeof(abm.m_bTag);
 if(buf){
	abm.m_bTag = 255;
	memcpy(p, &abm.m_bTag, chunk_size);
	p+=chunk_size;
 }
 count += chunk_size;
 
/*Write Bitmap Structs*/
 if(buf){
  *p++ = 0x42; /* B */
  *p++ = 0x4D; /* M */
 
  *p++ = len & 0x000000FF;
  *p++ = (len & 0x0000FF00) >> 8;
  *p++ = (len & 0x00FF0000) >> 16;
  *p++ = (len & 0xFF000000) >> 24;
 
  *p++ = 0x00;
  *p++ = 0x00;
  *p++ = 0x00;
  *p++ = 0x00;

  *p++ = header_len & 0x000000FF;
  *p++ = (header_len & 0x0000FF00) >> 8;
  *p++ = (header_len & 0x00FF0000) >> 16;
  *p++ = (header_len & 0xFF000000) >> 24;
 }
 count+=14;
 return count;
}
