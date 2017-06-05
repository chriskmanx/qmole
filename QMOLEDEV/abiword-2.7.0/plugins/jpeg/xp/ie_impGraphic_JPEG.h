/* -*- mode: C++; tab-width: 4; c-basic-offset: 4; -*- */

/* AbiWord
 * Copyright (C) 1998 AbiSource, Inc.
 * Copyright (C) 2001 Hubert Figuiere
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


#ifndef IE_IMPGRAPHIC_JPEG_H
#define IE_IMPGRAPHIC_JPEG_H

#include <stdio.h>  //required by jpeglib.h at least

#include "ut_bytebuf.h"
#include "png.h"

extern "C" 
{
#include "jpeglib.h"
}


#include "ie_impGraphic.h"

class IE_ImpGraphicJPEG_Sniffer : public IE_ImpGraphicSniffer
{
 public:
	virtual const IE_SuffixConfidence * getSuffixConfidence ();
	virtual const IE_MimeConfidence * getMimeConfidence () { return NULL; }
	virtual UT_Confidence_t recognizeContents (const char * szBuf, 
					UT_uint32 iNumbytes);
	virtual bool getDlgLabels (const char ** szDesc,
				   const char ** szSuffixList,
				   IEGraphicFileType * ft);
	virtual UT_Error constructImporter (IE_ImpGraphic ** ppieg);
};

class IE_ImpGraphic_JPEG : public IE_ImpGraphic
{
public:
        virtual UT_Error	importGraphic(UT_ByteBuf* pBB, 
					      FG_Graphic ** ppfg);
        virtual UT_Error	convertGraphic(UT_ByteBuf* pBB, 
					       UT_ByteBuf** ppBB);
 private:
	// PNG structures used
	png_structp m_pPNG;				// libpng structure for the PNG Object
	png_infop   m_pPNGInfo;			// libpng structure for info on the PNG Object
	UT_ByteBuf*  m_pPngBB;				// pBB Converted to PNG File

	UT_Error _convertGraphic (UT_ByteBuf* pBB);
	UT_Error Initialize_PNG();

	typedef struct {
	    struct jpeg_source_mgr pub;	/* public fields */
	    
	    UT_ByteBuf* sourceBuf;
	    UT_uint32 pos;
	} bytebuf_jpeg_source_mgr;
	typedef bytebuf_jpeg_source_mgr * bytebuf_jpeg_source_ptr;
	
	// JPEG callbacks
	void _jpegByteBufSrc (j_decompress_ptr cinfo, UT_ByteBuf* sourceBuf);
	static void _jpegInitSource (j_decompress_ptr cinfo);
	static boolean _jpegFillInputBuffer (j_decompress_ptr cinfo);
	static void _jpegSkipInputData (j_decompress_ptr cinfo, long num_bytes);
	static void _jpegTermSource (j_decompress_ptr cinfo);
	

};

#endif /* IE_IMPGRAPHIC_JPEG_H */
