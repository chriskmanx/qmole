/* -*- mode: C++; tab-width: 4; c-basic-offset: 4; -*- */

/* AbiWord
 * Copyright (C) 1998 AbiSource, Inc.
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


#ifndef IE_IMPGRAPHIC_BMP_H
#define IE_IMPGRAPHIC_BMP_H

#include "ut_bytebuf.h"

#include "png.h"

#include "ie_impGraphic.h"

class IE_ImpGraphicBMP_Sniffer : public IE_ImpGraphicSniffer
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

class IE_ImpGraphic_BMP : public IE_ImpGraphic
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

	// BMP File Header Data
	UT_uint16	m_iFileType;		// type - 'BM' for Bitmaps
	UT_uint32	m_iFileSize;		// file size in bytes
	UT_uint16	m_iXHotspot;		// 0 or x hotspot
	UT_uint16	m_iYHotspot;		// 0 or y hotspot
	UT_uint32	m_iOffset;			// Offset to BMP image

	// BMP Header Data
	UT_uint32	m_iHeaderSize;		// Size of Header Data
	UT_sint32	m_iWidth;			// Image Width in pixels
	UT_sint32	m_iHeight;			// Image Height in pixels
	UT_uint16	m_iPlanes;			// Number of Planes == 1
	UT_uint16	m_iBitsPerPlane;	// Bit per pixel
	UT_uint32	m_iCompression;		// compression flag
	UT_uint32	m_iImageSize;		// Image size in bytes
	UT_uint32	m_iXResolution;		// Horizontal Resolution (Pels/Meter)
	UT_uint32	m_iYResolution;		// Vertical Resolution (Pels/Meter)
	UT_uint32	m_iClrUsed;			// Color Table Size
	UT_uint32	m_iClrImportant;	// Important Color Count
	UT_uint16	m_iResolutionUnits; // Units of Measure
	UT_uint16	m_iPadding;			// Reserved
	UT_uint16	m_iOrigin;			// Recording Algorithm
	UT_uint16	m_iHalfToning;		// Halftoning Algorithm
	UT_uint32	m_iHalfToningParam1;// Size Value 1
	UT_uint32	m_iHalfToningParam2;// Size Value 2
	UT_uint32	m_iClrEncoding;		// Color Encoding
	UT_uint32	m_iIdentifier;		//

	// BMP Utility Data
	UT_uint32   m_iBytesRead;		// Number of Bytes Read
	bool		m_bOldBMPFormat;	// Older smaller file type
	bool		m_bHeaderDone;		// Check to see if finshed Reading Header

	void InitializePrivateClassData();
	UT_Error _convertGraphic(UT_ByteBuf * pBB);
   
	// Functions for Reading Bitmaps
	UT_Error Read_BMP_Header    (UT_ByteBuf* pBB);

	UT_Error Initialize_PNG();
	UT_Error Convert_BMP_Pallet (UT_ByteBuf* pBB);
	UT_Error Convert_BMP        (UT_ByteBuf* pBB);

	// Function to Read ByteBuffer
	UT_Byte		ReadByte  (UT_ByteBuf* pBB, UT_uint32 offset);
	UT_uint16	Read2Bytes(UT_ByteBuf* pBB, UT_uint32 offset);
	UT_uint32   Read4Bytes(UT_ByteBuf* pBB, UT_uint32 offset);
	UT_uint32   ReadBytes (UT_ByteBuf* pBB, UT_uint32 offset, UT_uint32 num_bytes);

	UT_ByteBuf*  m_pBB;				// pBB Converted to PNG File
};

#endif /* IE_IMPGRAPHIC_BMP_H */
