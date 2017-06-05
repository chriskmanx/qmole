/* -*- c-basic-offset: 4; tab-width: 4; indent-tabs-mode: t -*- */

/* AbiWord
 * Copyright (C) 1998 AbiSource, Inc.
 * Copyright (C) 2001 Hubert Figuiere
 *
 * Portions from JPEG Library
 * Copyright (C) 1994-1996, Thomas G. Lane.
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

/*
  This is the core of the JPEG importer.
 */

#include <stdio.h>
#include <string.h>

#include "ut_string.h"
#include "ut_debugmsg.h"
#include "ut_assert.h"
#include "ie_impGraphic_JPEG.h"
#include "fg_GraphicRaster.h"
#include "xap_Module.h"

#ifdef ABI_PLUGIN_BUILTIN
#define abi_plugin_register abipgn_jpeg_register
#define abi_plugin_unregister abipgn_jpeg_unregister
#define abi_plugin_supports_version abipgn_jpeg_supports_version
// dll exports break static linking
#define ABI_BUILTIN_FAR_CALL extern "C"
#else
#define ABI_BUILTIN_FAR_CALL ABI_FAR_CALL
ABI_PLUGIN_DECLARE("JPEG")
#endif

// TODO: shouldn't those function be part of a class instead of being duplicated each time ?
static void _write_png( png_structp png_ptr, 
		        png_bytep data, 
		        png_size_t length )
{
	UT_ByteBuf* bb = static_cast<UT_ByteBuf*>(png_get_io_ptr(png_ptr));
	bb->append(data, length);
}

//static void _write_flush(png_structp png_ptr) { } // Empty Fuction.

// supported suffixes
static IE_SuffixConfidence IE_ImpGraphicJPEG_Sniffer__SuffixConfidence[] = {
	{ "jpg", 	UT_CONFIDENCE_PERFECT 	},
	{ "jpeg", 	UT_CONFIDENCE_PERFECT 	},
	{ "", 	UT_CONFIDENCE_ZILCH 	}
};

const IE_SuffixConfidence * IE_ImpGraphicJPEG_Sniffer::getSuffixConfidence ()
{
	return IE_ImpGraphicJPEG_Sniffer__SuffixConfidence;
}

UT_Confidence_t IE_ImpGraphicJPEG_Sniffer::recognizeContents(const char * szBuf, UT_uint32 iNumbytes)
{
	bool isJPEG = false;
	if (iNumbytes >= 10) 
	{
		/* this file recognition has been done from /etc/magic file */
		isJPEG = (szBuf [0] == static_cast<char>(0xff)) && (szBuf [1] == static_cast<char>(0xd8));
		isJPEG = isJPEG && (strncmp(&(szBuf[6]), "JFIF", 4) == 0);
	}
   	return isJPEG ? UT_CONFIDENCE_PERFECT : UT_CONFIDENCE_ZILCH;
}

bool IE_ImpGraphicJPEG_Sniffer::getDlgLabels(const char ** pszDesc,
									   const char ** pszSuffixList,
									   IEGraphicFileType * ft)
{
    // TODO add a more complete list of suffixes
	*pszDesc = "JPEG Image (.jpg, .jpeg)";
	*pszSuffixList = "*.jpg; *.jpeg";
	*ft = getType ();
	return true;
}

UT_Error IE_ImpGraphicJPEG_Sniffer::constructImporter(IE_ImpGraphic **ppieg)
{
	*ppieg = new IE_ImpGraphic_JPEG();
	if (*ppieg == NULL)
	  return UT_IE_NOMEMORY;

	return UT_OK;
}

//  This actually creates our FG_Graphic object for a PNG
UT_Error IE_ImpGraphic_JPEG::importGraphic(UT_ByteBuf* pBB, 
										  FG_Graphic ** ppfg)
{
	UT_DEBUGMSG (("Using %s to import\n", __FILE__));
	UT_Error err = _convertGraphic(pBB); 
   	if (err != UT_OK) 
		return err;

	FG_GraphicRaster *pFGR;

	pFGR = new FG_GraphicRaster();
	if(pFGR == NULL)
		return UT_IE_NOMEMORY;

	if(!pFGR->setRaster_PNG(m_pPngBB)) {
		DELETEP(pFGR);
		
		return UT_IE_FAKETYPE;
	}

	*ppfg = static_cast<FG_Graphic *>(pFGR);
	return UT_OK;
}

UT_Error IE_ImpGraphic_JPEG::convertGraphic(UT_ByteBuf* pBB,
					   UT_ByteBuf** ppBB)
{
	UT_DEBUGMSG (("Using %s to convert\n", __FILE__));
   	if (!ppBB) return UT_ERROR;

   	UT_Error err = _convertGraphic(pBB);
   	if (err != UT_OK) return err;
   
	*ppBB = m_pPngBB;
   	return UT_OK;
}





UT_Error IE_ImpGraphic_JPEG::_convertGraphic (UT_ByteBuf* pJpegBB)
{
	UT_Error err;
	UT_uint16 colorType, bitsPerChannel;
	
	struct jpeg_decompress_struct cinfo;
	struct jpeg_error_mgr jerr;

	err = Initialize_PNG();
	if (err)
	{
		return err;
	}

	if (setjmp(m_pPNG->jmpbuf))
	{
		png_destroy_write_struct(&m_pPNG, &m_pPNGInfo);
		return UT_ERROR;
	}
	
	cinfo.err = jpeg_std_error(&jerr);
	jpeg_create_decompress(&cinfo);

	/* set the data source */
	_jpegByteBufSrc (&cinfo, pJpegBB);
	
	/**/
	jpeg_read_header(&cinfo, TRUE);
	// get image infos

	switch (cinfo.out_color_space) {
	case JCS_GRAYSCALE:
		colorType = PNG_COLOR_TYPE_GRAY;	
		bitsPerChannel = 8;  // 1 8 bits channel for Gray
		break;
	default:
		cinfo.out_color_space = JCS_RGB;
		colorType = PNG_COLOR_TYPE_RGB;	
		bitsPerChannel = 8;  // 3 8 bits channels for RGB
	}

	jpeg_start_decompress(&cinfo);

	JDIMENSION num_scanlines;

	png_set_IHDR ( m_pPNG,
			       m_pPNGInfo,
				   cinfo.output_width,			       
				   cinfo.output_height,			       
			       bitsPerChannel,
			       colorType,
			       PNG_INTERLACE_NONE,
			       PNG_COMPRESSION_TYPE_DEFAULT,
			       PNG_FILTER_TYPE_DEFAULT );

	png_write_info(m_pPNG, m_pPNGInfo);

	int row_stride = cinfo.output_width * cinfo.output_components;
	UT_Byte* pngScanline = new UT_Byte[row_stride];

	while (cinfo.output_scanline < cinfo.output_height)
	{
		num_scanlines = jpeg_read_scanlines(&cinfo, &pngScanline, 1);// TODO use a larger buffer for efficiency

		png_write_row(m_pPNG, pngScanline);
	}

	DELETEPV (pngScanline);
	jpeg_finish_decompress(&cinfo);
	/* This is an important step since it will release a good deal of memory. */
	jpeg_destroy_decompress(&cinfo);

	png_write_end(m_pPNG, m_pPNGInfo);

	return UT_OK;
}




UT_Error IE_ImpGraphic_JPEG::Initialize_PNG()
{
	/* Set up png structures for writing */
	m_pPNG = png_create_write_struct( PNG_LIBPNG_VER_STRING, 
		                              static_cast<void*>(NULL),
									  NULL, 
									  NULL );
	if( m_pPNG == NULL )
	{
		return UT_ERROR;
	}

	m_pPNGInfo = png_create_info_struct(m_pPNG);
	if ( m_pPNGInfo == NULL )
	{
		png_destroy_write_struct(&m_pPNG, static_cast<png_infopp>(NULL));
		return UT_ERROR;
	}

	/* Set error handling if you are using the setjmp/longjmp method (this is
	 * the normal method of doing things with libpng).  REQUIRED unless you
	 * set up your own error handlers in the png_create_read_struct() earlier.
	 */
	if (setjmp(m_pPNG->jmpbuf))
	{
		/* Free all of the memory associated with the png_ptr and info_ptr */
		png_destroy_write_struct(&m_pPNG, &m_pPNGInfo);
	  
		/* If we get here, we had a problem reading the file */
		return UT_ERROR;
	}
	m_pPngBB = new UT_ByteBuf;  /* Byte Buffer for Converted Data */

	/* Setting up the Data Writing Function */
	png_set_write_fn(m_pPNG, static_cast<void *>(m_pPngBB), static_cast<png_rw_ptr>(_write_png), NULL);

	return UT_OK;
}


/*
  JPEG Lib callbacks
 */



void IE_ImpGraphic_JPEG::_jpegByteBufSrc (j_decompress_ptr cinfo, UT_ByteBuf* sourceBuf)
{
	bytebuf_jpeg_source_ptr src;
	
	/* The source object and input buffer are made permanent so that a series
	 * of JPEG images can be read from the same file by calling jpeg_stdio_src
	 * only before the first one.  (If we discarded the buffer at the end of
	 * one image, we'd likely lose the start of the next one.)
	 * This makes it unsafe to use this manager and a different source
	 * manager serially with the same JPEG object.  Caveat programmer.
	 */
	if (cinfo->src == NULL) {	/* first time for this JPEG object? */
		cinfo->src = (struct jpeg_source_mgr *)
					 (*cinfo->mem->alloc_small) (reinterpret_cast<j_common_ptr>(cinfo), JPOOL_PERMANENT,
												 sizeof(bytebuf_jpeg_source_mgr));
	}
	
	src = reinterpret_cast<bytebuf_jpeg_source_ptr>(cinfo->src);
	src->pub.init_source = _jpegInitSource;
	src->pub.fill_input_buffer = _jpegFillInputBuffer;
	src->pub.skip_input_data = _jpegSkipInputData;
	src->pub.resync_to_restart = jpeg_resync_to_restart; /* use default method */
	src->pub.term_source = _jpegTermSource;
	src->sourceBuf = sourceBuf;
	src->pub.bytes_in_buffer = 0; /* forces fill_input_buffer on first read */
	src->pub.next_input_byte = NULL; /* until buffer loaded */
}

/*
 * Initialize source --- called by jpeg_read_header
 * before any data is actually read.
 */

void IE_ImpGraphic_JPEG::_jpegInitSource (j_decompress_ptr cinfo)
{
	bytebuf_jpeg_source_ptr src = reinterpret_cast<bytebuf_jpeg_source_ptr>(cinfo->src);
	
	src->pos = 0;
}

/*
 * Fill the input buffer --- called whenever buffer is emptied.
 *
 * In typical applications, this should read fresh data into the buffer
 * (ignoring the current state of next_input_byte & bytes_in_buffer),
 * reset the pointer & count to the start of the buffer, and return TRUE
 * indicating that the buffer has been reloaded.  It is not necessary to
 * fill the buffer entirely, only to obtain at least one more byte.
 *
 * There is no such thing as an EOF return.  If the end of the file has been
 * reached, the routine has a choice of ERREXIT() or inserting fake data into
 * the buffer.  In most cases, generating a warning message and inserting a
 * fake EOI marker is the best course of action --- this will allow the
 * decompressor to output however much of the image is there.  However,
 * the resulting error message is misleading if the real problem is an empty
 * input file, so we handle that case specially.
 *
 * In applications that need to be able to suspend compression due to input
 * not being available yet, a FALSE return indicates that no more data can be
 * obtained right now, but more may be forthcoming later.  In this situation,
 * the decompressor will return to its caller (with an indication of the
 * number of scanlines it has read, if any).  The application should resume
 * decompression after it has loaded more data into the input buffer.  Note
 * that there are substantial restrictions on the use of suspension --- see
 * the documentation.
 *
 * When suspending, the decompressor will back up to a convenient restart point
 * (typically the start of the current MCU). next_input_byte & bytes_in_buffer
 * indicate where the restart point will be if the current call returns FALSE.
 * Data beyond this point must be rescanned after resumption, so move it to
 * the front of the buffer rather than discarding it.
 */
boolean IE_ImpGraphic_JPEG::_jpegFillInputBuffer (j_decompress_ptr cinfo)
{
	bytebuf_jpeg_source_ptr src = reinterpret_cast<bytebuf_jpeg_source_ptr>(cinfo->src);
	
	// WARNING ! this assumes that the ByteBuf will NOT change during JPEG reading.
	src->pub.next_input_byte = src->sourceBuf->getPointer (src->pos);
	src->pub.bytes_in_buffer = src->sourceBuf->getLength ();
	
	return TRUE;
}

/*
 * Skip data --- used to skip over a potentially large amount of
 * uninteresting data (such as an APPn marker).
 *
 * Writers of suspendable-input applications must note that skip_input_data
 * is not granted the right to give a suspension return.  If the skip extends
 * beyond the data currently in the buffer, the buffer can be marked empty so
 * that the next read will cause a fill_input_buffer call that can suspend.
 * Arranging for additional bytes to be discarded before reloading the input
 * buffer is the application writer's problem.
 */

void IE_ImpGraphic_JPEG::_jpegSkipInputData (j_decompress_ptr cinfo, long num_bytes)
{
	if (num_bytes != 0) 
	{
		bytebuf_jpeg_source_ptr src = reinterpret_cast<bytebuf_jpeg_source_ptr>(cinfo->src);

		UT_ASSERT (num_bytes <= static_cast<long>(src->pub.bytes_in_buffer));
		if (num_bytes > static_cast<long>(src->pub.bytes_in_buffer))
		{
			num_bytes = src->pub.bytes_in_buffer;
		}
		src->pub.next_input_byte += static_cast<size_t>(num_bytes);
		src->pub.bytes_in_buffer -= static_cast<size_t>(num_bytes);
	}
}


/*
 * Terminate source --- called by jpeg_finish_decompress
 * after all data has been read.  Often a no-op.
 *
 * NB: *not* called by jpeg_abort or jpeg_destroy; surrounding
 * application must deal with any cleanup that should happen even
 * for error exit.
 */

void IE_ImpGraphic_JPEG::_jpegTermSource (j_decompress_ptr cinfo)
{
	/* no work necessary here */
}

/*******************************************************************/
/*******************************************************************/

// we use a reference-counted sniffer
static IE_ImpGraphicJPEG_Sniffer * m_impSniffer = 0;

ABI_FAR_CALL
int abi_plugin_register (XAP_ModuleInfo * mi)
{

	if (!m_impSniffer)
	{
	  m_impSniffer = new IE_ImpGraphicJPEG_Sniffer();
	}

	mi->name = "JPEG Import Plugin";
	mi->desc = "Import JPEG Images";
	mi->version = ABI_VERSION_STRING;
	mi->author = "Abi the Ant";
	mi->usage = "No Usage";

	IE_ImpGraphic::registerImporter (m_impSniffer);
	return 1;
}

ABI_FAR_CALL
int abi_plugin_unregister (XAP_ModuleInfo * mi)
{
	mi->name = 0;
	mi->desc = 0;
	mi->version = 0;
	mi->author = 0;
	mi->usage = 0;

	UT_ASSERT (m_impSniffer);

	IE_ImpGraphic::unregisterImporter (m_impSniffer);
	delete m_impSniffer;
	m_impSniffer = 0;

	return 1;
}

ABI_FAR_CALL
int abi_plugin_supports_version (UT_uint32 major, UT_uint32 minor, 
				 UT_uint32 release)
{
  return 1;
}

/*******************************************************************/
/*******************************************************************/
