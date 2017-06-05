/* -*- mode: C++; tab-width: 4; c-basic-offset: 4; -*- */

/* AbiWord
 * Copyright (C) 1998 AbiSource, Inc.
 * Copyright (C) 2000 Hubert Figuière
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

#ifndef IE_IMP_MSWRITE_H
#define IE_IMP_MSWRITE_H

#include <stdio.h>

#include "ut_bytebuf.h"
#include "ut_string_class.h"

#include "ie_imp.h"
#include "ie_impexp_MSWrite.h"

class PD_Document;

/* the fonts */
typedef struct wri_font {
	short	ffid;
	char	*name;
} wri_font;

typedef struct wri_image {
    unsigned char *png_image;
    int length;
} wri_image;

// The importer/reader for MS Write Files.

class IE_Imp_MSWrite_Sniffer : public IE_ImpSniffer
{
	friend class IE_Imp;

public:
	IE_Imp_MSWrite_Sniffer();
	virtual ~IE_Imp_MSWrite_Sniffer() {}

	virtual const IE_SuffixConfidence * getSuffixConfidence ();
	virtual UT_Confidence_t recognizeContents (const char * szBuf, 
									UT_uint32 iNumbytes);
	virtual const IE_MimeConfidence * getMimeConfidence () { return NULL; }
	virtual bool getDlgLabels (const char ** szDesc,
							   const char ** szSuffixList,
							   IEFileType * ft);
	virtual UT_Error constructImporter (PD_Document * pDocument,
										IE_Imp ** ppie);

};


class IE_Imp_MSWrite : public IE_Imp
{
public:
	IE_Imp_MSWrite(PD_Document * pDocument);
	~IE_Imp_MSWrite();
	
protected:
	virtual UT_Error    _loadFile(GsfInput * input);
	UT_Error			_parseFile();
	UT_Error			_writeHeader();
	
private:
	void free_ffntb ();
	int read_ffntb ();
	void translate_char (char ch, UT_UCS4String & buf);
	int read_pap ();
	int read_char (int fcFirst2, int fcLim2);
	int wri_pict_read (unsigned char *data, int size);
	int wri_pict_print_data ();
	
	GsfInput* mFile;
	
	UT_uint32 wri_fonts_count;
	struct wri_font *wri_fonts;
	struct wri_image **wri_images;
	UT_uint32 wri_images_count;
	
	struct wri_struct *write_file_header;
	struct wri_struct *write_picture;
	
	UT_UCS4String mCharBuf;    // buffer for char runs.
	UT_ByteBuf mTextBuf;       // complete text buffer as extracted out of the file.
};

#endif /* IE_IMP_MSWRITE_H */
