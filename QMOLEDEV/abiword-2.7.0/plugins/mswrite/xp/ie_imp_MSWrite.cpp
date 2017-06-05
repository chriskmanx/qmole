/* -*- mode: C++; tab-width: 4; c-basic-offset: 4; -*- */

/* AbiWord
 * Copyright (C) 2001 Sean Young <sean@mess.org>
 * Copyright (C) 2001 Hubert Figuiere
 * Copyright (C) 2001 Dom Lachowicz 
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
 *
 * TODO: 
 *  - pictures: clean up / polish off code, add wmf
 *  - OLE: no support yet
 *  - headers/footers (can only be page numbers)
 *  - page size and margins
 *  - convert character sets
 *  - speed it up!
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ut_locale.h"

#include "ut_types.h"
#include "ut_assert.h"
#include "ut_debugmsg.h"
#include "ut_string.h"
#include "ie_imp_MSWrite.h"
#include "ie_impexp_MSWrite.h"
#include "pd_Document.h"
#include "ut_growbuf.h"
#include "ut_string_class.h"
#include "xap_Module.h"

#ifdef ABI_PLUGIN_BUILTIN
#define abi_plugin_register abipgn_mswrite_register
#define abi_plugin_unregister abipgn_mswrite_unregister
#define abi_plugin_supports_version abipgn_mswrite_supports_version
// dll exports break static linking
#define ABI_BUILTIN_FAR_CALL extern "C"
#else
#define ABI_BUILTIN_FAR_CALL ABI_FAR_CALL
ABI_PLUGIN_DECLARE("MsWrite")
#endif

/*****************************************************************/
/*****************************************************************/

// completely generic code to allow this to be a plugin

// we use a reference-counted sniffer
static IE_Imp_MSWrite_Sniffer * m_sniffer = 0;

ABI_BUILTIN_FAR_CALL
int abi_plugin_register (XAP_ModuleInfo * mi)
{

	if (!m_sniffer)
	{
		m_sniffer = new IE_Imp_MSWrite_Sniffer ();
	}

	mi->name = "MSWrite Importer";
	mi->desc = "Import MSWrite Documents";
	mi->version = ABI_VERSION_STRING;
	mi->author = "Abi the Ant";
	mi->usage = "No Usage";

	IE_Imp::registerImporter (m_sniffer);
	return 1;
}

ABI_BUILTIN_FAR_CALL
int abi_plugin_unregister (XAP_ModuleInfo * mi)
{
	mi->name = 0;
	mi->desc = 0;
	mi->version = 0;
	mi->author = 0;
	mi->usage = 0;

	UT_ASSERT (m_sniffer);

	IE_Imp::unregisterImporter (m_sniffer);
	delete m_sniffer;
	m_sniffer = 0;

	return 1;
}

ABI_BUILTIN_FAR_CALL
int abi_plugin_supports_version (UT_uint32 /*major*/, UT_uint32 /*minor*/, 
				 UT_uint32 /*release*/)
{
  return 1;
}

/*****************************************************************/
/*****************************************************************/

IE_Imp_MSWrite_Sniffer::IE_Imp_MSWrite_Sniffer () :
  IE_ImpSniffer("AbiMSWrite::MSWrite")
{
  // 
}

// supported suffixes
static IE_SuffixConfidence IE_Imp_MSWrite_Sniffer__SuffixConfidence[] = {
	{ "wri", 	UT_CONFIDENCE_PERFECT 	},
	{ "", 	UT_CONFIDENCE_ZILCH 	}
};

const IE_SuffixConfidence * IE_Imp_MSWrite_Sniffer::getSuffixConfidence ()
{
	return IE_Imp_MSWrite_Sniffer__SuffixConfidence;
}

UT_Confidence_t IE_Imp_MSWrite_Sniffer::recognizeContents(const char * szBuf, 
											   UT_uint32 iNumbytes)
{
    if ( iNumbytes > 8 )
    {
        if ( (szBuf[0] == static_cast<char>(0x31) || szBuf[0] == static_cast<char>(0x32)) && 
             szBuf[1] == static_cast<char>(0xbe) &&
             szBuf[4] == static_cast<char>(0) && szBuf[5] == static_cast<char>(0xab) )
        {
            return(UT_CONFIDENCE_POOR);
        }
    }
    return(UT_CONFIDENCE_ZILCH);
}

UT_Error IE_Imp_MSWrite_Sniffer::constructImporter(PD_Document * pDocument,
												   IE_Imp ** ppie)
{
    IE_Imp_MSWrite * p = new IE_Imp_MSWrite(pDocument);
    *ppie = p;
    return UT_OK;
}

bool	IE_Imp_MSWrite_Sniffer::getDlgLabels(const char ** pszDesc,
											 const char ** pszSuffixList,
											 IEFileType * ft)
{
    *pszDesc = "Microsoft Write (.wri)";
    *pszSuffixList = "*.wri";
    *ft = getFileType();
    return true;
}

/*****************************************************************/
/*****************************************************************/

#define X_CleanupIfError(ies,exp)	do { if (((ies)=(exp)) != UT_OK) goto Cleanup; } while (0)
#define X_ReturnIfFail(exp,ies)		do { bool b = (exp); if (!b) return (ies); } while (0)
#define X_ReturnNoMemIfError(exp)	X_ReturnIfFail(exp,UT_IE_NOMEMORY)

/*****************************************************************/
/*****************************************************************/

/*
 * all the font stuff
 */

void IE_Imp_MSWrite::free_ffntb () 
{
    for (UT_uint32 i=0; i < wri_fonts_count; i++) {
      FREEP(wri_fonts[i].name);
    }
    FREEP(wri_fonts);
}

int IE_Imp_MSWrite::read_ffntb () 
{
    int	page, fcMac, font_count, cbFfn;
    unsigned char byt[2], ffid;
    char *ffn;
    struct wri_font *wri_fonts_tmp;
	
    /* if the page is the same as fcMac, there are no fonts */
    page = wri_struct_value (write_file_header, "pnFfntb");
    fcMac = wri_struct_value (write_file_header, "fcMac");
    if (page == fcMac) {
		wri_fonts_count = 0;
    }
	
    if (gsf_input_seek (mFile, page++ * 0x80, G_SEEK_SET) ) {
		perror ("wri_file");
		return 1;
    }
	
    /* the first two bytes are the number of fonts */
    if (!gsf_input_read (mFile, 2, byt)) {
		perror ("wri_file");
		return 1;
    }
    wri_fonts_count = byt[0] + 256 * byt[1];
	
    font_count = 0;
    wri_fonts = NULL;
	
    while (true) {
		if (!gsf_input_read (mFile, 2, byt)) {
			perror ("wri_file");
			return 1;
		}
		cbFfn = byt[0] + 256 * byt[1];
		if (cbFfn == 0) {
			break;
		}
		if (cbFfn == 0xffff) {
    	    if (gsf_input_seek (mFile, page++ * 0x80, G_SEEK_SET)) {
				perror ("wri_file");
				return 1;
			}
			continue;
		}
		wri_fonts_tmp  = (struct wri_font*) 
			realloc (static_cast<void*>(wri_fonts), (font_count + 1) * sizeof (wri_font));
		if (!wri_fonts_tmp) {
			UT_DEBUGMSG(("Out of memory!\n"));
			free_ffntb ();
		}
		wri_fonts = static_cast<struct wri_font*>(wri_fonts_tmp);
		
		/* this is the font family identifier; this can either be
		   FF_DONTCARE, FF_ROMAN, FF_SWISS, FF_MODERN, FF_SCRIPT,
		   FF_DECORATIVE; these are defined in <windows.h>, but 
		   I don't know what to do with them */
		if (!gsf_input_read (mFile, 1, &ffid)) {
			perror ("wri_file");
			return 1;
		}
		wri_fonts[font_count].ffid = ffid;
		cbFfn--; 
		ffn = static_cast<char*>(malloc (cbFfn));
		/* we've read the first byte, so we take one of cbFfn */
		if (!gsf_input_read (mFile, cbFfn, (guint8*)ffn)) {
			perror ("wri_file");
			return 1;
		}
		wri_fonts[font_count].name = ffn;
		font_count++;
    }
    if (static_cast<unsigned>(font_count) != wri_fonts_count) {
		wri_fonts_count = font_count;
		UT_DEBUGMSG(("write file lied about number of fonts\n"));
    }
    return 0;
}

void IE_Imp_MSWrite::translate_char (char ch, UT_UCS4String & buf) {
    switch (ch) {
    case 0x0c: /* page break */
		buf += UCS_LF;
		break;
    default:
		if (ch & 0x80) 
			ch = 'x';
		buf += ch;
    }
}

/* the paragraph information */
int IE_Imp_MSWrite::read_pap () 
{
    int page, cfod, fod;
    int fcFirst, fcLim, fcMac, n;
    unsigned char pap_page[0x80];
    static const char *text_align[] = { "left", "center", "right", "justify" };

	const gchar* pProps = "props";
    UT_String propBuffer;
    UT_String tempBuffer;
	
    fcMac = wri_struct_value (write_file_header, "fcMac");
    page = wri_struct_value (write_file_header, "pnPara");
    fcFirst = 0x80;
	
    while (true) {
		gsf_input_seek (mFile, page++ * 0x80, G_SEEK_SET);
		gsf_input_read (mFile, 0x80, pap_page);
		cfod = pap_page[0x7f];
		n = READ_DWORD (pap_page);
		if (n != fcFirst) {
			UT_DEBUGMSG(("fcFirst wrong, trying to continue...\n"));
		}
		for (fod=0;fod<cfod;fod++) {
			int jc, fGraphics, dyaLine, bfProp, cch, header, rhcPage;
			int tab_count, tabs[14], jcTab[14], dxaRight, dxaLeft, dxaLeft1;
			
            /* read FOD */	
			fcLim = READ_DWORD (pap_page + 4 + fod * 6);
			bfProp = READ_WORD (pap_page + 8 + fod * 6);
			
			/* default values */
			jc = 0;
			dyaLine = 240;
			fGraphics = 0;
			header = rhcPage = 0;
			tab_count = 0;
			dxaLeft = dxaRight = dxaLeft1 = 0;
			
			if ((bfProp != 0xffff) && (bfProp + 13 < 0x80)) {
				cch = pap_page[bfProp + 4];
				if (cch >= 2) {
					jc = pap_page[bfProp + 6]  & 3;
				}
				if (cch >= 12) {
					dyaLine = pap_page[bfProp + 15] + 
						pap_page[bfProp + 16] * 256;
				}
				if (dyaLine < 240) dyaLine = 240;
				if (cch >= 17) {
					fGraphics = pap_page[bfProp + 21] & 0x10;
					header = pap_page[bfProp + 21] & 6;
					rhcPage = pap_page[bfProp + 21] & 1;
				}
				if (cch >= 6) {
					dxaRight = pap_page[bfProp + 9] + 
						pap_page[bfProp + 10] * 256;
					if (dxaRight & 0x8000) 
						dxaRight = -0x10000 + dxaRight;
				}
				if (cch >= 8) {
					dxaLeft = pap_page[bfProp + 11] + 
						pap_page[bfProp + 12] * 256;
					if (dxaLeft & 0x8000) 
						dxaLeft = -0x10000 + dxaLeft;
				}
				if (cch >= 10) {
					dxaLeft1 = pap_page[bfProp + 13] + 
						pap_page[bfProp + 14] * 256;
					if (dxaLeft1 & 0x8000) 
						dxaLeft1 = -0x10000 + dxaLeft1;
				}
				
				for (n=0;n<14;n++) {
					if (cch >= (4 * (n + 1) + 26) ) {
						tabs[tab_count] = pap_page[bfProp + n * 4 + 27] +
							256 * pap_page[bfProp + n * 4 + 28];
						jcTab[tab_count] = (pap_page[bfProp + n * 4 + 29] & 3);
						tab_count++;
					}
				}
			}
			
			/* TODO: header/footer */
			if (header) {
				UT_DEBUGMSG(("Headers and footers not supported, skipping...\n")); 
            } else {
				UT_LocaleTransactor lt (LC_NUMERIC, "C");
				UT_String_sprintf (propBuffer, "text-align:%s; line-height:%.1f",
						   text_align[jc], static_cast<float>(dyaLine) / 240.0);

				/* tabs */
				if (tab_count) {
					propBuffer += "; tabstops:";
					for (n=0; n < tab_count; n++) {
					  UT_String_sprintf (tempBuffer, "%.4fin/%c0", static_cast<float>(tabs[n]) / 1440.0,
							     jcTab[n] ? 'D' : 'L');
						propBuffer += tempBuffer;
						if (n != (tab_count - 1)) {
							propBuffer += ",";
						}
					}
				}
				
				/* indentation */
				if (dxaLeft1) {
					UT_String_sprintf (tempBuffer, "; text-indent:%.4fin", 
							 static_cast<float>(dxaLeft1) / 1440.0);
					propBuffer += tempBuffer;
				}
				if (dxaLeft) {
					UT_String_sprintf (tempBuffer, "; margin-left:%.4fin", 
							   static_cast<float>(dxaLeft) / 1440.0);
					propBuffer += tempBuffer;
				}
				if (dxaRight) {
					UT_String_sprintf (tempBuffer, "; margin-right:%.4fin", 
							 static_cast<float>(dxaRight) / 1440.0);
					propBuffer += tempBuffer;
				}

				// end of formatting

				const gchar* propsArray[3];
				propsArray[0] = pProps;
				propsArray[1] = propBuffer.c_str();
				propsArray[2] = NULL;
				
				appendStrux (PTX_Block, propsArray);
				
				if (fGraphics) {
//					wri_pict_read (static_cast<unsigned char*>(wri_text) + fcFirst - 0x80, 
//								   fcLim - fcFirst, fout);
					UT_ASSERT (UT_NOT_IMPLEMENTED);
				} else {
					read_char (fcFirst, fcLim - 1);
				}

			}
			
			fcFirst = fcLim;
			if (fcLim >= fcMac) 
				return 0;
		}
    }
}

/* the character information stuff */
int IE_Imp_MSWrite::read_char (int fcFirst2, int fcLim2) {
    int page, cfod, fcFirst, n, fod ;
    int fcLim, fcMac;
    unsigned char char_page[0x80];

	const gchar* pProps = "props";
	UT_String propBuffer;
	UT_String tempBuffer;
	
    fcMac = wri_struct_value (write_file_header, "fcMac");
    page = (fcMac + 127) / 128;
    fcFirst = 0x80;
	
    while (true) {
		gsf_input_seek (mFile, page++ * 0x80, G_SEEK_SET);
		gsf_input_read (mFile, 0x80, char_page);
		cfod = char_page[0x7f];
		n = READ_DWORD (char_page);
		if (n != fcFirst) {
			UT_DEBUGMSG(("fcFirst wrong, trying to continue...\n"));
		}
		for (fod=0;fod<cfod;fod++) {
			int cch, bfProp, ftc, hps, bold, italic, underline, hpsPos;
			
			fcLim = READ_DWORD (char_page + 4 + fod * 6);
			bfProp = READ_WORD ((char_page + 8 + fod * 6));
			
			/* default values */
			ftc = 0;
			hps = 24;
			bold = italic = underline = hpsPos = 0;
			
			if ((bfProp != 0xffff) && (bfProp + 10 < 0x80)) {
				cch = char_page[bfProp + 4];
				
				if (cch >= 2) 
					ftc = char_page[bfProp + 6] / 4;
				if (cch >= 5) 
					ftc |= (char_page[bfProp + 9] & 3) * 64;
				if (cch >= 3)
					hps = char_page[bfProp + 7];
				if (cch >= 2) 
					bold = char_page[bfProp + 6] & 1;
				if (cch >= 2) 
					italic = char_page[bfProp + 6] & 2;
				if (cch >= 4) 
					underline = char_page[bfProp + 8] & 1;
				if (cch >= 6) 
					hpsPos = char_page[bfProp + 10];
			}
			
			if (static_cast<unsigned>(ftc) >= wri_fonts_count) {
				ftc = wri_fonts_count - 1;
			}
			
			if ((fcLim >= fcFirst2) && (fcFirst <= fcLim2))  {
				mCharBuf.clear ();
				UT_LocaleTransactor lt (LC_NUMERIC, "C");
				
				UT_String_sprintf (propBuffer, "font-weight:%s", bold ? "bold" : "normal");
				if (italic)  {
					propBuffer += "; font-style:italic";
				}
				if (underline) {
					propBuffer += "; font-decoration:underline";
				}
				if (hpsPos) {
					UT_String_sprintf (tempBuffer, "; font-position:%s; font-size:%dpt", 
							   hpsPos >= 128 ? "superscript" : "subscript",
							   hps / 2);
					propBuffer += tempBuffer;
				}
				if (wri_fonts_count) {
					UT_String_sprintf (tempBuffer, "; font-family:%s", wri_fonts[ftc].name);
					propBuffer += tempBuffer;
				}
				
				while (fcFirst2 >= fcFirst) {
					if ((fcFirst2 >= fcLim) || (fcFirst2 >= fcLim2) ||
						(fcFirst2 - 0x80 >= static_cast<UT_sint32>(mTextBuf.getLength()))) {
						break;
					}
					translate_char (*(mTextBuf.getPointer(fcFirst2 - 0x80)), mCharBuf);
					fcFirst2++;
				}
				
				const gchar* propsArray[3];
				propsArray[0] = pProps;
				propsArray[1] = propBuffer.c_str();
				propsArray[2] = NULL;
				
				if (mCharBuf.size() > 0) {
					appendFmt (propsArray);
					UT_DEBUGMSG (("Hub: About to append %d chars of text\n", mCharBuf.size()));
					appendSpan (reinterpret_cast<const UT_UCSChar *>(mCharBuf.ucs4_str()), mCharBuf.size());
				}
				else {
					UT_DEBUGMSG (("Hub: Ignoring 0 length span\n"));
				}
			}
			
			fcFirst=fcLim;
			if (fcMac == fcLim) {
				return 0;
			}
			if (fcFirst > fcLim2) {
				return 0;
			}
		}
    }
}

/*****************************************************************/
/*****************************************************************/

UT_Error IE_Imp_MSWrite::_loadFile (GsfInput * input)
{
    mFile = (GsfInput *)g_object_ref (G_OBJECT (input));
    if (!mFile)
    {
        return UT_ERROR;
    }
    
    UT_Error iestatus;
    
    X_CleanupIfError(iestatus, _writeHeader());
    X_CleanupIfError(iestatus, _parseFile());
    
    iestatus = UT_OK;
    
 Cleanup:
    g_object_unref(G_OBJECT(mFile));
    return iestatus;
}

/*****************************************************************/
/*****************************************************************/


/* the file header */
static const struct wri_struct WRITE_FILE_HEADER[] = {
	/* value, data, size, type, name */		/* word no. */
	{ 0,    NULL,   2,  CT_VALUE, "wIdent" },       /* 0 */
	{ 0,    NULL,   2,  CT_VALUE, "dty" },          /* 1 */
	{ 0,    NULL,   2,  CT_VALUE, "wTool" },        /* 2 */
	{ 0,    NULL,   2,  CT_VALUE, "reserved1" },    /* 3 */
	{ 0,    NULL,   2,  CT_VALUE, "reserved2" },    /* 4 */
	{ 0,    NULL,   2,  CT_VALUE, "reserved3" },    /* 5 */
	{ 0,    NULL,   2,  CT_VALUE, "reserved4" },    /* 6 */
	{ 0,    NULL,   4,  CT_VALUE, "fcMac" },        /* 7-8 */
	{ 0,    NULL,   2,  CT_VALUE, "pnPara" },       /* 9 */
	{ 0,    NULL,   2,  CT_VALUE, "pnFntb" },       /* 10 */
	{ 0,    NULL,   2,  CT_VALUE, "pnSep" },        /* 11 */
	{ 0,    NULL,   2,  CT_VALUE, "pnSetb" },       /* 12 */
	{ 0,    NULL,   2,  CT_VALUE, "pnPgtb" },       /* 13 */
	{ 0,    NULL,   2,  CT_VALUE, "pnFfntb" },      /* 14 */
	{ 0,    NULL,   64, CT_IGNORE, "szSsht" },      /* 15-47 */
	{ 0,    NULL,   2,  CT_VALUE, "pnMac" },        /* 48 */
	{ 0,    NULL,   0,  CT_IGNORE, NULL }           /* EOF */
};

static const struct wri_struct WRITE_PICTURE[] = {
	/* value, data, size, type, name */		/* word no. */
	/* METAFILEPICT structure */
	{ 0,	NULL, 	2,  CT_VALUE, "mm" },		/* 0 */
	{ 0, 	NULL,	2,  CT_VALUE, "xExt" },		/* 1 */
	{ 0,	NULL, 	2,  CT_VALUE, "yExt" },		/* 2 */
	{ 0,	NULL,	2,  CT_IGNORE, "hMF" },		/* 3 */
	/* end of METAFILEPICT */
	{ 0,	NULL,	2,  CT_VALUE, "dxaOffset" },	/* 4 */
		{ 0,	NULL,	2,  CT_VALUE, "dxaSize" },	/* 5 */
	{ 0,	NULL,	2,  CT_VALUE, "dyaSize" },	/* 6 */
	{ 0,	NULL,	2,  CT_VALUE, "cbOldSize" },	/* 7 */
	/* BITMAP structure */
	{ 0,	NULL,	2,  CT_VALUE, "bmType" },	/* 8 */
	{ 0, 	NULL,	2,  CT_VALUE, "bmWidth" },	/* 9 */
	{ 0, 	NULL,	2,  CT_VALUE, "bmHeight" },	/* 10 */
	{ 0,	NULL,	2,  CT_VALUE, "bmWidthBytes" },	/* 11 */
	{ 0,	NULL,	1,  CT_VALUE, "bmPlanes" },	/* 12.5 */
	{ 0,	NULL,   1,  CT_VALUE, "bmBitsPixel" },	/* 12 */
	{ 0,	NULL,	4,  CT_VALUE, "bmBits" },	/* 13-14 */
	/* end of BITMAP structure */
	{ 0,	NULL,	2,  CT_VALUE, "cbHeader" },	/* 15 */
	{ 0,	NULL,	4,  CT_VALUE, "cbSize" },	/* 16-17 */
	{ 0,	NULL,	2,  CT_VALUE, "mx" },		/* 18 */
	{ 0,	NULL,	2,  CT_VALUE, "my" },		/* 19 */
	{ 0,    NULL,   0,  CT_IGNORE, NULL }           /* EOF */
};

IE_Imp_MSWrite::~IE_Imp_MSWrite()
{
  free_wri_struct (write_file_header);
  free_wri_struct (write_picture);
}


IE_Imp_MSWrite::IE_Imp_MSWrite(PD_Document * pDocument)
  : IE_Imp(pDocument), mFile(0), wri_fonts_count(0),
    wri_fonts(0), wri_images(0), wri_images_count(0)
{
	write_file_header = static_cast<struct wri_struct*>(malloc (sizeof (WRITE_FILE_HEADER)));
	memcpy (write_file_header, WRITE_FILE_HEADER, sizeof (WRITE_FILE_HEADER));
	write_picture = static_cast<struct wri_struct*>(malloc (sizeof (WRITE_PICTURE)));
	memcpy (write_picture, WRITE_PICTURE, sizeof (WRITE_PICTURE));
}

/*****************************************************************/
/*****************************************************************/

UT_Error IE_Imp_MSWrite::_writeHeader()
{
    X_ReturnNoMemIfError(appendStrux(PTX_Section, NULL));
    
    return UT_OK;
}

UT_Error IE_Imp_MSWrite::_parseFile()
{
    int x, size;
    UT_Byte *thetext;
	
    if (read_wri_struct (write_file_header, mFile) ) {
		return UT_ERROR;
    }
	
    x = wri_struct_value (write_file_header, "wIdent");
    if (x == 0137062) {
		/* It's okay, but expect OLE objects */
    } else if (x != 0137061) {
		UT_DEBUGMSG(("Not a write file!\n"));
		return UT_ERROR;
    }
    if (wri_struct_value (write_file_header, "wTool") != 0125400) {
		UT_DEBUGMSG(("Not a write file!\n"));
		return UT_ERROR;
    }
    size = wri_struct_value (write_file_header, "fcMac") - 0x80;
    thetext = static_cast<UT_Byte*>(malloc (size));
    if (!thetext) {
		UT_DEBUGMSG(("Out of memory!\n"));
		return UT_ERROR;
    }
    if (gsf_input_seek (mFile, 0x80, G_SEEK_SET) ) {
		UT_DEBUGMSG(("Seek error!\n"));
		return UT_ERROR;
    }
    gsf_input_read (mFile, size, thetext);
    read_ffntb ();
	mTextBuf.truncate(0);
    mTextBuf.append(thetext, size);
    read_pap ();
    free_ffntb ();
#if 0
    wri_pict_print_data ();
#endif

    return UT_OK;
}

#if 0 // don't need that

static void wri_png_write_data (png_structp png_ptr, png_bytep data,
				png_size_t length, wri_image *img)
{
    unsigned char *p;

    p = realloc (img->png_image, img->length + length);
    if (!p) {
		UT_DEBUGMSG(("Cannot malloc22!\n"));
        return;
    } else {
		img->png_image = p;
		memcpy (img->png_image + img->length, data, length);
		img->length += length;
    }
}

/*
 * Encode a blob of data in base64. This is stolen from Apache
 */

static int base64encode_binary (char *encoded,
				const unsigned char *string, int len)
{
    static const char basis_64[] =
		"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    int i, n;
    char *p;
	
    p = encoded;
    n = 0;
    for (i = 0; i < len - 2; i += 3) {
        *p++ = basis_64[(string[i] >> 2) & 0x3F];
        *p++ = basis_64[((string[i] & 0x3) << 4) |
					   (static_cast<int>(string[i + 1] & 0xF0) >> 4)];
        *p++ = basis_64[((string[i + 1] & 0xF) << 2) |
					   (static_cast<int>(string[i + 2] & 0xC0) >> 6)];
        *p++ = basis_64[string[i + 2] & 0x3F];
        if (!(++n % 16)) *p++ = '\n';
    }
    if (i < len) {
        *p++ = basis_64[(string[i] >> 2) & 0x3F];
        if (i == (len - 1)) {
            *p++ = basis_64[((string[i] & 0x3) << 4)];
            *p++ = '=';
        }
        else {
            *p++ = basis_64[((string[i] & 0x3) << 4) |
						   (static_cast<int>(string[i + 1] & 0xF0) >> 4)];
            *p++ = basis_64[((string[i + 1] & 0xF) << 2)];
        }
        *p++ = '=';
    }
	
    *p++ = '\0';
    return p - encoded;
}
#endif

int IE_Imp_MSWrite::wri_pict_read (unsigned char *data, int size) 
{
#if 0
    png_structp png_ptr;
    png_infop info_ptr;
    png_byte **rows;
    int height, width, width_bytes, i;
    struct wri_image *img, **imgs;
#endif
    int mm;
	
    if (size < 40) {
		UT_DEBUGMSG(("Paragraph too small for object\n"));
        return 1;
    }
	
    read_wri_struct_mem (write_picture, data);
    /*dump_wri_struct (write_picture);*/

    mm = wri_struct_value (write_picture, "mm");

    if (mm == 0x88) { /* this is a wmf file */

	/* note from (data + 40) to (size - 49) there is the 
	   metafile; xExt and yExt contain the size in inch 
	   (divide by 1440.)) */
    } else if (mm == 0xe3) { /* this is a picture */
#if 0
		if (wri_struct_value (write_picture, "bmPlanes") != 1) {
			UT_DEBUGMSG(("Only one bitplane supported, please send this "
						 "write file to sean@mess.org\n"));
			goto err;
		}
		height = wri_struct_value (write_picture, "bmHeight");
		width = wri_struct_value (write_picture, "bmWidth");
		width_bytes = wri_struct_value (write_picture, "bmWidthBytes");
		if ((40 + width_bytes * height) < size) {
			UT_DEBUGMSG(("Not enough paragraph information for bitmap!\n"));
            goto err; 
		}
		img = malloc (sizeof (struct wri_image) );
		if (!img) {
			UT_DEBUGMSG(("Cannot malloc2!\n"));
			goto err;
        }
		memset (img, 0, sizeof (struct wri_image) );
		imgs = realloc (wri_images, 
						sizeof (struct wri_image*) * (wri_images_count + 1) );
		if (!imgs) {
			UT_DEBUGMSG(("Cannot realloc wri_images!\n"));
			free (img);
			goto err;
		}
		wri_images[wri_images_count] = img;
		
		png_ptr = png_create_write_struct (PNG_LIBPNG_VER_STRING, NULL,
										   NULL, NULL);
		if (!png_ptr) goto err;
		
		info_ptr = png_create_info_struct (png_ptr);
		if (!info_ptr) goto err;
		
		if (setjmp (png_ptr->jmpbuf) ) {
			png_destroy_write_struct (&png_ptr, &info_ptr);
			goto err;
		}
		
		/* install our custom output functions */
		png_set_write_fn (png_ptr, png_get_io_ptr (png_ptr), 
						  wri_png_write_data, wri_images[wri_image_count]);
		
		png_set_IHDR (png_ptr, info_ptr, width, height, 
					  wri_struct_value (write_picture, "bmBitsPixel"),
					  PNG_COLOR_TYPE_GRAY, PNG_INTERLACE_NONE,
					  PNG_FILTER_TYPE_BASE, PNG_COMPRESSION_TYPE_BASE);
		
		png_write_info (png_ptr, info_ptr);
		
		rows = malloc (height * sizeof (png_bytep) );
		for (i=0;i<height;i++) rows[i] = data + 40 + width_bytes * i;
		
		png_write_image (png_ptr, rows);
		
		png_write_end (png_ptr, info_ptr);
		png_destroy_write_struct (&png_ptr, &info_ptr);
		
		fprintf (fout, "<image dataid=\"image%d\"/>", wri_images_count);
		wri_images_count++;
		free (rows);
#endif
    } else {
		UT_DEBUGMSG(("OLE object not supported, skipping...\n"));
		//wri_ole_read (data, size, mFile);
    }
	
 //err:
    free_wri_struct (write_picture);
	
    return 0;
}
