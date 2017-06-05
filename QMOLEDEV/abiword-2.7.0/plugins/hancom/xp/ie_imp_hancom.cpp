/* -*- mode: C++; tab-width: 4; c-basic-offset: 4; -*- */

/* Abiword
 * Copyright (C) 2001 Christian Biesinger <cbiesinger@web.de>
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

#include <gsf/gsf-input-stdio.h>
#include <gsf/gsf-infile.h>
#include <gsf/gsf-infile-msole.h>

#include <memory.h>
#include "xap_Module.h"
#include "ie_imp.h"
#include "ut_types.h"
#include "ut_string.h"
#include "ut_iconv.h"
#include "ut_debugmsg.h"
#include "pd_Document.h"

#ifdef ABI_PLUGIN_BUILTIN
#define abi_plugin_register abipgn_hancom_register
#define abi_plugin_unregister abipgn_hancom_unregister
#define abi_plugin_supports_version abipgn_hancom_supports_version
// dll exports break static linking
#define ABI_BUILTIN_FAR_CALL extern "C"
#else
#define ABI_BUILTIN_FAR_CALL ABI_FAR_CALL
ABI_PLUGIN_DECLARE("Hancom")
#endif

static const UT_Byte hwpSignature[] = {0xD0, 0xCF, 0x11, 0xE0, 0xA1, 0xB1, 0x1A, 0xE1};

// -------------------------------------------------------------------------------------
// The importer
class IE_Imp_Hancom : public IE_Imp {
public:
  IE_Imp_Hancom(PD_Document *pDoc);
  virtual ~IE_Imp_Hancom();
  
protected:
	virtual UT_Error _loadFile(GsfInput * input);
private:
  GsfInfile *mDoc;
};

IE_Imp_Hancom::IE_Imp_Hancom(PD_Document* pDoc) : IE_Imp(pDoc), mDoc(NULL) {
}

IE_Imp_Hancom::~IE_Imp_Hancom() {
  if (mDoc) {
    g_object_unref (G_OBJECT (mDoc));
  }
}

UT_Error IE_Imp_Hancom::_loadFile(GsfInput * input) {
  mDoc = GSF_INFILE(gsf_infile_msole_new (input, NULL));

  if (!mDoc)
    return UT_IE_BOGUSDOCUMENT;

  GsfInput *textStream = gsf_infile_child_by_name(mDoc, "/PrvText");
  if (!textStream)
    return UT_IE_BOGUSDOCUMENT;

  size_t len = gsf_input_size(textStream);

  UT_DEBUGMSG(("HANCOM: Text length = %lu bytes\n", len));
  unsigned char* buf = new unsigned char[len];

  if (!buf) {
    g_object_unref (G_OBJECT (textStream));
    return UT_IE_NOMEMORY;
  }

  gsf_input_read(textStream, len, buf);
  g_object_unref (G_OBJECT (textStream));

  UT_uint32 length;
  UT_UCS4Char* text = reinterpret_cast<UT_UCS4Char*>(UT_convert((const char *)buf, len, "UCS-2LE", 
								UCS_INTERNAL, NULL, &length));
  delete[] buf;
  if (!text)
    return UT_IE_NOMEMORY;

  UT_DEBUGMSG(("HANCOM: Text successfully converted.\n"));

  if (!appendStrux(PTX_Section, NULL)) {
    FREEP(text);
    return UT_IE_NOMEMORY;
  }
  
  if (!appendStrux(PTX_Block, NULL)) {
    FREEP(text);
    return UT_IE_NOMEMORY;
  }
  
  if (!appendSpan(text, length/4)) {
    FREEP(text);
    return UT_IE_NOMEMORY;
  }

  FREEP(text);
  return UT_OK;
}

// -------------------------------------------------------------------------------------
// Sniffer

// supported suffixes
static IE_SuffixConfidence IE_Imp_Hancom_Sniffer__SuffixConfidence[] = {
	{ "hwp", 	UT_CONFIDENCE_PERFECT 	},
	{ "", 	UT_CONFIDENCE_ZILCH 	}
};

class IE_Imp_Hancom_Sniffer : public IE_ImpSniffer {
	public:
		IE_Imp_Hancom_Sniffer() :
		  IE_ImpSniffer("AbiHancom:HWP:")
		{
		  // 
		}
		virtual ~IE_Imp_Hancom_Sniffer() {}

		const IE_SuffixConfidence * getSuffixConfidence ()
		{
			return IE_Imp_Hancom_Sniffer__SuffixConfidence;
		}

		virtual const IE_MimeConfidence * getMimeConfidence ()
		{
			return NULL;
		}

		virtual UT_Confidence_t recognizeContents(const char* szBuf, UT_uint32 iNumBytes) {
			if (iNumBytes >= sizeof(hwpSignature))
				return (memcmp(szBuf, hwpSignature, sizeof(hwpSignature)) == 0) ? UT_CONFIDENCE_GOOD : UT_CONFIDENCE_ZILCH;
			return UT_CONFIDENCE_ZILCH;

		}

		virtual bool getDlgLabels(const char** szDesc, const char** szSuffixList, IEFileType *ft) {
			*szDesc = "Hancom Word (*.hwp)";
			*szSuffixList = "*.hwp";
			*ft = getFileType();
			return true;

		}

		virtual UT_Error constructImporter(PD_Document* pDocument, IE_Imp **ppie) {
			*ppie = new IE_Imp_Hancom(pDocument);
			if (!ppie)
				return UT_OUTOFMEM;
			return UT_OK;
		}

};

// -------------------------------------------------------------------------------------
// Plugin Code

// we use a reference-counted sniffer
static IE_Imp_Hancom_Sniffer * m_impSniffer = 0;

ABI_BUILTIN_FAR_CALL
int abi_plugin_register (XAP_ModuleInfo * mi)
{
    if (!m_impSniffer)
    {
    	m_impSniffer = new IE_Imp_Hancom_Sniffer ();
    }

    mi->name    = "Hancom .hwp file importer";
    mi->desc    = "Imports Hancom binary (OLE) documents";
    mi->version = ABI_VERSION_STRING;
    mi->author  = "Christian Biesinger <cbiesinger@web.de>";
    mi->usage   = "No Usage";
  
    IE_Imp::registerImporter (m_impSniffer);
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
  
    UT_ASSERT (m_impSniffer);

    IE_Imp::unregisterImporter (m_impSniffer);
	delete m_impSniffer;
	m_impSniffer = 0;

    return 1;
}

ABI_BUILTIN_FAR_CALL
int abi_plugin_supports_version (UT_uint32 /*major*/, UT_uint32 /*minor*/, 
                                 UT_uint32 /*release*/)
{
    return 1;
}
