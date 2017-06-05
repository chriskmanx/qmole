/* -*- mode: C++; tab-width: 4; c-basic-offset: 4; -*- */

/* AbiWord
 * Copyright (C) 2001 AbiSource, Inc.
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

#include <ctype.h>

#include "ut_string.h"
#include "ut_types.h"
#include "ut_misc.h"
#include "ut_units.h"
#include "ut_bytebuf.h"
#include "ut_base64.h"
#include "ut_debugmsg.h"
#include "pt_Types.h"
#include "pd_Document.h"
#include "pp_AttrProp.h"
#include "px_ChangeRecord.h"
#include "px_CR_Object.h"
#include "px_CR_Span.h"
#include "px_CR_Strux.h"
#include "pp_Property.h"
#include "ap_Prefs.h"
#include "pd_Style.h"
#include "xap_EncodingManager.h"
#include "fl_AutoNum.h"
#include "fp_PageSize.h"
#include "ut_string_class.h"
#include "ie_exp_KWord_1.h"
#include "ie_impexp_KWord_1.h"
#include "ut_locale.h"

/*****************************************************************/
/*****************************************************************/

class s_KWord_1_Listener : public PL_Listener
{
public:
	s_KWord_1_Listener(PD_Document * pDocument,
					  IE_Exp_KWord_1 * pie);
	virtual ~s_KWord_1_Listener();

	virtual bool		populate(PL_StruxFmtHandle sfh,
								 const PX_ChangeRecord * pcr);

	virtual bool		populateStrux(PL_StruxDocHandle sdh,
									  const PX_ChangeRecord * pcr,
									  PL_StruxFmtHandle * psfh);

	virtual bool		change(PL_StruxFmtHandle sfh,
							   const PX_ChangeRecord * pcr);

	virtual bool		insertStrux(PL_StruxFmtHandle sfh,
									const PX_ChangeRecord * pcr,
									PL_StruxDocHandle sdh,
									PL_ListenerId lid,
									void (* pfnBindHandles)(PL_StruxDocHandle sdhNew,
															PL_ListenerId lid,
															PL_StruxFmtHandle sfhNew));

	virtual bool		signal(UT_uint32 iSignal);
protected:

	void                _handlePageSize(PT_AttrPropIndex api);
	void                _handleAttributes(PT_AttrPropIndex api);
	void				_handleStyles(void);
	void				_handleDataItems(void);
	void                _outputData(const UT_UCSChar * data, UT_uint32 length);

	void				_convertFontSize(UT_String& szDest, const char* szFontSize);
	void                _convertColor(UT_String& szDest, const char* pszColor);

	void				_closeSection(void);
	void				_closeBlock(void);
	void				_closeSpan(void);
	void				_openBlock(PT_AttrPropIndex api);
	void				_openSection(PT_AttrPropIndex api);
	void				_openSpan(PT_AttrPropIndex api, PT_BlockOffset pos, UT_uint32 len);
	void                _writeMarginSize(PT_AttrPropIndex api, const char * name);

private:
	PD_Document *		m_pDocument;
	IE_Exp_KWord_1 *    m_pie;

	bool				m_bInSection;
	bool				m_bInBlock;
	bool				m_bInSpan;
	bool				m_bFirstWrite;

	UT_String			m_sFormats;
	UT_String			m_sLayout;

	int                 m_iImgCnt;
};

// KWord version 1 width/height are in mm
static const UT_Dimension kword_1_unit = DIM_MM;

/*****************************************************************/
/*****************************************************************/

IE_Exp_KWord_1::IE_Exp_KWord_1(PD_Document * pDocument)
	: IE_Exp(pDocument), m_pListener(0)
{
	m_error = UT_OK;
}

IE_Exp_KWord_1::~IE_Exp_KWord_1()
{
}

/*****************************************************************/
/*****************************************************************/

IE_Exp_KWord_1_Sniffer::IE_Exp_KWord_1_Sniffer (const char * _name) :
  IE_ExpSniffer(_name)
{
  // 
}

UT_Confidence_t IE_Exp_KWord_1_Sniffer::supportsMIME (const char * szMIME)
{
  if (strcmp (IE_FileInfo::mapAlias (szMIME), IE_MIMETYPE_KWord) == 0)
    {
      return UT_CONFIDENCE_GOOD;
    }
  return UT_CONFIDENCE_ZILCH;
}

bool IE_Exp_KWord_1_Sniffer::recognizeSuffix(const char * szSuffix)
{
	return (!g_ascii_strcasecmp(szSuffix,".kwd"));
}

UT_Error IE_Exp_KWord_1_Sniffer::constructExporter(PD_Document * pDocument,
											   IE_Exp ** ppie)
{
	IE_Exp_KWord_1 * p = new IE_Exp_KWord_1(pDocument);
	*ppie = p;
	return UT_OK;
}

bool IE_Exp_KWord_1_Sniffer::getDlgLabels(const char ** pszDesc,
									  const char ** pszSuffixList,
									  IEFileType * ft)
{
	*pszDesc = "KWord (.kwd)";
	*pszSuffixList = "*.kwd";
	*ft = getFileType();
	return true;
}

/*****************************************************************/
/*****************************************************************/

UT_Error IE_Exp_KWord_1::_writeDocument(void)
{
	m_pListener = new s_KWord_1_Listener(getDoc(),this);
	if (!m_pListener)
		return UT_IE_NOMEMORY;
	if (!getDoc()->tellListener(static_cast<PL_Listener *>(m_pListener)))
		return UT_ERROR;
	delete m_pListener;

	m_pListener = NULL;
	
	return ((m_error) ? UT_IE_COULDNOTWRITE : UT_OK);
}  

/*****************************************************************/
/*****************************************************************/

s_KWord_1_Listener::s_KWord_1_Listener(PD_Document * pDocument,
									 IE_Exp_KWord_1 * pie)
	: m_pDocument (pDocument), m_pie (pie), 
	m_bInSection(false), m_bInBlock(false), 
	m_bInSpan(false), m_bFirstWrite(true),
	m_sFormats(""), m_sLayout(""), m_iImgCnt(0)
{
  m_pie->write("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n");

	m_pie->write("<!-- This document was created by AbiWord -->\n");
	m_pie->write("<!-- AbiWord is a free, Open Source word processor. -->\n");
	m_pie->write("<!-- You may obtain more information about AbiWord at http://www.abisource.com -->\n\n");
	m_pie->write("<DOC editor=\"AbiWord\" mime=\"application/x-kword\" syntaxVersion=\"1\">\n");
}

s_KWord_1_Listener::~s_KWord_1_Listener()
{
	_closeSpan();
	_closeBlock();
	_closeSection();
	m_pie->write("</FRAMESETS>\n");
	_handleStyles();
	// FIXME: handle <PIXMAPS>
	// _handleDataItems();
	
	m_pie->write ("</DOC>\n");
}

bool s_KWord_1_Listener::populate(PL_StruxFmtHandle /*sfh*/,
								 const PX_ChangeRecord * pcr)
{
	switch (pcr->getType())
	{
	case PX_ChangeRecord::PXT_InsertSpan:
		{
			const PX_ChangeRecord_Span * pcrs = 
				static_cast<const PX_ChangeRecord_Span *> (pcr);

			PT_AttrPropIndex api = pcr->getIndexAP();
			if (api)
			{
				_openSpan(api,pcrs->getBlockOffset(),pcrs->getLength());
			}
			
			PT_BufIndex bi = pcrs->getBufIndex();
			_outputData(m_pDocument->getPointer(bi),pcrs->getLength());

			if (api)
			{
				_closeSpan();
			}
			return true;
		}

	case PX_ChangeRecord::PXT_InsertObject:
		{
			const PX_ChangeRecord_Object * pcro = static_cast<const PX_ChangeRecord_Object *> (pcr);
			//PT_AttrPropIndex api = pcr->getIndexAP();

			switch (pcro->getObjectType())
			{
			case PTO_Image:
			{
				return true;
			}

			case PTO_Field:
			{
				return true;
			}

				// todo: support these
			case PTO_Hyperlink:
			case PTO_Bookmark:
			case PTO_Math:
			case PTO_Embed:
			  return true;

			default:
			{
				UT_ASSERT(UT_TODO);
				return true;
			}

			}
		}

	case PX_ChangeRecord::PXT_InsertFmtMark:
		return true;
		
	default:
		UT_ASSERT(0);
		return false;
	}
}

bool s_KWord_1_Listener::populateStrux(PL_StruxDocHandle /*sdh*/,
									  const PX_ChangeRecord * pcr,
									  PL_StruxFmtHandle * psfh)
{
	UT_ASSERT(pcr->getType() == PX_ChangeRecord::PXT_InsertStrux);
	const PX_ChangeRecord_Strux * pcrx = static_cast<const PX_ChangeRecord_Strux *> (pcr);
	*psfh = 0;							// we don't need it.

	switch (pcrx->getStruxType())
	{
	case PTX_SectionEndnote:
	case PTX_Section:
	{
		_closeSpan();
		_closeBlock();
		_closeSection();
		
		PT_AttrPropIndex indexAP = pcr->getIndexAP();
		const PP_AttrProp* pAP = NULL;
		if (m_pDocument->getAttrProp(indexAP, &pAP) && pAP)
		{
			const gchar* pszSectionType = NULL;
			pAP->getAttribute("type", pszSectionType);
			if (
				!pszSectionType
				|| (0 == strcmp(pszSectionType, "doc"))
				)
			{
				_openSection(pcr->getIndexAP());
				m_bInSection = true;
			}
			else
			{
				m_bInSection = false;
			}
		}
		else
		{
			m_bInSection = false;
		}
		
		return true;
	}
	
	case PTX_SectionHdrFtr:
	{
		// TODO???
		return true;
	}
	
	case PTX_Block:
	{
		_closeSpan();
		_closeBlock();
		_openBlock(pcr->getIndexAP());
		return true;
	}

	case PTX_SectionTable:
	case PTX_EndTable:
	case PTX_SectionCell:
	case PTX_EndCell:
	  return true;

	case PTX_EndFrame:
	case PTX_EndMarginnote:
	case PTX_EndFootnote:
	case PTX_SectionFrame:
	case PTX_SectionMarginnote:
	case PTX_SectionFootnote:
	case PTX_EndEndnote:
	default:
		UT_ASSERT(UT_TODO);
		return true;
	}
}

bool s_KWord_1_Listener::signal(UT_uint32 /* iSignal */)
{
	UT_ASSERT(UT_SHOULD_NOT_HAPPEN);
	return false;
}

bool s_KWord_1_Listener::change(PL_StruxFmtHandle /*sfh*/,
							   const PX_ChangeRecord * /*pcr*/)
{
	UT_ASSERT(0);						// this function is not used.
	return false;
}

bool s_KWord_1_Listener::insertStrux(PL_StruxFmtHandle /*sfh*/,
									const PX_ChangeRecord * /*pcr*/,
									PL_StruxDocHandle /*sdh*/,
									PL_ListenerId /* lid */,
									void (* /*pfnBindHandles*/)(PL_StruxDocHandle /* sdhNew */,
																PL_ListenerId /* lid */,
																PL_StruxFmtHandle /* sfhNew */))
{
	UT_ASSERT(0);						// this function is not used.
	return false;
}

/*****************************************************************/
/*****************************************************************/

static const char *
justificationToNumber(const char * justification_name)
{
	if (! strcmp(justification_name,"left"))
		return "0";
	else if (! strcmp(justification_name,"right"))
		return "1";
	else if (! strcmp(justification_name,"center"))
		return "2";
	else if (! strcmp(justification_name,"justify"))
		return "3";
	else
	{
		UT_ASSERT(UT_SHOULD_NOT_HAPPEN);
		return "";
	}
}

// Converts the AbiWord page sizes into the KOffice sizes;
// compare abi/src/text/fmt/xp/fp_PageSize.h
// and koffice/lib/kofficeui/koGlobal.h
// FIXME: put a prototype above
static const char*
abiPageSizeToKoPageFormat (fp_PageSize abi_page_size)
{
	// The goofy order of the pagesizes is due to koGlobal.h
	switch (fp_PageSize::NameToPredefined(abi_page_size.getPredefinedName())) {
		case fp_PageSize::psA3:
			return "0";
			break;
		case fp_PageSize::psA4:
			return "1";
			break;
		case fp_PageSize::psA5:
			return "2";
			break;
		case fp_PageSize::psLetter:
			return "3";
			break;
		case fp_PageSize::psLegal:
			return "4";
			break;
		// FIXME: I don't understand what is meant in KWord by `screen'
		// sized paper
		case fp_PageSize::psCustom:
			return "6";
			break;
		case fp_PageSize::psB5:
			return "7";
			break;
#if 0 // This requires the `lot more page sizes' patch
		case fp_PageSize::psExecutive:
			return "8";
			break;
#endif
#if 0 // KWord DTD 2 sizes
		case fp_PageSize::psA0:
			return 9;
		case fp_PageSize::psA1:
			return 10;
		case fp_PageSize::psA2:
			return 11;
		case fp_PageSize::psA6:
			return 12;
		case fp_PageSize::psA7:
			return 13;
		case fp_PageSize::psA8:
			return 14;
		case fp_PageSize::psA9:
			return 15;
		case fp_PageSize::psB0:
			return 16;
		case fp_PageSize::psB1:
			return 17;
		case fp_PageSize::psB10:
			return 18;
		case fp_PageSize::psB2:
			return 19;
		case fp_PageSize::psB3:
			return 20;
		case fp_PageSize::psB4:
			return 21;
		case fp_PageSize::psB6:
			return 22;
#endif
		default:
			break;
	}
	return "6"; // Custom
}

void s_KWord_1_Listener::_writeMarginSize(PT_AttrPropIndex api, const char * name)
{
	UT_String buf;
	const gchar * szValue;
	const PP_AttrProp * pAP = NULL;
	bool bHaveProp = m_pDocument->getAttrProp(api,&pAP);
	UT_ASSERT((bHaveProp));

	UT_String_sprintf(buf, "page-margin-%s", name);
	szValue = PP_evalProperty(buf.c_str(), 
				  NULL, NULL, pAP, m_pDocument, true);
	UT_String_sprintf(buf," %s=\"%f",name, UT_convertToDimension(szValue, kword_1_unit));
	m_pie->write(buf);
	m_pie->write("\"");

}
		
void s_KWord_1_Listener::_handlePageSize(PT_AttrPropIndex api)
{
  //
  // Code to write out the PageSize Definitions to disk
  // 

	//const PP_AttrProp * pAP = NULL;
	//bool bHaveProp = m_pDocument->getAttrProp(api,&pAP);

	UT_LocaleTransactor lt (LC_NUMERIC, "C");

	m_pie->write("<PAPER");

	m_pie->write(" format=\"");
	m_pie->write(abiPageSizeToKoPageFormat(m_pDocument->m_docPageSize));
	m_pie->write("\"");
	
	m_pie->write(" orientation=\"");
	if (m_pDocument->m_docPageSize.isPortrait())
		m_pie->write("0");
	else
		m_pie->write("1");
	m_pie->write("\"");
	
	// FIXME: I think we need to create new frames for each change in
	// number of columns
	m_pie->write(" columns=\"1\"");
	// FIXME: put something useful here
	m_pie->write(" columnspacing=\"0\"");
	
	UT_String buf;
	m_pie->write(" width=\"");
	UT_String_sprintf(buf,"%f",m_pDocument->m_docPageSize.Width(kword_1_unit));
	m_pie->write(buf);
	m_pie->write("\"");

	m_pie->write(" height=\"");
	UT_String_sprintf(buf,"%f",m_pDocument->m_docPageSize.Height(kword_1_unit));
	m_pie->write(buf);
	m_pie->write("\"");
	
	m_pie->write(">\n");
	
	// PAPERBORDERS
	m_pie->write("<PAPERBORDERS");

	_writeMarginSize(api, "left");
	_writeMarginSize(api, "right");
	_writeMarginSize(api, "top");
	_writeMarginSize(api, "bottom");

	m_pie->write("/>\n");
	

	m_pie->write("</PAPER>\n");

	m_bFirstWrite = false;
	return;
}

void s_KWord_1_Listener::_handleAttributes(PT_AttrPropIndex /*api*/)
{
	m_pie->write("<ATTRIBUTES");
	m_pie->write(" processing=\"0\"");
	m_pie->write(" unit=\"");
	m_pie->write(UT_dimensionName(m_pDocument->m_docPageSize.getDims()));
	m_pie->write("\"");
	m_pie->write("/>\n");

	return;
}		

void s_KWord_1_Listener::_handleDataItems(void)
{
	const char * szName;
   	const char * szMimeType;
	const UT_ByteBuf * pByteBuf;

	for (UT_uint32 k=0; (m_pDocument->enumDataItems(k,NULL,&szName,&pByteBuf,reinterpret_cast<const void**>(&szMimeType))); k++)
	{	  	  
	  UT_UTF8String fname;	  
	  
	  if (!strcmp(szMimeType, "image/svg+xml"))
	      UT_UTF8String_sprintf(fname, "%s-%d.svg", m_pie->getFileName(), k);
	  if (!strcmp(szMimeType, "application/mathml+xml"))
	    UT_UTF8String_sprintf(fname, "%s-%d.mathml", m_pie->getFileName(), k);
	  else // PNG Image
	    UT_UTF8String_sprintf(fname, "%s-%d.png", m_pie->getFileName(), k);
	  
	  GsfOutput *fp = UT_go_file_create(fname.utf8_str(), NULL);
	  
	  if(!fp)
	    continue;
	  
	  gsf_output_write(fp, pByteBuf->getLength(), (const guint8*)pByteBuf->getPointer(0));
	  gsf_output_close(fp);
	  g_object_unref(G_OBJECT(fp));
	}
	
	return;
}

void s_KWord_1_Listener::_handleStyles(void)
{
	m_pie->write("<STYLES>\n");
	// FIXME: write out styles
	m_pie->write("</STYLES>\n");
}

void s_KWord_1_Listener::_openSection(PT_AttrPropIndex api)
{
	if (m_bFirstWrite)
	{
		_handlePageSize(api);
		_handleAttributes(api);
	    m_pie->write("<FRAMESETS>\n");
	}

	m_bInSection = true;

	m_pie->write("<FRAMESET");
	m_pie->write(" frameType=\"1\"");
	m_pie->write(" frameInfo=\"0\"");
	m_pie->write(" removable=\"0\"");
	m_pie->write(" visible=\"1\"");
	m_pie->write(" name=\"Frameset 1\"");
	m_pie->write(">\n");
	
	m_pie->write("<FRAME");
	//FIXME: I think KWord ignores these, because it's a `normal page' frame
	m_pie->write(" left=\"0\"");
	m_pie->write(" top=\"0\"");
	m_pie->write(" right=\"0\"");
	m_pie->write(" bottom=\"0\"");
	
	m_pie->write(" runaround=\"1\"");
	// These make the frame act like a normal main document frame
	m_pie->write(" autoCreateNewFrame=\"1\"");
	m_pie->write(" newFrameBehaviour=\"0\"");
	m_pie->write("/>\n");
}

// FIXME: prototype this
static const
UT_String measureToLengthsList(const char *pMeasure)
{
	double dLength;
	UT_String sLengths = "";
	
	sLengths += " pt=\"";
	dLength = UT_convertToDimension(pMeasure, DIM_PT);
	sLengths += UT_convertToDimensionlessString(dLength,"2.4");
	sLengths += "\"";
	
	sLengths += " mm=\"";
	dLength = UT_convertToDimension(pMeasure, DIM_MM);
	sLengths += UT_convertToDimensionlessString(dLength,"2.4");
	sLengths += "\"";
	
	sLengths += " inch=\"";
	dLength = UT_convertToDimension(pMeasure, DIM_IN);
	sLengths += UT_convertToDimensionlessString(dLength,"2.4");
	sLengths += "\"";
	
	return sLengths;
}
//#define USED() do {if(!used) used = true;} while (0)

void s_KWord_1_Listener::_openBlock(PT_AttrPropIndex api)
{
	if (!m_bInSection)
	{
		return;
	}

	const UT_Dimension kword_1_dimension = DIM_MM;
	const PP_AttrProp * pAP = NULL;
	bool bHaveProp = m_pDocument->getAttrProp(api,&pAP);

	m_bInBlock = true;
	m_pie->write("<PARAGRAPH>\n<TEXT>");

	// keep track of whether we have we written anything
	//bool used = false;

	m_sFormats = "";
	m_sFormats += "<FORMATS>\n";
	
	m_sLayout = "";
	m_sLayout += "<LAYOUT>\n";
	// FIXME: we should write a <LAYOUT> section only if there are any
	// non-empty, supported attributes, so we prolly need an if {...}
	// surrounding this -- actually, it may be easier to use `USED',
	// and at the end put: if (!used) m_sLayout = "";
	// FIXME: Once styles are supported, make sure we write out even
	// "false" settings, as these may be needed to override styles

	// FIXME: tabstops, default-tab-interval, background-color
	// NAME
	// <NAME value=\"Standard\"/>
	//  this should be the style attribute
	// FOLLOWING
	// FIXME: does AbiWord support this?
	if (bHaveProp && pAP)
	{
		const gchar * szValue;
	
		// FLOW
		// <FLOW align="left,right,center,justify"/>
		if (pAP->getProperty("text-align", szValue))
		{
			//m_sLayout += "<FLOW align=\""; // KWord DTD 2
			//m_sLayout += static_cast<const char *>(szValue); // KWord DTD 2
			m_sLayout += "<FLOW value=\""; // KWord DTD 1
			m_sLayout += justificationToNumber(static_cast<const char *>(szValue)); // KWord DTD 1
			m_sLayout += ("\"/>\n");
		}
	// FIXME: this is a bit tricky...
	// LINESPACING
	// extra space between lines, in points;
	// <LINESPACING pt="12"/> (or mm="5" or whatever)
	// KWord DTD 2:
	// extra space between lines, in points; can also be "oneandhalf" or "double"
	// <LINESPACING value="12"/>
	// NOTE: AbiWord uses 1.0 1.5 2.0 (for single, half again, double) spacing,
	// and 24pt 24pt+ for EXACT spacing, and AT LEAST spacing.
	// (and if you edit it manually, you can put in 3.0, 4.5, or any other multiple
	// you want instead of 1.0, 2.0, etc.)
	// These do not map well onto KWord's `extra spacing' format, as we need to
	// figure out how much space a given line of AbiWord text takes, though with
	// KWord DTD 2, we can preserve 1.5 and 2.0 times spacing.
	// Single space for 12pt text seems to be 13pt (need to check), 
	//	if (pAP->getProperty("line-height", szValue))

		// INDENTS
		// (in mm in KWord 1, in pt in KWord 2)
		// first=text-indent+margin-left
		// left=margin-left
		// right=margin-right // KWord DTD 2
		// e.g., <INDENTS first="16" left="0"/>

		double left_indent = 0;
		if (pAP->getProperty("margin-left", szValue))
			left_indent = UT_convertToDimension(szValue, kword_1_dimension);
		double first_indent = left_indent;
		if (pAP->getProperty("text-indent", szValue))
			first_indent += UT_convertToDimension(szValue, kword_1_dimension);
		double right_indent = 0;
		if (pAP->getProperty("margin-right", szValue))
			right_indent = UT_convertToDimension(szValue, kword_1_dimension);
		// Only write <INDENTS> if there are some non-trivial ones.
		//if ((left_indent != 0.) || (first_indent != 0.) || (right_indent != 0.))
		// WORK-AROUND: KWord 1.0 hangs on negative indents
		// FIXME: might need to write in zero for negative value to deal
		// with styles
		if ((left_indent > 0.) || (first_indent > 0.) || (right_indent > 0.))
		{
			m_sLayout += "<INDENTS";
			//if (left_indent != 0.)
			if (left_indent > 0.)
			{
				m_sLayout += " left=\"";
				m_sLayout += UT_convertToDimensionlessString(left_indent,"2.4");
				m_sLayout += "\"";
			}	
			//if (first_indent != 0.)
			if (first_indent > 0.)
			{
				m_sLayout += " first=\"";
				m_sLayout += UT_convertToDimensionlessString(first_indent,"2.4");
				m_sLayout += "\"";
			}
			//if (right_indent != 0.)
			if (right_indent > 0.)
			{
				m_sLayout += " right=\"";
				m_sLayout += UT_convertToDimensionlessString(right_indent,"2.4");
				m_sLayout += "\"";
			}
			m_sLayout += "/>\n";
		}

		// OFFSETS
		// (in mm in KWord 1, in pt in KWord 2)
		// before=margin-top
		// after=margin-bottom
		// e.g., <OFFSETS before="12" after="12"/>
#if 0 // KWord DTD 2
		double before_offset = 0;
		if (pAP->getProperty("margin-top", szValue))
			before_offset = UT_convertToDimension(szValue, kword_1_dimension);
		double after_offset = 0;
		if (pAP->getProperty("margin-bottom", szValue))
			after_offset = UT_convertToDimension(szValue, kword_1_dimension);
		// Only write <OFFSETS> if there are some non-trivial ones.
		if ((before_offset != 0.) || (after_offset != 0.))
		{
			m_sLayout += "<OFFSETS";
			if (before_offset != 0.)
			{
				m_sLayout += " before=\"";
				m_sLayout += UT_convertToDimensionlessString(before_offset,"2.4");
				m_sLayout += "\"";
			}	
			if (after_offset != 0.)
			{
				m_sLayout += " after=\"";
				m_sLayout += UT_convertToDimensionlessString(after_offset,"2.4");
				m_sLayout += "\"";
			}	
			m_sLayout += "/>\n";
		}
#endif // KWord DTD 2
		// WORK-AROUND: despite a DTD to the contrary, KWord 1 uses
		// <OHEAD pt="" mm="" inch=""/>
		// <OFOOT pt="" mm="" inch=""/>
		
		double before_offset = 0;
		if (pAP->getProperty("margin-top", szValue))
			before_offset = UT_convertToDimension(szValue, DIM_MM);
		if (before_offset != 0.)
		{
			m_sLayout += "<OHEAD";
			m_sLayout += measureToLengthsList(static_cast<const char*>(szValue));
			m_sLayout += "/>\n";
		}	
		double after_offset = 0;
		if (pAP->getProperty("margin-bottom", szValue))
			after_offset = UT_convertToDimension(szValue, kword_1_dimension);
		if (after_offset != 0.)
		{
			m_sLayout += "<OFOOT";
			m_sLayout += measureToLengthsList(static_cast<const char*>(szValue));
			m_sLayout += "/>\n";
		}	

	// PAGEBREAKING
	// e.g., <PAGEBREAKING linesTogether="true" keepWithNext="true"/>
		bool bLinesTogether=false;
		if (pAP->getProperty("keep-together", szValue))
			bLinesTogether = (g_ascii_strcasecmp(szValue,"true") == 0);
		bool bKeepWithNext=false;
		if (pAP->getProperty("keep-with-next", szValue))
			bKeepWithNext = (g_ascii_strcasecmp(szValue,"true") == 0);
	// FIXME: I should probably run this if and only if the above properties
	// exist (dealing with styles)
		if (bLinesTogether || bKeepWithNext)
		{
			m_sLayout += "<PAGEBREAKING";
			m_sLayout += " linesTogether=\"";
			if (bLinesTogether)
				m_sLayout += "true";
			else
				m_sLayout += "false";
			m_sLayout += "\"";

			m_sLayout += " keepWithNext=\"";
			if (bKeepWithNext)
				m_sLayout += "true";
			else
				m_sLayout += "false";
			m_sLayout += "\"";
			
			m_sLayout += "/>";
		}
		// FIXME: are widows unsupported in KWord?
		// if (pAP->getProperty("widows", szValue))
#if 0
	// put these inside a <FORMAT>
		if (pAP->getProperty("bgcolor", szValue))
		{
			USED();
			m_pie->write("background-color=\"#");
			m_pie->write(static_cast<const char *>(szValue));
			m_pie->write("\"");
		}

		if (pAP->getProperty("color", szValue))
		{
			USED();
			m_pie->write("color=\"#");
			m_pie->write(static_cast<const char *>(szValue));
			m_pie->write("\"");
		}

		if (pAP->getProperty("lang", szValue))
		{
			USED();
			m_pie->write("language=\"");
			m_pie->write(static_cast<const char *>(szValue));
			m_pie->write("\"");
		}
		
		if (pAP->getProperty("font-size", szValue))
		{
			USED();
			m_pie->write("font-size=\"");
			m_pie->write(static_cast<const char *>(szValue));
			m_pie->write("\"");
		}		

		if (pAP->getProperty("font-family", szValue))
		{
			USED();
			m_pie->write("font-family=\"");
			m_pie->write(static_cast<const char *>(szValue));
			m_pie->write("\"");
		}

		if (pAP->getProperty("font-weight", szValue))
		{
			USED();
			m_pie->write("font-weight=\"");
			m_pie->write(static_cast<const char *>(szValue));
			m_pie->write("\"");
		}

		if (pAP->getProperty("font-style", szValue))
		{
			USED();
			m_pie->write("font-style=\"");
			m_pie->write(static_cast<const char *>(szValue));
			m_pie->write("\"");
		}

		if (pAP->getProperty("font-stretch", szValue))
		{
			USED();
			m_pie->write("font-stretch=\"");
			m_pie->write(static_cast<const char *>(szValue));
			m_pie->write("\"");
		}
#endif
	}
	m_sLayout += "</LAYOUT>\n";
}

void s_KWord_1_Listener::_openSpan(PT_AttrPropIndex api, PT_BlockOffset pos, UT_uint32 len)
{
	if (!m_bInBlock)
	{
		return;
	}

	m_bInSpan = true;

	const PP_AttrProp * pAP = NULL;
	bool bHaveProp = m_pDocument->getAttrProp(api,&pAP);

	// keep track of whether we've written out anything
	//bool used = false;
	// FIXME: this is where all the formatting happens -- I need to
	// enqueue formatting information, to be written to file when
	// closeBlock is called
	m_sFormats += "<FORMAT id=\"1\""; // id="1" means normal text
	m_sFormats += " pos=\""; // current cursor position

	UT_String buf;

	UT_String_sprintf(buf,"%ld", static_cast<long>(pos));
	m_sFormats += buf;
	m_sFormats += "\"";
	m_sFormats += " len=\""; // length of span

	UT_String_sprintf(buf,"%ld", static_cast<long>(len));
	m_sFormats += buf;
	m_sFormats += "\"";
	m_sFormats += ">\n";

	// FIXME: I don't think KWord supports styles for spans, so we'll have to
	// do it manually?

	if (bHaveProp && pAP)
	{
		// query and output properties
		const gchar * szValue = 0;

		if (pAP->getProperty("color", szValue))
		{
		  UT_String red, green, blue;
		  UT_RGBColor rgb;

		  // convert from hex/decimal

		  UT_parseColor(szValue, rgb);

		  UT_String_sprintf(red, "%d", rgb.m_red);
		  UT_String_sprintf(green, "%d", rgb.m_grn);
		  UT_String_sprintf(blue, "%d", rgb.m_blu);

		  m_sFormats += "<COLOR red=\"";
		  m_sFormats += red;
		  m_sFormats += "\" green=\"";
		  m_sFormats += green;
		  m_sFormats += "\" blue=\"";
		  m_sFormats += blue;
		  m_sFormats += "\"/>\n";
		}

#if 0

		// FIXME: does KWord support background color for spans of text?
		if (pAP->getProperty("bgcolor", szValue))
		{
			USED();
			m_pie->write("background-color=\"#");
			m_pie->write(static_cast<const char *>(szValue));
			m_pie->write("\"");
		}

#endif

		// <FONT name="times"/>
		if (pAP->getProperty("font-family", szValue))
		{
			m_sFormats += "<FONT name=\"";
			m_sFormats += (static_cast<const char *>(szValue));
			m_sFormats += "\"/>\n";
		}
		// WORK-AROUND: actually, the <FONT> tag seems to be required;
		// maybe it isn't if you have a (default) style?
		// FIXME
		else
			m_sFormats += "<FONT name=\"times\"/>\n";

		// <SIZE value="12"/> size in pt
		if (pAP->getProperty("font-size", szValue))
		{
		  UT_String buf2;

			m_sFormats += "<SIZE value=\"";
			UT_String_sprintf(buf2,"%d", static_cast<int>(UT_convertToDimension(szValue, DIM_PT)));
			m_sFormats += buf2;
			m_sFormats += "\"/>\n";
		}		

		// <WEIGHT value="50"/> 50=normal, 75=bold
		if (pAP->getProperty("font-weight", szValue))
		{
			m_sFormats += "<WEIGHT value=\"";
			if (!g_ascii_strcasecmp(static_cast<const char *>(szValue), "bold"))
				m_sFormats += "75";
			else
				m_sFormats += "50";
			m_sFormats += "\"/>\n";
		}

		// <ITALIC value="1"/> 0=normal, 1=italic
		if (pAP->getProperty("font-style", szValue))
		{
			m_sFormats += "<ITALIC value=\"";
			if (!g_ascii_strcasecmp(static_cast<const char *>(szValue), "italic"))
				m_sFormats += "1";
			else
				m_sFormats += "0";
			m_sFormats += "\"/>\n";
		}

		// <UNDERLINE value="1"/> 0=normal, 1=underline
		// <STRIKEOUT value="1"/> 0=normal, 1=strikeout
		if (pAP->getProperty("text-decoration", szValue))
		{
			if (strstr (szValue, "underline"))
				m_sFormats += "<UNDERLINE value=\"1\"/>\n";
			else
				m_sFormats += "<UNDERLINE value=\"0\"/>\n";
			if (strstr (szValue, "line-through"))
				m_sFormats += "<STRIKEOUT value=\"1\"/>\n";
			else
				m_sFormats += "<STRIKEOUT value=\"0\"/>\n";
		}

		// <VERTALIGN value="0"/> 0=normal, 1=subscript, 2=superscript
		if (pAP->getProperty("text-position", szValue))
		{
			if (!g_ascii_strcasecmp (szValue, "subscript"))
				m_sFormats += "<VERTALIGN value=\"1\"/>\n";
			else if (!g_ascii_strcasecmp (szValue, "superscript"))
				m_sFormats += "<VERTALIGN value=\"2\"/>\n";
			else
				m_sFormats += "<VERTALIGN value=\"0\"/>\n";
		}
		
#if 0
		// ACK! KWord doesn't support languages, only Character sets!
		// <CHARSET value=""/>
		if (pAP->getProperty("lang", szValue))
		{
			USED();
			m_pie->write("language=\"");
			m_pie->write(static_cast<const char *>(szValue));
			m_pie->write("\"");
		}
		
		// FIXME: what IS font-stretch?
		// FIXME: dir/dir-override -- does KWord support these?
#endif

	}

	m_sFormats += "</FORMAT>\n";
}

#undef USED

void s_KWord_1_Listener::_closeBlock(void)
{
	if (!m_bInBlock)
	{
		return;
	}
	m_bInBlock = false;
	
	m_pie->write("</TEXT>\n");
	m_sFormats += "</FORMATS>\n";
	m_pie->write(m_sFormats.c_str()); // <FORMATS>...</FORMATS>
	m_pie->write(m_sLayout.c_str()); // <LAYOUT>...</LAYOUT>
	m_pie->write("</PARAGRAPH>\n");
}

void s_KWord_1_Listener::_closeSection(void)
{
	if (!m_bInSection)
	{
		return;
	}
	
	m_bInSection = false;
    m_pie->write("</FRAMESET>\n");
}

void s_KWord_1_Listener::_closeSpan(void)
{
	if (!m_bInSpan)
	{
		return;
	}

	m_bInSpan = false;

	// FIXME: I don't think I need to do anything here -- it should
	// all be done in openSpan
}

/*****************************************************************/
/*****************************************************************/

void s_KWord_1_Listener::_convertColor(UT_String& szDest, const char* pszColor)
{
	/* FIXME: Convert this to deal with KWord's format for colors. */
	szDest = pszColor;
}

void s_KWord_1_Listener::_convertFontSize(UT_String& szDest, const char* pszFontSize)
{
	szDest = pszFontSize;
}

/*****************************************************************/
/*****************************************************************/

void s_KWord_1_Listener::_outputData(const UT_UCSChar * data, UT_uint32 length)
{
	UT_UTF8String sBuf;
	const UT_UCSChar * pData;

	UT_ASSERT(sizeof(UT_Byte) == sizeof(char));

	for (pData=data; (pData<data+length); /**/)
	{
		switch (*pData)
		{
		case '<':
			sBuf += "&lt;";
			pData++;
			break;
			
		case '>':
			sBuf += "&gt;";
			pData++;
			break;
			
		case '&':
			sBuf += "&amp;";
			pData++;
			break;

		case UCS_LF:					// LF -- representing a Forced-Line-Break
			// TODO
			UT_ASSERT(UT_TODO);
			pData++;
			break;
			
		case UCS_VTAB:					// VTAB -- representing a Forced-Column-Break
			// TODO
			UT_ASSERT(UT_TODO);
			pData++;
			break;
			
		case UCS_FF:					// FF -- representing a Forced-Page-Break
			// TODO:
			UT_ASSERT(UT_TODO);
			pData++;
			break;
			
		default:
		  sBuf.appendUCS4(pData, 1);
		  pData++;
		}
	}

	m_pie->write(sBuf.utf8_str(), sBuf.byteLength());
}
