/* AbiWord
 * Copyright (C) 2001 Dom Lachowicz <doml@appligent.com>
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

#include <stdio.h>

#include "ie_exp.h"
#include "xap_Module.h"
#include "ut_string.h"
#include "ut_types.h"
#include "ut_units.h"

#include "ut_string_class.h"
#include "pd_Document.h"
#include "pp_AttrProp.h"
#include "px_ChangeRecord.h"
#include "px_CR_Object.h"
#include "px_CR_Span.h"
#include "px_CR_Strux.h"

#ifdef ABI_PLUGIN_BUILTIN
#define abi_plugin_register abipgn_nroff_register
#define abi_plugin_unregister abipgn_nroff_unregister
#define abi_plugin_supports_version abipgn_nroff_supports_version
// dll exports break static linking
#define ABI_BUILTIN_FAR_CALL extern "C"
#else
#define ABI_BUILTIN_FAR_CALL ABI_FAR_CALL
ABI_PLUGIN_DECLARE("Nroff")
#endif

/************************************************************************/
/************************************************************************/

/*
 * This is just my play filter for when I get some spare cycles.
 * I'd really love to be able to have Abi save its documents in
 * Nroff/Unix Manual format. I figure that we might be able to get
 * some more people using the product since we can kinda already
 * save in DocBook and HTML already, and it just seemed cool to think
 * that we could save in the Unix manual format too. I really doubt that
 * we'll ever be able to import the .1 documents, though, but saving as
 * them seems kinda cool. I know, I'm a geek...
 *
 * Anyway, this is just a stub, skeleton, emtpy shell of an exporter
 * at the moment. I don't expect that it compiles, let alone does
 * something useful. This is for my amusement, though others are
 * encouraged to help me out too. I've come to the conclusion that
 * only geeks CVS revision control their amusements... but anyway :-)
 *
 * -Dom
 */

/************************************************************************/
/************************************************************************/

/*********************************/
/* Document Listener Class */
/*********************************/
class IE_Exp_Nroff;

class s_Nroff_Listener : public PL_Listener
{
public:

  s_Nroff_Listener(PD_Document * pDocument,
		 IE_Exp_Nroff * pie)
	  : m_pDocument(pDocument),
		m_pie(pie)
  {
  }

  virtual ~s_Nroff_Listener()
  {
  }

  virtual bool populate(PL_StruxFmtHandle /*sfh*/,
			const PX_ChangeRecord * pcr)
  {
    switch (pcr->getType())
      {
      case PX_ChangeRecord::PXT_InsertSpan:
	{
	  const PX_ChangeRecord_Span * pcrs = static_cast<const PX_ChangeRecord_Span *> (pcr);
	  
	  PT_AttrPropIndex api = pcr->getIndexAP();
	  if (api)
	    {
		_openSpan(api);
	    }
	  
	  PT_BufIndex bi = pcrs->getBufIndex();
	  _outputData(m_pDocument->getPointer(bi),pcrs->getLength());
	  
	  if (api)
	    _closeSpan();
	  return true;
	}
	
      case PX_ChangeRecord::PXT_InsertObject:
	{
	  return true;
	}
      case PX_ChangeRecord::PXT_InsertFmtMark:
	{
	  return true;
	}
      default:
	{
	  return false;
	}
      }
  }
  
  virtual bool populateStrux(PL_StruxDocHandle /*sdh*/,
			     const PX_ChangeRecord * pcr,
			     PL_StruxFmtHandle * psfh)
  {
	const PX_ChangeRecord_Strux * pcrx = static_cast<const PX_ChangeRecord_Strux *> (pcr);
	*psfh = 0;							// we don't need it.
	
	switch (pcrx->getStruxType())
	{
	case PTX_Section:
		return true;

	case PTX_SectionHdrFtr:
		return true;

	case PTX_Block:
	{
		_closeSpan();
		_closeBlock();
		_openParagraph(pcr->getIndexAP());
		return true;
	}

	default:
		UT_ASSERT(UT_TODO);
		return true;
	}
  }
  
  virtual bool		insertStrux(PL_StruxFmtHandle /*sfh*/,
				    const PX_ChangeRecord * /*pcr*/,
				    PL_StruxDocHandle /*sdh*/,
				    PL_ListenerId /*lid*/,
				    void (* /*pfnBindHandles*/)(PL_StruxDocHandle sdhNew,
							    PL_ListenerId lid,
							    PL_StruxFmtHandle sfhNew))
  {
    // should not be used
    return false;
  }
  
  virtual bool		change(PL_StruxFmtHandle /*sfh*/,
			       const PX_ChangeRecord * /*pcr*/)
  {
    // should not be used
    return false;
  }

  virtual bool		signal(UT_uint32 /*iSignal*/)
  {
    // should not be used
    return false;
  }

protected:
	void	_closeBlock(void) {}
	void	_closeSpan(void) {}
	void	_openBlock(PT_AttrPropIndex /*api*/) {}
	void	_openParagraph(PT_AttrPropIndex /*api*/) {}
	void	_openSpan(PT_AttrPropIndex /*api*/) {}  
	void    _outputData(const UT_UCSChar * /*data*/, UT_uint32 /*length*/) {}
	
private:
	PD_Document *		m_pDocument;
	IE_Exp_Nroff *      m_pie;

};

/************************************************************************/
/************************************************************************/

/*********************************/
/* Exporter class */
/*********************************/

class IE_Exp_Nroff : public IE_Exp
{
public:

  IE_Exp_Nroff(PD_Document * pDocument)
    : IE_Exp(pDocument)
  {
  }

  virtual ~IE_Exp_Nroff()
  {
  }
  
protected:

  UT_Error _writeDocument(void)
  {
    s_Nroff_Listener m_pListener(getDoc(),this);
    if (!getDoc()->tellListener(static_cast<PL_Listener *>(&m_pListener)))
      return UT_ERROR;
    
    return ((m_error) ? UT_IE_COULDNOTWRITE : UT_OK);
  }

};

/*********************************/
/* Export sniffer */
/*********************************/

class IE_Exp_Nroff_Sniffer 
  : public IE_ExpSniffer
{
  friend class IE_Exp;
  
public:
  IE_Exp_Nroff_Sniffer () :
    IE_ExpSniffer("AbiNroff::Nroff")
  {
    // 
  }

  virtual ~IE_Exp_Nroff_Sniffer () {}

  /*!
   * Recognize the bz2 suffixes
   */
  virtual bool recognizeSuffix (const char * szSuffix)
  {
    return (!g_ascii_strcasecmp(szSuffix,".man") || 
	    !g_ascii_strcasecmp(szSuffix, ".1") || 
	    !g_ascii_strcasecmp(szSuffix, ".nroff"));
  }

  /*!
   * Get the dialog lables and the file type
   */
  virtual bool getDlgLabels (const char ** pszDesc,
			     const char ** pszSuffixList,
			     IEFileType * ft)
  {
    *pszDesc = "UNIX(tm) Nroff/Manual format (.nroff)";
    *pszSuffixList = "*.nroff";
    *ft = getFileType();
    return true;
  }

  /*!
   * Construct an exporter object to actually write to
   */
  virtual UT_Error constructExporter (PD_Document * pDocument,
				      IE_Exp ** ppie)
  {
    IE_Exp_Nroff * p = new IE_Exp_Nroff(pDocument);
    *ppie = p;
    return UT_OK;
  }
};

/************************************************************************/
/************************************************************************/

// we use a reference-counted sniffer
static IE_Exp_Nroff_Sniffer * m_expSniffer = 0;

ABI_BUILTIN_FAR_CALL
int abi_plugin_register (XAP_ModuleInfo * mi)
{
  if (!m_expSniffer)
    {
      m_expSniffer = new IE_Exp_Nroff_Sniffer ();
    }

  mi->name    = "UNIX(tm) Nroff/manual Export Filter";
  mi->desc    = "Save as UNIX(tm) nroff/manual Documents";
  mi->version = ABI_VERSION_STRING;
  mi->author  = "Dom Lachowicz <cinamod@hotmail.com>";
  mi->usage   = "No Usage";
  
  IE_Exp::registerExporter (m_expSniffer);
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
  
//  UT_ASSERT (m_impSniffer);
  UT_ASSERT (m_expSniffer);

  IE_Exp::unregisterExporter (m_expSniffer);
  delete m_expSniffer;
  m_expSniffer = 0;

  return 1;
}

ABI_BUILTIN_FAR_CALL
int abi_plugin_supports_version (UT_uint32 /*major*/, UT_uint32 /*minor*/, 
				 UT_uint32 /*release*/)
{
  return 1;
}
