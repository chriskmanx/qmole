/* AbiWord
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
 */

#include "ie_exp_EML.h"
#include "ut_string.h"
#include "ut_assert.h"
#include "xap_Module.h"

#ifdef ABI_PLUGIN_BUILTIN
#define abi_plugin_register abipgn_eml_register
#define abi_plugin_unregister abipgn_eml_unregister
#define abi_plugin_supports_version abipgn_eml_supports_version
// dll exports break static linking
#define ABI_BUILTIN_FAR_CALL extern "C"
#else
#define ABI_BUILTIN_FAR_CALL ABI_FAR_CALL
ABI_PLUGIN_DECLARE("EML")
#endif

//extern  IE_Exp_Text::;
/*****************************************************************/
/*****************************************************************/

IE_Exp_EML::IE_Exp_EML(PD_Document * pDocument)
    : IE_Exp_Text(pDocument, false)
  {
  }

IE_Exp_EML::~IE_Exp_EML ()
  {
  }

UT_Error IE_Exp_EML::_writeDocument(void)
  {
    write ("X-Unsent: 1\r\n\r\n");
    return this->IE_Exp_Text::_writeDocument ();
  }

/*****************************************************************/
/*****************************************************************/

IE_Exp_EML_Sniffer::IE_Exp_EML_Sniffer () :
    IE_ExpSniffer("AbiEML::EML")
  {
    // 
  }

bool IE_Exp_EML_Sniffer::recognizeSuffix(const char * szSuffix)
  {
    return (!g_ascii_strcasecmp(szSuffix,".eml"));
  }
  
UT_Error IE_Exp_EML_Sniffer::constructExporter(PD_Document * pDocument,
			     IE_Exp ** ppie)
  {
	IE_Exp_EML * p = new IE_Exp_EML(pDocument);
	*ppie = p;
	return UT_OK;
  }
  
bool IE_Exp_EML_Sniffer::getDlgLabels(const char ** pszDesc,
		    const char ** pszSuffixList,
		    IEFileType * ft)
  {
    *pszDesc = "Outlook Express Email (.eml)";
    *pszSuffixList = "*.eml";
    *ft = getFileType();
    return true;
  }

/*****************************************************************/
/*****************************************************************/

// we use a reference-counted sniffer
static IE_Exp_EML_Sniffer * m_sniffer = 0;

ABI_BUILTIN_FAR_CALL
int abi_plugin_register (XAP_ModuleInfo * mi)
{

	if (!m_sniffer)
	{
		m_sniffer = new IE_Exp_EML_Sniffer ();
	}

	mi->name = "Outlook Express EML Exporter";
	mi->desc = "Export AbiWord Documents to email clients";
	mi->version = ABI_VERSION_STRING;
	mi->author = "Dom Lachowicz <cinamod@hotmail.com>";
	mi->usage = "No Usage";

	IE_Exp::registerExporter (m_sniffer);
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

	IE_Exp::unregisterExporter (m_sniffer);
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
