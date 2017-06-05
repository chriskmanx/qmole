/*
 * AbiGrammar - Abiword Plugin for on-the-fly Grammar checking
 * Copyright (C) 2005 by Martin Sevior
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

#ifdef ABI_PLUGIN_BUILTIN
#define abi_plugin_register abipgn_abigrammar_register
#define abi_plugin_unregister abipgn_abigrammar_unregister
#define abi_plugin_supports_version abipgn_abigrammar_supports_version
#endif

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "ut_assert.h"
#include "ut_debugmsg.h"
#include "xap_Module.h"
#include "xap_App.h"
#include "xap_Frame.h"
#include "fv_View.h"
#include "xav_View.h"
#include "xav_Listener.h"
#include "fl_BlockLayout.h"
#include "pd_Document.h"

#include "ut_types.h"
#include "ut_misc.h"
#include "ut_units.h"
#include "AbiGrammarCheck.h"
#include "ut_sleep.h"
#include <sys/types.h>  
#include <sys/stat.h>
#ifdef TOOLKIT_WIN
#include <windows.h>
#else
#include <unistd.h>
#include <sys/wait.h>
#include <signal.h>
#include "ut_files.h"
#endif


// -----------------------------------------------------------------------
//
//     AbiDash code
//
// -----------------------------------------------------------------------

class AbiGrammar : public AV_ListenerExtra
{
public:
AbiGrammar(XAP_App * pApp):
	m_pApp(pApp),
	m_pView(NULL),
	m_pDoc(NULL),
	m_pBlock(NULL)
		{
		}

virtual ~AbiGrammar(void)
		{
		}
void setID(AV_ListenerId id)
		{
			m_lid = id;
		}

virtual  AV_ListenerType getType(void) { return AV_LISTENER_PLUGIN_EXTRA;}

AV_ListenerId	getID(void)
		{
			return m_lid;
		}
virtual bool notify(AV_View * , const AV_ChangeMask )
		{
		  return false;
		}

virtual bool notify(AV_View * pAView, const AV_ChangeMask mask, void * pPrivateData)
		{
		  if(mask != AV_CHG_BLOCKCHECK)
		  {
		    return true;
		  }
		  //		  printf("I've been notified!! View = %x hint mask %d \n",pAView,mask);
		  m_pView = static_cast<FV_View *>(pAView);
		  m_pBlock = reinterpret_cast<fl_BlockLayout *>(pPrivateData);
		  m_pDoc = m_pView->getDocument();
		  UT_UTF8String sText;
		  const char * pText = NULL;
		  m_pBlock->appendUTF8String(sText);
		  if (sText.byteLength()==0) 
		    return true;
		  pText =  sText.utf8_str();
		  //printf("I've been notified!! Text is |%s|\n", pText );
		  m_GrammarCheck.CheckBlock(m_pBlock);
		  return true;
		}

private:
	XAP_App *        m_pApp;
	FV_View *        m_pView;
	PD_Document *    m_pDoc;
	fl_BlockLayout * m_pBlock;
	AV_ListenerId    m_lid;
  Abi_GrammarCheck       m_GrammarCheck;
};


static AV_ListenerId listenerID = 0; 
static AbiGrammar * pAbiGrammar = NULL;

ABI_PLUGIN_DECLARE(AbiGrammar)

// -----------------------------------------------------------------------
//
//      Abiword Plugin Interface 
//
// -----------------------------------------------------------------------

  
ABI_FAR_CALL
int abi_plugin_register (XAP_ModuleInfo * mi)
{
    mi->name = "AbiGrammar";
    mi->desc = "The plugin allows AbiWord to be Grammar checked";
    mi->version = ABI_VERSION_STRING;
    mi->author = "Martin Sevior <msevior@physics.unimelb.edu.au>";
    mi->usage = "No Usage";
    
    // Add to AbiWord's plugin listeners
    XAP_App * pApp = XAP_App::getApp();

#ifdef TOOLKIT_COCOA
    if (const char * resources = getenv ("ABIWORD_COCOA_BUNDLED_RESOURCES"))
    {
        UT_UTF8String dict_dir = resources;
	dict_dir += "/link-grammar";
        setenv ("DICTPATH", dict_dir.utf8_str (), 1);
    }
#endif

    pAbiGrammar = new AbiGrammar(pApp);
    pApp->addListener(pAbiGrammar, &listenerID);
    pAbiGrammar->setID(listenerID);
    UT_DEBUGMSG(("Class AbiGrammar %x created! Listener Id %d \n",pAbiGrammar,listenerID));
    
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

    XAP_App * pApp = XAP_App::getApp();
    pApp->removeListener(listenerID);

    return 1;
}


ABI_FAR_CALL
int abi_plugin_supports_version (UT_uint32 /*major*/, UT_uint32 /*minor*/, UT_uint32 /*release*/)
{
    return 1; 
}
