/*
 * 
 * Copyright (C) 2001 by Dom Lachowicz
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

#include "xap_Module.h"
#include "xap_App.h"
#include "xap_Frame.h"
#include "fv_View.h"
#include "ap_Menu_Id.h"
#include "ev_Menu_Actions.h"
#include "ev_Menu.h"
#include "ev_Menu_Layouts.h"
#include "ev_Menu_Labels.h"
#include "ev_EditMethod.h"
#include "xap_Menu_Layouts.h"
#include "ut_string_class.h"
#include "ut_wctomb.h"

#include "xap_Dialog_Id.h"
#include "xap_DialogFactory.h"
#include "xap_Dlg_Language.h"

#ifdef ABI_PLUGIN_BUILTIN
#define abi_plugin_register abipgn_babelfish_register
#define abi_plugin_unregister abipgn_babelfish_unregister
#define abi_plugin_supports_version abipgn_babelfish_supports_version
// dll exports break static linking
#define ABI_BUILTIN_FAR_CALL extern "C"
#else
#define ABI_BUILTIN_FAR_CALL ABI_FAR_CALL
ABI_PLUGIN_DECLARE("Babelfish")
#endif

#define MY_MB_LEN_MAX 6

// Babelfish currently uses UTF-8 encoding
// 
// Babelfish's language pairs at 7 Dec 2001:
// 
// From/To
// -------
// English/Chinese
// English/French
// English/German
// English/Italian
// English/Japanese
// English/Korean
// English/Portuguese
// English/Spanish
// Chinese/English
// French/English
// French/German
// German/English
// German/French
// Italian/English
// Japanese/English
// Korean/English
// Portuguese/English
// Russian/English
// Spanish/English

// -----------------------------------------------------------------------
// -----------------------------------------------------------------------

//
// _ucsToUTF8
// -----------------------
//   Helper function to convert UCS-4 strings into UTF-8
//   Doing this gains us CJK and Russian translation!
//
static void _ucsToUTF8(UT_String& dest, const UT_UCSChar* src)
{
  // calculate length of text
  const unsigned int length = UT_UCS4_strlen(static_cast<const UT_UCS4Char *>(src));
  
	UT_Wctomb wctomb("UTF-8");
	const UT_UCSChar * pData;
	int mbLen;
	char pC[MY_MB_LEN_MAX];
  
  // do the string conversion.
	for (pData=src; (pData<src+length); ++pData)
    {
		if (!wctomb.wctomb(pC,mbLen,*pData))
		{
			mbLen=1;
			pC[0]='?';
			wctomb.initialize();
		}
		else
			pC[mbLen]=0;
		dest += pC;
    }
}

static bool _getTranslationCode (FV_View * pView, UT_String & langCode)
{
  XAP_Frame * pFrame = static_cast<XAP_Frame *> (pView->getParentData());
  UT_return_val_if_fail(pFrame,false);
  
  bool bRet = false;

  pFrame->raise();
  
  XAP_Dialog_Id id = XAP_DIALOG_ID_LANGUAGE;
  
  XAP_DialogFactory * pDialogFactory
    = static_cast<XAP_DialogFactory *>(pFrame->getDialogFactory());

  XAP_Dialog_Language * pDialog
    = static_cast<XAP_Dialog_Language *>(pDialogFactory->requestDialog(id));
  UT_return_val_if_fail(pDialog,false);

  UT_String code ("en-US");
  
  const gchar ** props_in = NULL;
  if (pView->getCharFormat(&props_in))
    {
      const gchar * xml_code = UT_getAttribute("lang", props_in);
      if ( xml_code )
	{
	  code = xml_code ;
	  if ( code.size() >= 2 )
	    {
	      code = code.substr (0, 2);
	      code += '_';
	    }
	}
      
      pDialog->setLanguageProperty(UT_getAttribute("lang", props_in));
      FREEP(props_in);
    }
  
  // run the dialog  
  pDialog->runModal(pFrame);
  
  // extract what they did  
  bool bOK = (pDialog->getAnswer() == XAP_Dialog_Language::a_OK);
  
  if (bOK)
    {
      const gchar * s;
      if (pDialog->getChangedLangProperty(&s))
	{
	  UT_String changedLang = s;
	  if (changedLang.size() >= 2)
	    {
	      code += changedLang.substr(0, 2);
	      langCode = code;
	      bRet = true;
	    }
	}
    }

  pDialogFactory->releaseDialog(pDialog);

  return bRet;
}

//
// BabelFish_invoke
// -------------------
//   This is the function that we actually call to invoke the 
//   online babelfish translation
//   It should be called when the user selects from the context menu
//
static bool BabelFish_invoke(AV_View* /*v*/, EV_EditMethodCallData */*d*/)
{
  // Get the current view that the user is in.
  XAP_Frame *pFrame = XAP_App::getApp()->getLastFocussedFrame();
  FV_View* pView = static_cast<FV_View*>(pFrame->getCurrentView());
  

  UT_String url ("http://babelfish.altavista.com");
  
  if (!pView->isSelectionEmpty())
    {
      // TODO: generate the correct language binding via the current
      // TODO: language and then the language dialog. Currently
      // TODO: english->german is hardcoded
      UT_String langCode;
      if ( _getTranslationCode ( pView, langCode ) )
	{
	  // Now we will figure out what words to translate
	  url = "http://babelfish.altavista.com/tr?doit=done&tt=urltext";
	  url += "&lp=";
	  url += langCode;
	  url += "&urltext=";
	  
	  // We need to get the UTF-8 version of the current word.
	  UT_String utf8;
	  UT_UCS4Char *ucs4ST;
	  pView->getSelectionText(*&ucs4ST);
	  _ucsToUTF8(
					utf8,
				       ucs4ST
					);	  

	  // URL encode the string (' ' -> %20, ...)
	  // TODO this is not complete
	  UT_String srcText;

	  //for (char *p = utf8; p && *p; ++p)
	  for (UT_uint32 i = 0; i < utf8.size(); ++i)
	  {
		  if (utf8[i] == ' ' || utf8[i] == '%'
			  || utf8[i] == '&' || utf8[i] == '?' || (utf8[i] & 128))
		  {
			  char temp[4] = "";
			  sprintf(&temp[0], "%%%x", utf8[i]);
			  srcText += temp;
		  } else
			  srcText += utf8[i];
	  }
	  url += srcText;
	  FREEP(ucs4ST);

	  XAP_App::getApp()->openURL( url.c_str() ); 
	}
      // else didn't get the translation code. don't show anything
    }
  else
    {
      XAP_App::getApp()->openURL( url.c_str() ); 
    }

  return true;
}

static const char* BabelFish_MenuLabel = "Use &Babelfish Translation";
static const char* BabelFish_MenuTooltip = "Opens the on-line babelfish translator";

static void
BabelFish_addToMenus()
{
  // First we need to get a pointer to the application itself.
  XAP_App *pApp = XAP_App::getApp();
    
  // Create an EditMethod that will link our method's name with
  // it's callback function.  This is used to link the name to 
  // the callback.
  EV_EditMethod *myEditMethod = new EV_EditMethod(
						  "BabelFish_invoke",  // name of callback function
						  BabelFish_invoke,    // callback function itself.
						  0,                      // no additional data required.
						  ""                      // description -- allegedly never used for anything
						  );
  
  // Now we need to get the EditMethod container for the application.
  // This holds a series of Edit Methods and links names to callbacks.
  EV_EditMethodContainer* pEMC = pApp->getEditMethodContainer();
  
  // We have to add our EditMethod to the application's EditMethodList
  // so that the application will know what callback to call when a call
  // to "AiksaurusABI_invoke" is received.
  pEMC->addEditMethod(myEditMethod);
  
  
  // Now we need to grab an ActionSet.  This is going to be used later
  // on in our for loop.  Take a look near the bottom.
  EV_Menu_ActionSet* pActionSet = pApp->getMenuActionSet();
  
  
  // We need to go through and add the menu element to each "frame" 
  // of the application.  We can iterate through the frames by doing
  // XAP_App::getFrameCount() to tell us how many frames there are,
  // then calling XAP_App::getFrame(i) to get the i-th frame.
  
  int frameCount = pApp->getFrameCount();
  XAP_Menu_Factory * pFact = pApp->getMenuFactory();

  //
  // Put it in the context menu.
  //
  XAP_Menu_Id newID = pFact->addNewMenuAfter("contextText",NULL,"Bullets and &Numbering",EV_MLF_Normal);
  pFact->addNewLabel(NULL,newID,BabelFish_MenuLabel, BabelFish_MenuTooltip);

  //
  // Also put it under word Wount in the main menu,
  //
  pFact->addNewMenuAfter("Main",NULL,"&Word Count",EV_MLF_Normal,newID);
  
  // Create the Action that will be called.
  EV_Menu_Action* myAction = new EV_Menu_Action(
						newID,                     // id that the layout said we could use
						0,                      // no, we don't have a sub menu.
						0,                      // yes, we raise a dialog.
						0,                      // no, we don't have a checkbox.
						0,
						"BabelFish_invoke",  // name of callback function to call.
						NULL,                   // don't know/care what this is for
						NULL                    // don't know/care what this is for
						);
  
  // Now what we need to do is add this particular action to the ActionSet
  // of the application.  This forms the link between our new ID that we 
  // got for this particular frame with the EditMethod that knows how to 
  // call our callback function.  
  
  pActionSet->addAction(myAction);
  
  for(int i = 0;i < frameCount;++i)
    {
      // Get the current frame that we're iterating through.
      XAP_Frame* pFrame = pApp->getFrame(i);
      pFrame->rebuildMenus();
    }
}

static void
BabelFish_RemoveFromMenus ()
{
  // First we need to get a pointer to the application itself.
  XAP_App *pApp = XAP_App::getApp();

  // remove the edit method
  EV_EditMethodContainer* pEMC = pApp->getEditMethodContainer() ;
  EV_EditMethod * pEM = ev_EditMethod_lookup ( "BabelFish_invoke" ) ;
  pEMC->removeEditMethod ( pEM ) ;
  DELETEP( pEM ) ;

  // now remove crap from the menus
  int frameCount = pApp->getFrameCount();
  XAP_Menu_Factory * pFact = pApp->getMenuFactory();

  pFact->removeMenuItem("Main",NULL,BabelFish_MenuLabel);
  pFact->removeMenuItem("contextText",NULL,BabelFish_MenuLabel);
  for(int i = 0;i < frameCount;++i)
    {
      // Get the current frame that we're iterating through.
      XAP_Frame* pFrame = pApp->getFrame(i);
      pFrame->rebuildMenus();
    }
}

// -----------------------------------------------------------------------
//
//      Abiword Plugin Interface 
//
// -----------------------------------------------------------------------

ABI_BUILTIN_FAR_CALL
int abi_plugin_register (XAP_ModuleInfo * mi)
{
    mi->name = "BabelFish plugin";
    mi->desc = "On-line Translation support for AbiWord. Based upon the Babelfish translation tool which is powered by Systran.";
    mi->version = ABI_VERSION_STRING;
    mi->author = "Dom Lachowicz <cinamod@hotmail.com>";
    mi->usage = "No Usage";
    
    // Add the translator to AbiWord's menus.
    BabelFish_addToMenus();
    
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

    BabelFish_RemoveFromMenus () ;

    return 1;
}


ABI_BUILTIN_FAR_CALL
int abi_plugin_supports_version (UT_uint32 /*major*/, UT_uint32 /*minor*/, 
				 UT_uint32 /*release*/)
{
    return 1; 
}
