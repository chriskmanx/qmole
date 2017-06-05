/*
 * AiksaurusABI - Abiword plugin for Aiksaurus
 * Copyright (C) 2001 by Jared Davis
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
#define abi_plugin_register abipgn_aiksaurus_register
#define abi_plugin_unregister abipgn_aiksaurus_unregister
#define abi_plugin_supports_version abipgn_aiksaurus_supports_version
#endif

#ifdef WIN32
	#include "../win/AiksaurusApp.h"
#else
	#include <AiksaurusGTK.h>
#endif
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

#include <iostream>
using namespace std;

ABI_PLUGIN_DECLARE(Aiksaurus)

bool AiksaurusABI_invoke(AV_View* v, EV_EditMethodCallData *d);

static const char* AiksaurusABI_MenuLabel = "&Thesaurus";
static const char* AiksaurusABI_MenuTooltip = "Opens the thesaurus and finds synonyms.";


//
// AiksaurusABI_addToMenus
// -----------------------
//   Adds "Thesaurus" option to AbiWord's Tools Menu.
//
static void
AiksaurusABI_addToMenus()
{
    // First we need to get a pointer to the application itself.
    XAP_App *pApp = XAP_App::getApp();

    
    // Create an EditMethod that will link our method's name with
    // it's callback function.  This is used to link the name to 
    // the callback.
    EV_EditMethod *myEditMethod = new EV_EditMethod(
        "AiksaurusABI_invoke",  // name of callback function
        AiksaurusABI_invoke,    // callback function itself.
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
    pFact->addNewLabel(NULL,newID,AiksaurusABI_MenuLabel, AiksaurusABI_MenuTooltip);
//
// Also put it under word Wount in the main menu,
//
    pFact->addNewMenuAfter("Main",NULL,"&Word Count",EV_MLF_Normal,newID);

    // Create the Action that will be called.
    EV_Menu_Action* myAction = new EV_Menu_Action(
	newID,                     // id that the layout said we could use
	0,                      // no, we don't have a sub menu.
	1,                      // yes, we raise a dialog.
	0,                      // no, we don't have a checkbox.
	0,
	"AiksaurusABI_invoke",  // name of callback function to call.
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
AikSaurusABI_RemoveFromMenus ()
{
  // First we need to get a pointer to the application itself.
  XAP_App *pApp = XAP_App::getApp();

  // remove the edit method
  EV_EditMethodContainer* pEMC = pApp->getEditMethodContainer() ;
  EV_EditMethod * pEM = ev_EditMethod_lookup ( "AiksaurusABI_invoke" ) ;
  pEMC->removeEditMethod ( pEM ) ;
  DELETEP( pEM ) ;

  // now remove crap from the menus
  int frameCount = pApp->getFrameCount();
  XAP_Menu_Factory * pFact = pApp->getMenuFactory();

  pFact->removeMenuItem("Main",NULL,AiksaurusABI_MenuLabel);
  pFact->removeMenuItem("contextText",NULL,AiksaurusABI_MenuLabel);
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
    
ABI_FAR_CALL
int abi_plugin_register (XAP_ModuleInfo * mi)
{
    mi->name = "Aiksaurus";
    mi->desc = "English-language thesaurus based on the Aiksaurus library: "
               "http://www.aiksaurus.com/";
    mi->version = ABI_VERSION_STRING;
    mi->author = "Jared Davis <jared@aiksaurus.com>";
    mi->usage = "No Usage";
    
    // Add the thesaurus to AbiWord's menus.
    AiksaurusABI_addToMenus();
    
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

    AikSaurusABI_RemoveFromMenus ();

    return 1;
}


ABI_FAR_CALL
int abi_plugin_supports_version (UT_uint32 /*major*/, UT_uint32 /*minor*/, UT_uint32 /*release*/)
{
    return 1; 
}

// -----------------------------------------------------------------------
//
//     Aiksaurus Invocation Code
//
// -----------------------------------------------------------------------


//
// AiksaurusABI_ucsToAscii
// -----------------------
//   Helper function to convert UCS-4 strings into Ascii.
//   NOTE: you must call delete[] on the returned test!!!
//
//   The thesaurus operates in pure ascii, and abiword documents operate
//   in UCS-4, so we need to be able to convert UCS-4 to Ascii in order
//   to tell what word the user has selected when they invoke the thesaurus.
//
inline static char*
AiksaurusABI_ucsToAscii(const UT_UCSChar* text)
{
    // calculate length of text so that we can create a character
    // buffer of equal size.
        const unsigned int length = UT_UCS4_strlen(static_cast<const UT_UCS4Char *>(text));
        
    // allocate ascii characters plus room for a null terminator.    
        char* ret = new char[length+1];
        
    // do the string conversion.  this is simple we just cast to 
    // char since UCS-4 is the same as Ascii for english.    
        for(unsigned int i = 0;i < length;++i)
        {
            ret[i] = static_cast<char>(text[i]);
        }
        
    // finally null terminate the string.    
        ret[length] = '\0';

    // and now return it.
        return ret;    
}



//
// AiksaurusABI_asciiToUcs
// -----------------------
//   Helper function to convert Ascii strings into UCS-2.
//   NOTE: you must call delete[] on the returned text!!!
//
//   The thesaurus returns the replacement word in pure Ascii, but Abiword
//   documents operate in UCS-2. So, we have to convert to ascii to UCS-2
//   in order to be able to replace the user's word with their replacement
//   word.
//
inline static UT_UCSChar* 
AiksaurusABI_asciiToUcs(const char* text, int& length)
{
  length = 0 ;
  if ( !text )
    return static_cast<UT_UCSChar*>(NULL);

    // calculate the length of our text so we can create a UCS-2
    // buffer of equal size.
        length = strlen(text);
    
    // allocate UCS-2 character buffer of same size, plus room for 
    // a null terminator.    
        UT_UCSChar* ret = new UT_UCSChar[length+1];
    
    // convert ascii to UCS-2.  This is simply a cast-loop really.
        for(int i = 0;i < length;++i)
        {
            ret[i] = static_cast<UT_UCSChar>(text[i]);
        }
    
    // remember to null terminate the string.
        ret[length] = 0;

    // now return the string.
        return ret;
}


#include <iostream>

//
// AiksaurusABI_invoke
// -------------------
//   This is the function that we actually call to invoke the thesaurus.
//   It should be called when the user hits the thesaurus key (shift+f7?)
//   or chooses "thesaurus" from a menu.
//
bool 
AiksaurusABI_invoke(AV_View* /*v*/, EV_EditMethodCallData * /*d*/)
{
    // Get the current view that the user is in.
    XAP_Frame *pFrame = XAP_App::getApp()->getLastFocussedFrame();
    FV_View* pView = static_cast<FV_View*>(pFrame->getCurrentView());

    // If the user is on a word, but does not have it selected, we need
    // to go ahead and select that word so that the search/replace goes
    // correctly.
    pView->moveInsPtTo(FV_DOCPOS_EOW_MOVE);
    pView->moveInsPtTo(FV_DOCPOS_BOW);
    pView->extSelTo(FV_DOCPOS_EOW_SELECT);  
	
    // Now we will figure out what word to look up when we open our dialog.
    char* search = 0;
    if (!pView->isSelectionEmpty())
    {
        // We need to get the Ascii version of the current word.
        UT_UCS4Char * ucs4ST;
	 pView->getSelectionText(*&ucs4ST);
	 search = AiksaurusABI_ucsToAscii(
					 ucs4ST
        );
    }

    // Now we will run the thesaurus dialog and get a response.
    // We will automatically do a search for the selected/current word. 

#ifdef WIN32
	AiksaurusApp thesaurus;
	thesaurus.setInstance( (HINSTANCE)s_hModule );
#else
    AiksaurusGTK thesaurus;    
#endif
    thesaurus.setTitle("Abiword Thesaurus");
    thesaurus.setInitialMessage("Welcome to Aiksaurus");
    const char* response = thesaurus.runThesaurus(search);

    if (response)
    {
        // Now that we have our replacement, we need to convert it to UCS-2. 
        int length;
        UT_UCSChar* replacement = AiksaurusABI_asciiToUcs(response, length);
    
        // Now, if our replacement has length, we can go ahead and run the 
        // replacement.  If the replacement has no length, we will do nothing.
        if (length)
            pView->cmdCharInsert(replacement, length);
    
        // all done with replacement.
        delete[] replacement;
    }
    
    // Finally, we need to remember to delete search and replacement strings.
    // Note that "search" might be null so we only want to delete[] it if it
    // was actually initialized above. 
    if (search) 
        delete[] search;
    
    return true;
}
