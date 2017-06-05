/*
* * Copyright (C) 2004 Luca Padovani <lpadovan@cs.unibo.it>
 * Copyright (C) 2005 Martin Sevior <msevior@physics.unimelb.edu.au>
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
#define abi_plugin_register abipgn_abimathview_register
#define abi_plugin_unregister abipgn_abimathview_unregister
#define abi_plugin_supports_version abipgn_abimathview_supports_version
#endif

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <MathView/Init.hh>
#include <MathView/MathMLElement.hh>
#include <MathView/ShaperManager.hh>
#include <MathView/SpaceShaper.hh>
#include <MathView/libxml2_MathView.hh>
#include <MathView/MathMLElement.hh>
#include <MathView/Logger.hh>
#include <MathView/AbstractLogger.hh>
#include <MathView/BoundingBox.hh>
#include <MathView/MathMLNamespaceContext.hh>
#include <MathView/NamespaceContext.hh>
#include <MathView/MathMLElement.hh>
#include <MathView/MathMLOperatorDictionary.hh>
#include "gr_Abi_MathGraphicDevice.h"
#include "gr_Abi_RenderingContext.h"

#include "AbiMathView.h"
#include "gr_Abi_DefaultShaper.h"
#include "gr_Abi_StandardSymbolsShaper.h"
#include "gr_Abi_RenderingContext.h"
#include "pd_Document.h"
#include "pp_Property.h"
#include "pp_AttrProp.h"
#include "ut_mbtowc.h"
#include "gr_Painter.h"
#include "xad_Document.h"
#include "xap_Module.h"
#include "ap_App.h"
#include "ie_imp.h"
#include "ie_impGraphic.h"
#include "ut_assert.h"
#include "ut_debugmsg.h"
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
#include "ie_exp.h"
#include "ie_types.h"
#include "xap_Dialog_Id.h"
#include "ap_Dialog_Id.h"
#include "xap_Dlg_FileOpenSaveAs.h"
#include "xap_DialogFactory.h"
#include "xap_Dlg_MessageBox.h"
#include "ap_Strings.h"
#include "ap_Dialog_Latex.h"
#include "ut_mbtowc.h"
#include "ap_Menu_Id.h"

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

#include "itex2MML.h"

static GR_MathManager * pMathManager = NULL; // single plug-in instance of GR_MathManager

//
// AbiMathView_addToMenus
// -----------------------
//   Adds "Equation" "From File" "From Latex" options to AbiWord's Main Menu.
//

static bool AbiMathView_FileInsert(AV_View* v, EV_EditMethodCallData *d);
static bool AbiMathView_LatexInsert(AV_View* v, EV_EditMethodCallData *d);

// FIXME make these translatable strings
/*
static const char * AbiMathView_MenuLabelEquation = "Equation";
static const char * AbiMathView_MenuTooltipEquation = "Insert Equation";
static const char* AbiMathView_MenuLabelFileInsert = "From File";
static const char* AbiMathView_MenuTooltipFileInsert = "Insert MathML from a file";
static const char* AbiMathView_MenuLabelLatexInsert = "From Latex";
static const char* AbiMathView_MenuTooltipLatexInsert = "Insert Equation from a Latex expression";
*/
static const char * AbiMathView_MenuLabelEquation = NULL;
static const char * AbiMathView_MenuTooltipEquation = NULL;
static const char* AbiMathView_MenuLabelFileInsert = NULL;
static const char* AbiMathView_MenuTooltipFileInsert = NULL;
static const char* AbiMathView_MenuLabelLatexInsert = NULL;
static const char* AbiMathView_MenuTooltipLatexInsert = NULL;
static const char* AbiMathView_MenuEndEquation= "EndEquation";
static XAP_Menu_Id newEquationID;
static XAP_Menu_Id FromFileID;
static XAP_Menu_Id FromLatexID;
static XAP_Menu_Id endEquationID;
static void AbiMathView_addToMenus()
{
    // First we need to get a pointer to the application itself.
    XAP_App *pApp = XAP_App::getApp();
    //
    // Translated Strings
    //
    const XAP_StringSet * pSS = pApp->getStringSet();
    AbiMathView_MenuLabelEquation= pSS->getValue(AP_STRING_ID_MENU_LABEL_INSERT_EQUATION);
    AbiMathView_MenuTooltipEquation = pSS->getValue(AP_STRING_ID_MENU_LABEL_TOOLTIP_INSERT_EQUATION);
    AbiMathView_MenuLabelFileInsert = pSS->getValue(AP_STRING_ID_MENU_LABEL_INSERT_EQUATION_FILE);
    AbiMathView_MenuTooltipFileInsert = pSS->getValue(AP_STRING_ID_MENU_LABEL_TOOLTIP_INSERT_EQUATION_FILE);
    AbiMathView_MenuLabelLatexInsert = pSS->getValue(AP_STRING_ID_MENU_LABEL_INSERT_EQUATION_LATEX);
    AbiMathView_MenuTooltipLatexInsert = pSS->getValue(AP_STRING_ID_MENU_LABEL_TOOLTIP_INSERT_EQUATION_LATEX);
    
    // Create an EditMethod that will link our method's name with
    // it's callback function.  This is used to link the name to 
    // the callback.
    EV_EditMethod *myEditMethodFile = new EV_EditMethod(
        "AbiMathView_FileInsert",  // name of callback function
        AbiMathView_FileInsert,    // callback function itself.
        0,                      // no additional data required.
        ""                      // description -- allegedly never used for anything
    );

    EV_EditMethod *myEditMethodLatex = new EV_EditMethod(
        "AbiMathView_LatexInsert",  // name of callback function
        AbiMathView_LatexInsert,    // callback function itself.
        0,                      // no additional data required.
        ""                      // description -- allegedly never used for anything
    );
   
    // Now we need to get the EditMethod container for the application.
    // This holds a series of Edit Methods and links names to callbacks.
    EV_EditMethodContainer* pEMC = pApp->getEditMethodContainer();
    
    // We have to add our EditMethod to the application's EditMethodList
    // so that the application will know what callback to call when a call
    // to "AbiMathView_FileInsert" is received.
    pEMC->addEditMethod(myEditMethodFile);
    pEMC->addEditMethod(myEditMethodLatex);
  

    // Now we need to grab an ActionSet.  This is going to be used later
    // on in our for loop.  Take a look near the bottom.
    EV_Menu_ActionSet* pActionSet = pApp->getMenuActionSet();

    XAP_Menu_Factory * pFact = pApp->getMenuFactory();

// Put it after Insert Picture in the Main menu

    newEquationID= pFact->addNewMenuAfter("Main",NULL,AP_MENU_ID_INSERT_ENDNOTE,EV_MLF_BeginSubMenu);
   UT_DEBUGMSG(("newEquationID %d \n",newEquationID));


    pFact->addNewLabel(NULL,newEquationID,AbiMathView_MenuLabelEquation, AbiMathView_MenuTooltipEquation);

    // Create the Action that will be called.
    EV_Menu_Action* myEquationAction = new EV_Menu_Action(
	newEquationID,          // id that the layout said we could use
	1,                      // yes, we have a sub menu.
	0,                      // no, we don't raise a dialog.
	0,                      // no, we don't have a checkbox.
	0,                      // no radio buttons for me, thank you
	NULL,                   //  no callback function to call.
	NULL,                   // don't know/care what this is for
	NULL                    // don't know/care what this is for
        );

    // Now what we need to do is add this particular action to the ActionSet
    // of the application.  This forms the link between our new ID that we 
    // got for this particular frame with the EditMethod that knows how to 
    // call our callback function.  

    pActionSet->addAction(myEquationAction);

    FromFileID= pFact->addNewMenuAfter("Main",NULL,newEquationID,EV_MLF_Normal);
    UT_DEBUGMSG(("FromFile ID %d \n",FromFileID));

    pFact->addNewLabel(NULL,FromFileID,AbiMathView_MenuLabelFileInsert, AbiMathView_MenuTooltipFileInsert);


    // Create the Action that will be called.
    EV_Menu_Action* myFileAction = new EV_Menu_Action(
	FromFileID,                     // id that the layout said we could use
	0,                      // no, we don't have a sub menu.
	1,                      // yes, we raise a dialog.
	0,                      // no, we don't have a checkbox.
	0,                      // no radio buttons for me, thank you
	"AbiMathView_FileInsert",  // name of callback function to call.
	NULL,                   // don't know/care what this is for
	NULL                    // don't know/care what this is for
        );

    // Now what we need to do is add this particular action to the ActionSet
    // of the application.  This forms the link between our new ID that we 
    // got for this particular frame with the EditMethod that knows how to 
    // call our callback function.  

    pActionSet->addAction(myFileAction);

   FromLatexID= pFact->addNewMenuAfter("Main",NULL,FromFileID,EV_MLF_Normal);
   UT_DEBUGMSG(("Latex ID %d \n",FromLatexID));
 
    pFact->addNewLabel(NULL,FromLatexID,AbiMathView_MenuLabelLatexInsert, AbiMathView_MenuTooltipLatexInsert);


    // Create the Action that will be called.
    EV_Menu_Action* myLatexAction = new EV_Menu_Action(
	FromLatexID,                     // id that the layout said we could use
	0,                      // no, we don't have a sub menu.
	1,                      // yes, we raise a dialog.
	0,                      // no, we don't have a checkbox.
	0,                      // no radio buttons for me, thank you
	"AbiMathView_LatexInsert",  // name of callback function to call.
	NULL,                   // don't know/care what this is for
	NULL                    // don't know/care what this is for
        );


    pActionSet->addAction(myLatexAction);

   endEquationID= pFact->addNewMenuAfter("Main",NULL,AbiMathView_MenuLabelLatexInsert,EV_MLF_EndSubMenu);
   UT_DEBUGMSG(("End Equation ID %d \n",endEquationID));
    pFact->addNewLabel(NULL,endEquationID,AbiMathView_MenuEndEquation,NULL);

 
 // Create the Action that will be called.
    EV_Menu_Action* myEndEquationAction = new EV_Menu_Action(
	endEquationID,          // id that the layout said we could use
	0,                      // no, we don't have a sub menu.
	0,                      // no, we raise a dialog.
	0,                      // no, we don't have a checkbox.
	0,                      // no radio buttons for me, thank you
	NULL,                   // name of callback function to call.
	NULL,                   // don't know/care what this is for
	NULL                    // don't know/care what this is for
        );


    pActionSet->addAction(myEndEquationAction);
    
    pApp->rebuildMenus();
}

static void
AbiMathView_removeFromMenus ()
{
	// First we need to get a pointer to the application itself.
	XAP_App *pApp = XAP_App::getApp();

	// remove the edit method
	EV_EditMethodContainer* pEMC = pApp->getEditMethodContainer() ;
	EV_EditMethod * pEM = ev_EditMethod_lookup ( "AbiMathView_FileInsert" ) ;
	pEMC->removeEditMethod ( pEM ) ;
	DELETEP( pEM ) ;
	pEM = ev_EditMethod_lookup ( "AbiMathView_LatexInsert" ) ;
	pEMC->removeEditMethod ( pEM ) ;
	DELETEP( pEM ) ;

	// now remove crap from the menus
	XAP_Menu_Factory * pFact = pApp->getMenuFactory();

	pFact->removeMenuItem("Main",NULL,newEquationID);
	pFact->removeMenuItem("Main",NULL,FromFileID);
	pFact->removeMenuItem("Main",NULL,FromLatexID);
	pFact->removeMenuItem("Main",NULL, endEquationID);

	pApp->rebuildMenus();
}
 

XAP_Dialog_MessageBox::tAnswer s_CouldNotLoadFileMessage(XAP_Frame * pFrame, const char * pNewFile, UT_Error errorCode)
{
	XAP_String_Id String_id;

	switch (errorCode)
	{
		case -301:
			String_id = AP_STRING_ID_MSG_IE_FileNotFound;
			break;

		case -302:
			String_id = AP_STRING_ID_MSG_IE_NoMemory;
			break;

		case -303:
			String_id = AP_STRING_ID_MSG_IE_UnsupportedType;
			//AP_STRING_ID_MSG_IE_UnknownType;
			break;

		case -304:
			String_id = AP_STRING_ID_MSG_IE_BogusDocument;
			break;

		case -305:
			String_id = AP_STRING_ID_MSG_IE_CouldNotOpen;
			break;

		case -306:
			String_id = AP_STRING_ID_MSG_IE_CouldNotWrite;
			break;

		case -307:
			String_id = AP_STRING_ID_MSG_IE_FakeType;
			break;

		case -311:
			String_id = AP_STRING_ID_MSG_IE_UnsupportedType;
			break;

		default:
			String_id = AP_STRING_ID_MSG_ImportError;
	}

	return pFrame->showMessageBox(String_id,
			XAP_Dialog_MessageBox::b_O,
			XAP_Dialog_MessageBox::a_OK,
			pNewFile);
}

static bool s_AskForMathMLPathname(XAP_Frame * pFrame,
					   char ** ppPathname)
{
	// raise the file-open dialog for inserting a MathML equation.
	// return a_OK or a_CANCEL depending on which button
	// the user hits.
	// return a pointer a g_strdup()'d string containing the
	// pathname the user entered -- ownership of this goes
	// to the caller (so free it when you're done with it).

	UT_DEBUGMSG(("s_AskForMathMLPathname: frame %p\n", pFrame));

	UT_return_val_if_fail (ppPathname, false);
	*ppPathname = NULL;

	pFrame->raise();

	XAP_DialogFactory * pDialogFactory
		= static_cast<XAP_DialogFactory *>(pFrame->getDialogFactory());

	XAP_Dialog_FileOpenSaveAs * pDialog
		= static_cast<XAP_Dialog_FileOpenSaveAs *>(pDialogFactory->requestDialog(XAP_DIALOG_ID_INSERTMATHML));
	UT_return_val_if_fail (pDialog, false);

	pDialog->setCurrentPathname(NULL);
	pDialog->setSuggestFilename(false);

	/* 
	TODO: add a "MathML (.xml)" entry to the file type list, and set is as the default file type

	pDialog->setFileTypeList(szDescList, szSuffixList, static_cast<const UT_sint32 *>(nTypeList));
	*/

	pDialog->runModal(pFrame);

	XAP_Dialog_FileOpenSaveAs::tAnswer ans = pDialog->getAnswer();
	bool bOK = (ans == XAP_Dialog_FileOpenSaveAs::a_OK);

	if (bOK)
	{
		const char * szResultPathname = pDialog->getPathname();
		UT_DEBUGMSG(("MATHML Path Name selected = %s \n",szResultPathname));
		if (szResultPathname && *szResultPathname)
			*ppPathname = g_strdup(szResultPathname);

		UT_sint32 type = pDialog->getFileType();

		// If the number is negative, it's a special type.
		// Some operating systems which depend solely on filename
		// suffixes to identify type (like Windows) will always
		// want auto-detection.
		if (type < 0)
		{
			switch (type)
			{
			case XAP_DIALOG_FILEOPENSAVEAS_FILE_TYPE_AUTO:
				// do some automagical detecting
				break;
			default:
				// it returned a type we don't know how to handle
				UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
			}
		}
		else
		{
			/* todo */
		}
	}

	pDialogFactory->releaseDialog(pDialog);

	return bOK;
}

//
// AbiMathView_FileInsert
// -------------------
//   This is the function that we actually call to insert the MathML.
//
bool 
AbiMathView_FileInsert(AV_View* /*v*/, EV_EditMethodCallData */*d*/)
{
	// Get the current view that the user is in.
	XAP_Frame *pFrame = XAP_App::getApp()->getLastFocussedFrame();
	FV_View* pView = static_cast<FV_View*>(pFrame->getCurrentView());
	PD_Document * pDoc = static_cast<PD_Document *>(pFrame->getCurrentDoc());
	char* pNewFile = NULL;


	bool bOK = s_AskForMathMLPathname(pFrame,&pNewFile);
	
	if (!bOK || !pNewFile)
	{
		UT_DEBUGMSG(("ARRG! bOK = %d pNewFile = %x \n",bOK,pNewFile));
		return false;
	}
	UT_UTF8String sNewFile = pNewFile;

	// we own storage for pNewFile and must free it.
	FREEP(pNewFile);

	UT_DEBUGMSG(("fileInsertMathML: loading [%s]\n",sNewFile.utf8_str()));
   
	IE_Imp_MathML * pImpMathML = new IE_Imp_MathML(pDoc, pMathManager->EntityTable());
	UT_Error errorCode = pImpMathML->importFile(sNewFile.utf8_str());

	if (errorCode != UT_OK)
	{
		s_CouldNotLoadFileMessage(pFrame, sNewFile.utf8_str(), errorCode);
		DELETEP(pImpMathML);
		return false;
	}

	/* Create the data item */
	const char* mimetypeMATHML = NULL;
	mimetypeMATHML = g_strdup("application/mathml+xml");
	UT_uint32 uid = pDoc->getUID(UT_UniqueId::Image);
	UT_UTF8String sUID;
	UT_UTF8String_sprintf(sUID,"%d",uid);
	pDoc->createDataItem(sUID.utf8_str(), false, pImpMathML->getByteBuf(), static_cast<void *>(const_cast<char *>(mimetypeMATHML)), NULL);

	/* Insert the MathML Object */
	PT_DocPosition pos = pView->getPoint();
	pView->cmdInsertMathML(sUID.utf8_str(),pos);

	DELETEP(pImpMathML);

	return true;
}


//
// AbiMathView_LatexInsert
// -------------------
//   This is the function that we actually call to insert the MathML from
//   a Latex expression.
//
bool 
AbiMathView_LatexInsert(AV_View* v, EV_EditMethodCallData */*d*/)
{
	FV_View * pView = static_cast<FV_View *>(v);
	XAP_Frame * pFrame = static_cast<XAP_Frame *> ( pView->getParentData());
	pFrame->raise();

	XAP_DialogFactory * pDialogFactory
		= static_cast<XAP_DialogFactory *>(pFrame->getDialogFactory());

	AP_Dialog_Latex * pDialog 
		= static_cast<AP_Dialog_Latex *>(pDialogFactory->requestDialog(AP_DIALOG_ID_LATEX));
	UT_return_val_if_fail(pDialog, false);

	if (pDialog->isRunning())
	{
		pDialog->activate();
	}
	else
	{
		pDialog->runModeless(pFrame);
	}

	return true;
}

GR_AbiMathItems::GR_AbiMathItems(void): 
	m_iAPI(0),
	m_bHasSnapshot(false)
{
}

GR_AbiMathItems::~GR_AbiMathItems(void)
{
}

GR_MathManager::GR_MathManager(GR_Graphics* pG) :
	GR_EmbedManager(pG), 
	m_CurrentUID(-1),
	m_pLogger(NULL),
	m_pMathGraphicDevice(NULL),
	m_pAbiContext(NULL),
	m_pOperatorDictionary(NULL),
	m_pDoc(NULL)
{
	m_vecMathView.clear();
	m_vecItems.clear();
}

GR_MathManager::~GR_MathManager()
{ 
	DELETEP(m_pAbiContext);
	m_pAbiContext = 0;
	UT_VECTOR_PURGEALL(GR_AbiMathItems *,m_vecItems);
}

GR_EmbedManager * GR_MathManager::create(GR_Graphics * pG)
{
	return static_cast<GR_EmbedManager *>(new GR_MathManager(pG));
}

const char * GR_MathManager::getObjectType(void) const
{
	return "mathml";
}

void GR_MathManager::initialize(void)
{
	XAP_App *pApp = XAP_App::getApp();

	// load the gtkmathview settings and operator dictionaries from the private user directory, ...
	UT_UTF8String userConfDir(pApp->getUserPrivateDirectory());
	UT_UTF8String userDictDir(pApp->getUserPrivateDirectory());
	UT_UTF8String userCombiningDictDir(pApp->getUserPrivateDirectory());
	UT_UTF8String userLocalDictDir(pApp->getUserPrivateDirectory());
#if defined(WIN32)
	userConfDir += "\\math\\gtkmathview.conf.xml";
	userDictDir += "\\math\\dictionary.xml";
	userCombiningDictDir += "\\math\\dictionary-combining.xml";
	userLocalDictDir += "\\math\\dictionary-local.xml";
#else
	userConfDir += "/math/gtkmathview.conf.xml";
	userDictDir += "/math/dictionary.xml";
	userCombiningDictDir += "/math/dictionary-combining.xml";
	userLocalDictDir += "/math/dictionary-local.xml";
#endif

	// ... or from the (common) AbiSuite directory
	UT_UTF8String libConfDir(pApp->getAbiSuiteLibDir());
	UT_UTF8String libDictDir(pApp->getAbiSuiteLibDir());
	UT_UTF8String libCombiningDictDir(pApp->getAbiSuiteLibDir());
	UT_UTF8String libLocalDictDir(pApp->getAbiSuiteLibDir());
#if defined(WIN32)	 
	libConfDir += "\\math\\gtkmathview.conf.xml";
	libDictDir += "\\math\\dictionary.xml";
	libCombiningDictDir += "\\math\\dictionary-combining.xml";
	libLocalDictDir += "\\math\\dictionary-local.xml";
#else
	libConfDir += "/math/gtkmathview.conf.xml";
	libDictDir += "/math/dictionary.xml";
	libCombiningDictDir += "/math/dictionary-combining.xml";
	libLocalDictDir += "/math/dictionary-local.xml";
#endif

	// add the configuration paths
 
#ifdef TOOLKIT_COCOA
	if (const char * resources = getenv("ABIWORD_COCOA_BUNDLED_RESOURCES"))
	{
		UT_UTF8String bundleConfDir(resources);
		bundleConfDir += "/gtkmathview/gtkmathview.conf.xml";
		Configuration::addConfigurationPath(bundleConfDir.utf8_str());
	}
#endif
	Configuration::addConfigurationPath( libConfDir.utf8_str() );
	Configuration::addConfigurationPath( userConfDir.utf8_str() );

	SmartPtr<AbstractLogger> logger = Logger::create();
	m_pLogger = logger;
	SmartPtr<Configuration> configuration = initConfiguration<libxml2_MathView>(logger, getenv("GTKMATHVIEWCONF"));
	logger->setLogLevel(LOG_INFO);
	 
	// add the dictionary paths

#ifdef TOOLKIT_COCOA
	if (const char * resources = getenv("ABIWORD_COCOA_BUNDLED_RESOURCES"))
	{
		UT_UTF8String bundleDictDir(resources);
		UT_UTF8String bundleCombiningDictDir(resources);
		UT_UTF8String bundleLocalDictDir(resources);

		bundleDictDir += "/gtkmathview/dictionary.xml";
		bundleCombiningDictDir += "/gtkmathview/dictionary-combining.xml";
		bundleLocalDictDir += "/gtkmathview/dictionary-local.xml";

		configuration->add("dictionary/path", bundleDictDir.utf8_str());
		configuration->add("dictionary/path", bundleCombiningDictDir.utf8_str());
		configuration->add("dictionary/path", bundleLocalDictDir.utf8_str());
	}
#endif
	configuration->add("dictionary/path", libDictDir.utf8_str());
	configuration->add("dictionary/path", libCombiningDictDir.utf8_str());
	configuration->add("dictionary/path", libLocalDictDir.utf8_str());

	configuration->add("dictionary/path", userDictDir.utf8_str());
	configuration->add("dictionary/path", userCombiningDictDir.utf8_str());
	configuration->add("dictionary/path", userLocalDictDir.utf8_str());

	SmartPtr<GR_Abi_MathGraphicDevice> mathGraphicDevice 
			= GR_Abi_MathGraphicDevice::create(logger, configuration, getGraphics());
	m_pMathGraphicDevice = mathGraphicDevice;
	m_pAbiContext = new GR_Abi_RenderingContext(getGraphics());
	UT_DEBUGMSG(("MAthView New rendering context %x Graphics %x \n",m_pAbiContext,getGraphics()));
	m_pOperatorDictionary = initOperatorDictionary<libxml2_MathView>(logger, configuration);
}

UT_sint32  GR_MathManager::_makeMathView(void)
{
	UT_ASSERT(m_pLogger);
	SmartPtr<libxml2_MathView> pMathView = libxml2_MathView::create(m_pLogger);
	m_vecMathView.addItem(pMathView);
	pMathView->setOperatorDictionary(m_pOperatorDictionary);
	pMathView->setMathMLNamespaceContext(
				     MathMLNamespaceContext::create(pMathView, 
                                     m_pMathGraphicDevice));
	return static_cast<UT_sint32>(m_vecMathView.getItemCount()-1);
}

void GR_MathManager::_loadMathML(UT_sint32 uid, UT_UTF8String & sMathBuf)
{
	SmartPtr<libxml2_MathView> pMathView = m_vecMathView.getNthItem(uid);
	UT_return_if_fail(pMathView);
	UT_DEBUGMSG(("loading |%s| \n",sMathBuf.utf8_str()));
	bool b = pMathView->loadBuffer(sMathBuf.utf8_str());
	if(!b)
	{
		UT_DEBUGMSG(("Attempt to load |%s| \n failed \n",sMathBuf.utf8_str()));

		UT_UTF8String sFailed = "<math xmlns='http://www.w3.org/1998/Math/MathML' display='inline'><merror><mtext>";
		sFailed += "failed"; // TODO: need a better message!
		sFailed += "</mtext></merror></math>";

		pMathView->loadBuffer(sFailed.utf8_str());
	}
}

void GR_MathManager::setDefaultFontSize(UT_sint32 uid, UT_sint32 iSize)
{
	SmartPtr<libxml2_MathView>  pMathView = m_vecMathView.getNthItem(uid);
	UT_return_if_fail(pMathView);
	pMathView->setDefaultFontSize(iSize);
}

UT_sint32 GR_MathManager::makeEmbedView(AD_Document * pDoc, UT_uint32 api, const char * /*szDataID*/)
{
	if (m_pDoc == NULL)
	{
		m_pDoc = static_cast<PD_Document *>(pDoc);
	}
	else
	{
		UT_ASSERT(m_pDoc == static_cast<PD_Document *>(pDoc));
	}

	UT_sint32 iNew = _makeMathView();
	GR_AbiMathItems * pItem = new GR_AbiMathItems();
	pItem->m_iAPI = api;
	pItem->m_bHasSnapshot = false;
	m_vecItems.addItem(pItem);
	UT_ASSERT(static_cast<UT_sint32>(m_vecItems.getItemCount()) == (iNew+1));

	return iNew;
}

void GR_MathManager::makeSnapShot(UT_sint32 uid, UT_Rect & rec)
{
	const PP_AttrProp * pSpanAP = NULL;
	const char * pszDataID = NULL;
	bool bFoundDataID = false;

	if (!getGraphics()->queryProperties(GR_Graphics::DGP_SCREEN))
	{
		return;
	}

	GR_AbiMathItems * pItem = m_vecItems.getNthItem(uid);
	UT_return_if_fail(pItem);  
	PT_AttrPropIndex api = pItem->m_iAPI;
	m_pDoc->getAttrProp(api, &pSpanAP);

	bFoundDataID = pSpanAP->getAttribute("dataid", pszDataID);
	UT_ASSERT((bFoundDataID));

	if (pItem->m_bHasSnapshot)
	{
		updatePNGSnapshot(static_cast<AD_Document *>(m_pDoc),rec,pszDataID);
	}
	else
	{
		createPNGSnapshot(static_cast<AD_Document *>(m_pDoc),rec,pszDataID);
		pItem->m_bHasSnapshot = true;
	}
}

bool GR_MathManager::isDefault(void)
{
	return false;
}


bool GR_MathManager::createPNGSnapshot(AD_Document * pDoc, UT_Rect & rec,
					   const char * szDataID)
{
	const char* mimetypePNG = NULL;

	if (isDefault())
	{
		return false;
	}

	if ((rec.width == 0) || (rec.height ==0))
	{
		return false;
	}

	GR_Painter painter(getGraphics());
	GR_Image * pImage = painter.genImageFromRectangle(rec);
	if(pImage == NULL)
	{
		return false;
	}
	UT_ByteBuf * pBuf = NULL;
	pImage->convertToBuffer(&pBuf);
	UT_UTF8String sID = "snapshot-png-";
	sID += szDataID;

	mimetypePNG = g_strdup("image/png");
	UT_DEBUGMSG(("Making data-item of name %s \n",sID.utf8_str()));
	pDoc->createDataItem(sID.utf8_str(),false,reinterpret_cast< const UT_ByteBuf *>(pBuf),mimetypePNG,NULL);

	DELETEP(pBuf);
	DELETEP(pImage);

	return true;
}


bool GR_MathManager::updatePNGSnapshot(AD_Document * pDoc, UT_Rect & rec,
					   const char * szDataID)
{
	if (isDefault())
	{
		return false;
	}

	GR_Painter painter(getGraphics());
	GR_Image * pImage = painter.genImageFromRectangle(rec);
	if(pImage == NULL)
	{
	    return false;
	}
	UT_ByteBuf * pBuf = NULL;

	pImage->convertToBuffer(&pBuf);
	UT_UTF8String sID = "snapshot-png-";
	sID += szDataID;

	pDoc->replaceDataItem(sID.utf8_str(),reinterpret_cast< const UT_ByteBuf *>(pBuf));

	DELETEP(pBuf);
	DELETEP(pImage);

	return true;
}

bool GR_MathManager::modify(UT_sint32 /*uid*/)
{
	return false;
}

void GR_MathManager::initializeEmbedView(UT_sint32 uid)
{
	SmartPtr<libxml2_MathView>  pMathView = m_vecMathView.getNthItem(uid);
	//	printf("pMathView uid is %d \n",uid);
	UT_return_if_fail(pMathView);
	// printf("Dirty Layout is set \n");
	pMathView->setDirtyLayout();
	UT_return_if_fail(pMathView->getMathMLNamespaceContext());
	UT_return_if_fail(pMathView->getMathMLNamespaceContext()->getGraphicDevice());
	// printf("Clear Cache is called \n");
	pMathView->getMathMLNamespaceContext()->getGraphicDevice()->clearCache();
}

void GR_MathManager::loadEmbedData(UT_sint32 uid)
{
	SmartPtr<libxml2_MathView>  pMathView = m_vecMathView.getNthItem(uid);
	UT_return_if_fail(pMathView);

	const PP_AttrProp * pSpanAP = NULL;
	GR_AbiMathItems * pItem = m_vecItems.getNthItem(uid);
	UT_return_if_fail(pItem);  

	PT_AttrPropIndex api = pItem->m_iAPI;
	m_pDoc->getAttrProp(api, &pSpanAP);

	const char * pszDataID = NULL;
	bool bFoundDataID = pSpanAP->getAttribute("dataid", pszDataID);
	UT_DEBUGMSG(("DataID val =%s \n",pszDataID));

	UT_UTF8String sMathML;
	if (bFoundDataID && pszDataID)
	{
		const UT_ByteBuf * pByteBuf = NULL;
		bFoundDataID = m_pDoc->getDataItemDataByName(pszDataID, 
						const_cast<const UT_ByteBuf **>(&pByteBuf),
						NULL, NULL);
		UT_return_if_fail(pByteBuf && bFoundDataID);
		UT_UCS4_mbtowc myWC;
		sMathML.appendBuf( *pByteBuf, myWC);
	}

	UT_return_if_fail(bFoundDataID);
	UT_return_if_fail(pszDataID);
	UT_DEBUGMSG(("MATH ML string is... \n %s \n",sMathML.utf8_str()));
	// printf("MATH ML string is... \n %s \n",sMathML.utf8_str());
	_loadMathML(uid, sMathML);
}

UT_sint32 GR_MathManager::getWidth(UT_sint32 uid)
{
	SmartPtr<libxml2_MathView>  pMathView = m_vecMathView.getNthItem(uid);
	BoundingBox box = pMathView->getBoundingBox();
	return m_pAbiContext->toAbiLayoutUnits(box.width);
}

UT_sint32 GR_MathManager::getAscent(UT_sint32 uid)
{
	SmartPtr<libxml2_MathView>  pMathView = m_vecMathView.getNthItem(uid);
	BoundingBox box = pMathView->getBoundingBox();
	UT_DEBUGMSG(("GetAscent New rendering context %x Graphics %x Device resolution %d \n",m_pAbiContext,getGraphics(),getGraphics()->getDeviceResolution()));
	return m_pAbiContext->toAbiLayoutUnits(box.height);
}

UT_sint32 GR_MathManager::getDescent(UT_sint32 uid)
{
	SmartPtr<libxml2_MathView>  pMathView = m_vecMathView.getNthItem(uid);
	BoundingBox box = pMathView->getBoundingBox();
	return m_pAbiContext->toAbiLayoutUnits(box.depth);
}

void GR_MathManager::setColor(UT_sint32 /*uid*/, UT_RGBColor c)
{
	m_pAbiContext->setColor(c);
}

void GR_MathManager::render(UT_sint32 uid, UT_Rect & rec)
{
	scaled x = m_pAbiContext->fromAbiX(rec.left);
	scaled y = m_pAbiContext->fromAbiLayoutUnits(-rec.top);// should be fromAbiY()
	SmartPtr<libxml2_MathView>  pMathView = m_vecMathView.getNthItem(uid);
	UT_return_if_fail(pMathView);
	pMathView->render(*m_pAbiContext, x, y);
}

void GR_MathManager::releaseEmbedView(UT_sint32 uid)
{
	SmartPtr<libxml2_MathView>  pMathView = m_vecMathView.getNthItem(uid);
	UT_return_if_fail(pMathView);
	pMathView->resetRootElement();
}

bool GR_MathManager::convert(UT_uint32 iConType, UT_ByteBuf & From, UT_ByteBuf & To)
{
	XAP_App * pApp = XAP_App::getApp();
	XAP_Frame * pFrame = pApp->getLastFocussedFrame();

	if (iConType != 0)
	{
		return false;
	}

	/* add a pair of enclosing brackets \[ \] */
	UT_UTF8String sLatex;
	UT_UCS4_mbtowc myWC;
	sLatex += "\\[";
	sLatex.appendBuf(From, myWC);
	sLatex += "\\]";

	char * mathml = itex2MML_parse(sLatex.utf8_str(), sLatex.size());
	
	if (!mathml)
	{
		pFrame->showMessageBox("itex2MML failed to convert the LaTeX equation into MathML, sorry!\n", // TODO: fix message
			XAP_Dialog_MessageBox::b_O, 
			XAP_Dialog_MessageBox::a_OK);
		return false;
	}

	UT_UTF8String sMathML(mathml);
	itex2MML_free_string(mathml);

	if (sMathML.size() == 0)
	{
	  UT_UTF8String sErrMessage = "itex2MML conversion from LaTex equation resulted in zero-length MathML!\n"; // TODO: fix message
		//sErrMessage += sLatex;
		sErrMessage += "\n";
		// sErrMessage += FullLine;
		pFrame->showMessageBox(sErrMessage.utf8_str(),
				XAP_Dialog_MessageBox::b_O,
				XAP_Dialog_MessageBox::a_OK);
		return false;
	}

	UT_DEBUGMSG(("Input MathML %s \n", sMathML.utf8_str()));

	return EntityTable().convert(sMathML.utf8_str(), sMathML.size(), To);
}

static UT_uint32 MathManagerUID = 0; 

ABI_PLUGIN_DECLARE(AbiMathView)

// -----------------------------------------------------------------------
//
//      Abiword Plugin Interface 
//
// -----------------------------------------------------------------------
  
ABI_FAR_CALL
int abi_plugin_register (XAP_ModuleInfo * mi)
{
	mi->name = "AbiMathView";
	mi->desc = "The plugin allows AbiWord to import MathML documents";
	mi->version = ABI_VERSION_STRING;
	mi->author = "Martin Sevior <msevior@physics.unimelb.edu.au>";
	mi->usage = "No Usage";
    
	// Add to AbiWord's plugin listeners
	XAP_App * pApp = XAP_App::getApp();	
	pMathManager = new GR_MathManager(NULL);
	MathManagerUID = pApp->registerEmbeddable(pMathManager);

	// Add to AbiWord's menus
	AbiMathView_addToMenus();
     
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
	pApp->unRegisterEmbeddable(MathManagerUID);
	DELETEP(pMathManager);
	AbiMathView_removeFromMenus();

	return 1;
}

ABI_FAR_CALL
int abi_plugin_supports_version (UT_uint32 /*major*/, UT_uint32 /*minor*/, UT_uint32 /*release*/)
{
	return 1; 
}
