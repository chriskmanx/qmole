/*
 * LoadBindings
 * Copyright (C) 2007 by Martin Sevior
 * Copyright (C) 2007 by Marc 'Foddex' Oude Kotte
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
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <errno.h>
#include <sys/stat.h>
#include <glib.h>
#include <set>
#include <libxml/xmlreader.h>
#include <libxml/xmlerror.h>
#if !defined(LIBXML_READER_ENABLED) 
#error LIBXML reader not enabled! Recompile your libxml library binaries!
#endif

#include "xap_Module.h"
#include "xap_App.h"
#include "xap_Frame.h"
#include "fv_View.h"
#include "ev_EditMethod.h"
#include "ie_imp.h"
#include "ie_exp.h"
#include "ie_types.h"
#include "ap_Convert.h"
#include "ap_EditMethods.h"
#include "ev_EditBits.h"
#include "LoadBindings.h"
#include "ap_LoadBindings.h"
#include "ut_path.h"
#include "ap_Menu_Id.h"
#include "ev_Menu_Actions.h"
#include "ev_Menu.h"
#include "ev_Menu_Layouts.h"
#include "ev_Menu_Labels.h"
#include "xap_Menu_Layouts.h"
#include "xap_DialogFactory.h"
#include "xap_Dlg_FileOpenSaveAs.h"
#include "xap_Dialog_Id.h"
#include "ev_NamedVirtualKey.h"

#ifdef ABI_PLUGIN_BUILTIN
#define abi_plugin_register abipgn_loadbindings_register
#define abi_plugin_unregister abipgn_loadbindings_unregister
#define abi_plugin_supports_version abipgn_loadbindings_supports_version
// dll exports break static linking
#define ABI_BUILTIN_FAR_CALL extern "C"
#else
#define ABI_BUILTIN_FAR_CALL ABI_FAR_CALL
ABI_PLUGIN_DECLARE (LoadBindings)
#endif

#define RES_TO_STATUS(a) ((a) ? 0 : -1)

// -----------------------------------------------------------------------
//      Edit methods and menu items
// -----------------------------------------------------------------------

static bool LoadBindingsDlg_invoke (AV_View * v, EV_EditMethodCallData * d);
static bool LoadBindingsFromURI_invoke (AV_View * v, EV_EditMethodCallData * d);
static bool LoadBindingsFromMemory_invoke (AV_View * v, EV_EditMethodCallData * d);
static bool DumpEditMethods_invoke (AV_View * v, EV_EditMethodCallData * d);
static bool SaveBindings_invoke(AV_View * v, EV_EditMethodCallData* d);

static XAP_Menu_Id loadBindingID;
static XAP_Menu_Id dumpEditMethodsID;
static XAP_Menu_Id saveBindingID;
static const char * szLoadBinding = "Load keybindings";
static const char * szLoadBindingStatus = "Load keybindings from a file";
static const char * szDumpEditMethods = "Dump edit methods";
static const char * szDumpEditMethodsStatus = "Dump edit methods to your console";
static const char * szSaveBinding = "Save keybindings";
static const char * szSaveBindingStatus = "Save keybindings to your profile directory";

static void LoadBindings_registerMethod () 
{
	// prepare
	XAP_App *pApp = XAP_App::getApp ();
	EV_EditMethodContainer *pEMC = pApp->getEditMethodContainer ();
	EV_EditMethod *myEditMethod;
	XAP_Menu_Factory* pFact = pApp->getMenuFactory();

	// add edit methods
	myEditMethod = new EV_EditMethod ("com.abisource.abiword.loadbindings.loadBindingsDlg", LoadBindingsDlg_invoke, 0, "" );
	pEMC->addEditMethod (myEditMethod);

	myEditMethod = new EV_EditMethod ("com.abisource.abiword.loadbindings.fromURI", LoadBindingsFromURI_invoke, 0, "" );
	pEMC->addEditMethod (myEditMethod);
	
	myEditMethod = new EV_EditMethod ("com.abisource.abiword.loadbindings.fromMemory", LoadBindingsFromMemory_invoke, 0, "" );
	pEMC->addEditMethod (myEditMethod);
	
	myEditMethod = new EV_EditMethod ("com.abisource.abiword.loadbindings.dumpEditMethods", DumpEditMethods_invoke, 0, "" );
	pEMC->addEditMethod (myEditMethod);
	
	myEditMethod = new EV_EditMethod ("com.abisource.abiword.loadbindings.saveCurrent", SaveBindings_invoke, 0, "" );
	pEMC->addEditMethod (myEditMethod);
	
#if defined(DEBUG)
	// add menu item
	loadBindingID = pFact->addNewMenuAfter("Main",NULL,AP_MENU_ID_FMT_STYLIST,EV_MLF_Normal);	// named _FMT_, but actually in tools menu
	pFact->addNewLabel(NULL,loadBindingID,szLoadBinding,szLoadBindingStatus);
	EV_Menu_Action* myLoadBindingAction = new EV_Menu_Action(
		loadBindingID,          // id that the layout said we could use
		0,                      // no, we don't have a sub menu.
		1,                      // yes, we raise a dialog.
		0,                      // no, we don't have a checkbox.
		0,                      // no radio buttons for me, thank you
		"com.abisource.abiword.loadbindings.loadBindingsDlg",  // callback function to call.
		NULL,                   // don't know/care what this is for
		NULL                    // don't know/care what this is for
		);
    pApp->getMenuActionSet()->addAction(myLoadBindingAction);
	
	// add menu item
	dumpEditMethodsID = pFact->addNewMenuAfter("Main",NULL,loadBindingID,EV_MLF_Normal);
	pFact->addNewLabel(NULL,dumpEditMethodsID,szDumpEditMethods,szDumpEditMethodsStatus);
	EV_Menu_Action* myDumpEditMethodsAction = new EV_Menu_Action(
		dumpEditMethodsID,        // id that the layout said we could use
		0,                      // no, we don't have a sub menu.
		0,                      // no, we don't raise a dialog.
		0,                      // no, we don't have a checkbox.
		0,                      // no radio buttons for me, thank you
		"com.abisource.abiword.loadbindings.dumpEditMethods",  // callback function to call.
		NULL,                   // don't know/care what this is for
		NULL                    // don't know/care what this is for
		);
    pApp->getMenuActionSet()->addAction(myDumpEditMethodsAction);
	
	// add menu item
	saveBindingID = pFact->addNewMenuAfter("Main",NULL,dumpEditMethodsID,EV_MLF_Normal);
	pFact->addNewLabel(NULL,saveBindingID,szSaveBinding,szSaveBindingStatus);
	EV_Menu_Action* mySaveBindingAction = new EV_Menu_Action(
		saveBindingID,          // id that the layout said we could use
		0,                      // no, we don't have a sub menu.
		0,                      // no, we don't raise a dialog.
		0,                      // no, we don't have a checkbox.
		0,                      // no radio buttons for me, thank you
		"com.abisource.abiword.loadbindings.saveCurrent",  // callback function to call.
		NULL,                   // don't know/care what this is for
		NULL                    // don't know/care what this is for
		);
    pApp->getMenuActionSet()->addAction(mySaveBindingAction);
#endif
}

static void LoadBindings_RemoveFromMethods ()
{
	// prepare
	XAP_App *pApp = XAP_App::getApp ();
	EV_EditMethodContainer *pEMC = pApp->getEditMethodContainer ();
	EV_EditMethod *pEM;
	XAP_Menu_Factory * pFact = pApp->getMenuFactory();
	
	// remove edit methods
	pEM = ev_EditMethod_lookup ("com.abisource.abiword.loadbindings.dumpEditMethods");
	pEMC->removeEditMethod (pEM);
	DELETEP (pEM);
	
	pEM = ev_EditMethod_lookup ("com.abisource.abiword.loadbindings.fromMemory");
	pEMC->removeEditMethod (pEM);
	DELETEP (pEM);

	pEM = ev_EditMethod_lookup ("com.abisource.abiword.loadbindings.fromURI");
	pEMC->removeEditMethod (pEM);
	DELETEP (pEM);

	pEM = ev_EditMethod_lookup ("com.abisource.abiword.loadbindings.loadBindingsDlg");
	pEMC->removeEditMethod (pEM);
	DELETEP (pEM);
	
	pEM = ev_EditMethod_lookup ("com.abisource.abiword.loadbindings.saveCurrent");
	pEMC->removeEditMethod (pEM);
	DELETEP (pEM);
	
#if defined(DEBUG)
	// remove menu items
	pFact->removeMenuItem("Main",NULL,loadBindingID);
	pFact->removeMenuItem("Main",NULL,dumpEditMethodsID);
	pFact->removeMenuItem("Main",NULL,saveBindingID);
#endif
}

// -----------------------------------------------------------------------
//      Abiword Plugin Interface 
// -----------------------------------------------------------------------

static void LoadKeybindings(const char* uri)
{
	UT_return_if_fail(uri);
	UT_DEBUGMSG(("[LoadBindings] trying file %s\n", uri));
		
	// find out if the file exists at all
	GsfInput* in = UT_go_file_open(uri, NULL);
	if (in) 
	{
		// it seems to exist, cleanup after ourselves ...
		g_object_unref(G_OBJECT(in));
		// ... and let LoadBindings_invoke do its thing
		UT_DEBUGMSG(("[LoadBindings] invoking loader on %s\n", uri));
		EV_EditMethodCallData userFileData(uri, strlen(uri));
		LoadBindingsFromURI_invoke(NULL, &userFileData);
	}
}

ABI_BUILTIN_FAR_CALL int abi_plugin_register (XAP_ModuleInfo * mi) 
{
	mi->name = "LoadBindings";
	mi->desc = "This allows Keybindings to be loaded from an Ascii file";
	mi->version = ABI_VERSION_STRING;
	mi->author = 
		"Original version by Martin Sevior <msevior@physics.unimelb.edu.au>\n"
		"Refactored to support XML by Marc 'Foddex' Oude Kotte <foddex@foddex.net>";
	mi->usage = "LoadBindingsDlg_invoke";

	LoadBindings_registerMethod ();
	
	// load the keybindings.xml file from the application directory, if present
	UT_UTF8String appFile = XAP_App::getApp()->getAbiSuiteAppDir();
	appFile += "/keybindings.xml";
	char * appUri = UT_go_filename_to_uri(appFile.utf8_str());
	if (appUri)
	{
		LoadKeybindings(appUri);
		FREEP(appUri);
	}
	
	// load the keybindings.xml file from the user's home directory, if present
	UT_UTF8String userFile = XAP_App::getApp()->getUserPrivateDirectory();
	userFile += "/keybindings.xml";
	char * userUri = UT_go_filename_to_uri(userFile.utf8_str());
	if (userUri)
	{
		LoadKeybindings(userUri);
		FREEP(userUri);
	}

	return 1;
}

ABI_BUILTIN_FAR_CALL int abi_plugin_unregister (XAP_ModuleInfo * mi) 
{
	mi->name = 0;
	mi->desc = 0;
	mi->version = 0;
	mi->author = 0;
	mi->usage = 0;

	LoadBindings_RemoveFromMethods ();
	return 1;
}

ABI_BUILTIN_FAR_CALL int abi_plugin_supports_version (UT_uint32 /*major*/, UT_uint32 /*minor*/, UT_uint32 /*release*/) 
{
	return 1;
}

// -----------------------------------------------------------------------
//     LoadBindings Invocation Code
// -----------------------------------------------------------------------

// Utility function for Abiword that first shows an open file dialog,
// then automatically invokes the LoadBindings and SetBindings functions
static bool LoadBindingsDlg_invoke (AV_View *, EV_EditMethodCallData * /*d*/) 
{
	// ask user what file to open
	XAP_Frame *pFrame = XAP_App::getApp()->getLastFocussedFrame();
	XAP_DialogFactory * pDialogFactory = static_cast<XAP_DialogFactory *>(XAP_App::getApp()->getDialogFactory());
	XAP_Dialog_FileOpenSaveAs * pDialog = static_cast<XAP_Dialog_FileOpenSaveAs *>(pDialogFactory->requestDialog(XAP_DIALOG_ID_FILE_OPEN));
	UT_return_val_if_fail (pDialog, false);
	pDialog->setSuggestFilename(false);
	pDialog->runModal( pFrame );
	XAP_Dialog_FileOpenSaveAs::tAnswer ans = pDialog->getAnswer();
	bool bOK = (ans == XAP_Dialog_FileOpenSaveAs::a_OK);
	std::string resultPathname = bOK ? pDialog->getPathname() : "";
	pDialogFactory->releaseDialog(pDialog);
	
	// call LoadBindings_invoke
	EV_EditMethodCallData d2( resultPathname.c_str(), UT_uint32(resultPathname.size()) );
	return LoadBindingsFromURI_invoke( NULL, &d2 );
}

// Loads the keybindings from an XML file to a named set of keybindings.
static bool LoadBindingsFromURI_invoke (AV_View *, EV_EditMethodCallData * d) 
{
	LoadBindings loadBindings(d, FROM_URI);
	if (!loadBindings.Load()) return false;
	return loadBindings.Set();
}

// Loads the keybindings from memory to a named set of keybindings.
static bool LoadBindingsFromMemory_invoke (AV_View *, EV_EditMethodCallData * d) 
{
	LoadBindings loadBindings(d, FROM_MEMORY);
	if (!loadBindings.Load()) return false;
	return loadBindings.Set();
}

static bool compareEditMethods( const EV_EditMethod* lhs, const EV_EditMethod* rhs ) 
{
	return strcmp( lhs->getName(), rhs->getName() ) < 0;
}

// Dumps (debug only) all edit methods on the console
static bool DumpEditMethods_invoke(AV_View *, EV_EditMethodCallData *) 
{
	EV_EditMethodContainer *pEMC = XAP_App::getApp()->getEditMethodContainer ();
	
	// fill vector with bindable edit methods
	std::vector<EV_EditMethod*> list;
	for (UT_uint32 i=0; i<pEMC->countEditMethods(); ++i) {
		EV_EditMethod* method = pEMC->getNthEditMethod(i);
		if (method) {
			if (!(method->getType() & EV_EMT_REQUIREDATA)) {
				list.push_back( method );
			}
		}
	}
	
	// qsort them by name
	std::sort( list.begin(), list.end(), compareEditMethods );
	
	// print them
	printf("%u bindable edit methods (don't require data)\n", list.size());
	for (size_t i=0; i<list.size(); ++i) printf("%s\n", list[i]->getName());

	return true;
}

static bool SaveBindings_invoke(AV_View * /*v*/, EV_EditMethodCallData* d)
{
	// get binding set
	AP_BindingSet* pBSet = static_cast<AP_BindingSet *>(XAP_App::getApp()->getBindingSet());
	UT_return_val_if_fail(pBSet,false);
	
	// get current map
	const char* curMapName = XAP_App::getApp()->getInputMode();
	UT_return_val_if_fail(curMapName,false);
	EV_EditBindingMap* curMap = pBSet->getMap( curMapName );
	UT_return_val_if_fail(curMap,false);
	
	// get target filename
	std::string targetFilename;
	if (d->m_pData && d->m_dataLength) {
		UT_UCS4String ucs4(reinterpret_cast<const UT_UCS4Char *>(d->m_pData),d->m_dataLength);
		targetFilename = ucs4.utf8_str();
	} else {
		targetFilename = XAP_App::getApp()->getUserPrivateDirectory();
		targetFilename += '/';
		targetFilename += "keybindings-";
		targetFilename += curMapName;
		targetFilename += "-";
		targetFilename += UT_UTF8String_sprintf( "%u", time(0) ).utf8_str();
		targetFilename += ".xml";
	}
	UT_DEBUGMSG(("Saving current keybindings %s to %s\n", curMapName, targetFilename.c_str()));

	// open file
	GsfOutput* file;
	file = UT_go_file_create( targetFilename.c_str(), NULL );
	if (!file) {
		const char* URI = UT_go_filename_to_uri(targetFilename.c_str());
		file = UT_go_file_create( URI, NULL );
		FREEP(URI);
	}
	UT_return_val_if_fail(file,false);
	
	// generate file
	std::string contents;
	contents += "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
	contents += UT_UTF8String_sprintf( 
		"<editbindings name=\"%s\" mode=\"replace\" "
		"xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
		"xsi:noNamespaceSchemaLocation=\"http://www.abisource.com/abiword-keybindings-1.0.xsd\">\n",
		curMapName ).utf8_str();
	std::map<EV_EditBits,const char*> map;
	curMap->getAll( map );
	std::set<EV_EditBits> doneMouseContexts;
	
	// walk in reverse order, so mousecontexts come first
	// this is because our XSD demands mousecontexts come before keystrokes
	for (std::map<EV_EditBits,const char*>::reverse_iterator it=map.rbegin(); it!=map.rend(); ++it) {
		// prepare general stuff
		EV_EditBits bits = (*it).first;
		const char* methodName = (*it).second;
		std::string modAtt;
		if (bits & EV_EMS_CONTROL) modAtt += "control=\"true\"";
		if (bits & EV_EMS_SHIFT) {
			if (modAtt.size()) modAtt += " ";	// yes, I'm anal
			modAtt += " shift=\"true\"";
		}
		if (bits & EV_EMS_ALT) {
			if (modAtt.size()) modAtt += " ";
			modAtt += " alt=\"true\"";
		}
		
		// if it's a mouse context
		if (EV_IsMouse(bits)) {
			// have we seen this one yet in another run?
			if (doneMouseContexts.find( bits & ~EV_EMO__MASK__ )!=doneMouseContexts.end()) continue;
			doneMouseContexts.insert( bits & ~EV_EMO__MASK__ );
			
			// get button
			int button = 0;
			switch (bits & EV_EMB__MASK__) {
				case EV_EMB_BUTTON0: button = 0; break;
				case EV_EMB_BUTTON1: button = 1; break;
				case EV_EMB_BUTTON2: button = 2; break;
				case EV_EMB_BUTTON3: button = 3; break;
				case EV_EMB_BUTTON4: button = 4; break;
				case EV_EMB_BUTTON5: button = 5; break;
				default:
					UT_DEBUGMSG(("XXX: bits=0x%x, EMB=%x\n", bits, bits & EV_EMB__MASK__));
					UT_ASSERT(UT_SHOULD_NOT_HAPPEN); 
					break;
			}
			// get context
			std::string context;
			switch (bits & EV_EMC__MASK__) {
				case EV_EMC_UNKNOWN: 		context = "EV_EMC_UNKNOWN"; break;
				case EV_EMC_TEXT:			context = "EV_EMC_TEXT"; break;
				case EV_EMC_LEFTOFTEXT:		context = "EV_EMC_LEFTOFTEXT"; break;
				case EV_EMC_MISSPELLEDTEXT:	context = "EV_EMC_MISSPELLEDTEXT"; break;
				case EV_EMC_IMAGE:			context = "EV_EMC_IMAGE"; break;
				case EV_EMC_IMAGESIZE:		context = "EV_EMC_IMAGESIZE"; break;
				case EV_EMC_FIELD:			context = "EV_EMC_FIELD"; break;
				case EV_EMC_HYPERLINK:		context = "EV_EMC_HYPERLINK"; break;
				case EV_EMC_RIGHTOFTEXT:	context = "EV_EMC_RIGHTOFTEXT"; break;
				case EV_EMC_REVISION:		context = "EV_EMC_REVISION"; break;
				case EV_EMC_VLINE:			context = "EV_EMC_VLINE"; break;
				case EV_EMC_HLINE:			context = "EV_EMC_HLINE"; break;
				case EV_EMC_FRAME:			context = "EV_EMC_FRAME"; break;
				case EV_EMC_VISUALTEXTDRAG:	context = "EV_EMC_VISUALTEXTDRAG"; break;
				case EV_EMC_TOPCELL:		context = "EV_EMC_TOPCELL"; break;
				case EV_EMC_TOC:			context = "EV_EMC_TOC"; break;
				case EV_EMC_POSOBJECT:		context = "EV_EMC_POSOBJECT"; break;
				case EV_EMC_MATH:			context = "EV_EMC_MATH"; break;
				case EV_EMC_EMBED:			context = "EV_EMC_EMBED"; break;
				default:
					UT_DEBUGMSG(("XXX: bits=0x%x, EMC=%x\n", bits, bits & EV_EMC__MASK__));
					UT_ASSERT(UT_SHOULD_NOT_HAPPEN); 
					break;
			}
			contents += UT_UTF8String_sprintf(
					"\t<mousecontext context=\"%s\" button=\"%d\"%s%s>\n",
					/* context */ context.c_str(), 
					/* button */ button,
					/* modifiers */ modAtt.size()?" ":"", modAtt.c_str()).utf8_str();
			size_t seen = 0;
			for (EV_EditMouseOp op=EV_EMO_SINGLECLICK; op<=EV_EMO_DOUBLERELEASE; op+=EV_EMO_SINGLECLICK) {
				std::map<EV_EditBits,const char*>::iterator it2 = map.find( (bits & ~EV_EMO__MASK__)|op );
				if (it2!=map.end()) {
					
					// get type
					std::string type;
					switch (op) {
						case EV_EMO_SINGLECLICK:	type = "click"; break;
						case EV_EMO_DOUBLECLICK:	type = "doubleclick"; break;
						case EV_EMO_DRAG:			type = "drag"; break;
						case EV_EMO_DOUBLEDRAG:		type = "doubledrag"; break;
						case EV_EMO_RELEASE:		type = "release"; break;
						case EV_EMO_DOUBLERELEASE:	type = "doublerelease"; break;
						default:					
							UT_DEBUGMSG(("XXX: bits=0x%x, EMO=%x\n", bits, bits & EV_EMO__MASK__));
							UT_ASSERT(UT_SHOULD_NOT_HAPPEN); 
							break;
						
					}
					// add to contents
					contents += UT_UTF8String_sprintf(
						"\t\t<operation type=\"%s\" handler=\"%s\"/>\n",
						/* type */ type.c_str(),
						/* handler */ (*it2).second ).utf8_str();
					++seen;
				}
			}
			UT_ASSERT(seen>0);
			contents += "\t</mousecontext>\n";
		}
		// if it's a key
		else if (EV_IsKeyboard(bits)) {
			std::string key;
			if (bits & EV_EKP_NAMEDKEY) {
				const char* keyName = EV_NamedVirtualKey::getName( bits & 0xFF );
				if (!keyName) continue; // whoopsie?
				key = keyName;
			} else {
				key = UT_UTF8String_sprintf("0x%x", bits & 0xFF).utf8_str();
			}
			contents += UT_UTF8String_sprintf("\t<keystroke key=\"%s\" handler=\"%s\"%s%s/>\n",
				key.c_str(), methodName, modAtt.size()?" ":"", modAtt.c_str() ).utf8_str();
		}
		// whuh?
		else {
			UT_ASSERT(UT_SHOULD_NOT_HAPPEN);
		}
	}
	contents += "</editbindings>\n";
	
	// save file
	gsf_output_write(file, contents.size(), reinterpret_cast<const guint8*>( contents.c_str() ));
	gsf_output_close(file);
	g_object_unref(G_OBJECT(file));
	
	// done!
	return true;
}

// -----------------------------------------------------------------------
// 		LoadBindings class
// -----------------------------------------------------------------------

LoadBindings::LoadBindings (EV_EditMethodCallData * d, _FROM_URI )
: m_pApp( XAP_App::getApp() )
, m_pXMLDoc( NULL ) 
, m_bReplace( false ) 
{
	UT_UCS4String ucs4(reinterpret_cast<const UT_UCS4Char *>(d->m_pData),d->m_dataLength);
	const char* input = ucs4.utf8_str();
	
	// check for regular filename
	struct stat buf;
	GsfInput* in = NULL;
	if (stat(input, &buf)==0) {
		m_pXMLDoc = xmlReadFile( input, NULL, XML_PARSE_NOBLANKS );
	} 
	// check for URI
	else if ( (in = UT_go_file_open(input, NULL)) ) {
		size_t fileSize = gsf_input_size(in);
		guint8 const* contents = gsf_input_read(in, fileSize, NULL);
		if (contents) {
			m_pXMLDoc = xmlReadMemory( reinterpret_cast<const char*>(contents), fileSize, "", NULL, XML_PARSE_NOBLANKS );
		}
		g_object_unref(G_OBJECT(in));
	}
}

LoadBindings::LoadBindings (EV_EditMethodCallData * d, _FROM_MEMORY )
: m_pApp( XAP_App::getApp() )
, m_pXMLDoc( NULL ) 
, m_bReplace( false ) 
{
	UT_UCS4String ucs4(reinterpret_cast<const UT_UCS4Char *>(d->m_pData),d->m_dataLength);
	const char* input = ucs4.utf8_str();
	m_pXMLDoc = xmlReadMemory( input, strlen(input), "", NULL, XML_PARSE_NOBLANKS ); 
}

LoadBindings::~LoadBindings() 
{
	if (m_pXMLDoc) {
		xmlFreeDoc( m_pXMLDoc );
		m_pXMLDoc = NULL;
	}
}

bool LoadBindings::Load() 
{
	// check input
	if (!m_pXMLDoc) {
		ReportError( "XML file failed to load" );
		return false;
	}
	
	// find the document node
	xmlNodePtr node = (xmlNodePtr)m_pXMLDoc;
	while (node && node->type!=XML_DOCUMENT_NODE) node = node->next;
	if (!node) {
		ReportError("couldn't find document node");
		return false;
	}
	
	// first child should be element node named editbindings
	node = node->children;
	if (!node || node->type!=XML_ELEMENT_NODE || strcmp( node->name, "editbindings" )) {
		ReportError("expected editbindings node");
		return false;
	}
	
	// get mode and name
	const char* nameStr = FindAttribute( node, "name" );
	if (!nameStr) {
		ReportError("editbinding missing mandatory name attribute");
		return false;
	}
	m_sName = nameStr;
	const char* modeStr = FindAttribute( node, "mode" );
	if (!modeStr) {
		ReportError("editbinding missing mandatory mode attribute");
	}
	if (!strcmp( modeStr, "replace" )) {
		m_bReplace = true;
	} 
	else if (!strcmp( modeStr, "append" )) {
		m_bReplace = false;
	}
	else {
		ReportError("invalid mode attribute value %s, expected replace or append", modeStr);
		return false;
	}
	
	// parse mouse and keystrokes
	node = node->children;
	while (node) {
		// bool get keyboard modifiers (alt/control/shift)
		EV_EditModifierState modifiers = GetModifiers( node );

		// check type
		if (!strcmp( node->name, "mousecontext" )) {
			// get mouse context
			EV_EditMouseContext context = 0;
			const char* contextStr = FindAttribute( node, "context" );
			if (!contextStr) {
				ReportError("mousecontext requires a context attribute");
				return false;
			}
			if (!strcmp("EV_EMC_UNKNOWN", contextStr)) context = EV_EMC_UNKNOWN;
			else if (!strcmp("EV_EMC_TEXT", contextStr)) context = EV_EMC_TEXT;
			else if (!strcmp("EV_EMC_LEFTOFTEXT", contextStr)) context = EV_EMC_LEFTOFTEXT;
			else if (!strcmp("EV_EMC_MISSPELLEDTEXT", contextStr)) context = EV_EMC_MISSPELLEDTEXT;
			else if (!strcmp("EV_EMC_IMAGE", contextStr)) context = EV_EMC_IMAGE;
			else if (!strcmp("EV_EMC_IMAGESIZE", contextStr)) context = EV_EMC_IMAGESIZE;
			else if (!strcmp("EV_EMC_FIELD", contextStr)) context = EV_EMC_FIELD;
			else if (!strcmp("EV_EMC_HYPERLINK", contextStr)) context = EV_EMC_HYPERLINK;
			else if (!strcmp("EV_EMC_RIGHTOFTEXT", contextStr)) context = EV_EMC_RIGHTOFTEXT;
			else if (!strcmp("EV_EMC_REVISION", contextStr)) context = EV_EMC_REVISION;
			else if (!strcmp("EV_EMC_VLINE", contextStr)) context = EV_EMC_VLINE;
			else if (!strcmp("EV_EMC_HLINE", contextStr)) context = EV_EMC_HLINE;
			else if (!strcmp("EV_EMC_FRAME", contextStr)) context = EV_EMC_FRAME;
			else if (!strcmp("EV_EMC_VISUALTEXTDRAG", contextStr)) context = EV_EMC_VISUALTEXTDRAG;
			else if (!strcmp("EV_EMC_TOPCELL", contextStr)) context = EV_EMC_TOPCELL;
			else if (!strcmp("EV_EMC_TOC", contextStr)) context = EV_EMC_TOC;
			else if (!strcmp("EV_EMC_POSOBJECT", contextStr)) context = EV_EMC_POSOBJECT;
			else if (!strcmp("EV_EMC_MATH", contextStr)) context = EV_EMC_MATH;
			else if (!strcmp("EV_EMC_EMBED", contextStr)) context = EV_EMC_EMBED;
			else {
				ReportError("unrecognized context attribute value %s in mousecontext element", contextStr);
				return false;
			}
			// get mouse button
			EV_EditMouseButton button = 0;
			const char* buttonStr = FindAttribute( node, "button" );
			if (buttonStr) {
				switch (atoi( buttonStr )) {
					case 0: break;
					case 1: button = EV_EMB_BUTTON1; break;
					case 2: button = EV_EMB_BUTTON2; break;
					case 3: button = EV_EMB_BUTTON3; break;
					case 4: button = EV_EMB_BUTTON4; break;
					case 5: button = EV_EMB_BUTTON5; break;
					default:
						ReportError("button value must be 0<=x<=5, found %d", atoi(buttonStr));
						return false;
				}
			}
			if (!button) button = EV_EMB_BUTTON0;
			// now find handlers for the various operations
			xmlNodePtr operationNode = node->children;
			while (operationNode) {
				// get operation
				if (strcmp( operationNode->name, "operation" )) {
					ReportError("mousecontext element may only have operation sub elements, found %s element", operationNode->name);
					return false;
				}
				const char* opType = FindAttribute( operationNode, "type" );
				if (!opType) {
					ReportError("operation element missing type attribute");
					return false;
				}
				EV_EditMouseOp op = 0;
				if (!strcmp( opType, "click" )) op = EV_EMO_SINGLECLICK;
				else if (!strcmp( opType, "doubleclick" )) op = EV_EMO_DOUBLECLICK;
				else if (!strcmp( opType, "drag" )) op = EV_EMO_DRAG;
				else if (!strcmp( opType, "doubledrag" )) op = EV_EMO_DOUBLEDRAG;
				else if (!strcmp( opType, "release" )) op = EV_EMO_RELEASE;
				else if (!strcmp( opType, "doublerelease" )) op = EV_EMO_DOUBLERELEASE;
				else {
					ReportError("unrecognized type attribute value %s in operation element",opType);
					return false;
				}
				
				// get command to invoke
				const char* command = FindAttribute( operationNode, "handler" );
				if (!command) {
					ReportError("operation element missing handler attribute");
					return false;
				}
				
				// save to internal map
				if (!AddMapping( op | button | context | modifiers, command )) return false;
				
				// next
				operationNode = operationNode->next;
			}
		}
		else if (!strcmp( node->name, "keystroke" )) {
			const char* handlerStr = FindAttribute( node, "handler" );
			if (!handlerStr) {
				ReportError("keystroke element missing mandatory handler attribute");
				return false;
			}
			const char* keyStr = FindAttribute( node, "key" );
			if (!keyStr) {
				ReportError("keystroke element missing mandatory key attribute");
				return false;
			}
			if (!strncmp( keyStr, "0x", 2 )) {
				// handle key press
				EV_EditKeyPress key;
				sscanf(keyStr,"%x",&key);
				if (!AddMapping( modifiers | key | EV_EKP_PRESS, handlerStr )) return false;
			} 
			else {
				// handle named virtual key
				EV_EditBits vkey = EV_NamedVirtualKey::getEB( keyStr );
				if (!vkey) {
					ReportError("unrecognized named virtual key %s", keyStr);
					return false;
				}
				if (!AddMapping( modifiers | vkey | EV_EKP_NAMEDKEY, handlerStr )) return false;
			}
		}
		else if (!strcmp( node->name, "unbind-mappings" )) {
			const char* handlerStr = FindAttribute( node, "handler" );
			if (!handlerStr) {
				ReportError("unbind-mappings element missing mandatory handler attribute");
				return false;
			}
			UT_uint8 unbind = 0;
			const char* keystrokesStr = FindAttribute( node, "keystrokes" );
			if (keystrokesStr && !strcmp( keystrokesStr, "true" )) {
				unbind |= DONT_UNBIND_KEYSTROKES;
			}
			const char* mousecontextsStr = FindAttribute( node, "mousecontexts" );
			if (mousecontextsStr && !strcmp( mousecontextsStr, "true" )) {
				unbind |= DONT_UNBIND_MOUSECONTEXTS;
			}
			if ((unbind & DONT_UNBIND_ANYTHING)==DONT_UNBIND_ANYTHING) {
				ReportError("got unbind-mappings for handler %s that unbinds nothing", handlerStr);
				return false;
			}
			RemoveMapping( handlerStr, unbind );
		}
		else {
			ReportError("unrecognized element %s, expecting 'mousecontext', 'keystroke' or 'unbind-mappings'", node->name);
			return false;
		}
		// next sibling
		node = node->next;
	}
	return true;
}

EV_EditModifierState LoadBindings::GetModifiers( xmlNodePtr node ) 
{
	EV_EditModifierState mod = 0;
	_xmlAttr* prop = node->properties;
	while (prop) {
		if (prop->name && prop->children && prop->children->content) {
			if (!strcmp( prop->name, "control" )) {
				if (!strcmp( prop->children->content, "true" )) {
					mod |= EV_EMS_CONTROL;
				}
			}
			else if (!strcmp( prop->name, "alt" )) {
				if (!strcmp( prop->children->content, "true" )) {
					mod |= EV_EMS_ALT;
				}
			}
			else if (!strcmp( prop->name, "shift" )) {
				if (!strcmp( prop->children->content, "true" )) {
					mod |= EV_EMS_SHIFT;
				}
			}
		}
		prop = prop->next;
	}
	return mod;
}

const char* LoadBindings::FindAttribute( xmlNodePtr node, const char* name ) 
{
	_xmlAttr* prop = node->properties;
	while (prop) {
		if (prop->name && prop->children) {
			if (!strcmp( prop->name, name )) {
				return reinterpret_cast<const char*>(prop->children->content);
			}
		}
		prop = prop->next;
	}
	return NULL;
}

bool LoadBindings::AddMapping( UT_uint32 binding, const char* command ) 
{
	UT_DEBUGMSG(("[LoadBindings] Adding 0x%08x: '%s'\n", binding, command));
	if (!m_BindMap.insert( BindingMap::value_type( binding, command ) ).second) {
		ReportError("overlapping mappings detected for binding 0x%x (see command %s)", binding, command);
		return false;
	}
	return true;
}

bool LoadBindings::RemoveMapping( const char* command, UT_uint8 unbinding ) 
{
	UT_DEBUGMSG(("[LoadBindings] Removing 0x%02x: '%s'\n", unbinding, command));
	if (!m_UnbindMap.insert( UnbindMap::value_type( command, unbinding) ).second) {
		ReportWarning("duplicate unbind-mappings detected for command %s", command);
	}
	return true;
}

void LoadBindings::ReportError( const char* format, ... ) const 
{
	fprintf( stderr, "[LoadBindings] Error: " );
	va_list args;
	va_start(args, format);
	vfprintf( stderr, format, args );
	va_end(args);
	fprintf( stderr, "\n" );
}

void LoadBindings::ReportWarning( const char* format, ... ) const 
{
	fprintf( stderr, "[LoadBindings] Warning: " );
	va_list args;
	va_start(args, format);
	vfprintf( stderr, format, args );
	va_end(args);
	fprintf( stderr, "\n" );
}

bool LoadBindings::Set() const 
{
	AP_BindingSet* pBSet = static_cast<AP_BindingSet *>(m_pApp->getBindingSet());
	UT_return_val_if_fail(pBSet,false);
	
	EV_EditBindingMap* map;
	if (m_bReplace) {
		// we're replacing a map, if it doesn't exist yet
		// this doesn't matter
		map = pBSet->getMap(m_sName.c_str());
		if (!map) {
			map = pBSet->createMap(m_sName.c_str());
			UT_return_val_if_fail(map,false);
		} else {
			// reset map!
			map->resetAll();
		}
	}
	else {
		// we're appending to an existing map, if it doesn't exist
		// this is a failure!
		map = pBSet->getMap(m_sName.c_str());
		UT_return_val_if_fail(map,false);
	}
	
	// set bindings!
	for (BindingMap::const_iterator it=m_BindMap.begin(); it!=m_BindMap.end(); ++it) {
		// first remove current command for the editbits
		// we MUST do this, or setBinding might fail!
		map->removeBinding( static_cast<EV_EditBits>( (*it).first ) );
		// then set our new command
		if (!map->setBinding( static_cast<EV_EditBits>( (*it).first ), (*it).second.c_str() )) {
			ReportWarning( "Failed to set binding for EV 0x%x handler %s", (*it).first, (*it).second.c_str() );
		}
	}
	// remove bindings
	for (UnbindMap::const_iterator it=m_UnbindMap.begin(); it!=m_UnbindMap.end(); ++it) {
		std::vector<EV_EditBits> editBits;
		map->findEditBits( (*it).first.c_str(), editBits );
		for (size_t i=0; i<editBits.size(); ++i) {
			if (EV_IsMouse( editBits[i] )) {
				if (!((*it).second & DONT_UNBIND_MOUSECONTEXTS)) {
					if (!map->removeBinding( editBits[i] )) {
						ReportWarning( "Failed to remove binding for EV 0x%x handler %s", editBits[i], (*it).first.c_str() );
					}
				}
			}
			else if (EV_IsKeyboard( editBits[i] )) {
				if (!((*it).second & DONT_UNBIND_KEYSTROKES)) {
					if (!map->removeBinding( editBits[i] )) {
						ReportWarning( "Failed to remove binding for EV 0x%x handler %s", editBits[i], (*it).first.c_str() );
					}
				}
			}
			else {
				if (!map->removeBinding( editBits[i] )) {
					ReportWarning( "Failed to remove binding for EV 0x%x handler %s", editBits[i], (*it).first.c_str() );
				}
			}
		}
	}
	return m_pApp->setInputMode( m_sName.c_str(), true /* force update! */ )>=0;
}

// -----------------------------------------------------------------------
//      EV_NamedVirtualKey - moved from Abi to here, unused in Abi
// -----------------------------------------------------------------------

static const char * s_Abi_NVKTable[] =
{	"",				// must be at index zero
	"backspace",
	"space",
	"tab",
	"return",
	"escape",
	"pageup",
	"pagedown",
	"end",
	"home",
	"left",
	"up",
	"right",
	"down",
	"insert",
	"delete",
	"help",
	"f1",
	"f2",
	"f3",
	"f4",
	"f5",
	"f6",
	"f7",
	"f8",
	"f9",
	"f10",
	"f11",
	"f12",
	"f13",
	"f14",
	"f15",
	"f16",
	"f17",
	"f18",
	"f19",
	"f20",
	"f21",
	"f22",
	"f23",
	"f24",
	"f25",
	"f26",
	"f27",
	"f28",
	"f29",
	"f30",
	"f31",
	"f32",
	"f33",
	"f34",
	"f35",
	"DeadGrave",
	"DeadAcute",
	"DeadCircumflex",
	"DeadTilde",
	"DeadMacron",
	"DeadBreve",
	"DeadAboveDot",
	"DeadDiaeresis",
	"DeadDoubleAcute",
	"DeadCaron",
	"DeadCedilla",
	"DeadOgonek",
	"DeadIota",
	"MenuShortCut"
	// TODO as other items are added to ev_NamedVirtualKey, add items here.
};

const char * EV_NamedVirtualKey::getName(EV_EditBits eb)
{
	UT_ASSERT((g_ascii_strcasecmp(s_Abi_NVKTable[EV_NVK_F35&~EV_EKP_NAMEDKEY],"f35")==0));
	UT_ASSERT((G_N_ELEMENTS(s_Abi_NVKTable) == EV_COUNT_NVK));

	EV_EditVirtualKey evk = eb & ~EV_EKP_NAMEDKEY;
	if (evk < G_N_ELEMENTS(s_Abi_NVKTable))
		return s_Abi_NVKTable[evk];
	return 0;
}

EV_EditBits EV_NamedVirtualKey::getEB(const char * szName)
{
	UT_ASSERT((g_ascii_strcasecmp(s_Abi_NVKTable[EV_NVK_F35&~EV_EKP_NAMEDKEY],"f35")==0));
	UT_ASSERT((G_N_ELEMENTS(s_Abi_NVKTable) == EV_COUNT_NVK));

	for (UT_uint32 k=1; k<G_N_ELEMENTS(s_Abi_NVKTable); k++)
		if (g_ascii_strcasecmp(s_Abi_NVKTable[k],szName)==0)
			return EV_NamedKey(k);
	return 0;
}
