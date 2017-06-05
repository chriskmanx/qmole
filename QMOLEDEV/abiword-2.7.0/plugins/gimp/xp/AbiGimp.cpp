/*
 * AbiGimp - Abiword plugin for the GIMP (or other) Image Editors
 * Copyright (C) 2002 by Martin Sevior
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

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
#include "fg_Graphic.h"
#include "ie_imp.h"
#include "ie_impGraphic.h"
#include "ie_exp.h"
#include "ie_types.h"
#include "ut_sleep.h"
#include <sys/types.h>  
#include <sys/stat.h>
#include <unistd.h>
#ifdef WIN32
#include <windows.h>
#else
#include <sys/wait.h>
#include <signal.h>
#include "ut_files.h"
#endif

#ifdef ABI_PLUGIN_BUILTIN
#define abi_plugin_register abipgn_gimp_register
#define abi_plugin_unregister abipgn_gimp_unregister
#define abi_plugin_supports_version abipgn_gimp_supports_version
// dll exports break static linking
#define ABI_BUILTIN_FAR_CALL extern "C"
#else
#define ABI_BUILTIN_FAR_CALL ABI_FAR_CALL
ABI_PLUGIN_DECLARE("Gimp")
#endif

static bool AbiGimp_invoke(AV_View* v, EV_EditMethodCallData *d);

static const char* AbiGimp_MenuLabel = "&Edit Image via GIMP";
static const char* AbiGimp_MenuTooltip = "Opens the selected image in the GIMP for editing.";

#ifdef WIN32
static BOOL CreateChildProcess(char * appName, char *cmdline,
				PROCESS_INFORMATION *procInfo,
				STARTUPINFO *startInfo);
#endif

//
// AbiGimp_addToMenus
// -----------------------
//   Adds "Edit Image" option to AbiWord's Context Menu.
//
static void
AbiGimp_addToMenus()
{
    // First we need to get a pointer to the application itself.
    XAP_App *pApp = XAP_App::getApp();

    
    // Create an EditMethod that will link our method's name with
    // it's callback function.  This is used to link the name to 
    // the callback.
    EV_EditMethod *myEditMethod = new EV_EditMethod(
        "AbiGimp_invoke",  // name of callback function
        AbiGimp_invoke,    // callback function itself.
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
    XAP_Menu_Id newID = pFact->addNewMenuAfter("ContextImageT",NULL,"&Save Image As",EV_MLF_Normal);
    pFact->addNewLabel(NULL,newID,AbiGimp_MenuLabel, AbiGimp_MenuTooltip);

// Put it after Word Count in the Main menu

    pFact->addNewMenuAfter("Main",NULL,"&Word Count",EV_MLF_Normal,newID);
//
// Also put it under word Count in the main menu,
//
    // Create the Action that will be called.
    EV_Menu_Action* myAction = new EV_Menu_Action(
	newID,                     // id that the layout said we could use
	0,                      // no, we don't have a sub menu.
	1,                      // yes, we raise a dialog.
	0,                      // no, we don't have a checkbox.
	0,                      // no radio buttons for me, thank you
	"AbiGimp_invoke",  // name of callback function to call.
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
AbiGimp_RemoveFromMenus ()
{
  // First we need to get a pointer to the application itself.
  XAP_App *pApp = XAP_App::getApp();

  // remove the edit method
  EV_EditMethodContainer* pEMC = pApp->getEditMethodContainer() ;
  EV_EditMethod * pEM = ev_EditMethod_lookup ( "AbiGimp_invoke" ) ;
  pEMC->removeEditMethod ( pEM ) ;
  DELETEP( pEM ) ;

  // now remove crap from the menus
  int frameCount = pApp->getFrameCount();
  XAP_Menu_Factory * pFact = pApp->getMenuFactory();

  pFact->removeMenuItem("Main",NULL,AbiGimp_MenuLabel);
  pFact->removeMenuItem("ContextImageT",NULL,AbiGimp_MenuLabel);
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
#if !defined(TOOLKIT_WIN)
  // gimp doesn't exist, don't let the plugin get loaded
  if (!progExists("gimp"))
    return 0;
#endif

    mi->name = "AbiGimp";
    mi->desc = "Use this to edit an image with the GIMP from within AbiWord";
    mi->version = ABI_VERSION_STRING;
    mi->author = "Martin Sevior <msevior@physics.unimelb.edu.au>";
    mi->usage = "No Usage";
    
    // Add to AbiWord's menus.
    AbiGimp_addToMenus();
    
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

    AbiGimp_RemoveFromMenus ();

    return 1;
}


ABI_BUILTIN_FAR_CALL
int abi_plugin_supports_version (UT_uint32 /*major*/, UT_uint32 /*minor*/, UT_uint32 /*release*/)
{
    return 1; 
}

// -----------------------------------------------------------------------
//
//     AbiGimp Invocation Code
//
// -----------------------------------------------------------------------


//
// AbiGimp_invoke
// -------------------
//   This is the function that we actually call to invoke the Gimp.
//
bool 
AbiGimp_invoke(AV_View* /*v*/, EV_EditMethodCallData *d)
{
    // Get the current view that the user is in.
    XAP_Frame *pFrame = XAP_App::getApp()->getLastFocussedFrame();
    FV_View* pView = static_cast<FV_View*>(pFrame->getCurrentView());

//
// generate a temp file name...
//
	char *szTempFileName = NULL;
	GError *err = NULL;
	gint fp = g_file_open_tmp ("XXXXXX", &szTempFileName, &err);
	if (err) {
		g_warning ("%s", err->message);
		g_error_free (err); err = NULL;
		return FALSE;
	}
	close(fp);

	UT_String szTmp = szTempFileName;
	szTmp += ".png";
	unlink(szTempFileName);	
	g_free (szTempFileName); szTempFileName = NULL;
	
	PT_DocPosition pos = pView->saveSelectedImage(static_cast<const char *>(szTmp.c_str()));
//
// Get the initial file status.
//
	if(pos == 0)
	{
		pFrame->showMessageBox("You must select an Image before editing it", XAP_Dialog_MessageBox::b_O,XAP_Dialog_MessageBox::a_OK);
		return false;
	}

//
// Get some pointers so we can call the editMethod to lock out GUI 
// operations on AbiWord while editing the image.
//
    XAP_App *pApp = XAP_App::getApp();
    // Now we need to get the EditMethod container for the application.
    // This holds a series of Edit Methods and links names to callbacks.

    EV_EditMethodContainer* pEMC = pApp->getEditMethodContainer();
//
// OK now get the methods to lock and unlock GUI operations
//
    const EV_EditMethod * lockGUI = pEMC->findEditMethodByName("lockGUI");
    const EV_EditMethod * unlockGUI = pEMC->findEditMethodByName("unlockGUI");

//	
// Fire up the gimp...
//

#ifdef WIN32
	UT_String cmdline = "gimp \"" + szTmp + "\"";

	PROCESS_INFORMATION procInfo;
	STARTUPINFO startInfo;
	if (!CreateChildProcess(NULL, const_cast<char *>(cmdline.c_str()), &procInfo, &startInfo))
	{
		UT_String msg = "Unable to run program: ";  msg += cmdline;

		// try again, but with default install locations in 'path' env var
		char *pathEnvVar = getenv("PATH");
		if (pathEnvVar == NULL) pathEnvVar = "C:\\Winnt;C:\\Winnt\\System32";
		char *pFiles = getenv("ProgramFiles");
		if (pFiles == NULL) pFiles = "C:\\Program Files";

		UT_String newPath = "PATH=";
		newPath += pathEnvVar;
		newPath += ";";
		newPath += pFiles;
            newPath += "\\GIMP\\bin\\;";
            newPath += pFiles;
            newPath += "\\common files\\gnu;";

		putenv(newPath.c_str());

		if (!CreateChildProcess(NULL, const_cast<char *>(cmdline.c_str()), &procInfo, &startInfo))
		{
			pFrame->showMessageBox(msg.c_str(), XAP_Dialog_MessageBox::b_O,XAP_Dialog_MessageBox::a_OK);
			UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);

			procInfo.hProcess = 0;
			unlink(szTmp.c_str());
			return false;
		}
	}

	DWORD status;
#else
	char * gimpArgs[3];
	// this is pretty ugly to const_cast. No choice.
	gimpArgs[0] = const_cast<char *>("gimp");
	gimpArgs[1] = const_cast<char *>(szTmp.c_str());
	gimpArgs[2] = NULL;
	UT_sint32 pid;
	if((pid = fork())== 0)
	{
		execvp(static_cast<const char *>("gimp"),gimpArgs);
	}
	int status;
#endif
	off_t size;
	struct stat myFileStat;
	int ok = stat(szTmp.c_str(),&myFileStat);
	time_t mod_time = myFileStat.st_mtime;
	if(ok < 0)
	{
		UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
		goto Cleanup;
	}
//
// Lock out the GUI in AbiWord
//
	ev_EditMethod_invoke(lockGUI,d);
	UT_uint32 icount;

#ifndef WIN32
	while (pid != waitpid (pid, &status, WNOHANG)) 
#else
	while ((GetExitCodeProcess(procInfo.hProcess, &status))?(status == STILL_ACTIVE):0)
#endif
	{
		UT_usleep(10000); // wait 10 milliseconds
		pFrame->nullUpdate();
		icount++;
		if(icount > 10)
		{
			icount =0;
			ok = stat(szTmp.c_str(),&myFileStat);
			if(ok == 0)
			{ 
				if(myFileStat.st_mtime != mod_time)
				{
					size = myFileStat.st_size;
					UT_usleep(100000); // wait 100 milliseconds
					ok = stat(szTmp.c_str(),&myFileStat);
					while(size > 0 && size != myFileStat.st_size)
					{
						size = myFileStat.st_size;
						ok = stat(szTmp.c_str(),&myFileStat);
						UT_usleep(100000); // wait 100 milliseconds
					}
					mod_time = myFileStat.st_mtime;
//
// OK replace the current image with this.
//
					IEGraphicFileType iegft = IEGFT_Unknown;
					FG_Graphic* pFG;
		
					UT_Error errorCode;
					
					errorCode = IE_ImpGraphic::loadGraphic(szTmp.c_str(),iegft, &pFG);
					if(errorCode)
					{
						UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
						pFrame->showMessageBox("Error making pFG. Could not put image back into Abiword", XAP_Dialog_MessageBox::b_O,XAP_Dialog_MessageBox::a_OK);
						goto Cleanup;
					}
//
// UnLock the GUI in AbiWord so we can do our thing.
//
					ev_EditMethod_invoke(unlockGUI,d);
					pView->cmdUnselectSelection();
					pView->setPoint(pos);
					pView->extSelHorizontal(true, 1); // move point forward one
					errorCode = pView->cmdInsertGraphic(pFG);
					if (errorCode)
					{
						pFrame->showMessageBox("Could not put image back into Abiword", XAP_Dialog_MessageBox::b_O,XAP_Dialog_MessageBox::a_OK);
						UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
						DELETEP(pFG);
						goto Cleanup;
					}
					DELETEP(pFG);
//
// Reselect the image
//
					pView->setPoint(pos);
					pView->extSelHorizontal(true, 1); // move point forward one
//
// Lock out the GUI in AbiWord again
//
					ev_EditMethod_invoke(lockGUI,d);
				}
			}
		}
	}
//
// Now delete the tempfile
//
	ok = unlink(szTmp.c_str());
//
// UnLock the GUI in AbiWord
//
	 ev_EditMethod_invoke(unlockGUI,d);
//
// Done!
//    
    return true;
//
// Something went wrong.
//
 Cleanup: 
	ok = unlink(szTmp.c_str());
//
// UnLock the GUI in AbiWord
//
	 ev_EditMethod_invoke(unlockGUI,d);
//
// Kill the gimp.
//
#ifdef WIN32
	if (procInfo.hProcess)
		TerminateProcess(procInfo.hProcess, -1);
#else
	kill(pid,9);
#endif
	return false;
}


#ifdef WIN32
// our equivalent of fork()
static BOOL CreateChildProcess(char * appName, char *cmdline,
				PROCESS_INFORMATION *procInfo,
				STARTUPINFO *startInfo) 
{
	//initialize structures used to return info
	ZeroMemory( procInfo, sizeof(PROCESS_INFORMATION) ); 
	ZeroMemory( startInfo, sizeof(STARTUPINFO) ); 
	startInfo->cb = sizeof(STARTUPINFO); 

	// Create the child process. 
	return CreateProcess(
			appName,   // application module to execute
			cmdline,   // command line 
			NULL,      // process security attributes 
			NULL,      // primary thread security attributes 
			FALSE,     // handles not are inherited 
			0,         // creation flags 
			NULL,      // use parent's environment 
			NULL,      // use parent's current directory 
			startInfo, // STARTUPINFO pointer 
			procInfo   // receives PROCESS_INFORMATION 
	);
} 
#endif
