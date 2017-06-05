/*
 * AbiCommand - Abiword plugin for a command line interface
 * Copyright (C) 2002 by Martin Sevior
 * Copyright (C) 2005 by Dom Lachowicz
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

/*!
 * \todo build fails when abiword built without printing support.
 */

#ifdef ABI_PLUGIN_BUILTIN
#define abi_plugin_register abipgn_abicommand_register
#define abi_plugin_unregister abipgn_abicommand_unregister
#define abi_plugin_supports_version abipgn_abicommand_supports_version
#endif

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <sys/types.h>
#include <errno.h>

#include <glib.h>

#include "xap_Module.h"
#include "xap_App.h"
#include "xap_Frame.h"
#include "fv_View.h"
#include "fl_DocLayout.h"
#include "ev_EditMethod.h"
#include "ie_imp.h"
#include "ie_exp.h"
#include "ie_types.h"
#include "ap_Convert.h"
#include "ap_UnixApp.h"
#include "gr_UnixCairoGraphics.h"
#include "gr_UnixNullGraphics.h"
#include "ap_UnixFrame.h"
#include "gr_DrawArgs.h"
#include "ap_EditMethods.h"
#include "xap_DialogFactory.h"
#include "xap_Dlg_Print.h"
#include "ap_Dialog_Id.h"

#include "AbiCommand.h"

ABI_PLUGIN_DECLARE (AbiCommand)

#define RES_TO_STATUS(a) ((a) ? 0 : -1)

static bool AbiCommand_invoke (AV_View * v, EV_EditMethodCallData * d);

//
// AbiCommand_registerMethod()
// -----------------------
//   Adds AbiCommand_invoke to the EditMethod list
//
static void
AbiCommand_registerMethod ()
{
	// First we need to get a pointer to the application itself.
	XAP_App *pApp = XAP_App::getApp ();

	// Create an EditMethod that will link our method's name with
	// it's callback function.  This is used to link the name to 
	// the callback.
	EV_EditMethod *myEditMethod = new EV_EditMethod ("AbiCommand_invoke",	// name of callback function
							 AbiCommand_invoke,	// callback function itself.
							 0,	// no additional data required.
							 ""	// description -- allegedly never used for anything
							 );

	// Now we need to get the EditMethod container for the application.
	// This holds a series of Edit Methods and links names to callbacks.
	EV_EditMethodContainer *pEMC = pApp->getEditMethodContainer ();

	// We have to add our EditMethod to the application's EditMethodList
	// so that the application will know what callback to call when a call

	pEMC->addEditMethod (myEditMethod);
}

static void
AbiCommand_RemoveFromMethods ()
{
	// First we need to get a pointer to the application itself.
	XAP_App *pApp = XAP_App::getApp ();

	// remove the edit method
	EV_EditMethodContainer *pEMC = pApp->getEditMethodContainer ();
	EV_EditMethod *pEM = ev_EditMethod_lookup ("AbiCommand_invoke");

	pEMC->removeEditMethod (pEM);
	DELETEP (pEM);
}

// -----------------------------------------------------------------------
//
//      Abiword Plugin Interface 
//
// -----------------------------------------------------------------------

ABI_FAR_CALL int
abi_plugin_register (XAP_ModuleInfo * mi)
{
	mi->name = "AbiCommand";
	mi->desc = "This is a command line interface to AbiWord";
	mi->version = ABI_VERSION_STRING;
	mi->author = "Martin Sevior <msevior@physics.unimelb.edu.au>";
	mi->usage = "AbiCommand_invoke";

	AbiCommand_registerMethod ();
	return 1;
}

ABI_FAR_CALL int
abi_plugin_unregister (XAP_ModuleInfo * mi)
{
	mi->name = 0;
	mi->desc = 0;
	mi->version = 0;
	mi->author = 0;
	mi->usage = 0;

	AbiCommand_RemoveFromMethods ();

	return 1;
}

ABI_FAR_CALL int
abi_plugin_supports_version (UT_uint32 /*major*/, UT_uint32 /*minor*/, UT_uint32 /*release*/)
{
	return 1;
}

// -----------------------------------------------------------------------
//
//     AbiCommand Invocation Code
//
// -----------------------------------------------------------------------

//
// AbiCommand_invoke
// -------------------
//   This is the function that we actually call to make command line 
//   interface.
//
static bool
AbiCommand_invoke (AV_View * /*v*/, EV_EditMethodCallData * /*d*/)
{
	AbiCommand myCommand;

	myCommand.doCommands ();

	return true;
}

AbiCommand::AbiCommand (void) :
	m_pCurDoc (NULL),
	m_pCurFile (new UT_UTF8String),
	m_pCurFrame (NULL),
	m_pCurView (NULL),
	m_pG (NULL),
	m_pLayout (NULL),
	m_bViewDoc (false), 
	m_bRunAsServer (false), 
	m_iPID (0), 
	m_bRunAsAbiCollab(false),
	m_sErrorFile ("")
{
	m_pApp = XAP_App::getApp ();
	m_pApp->getGraphicsFactory()->registerAsDefault(GRID_UNIX_NULL,true);
}


AbiCommand::AbiCommand (bool bAbiCollab) :
	m_pCurDoc (NULL),
	m_pCurFile (new UT_UTF8String),
	m_pCurFrame (NULL),
	m_pCurView (NULL),
	m_pG (NULL),
	m_pLayout (NULL),
	m_bViewDoc (false), 
	m_bRunAsServer (false), 
	m_iPID (0), 
	m_bRunAsAbiCollab(bAbiCollab),
	m_sErrorFile ("")
{
	m_pApp = XAP_App::getApp ();
	m_pApp->getGraphicsFactory()->registerAsDefault(GRID_UNIX_NULL,true);
}

AbiCommand::~AbiCommand (void)
{
	deleteCurrentDoc ();
	DELETEP (m_pCurFile);
}

void
AbiCommand::deleteCurrentDoc (void)
{
	//
	// Delete the current view, frame and document.
	//
	bool bUnref = (m_pCurFrame == NULL);

	if (m_pCurFrame != NULL)
		m_pApp->forgetFrame (m_pCurFrame);

	//
	// Deleting the frame also deletes the layout, view and graphics classes
	//
	DELETEP (m_pCurFrame);
	if (bUnref)
	{
		UNREFP (m_pCurDoc);
	}

	m_pCurView = NULL;
	m_pG = NULL;
	m_pLayout = NULL;
	m_pCurView = NULL;
}

void
AbiCommand::doCommands (void)
{
	bool bQuit = false;

	printf ("AbiWord command line plugin: Type \"quit\" to exit \n");
	while (!bQuit)
	{
		//
		// Read a line
		//
		char *pCom = readline ("AbiWord:> ");

		// Quit on EOF
		if (!pCom)
			break;

		//
		// break it into tokens
		//
		UT_GenericVector<const UT_UTF8String*> toks;

		tokenizeString (toks, pCom);
		if (toks.getItemCount () > 0)
		{
			const UT_UTF8String *pTok =
				static_cast < const UT_UTF8String * >(toks.getNthItem (0));

			if (pTok && (strcmp (pTok->utf8_str (), "quit") == 0
				|| strcmp (pTok->utf8_str (), "q") == 0))
				bQuit = true;
			else
			{
				UT_sint32 bres = parseTokens (&toks);

				if (bres == 0)
 					printf ("OK\n");
				else
				{
					if (m_bRunAsServer)
					{
						FILE *errF = fopen (m_sErrorFile.utf8_str (), "a");
						if (errF) {
							fprintf (errF, "Error in command \"%s\" number %d \n",
								 	pCom, bres);
							fclose (errF);
						} else
							printf("Failed to open error log: %s", strerror(errno));
					}

					printf ("error %d \n", bres);
				}
			}
		}

		clearTokenVector (toks);
		FREEP (pCom);
	}
}

/*!
 * Break the string into tokens. Handles quotes and double quotes.
\params char * pStr String from readline
\params UT_GenericVector<const UT_UTF8String*> & reference to the vector we'll fill with UT_UTF8String * pointers.
 */
bool
AbiCommand::tokenizeString (UT_GenericVector<const UT_UTF8String*> & tok, char *pStr)
{
	int _argc = 0;
	char **_argv = NULL;

	if (g_shell_parse_argv (pStr, &_argc, &_argv, NULL))
	{
		for (int i = 0; i < _argc; i++)
		{
			const UT_UTF8String *pTok = new UT_UTF8String (_argv[i]);
			tok.addItem (pTok);
		}

		g_strfreev (_argv);

		return true;
	}

	return false;
}

/*!
 * clear the token vector pointed to by pvecToks
 */
void
AbiCommand::clearTokenVector (UT_GenericVector<const UT_UTF8String*> & vecToks)
{
	UT_sint32 i = 0;

	for (i = 0; i < vecToks.getItemCount (); i++)
	{
		const UT_UTF8String *pComm = vecToks.getNthItem (i);
		delete pComm;
	}

	vecToks.clear ();
}

//
// parse the UT_UTF8String * tokens within the vector pToks.
// returns 0 on success; -1 otherwise
//
UT_sint32
AbiCommand::parseTokens (UT_GenericVector<const UT_UTF8String*> * pToks)
{
	UT_sint32 count = pToks->getItemCount ();
	UT_sint32 i = 0;

	if (count == 0)
		return -1;

	const UT_UTF8String *pCom0 = pToks->getNthItem (0);

	//
	// New document
	//
	if (strcmp (pCom0->utf8_str (), "new") == 0)
	{
		printf ("Attempting to create a new document \n");
		PD_Document *pDoc = new PD_Document ();

		UT_Error error = pDoc->newDocument ();

		if (error != UT_OK)
		{
			UNREFP (pDoc);
			printf ("Error creating new document error %d \n", error);
			return static_cast < UT_sint32 > (error);
		}

		replaceDocument (pDoc);
		m_pCurFile->assign ("");

		return 0;
	}

	//
	// Load in a document
	//
	if (strcmp (pCom0->utf8_str (), "load") == 0)
	{
		printf ("Attempting to load a document \n");
		if (count >= 2)
		{
			const UT_UTF8String *pCom1 = pToks->getNthItem (1);
			PD_Document *pDoc = new PD_Document ();

			UT_Error error = pDoc->readFromFile (pCom1->utf8_str (), IEFT_Unknown);

			if (error != UT_OK)
			{
				UNREFP (pDoc);
				printf ("Error loading %s error %d \n", pCom1->utf8_str (),
					error);
				return static_cast < UT_sint32 > (error);
			}
			replaceDocument (pDoc);
			m_pCurFile->assign (pCom1->utf8_str ());
			return 0;
		}
	}

	//
	// printfile
	//
	else if (strcmp (pCom0->utf8_str (), "printfile") == 0)
	{
		if (count >= 2)
		{
			if (printFiles (pToks))
			    return 0;

			return -1;
		}
	}

	//
	// inserttext
	//
	else if (strcmp (pCom0->utf8_str (), "inserttext") == 0)
	{
		if (count >= 2)
		{
			if (insertText (pToks))
			    return 0;

			return -1;
		}
	}

	//
	// delete
	//
	else if (strcmp (pCom0->utf8_str (), "delete") == 0)
	{
		if (deleteText (pToks))
			return 0;

		return -1;
	}

	//
	// Replace Next
	//
	else if (strcmp (pCom0->utf8_str (), "replacenext") == 0)
	{
		if (count > 2)
			return RES_TO_STATUS (replaceNext (pToks));
	}

	//
	// replaceAll
	//
	else if (strcmp (pCom0->utf8_str (), "replaceall") == 0)
	{
		if (count > 2)
			return RES_TO_STATUS (replaceAll (pToks));
	}

	//
	// Move point to somewhere
	//
	else if (strcmp (pCom0->utf8_str (), "movept") == 0)
	{
		if (count > 1)
			return RES_TO_STATUS (movePoint (pToks));
	}

	//
	// Open a graphical window on the document
	//
	else if (strcmp (pCom0->utf8_str (), "visualedit") == 0)
	{
		return RES_TO_STATUS (viewDoc ());
	}

	//
	// Start selection
	//
	else if (strcmp (pCom0->utf8_str (), "selectstart") == 0)
	{
		if (m_pCurView)
		{
			PT_DocPosition pos = m_pCurView->getPoint ();

			static_cast < FV_View * >(m_pCurView)->cmdSelect (pos, pos);

			return 0;
		}
		else
			return -1;
	}

	//
	// Clear selection
	//
	else if (strcmp (pCom0->utf8_str (), "selectclear") == 0)
	{
		if (m_pCurView)
		{
			m_pCurView->cmdUnselectSelection ();

			return 0;
		}
		else
			return -1;
	}

	//
	// findnext
	//
	else if (strcmp (pCom0->utf8_str (), "findnext") == 0)
	{
		if (m_pCurView && (pToks->getItemCount () > 1))
		{
			bool bEOD = false;
			const UT_UTF8String *pFind = pToks->getNthItem (1);
			const UT_UCSChar *pUCSFind =
				reinterpret_cast < UT_UCSChar * >(UT_calloc (pFind->size () + 1, sizeof (UT_UCSChar)));
			static_cast < FV_View * >(m_pCurView)->findSetMatchCase (true);
			static_cast < FV_View * >(m_pCurView)->findNext (pUCSFind, bEOD);
			FREEP (pUCSFind);

			if (!bEOD)
				return 0;

			return -1;
		}
		else
			return -1;
	}

	//
	// Save
	//
	else if (strcmp (pCom0->utf8_str (), "save") == 0)
	{
		if (m_pCurDoc)
		{
			IEFileType ieft = 0;

			if (pToks->getItemCount () > 1)
			{
				const UT_UTF8String *pCom1 = pToks->getNthItem (1);
				printf(" Filename %s \n",pCom1->utf8_str());
				const char *suffix = rindex (pCom1->utf8_str (), '.');

				if (suffix != NULL)
				{
					ieft = IE_Exp::fileTypeForSuffix (suffix);
					printf ("Doing file export as %d for %s \n", ieft,
							pCom1->utf8_str ());
				}

				else
				{
					ieft =
						static_cast < IEFileType >
						(m_pCurDoc->getLastOpenedType ());
				}

				static_cast<AD_Document*>(m_pCurDoc)->saveAs (pCom1->utf8_str (), ieft);

				return 0;
			}

			ieft = static_cast < IEFileType > (m_pCurDoc->getLastOpenedType ());
			static_cast<AD_Document*>(m_pCurDoc)->saveAs (m_pCurFile->utf8_str (), ieft);

			return 0;
		}

		return -1;
	}

	//
	// Convert to Text
	//
	else if (strcmp (pCom0->utf8_str (), "converttotext") == 0 ||
			 strcmp (pCom0->utf8_str (), "convert") == 0)
	{
		AP_Convert APConvert;
		UT_UTF8String src;
		UT_UTF8String dest;
		IEFileType ieft;

		if (pToks->getItemCount () < 2)
			return -1;

		// input filename is pToks[1]
		src = *pToks->getNthItem (1);
		if (pToks->getItemCount () > 2)
		{
			// destination filename is pToks[2]
			dest = *pToks->getNthItem (2);
		}
		else
		{
			dest = src;
			dest += ".txt";
		}

		if (pToks->getItemCount () > 3)
		{
			UT_UTF8String extension = ".";

			extension += *pToks->getNthItem (3);
			ieft = IE_Exp::fileTypeForSuffix (extension.utf8_str ());
		}
		else
 			ieft = IE_Exp::fileTypeForSuffix (".txt");

		if (!g_file_test (src.utf8_str (), G_FILE_TEST_EXISTS)) 
			return -1;

		APConvert.convertTo (src.utf8_str (), IEFT_Unknown, dest.utf8_str (),
							 ieft);
							 
		if (g_file_test (dest.utf8_str (), G_FILE_TEST_EXISTS))
			return 0;

		return -1;
	}

	//
	// Write PID to file
	//
	else if (strcmp (pCom0->utf8_str (), "writepid") == 0)
	{
		if (pToks->getItemCount () < 2)
			return -1;

		UT_UTF8String pidFile = *pToks->getNthItem (1);
		FILE *pidF = fopen (pidFile.utf8_str (), "wb");

		if (pidF)
		{
			fprintf (pidF, "%d", getpid ());
			fclose (pidF);
			return 0;
		}

		return -1;
	}

	//
	// server
	//      
	else if (strcmp (pCom0->utf8_str (), "server") == 0)
	{
		if (pToks->getItemCount () < 2)
			return -1;

		m_bRunAsServer = true;
		m_iPID = getpid ();
		m_sErrorFile = *pToks->getNthItem (1);

		return 0;
	}

	//
	// PNG preview
	//
	else if (strcmp (pCom0->utf8_str (), "previewpng") == 0)
	{
		UT_UTF8String src, destPNG, sWidth, sHeight;

		if (pToks->getItemCount () < 5)
			return -1;

		src = *pToks->getNthItem (1);
		destPNG = *pToks->getNthItem (2);
		sWidth = *pToks->getNthItem (3);
		sHeight = *pToks->getNthItem (4);

		UT_sint32 iWidth = atoi (sWidth.utf8_str ());
		UT_sint32 iHeight = atoi (sHeight.utf8_str ());

		if ((iWidth <= 0) || (iHeight <= 0))
			return -1;

		AP_UnixApp *pUnixApp = static_cast < AP_UnixApp * >(m_pApp);

		bool res = pUnixApp->makePngPreview (src.utf8_str (), destPNG.utf8_str (), iWidth, iHeight);

		return RES_TO_STATUS (res);
	}

	//
	// Help
	//
	else if (strcmp (pCom0->utf8_str (), "help") == 0)
	{
		printf ("Currently implemented commands are:\n");
		printf ("help                        - Prints this message\n");
		printf ("quit                        - Exits the program\n");
		printf ("new                         - Create a new empty document.\n");
		printf ("load <filename>             - Load <filename> replacing the current document.\n");
		printf ("printfile <filename1> <filename2> <...> - Print the current document into the\n");
		printf ("                              filenames listed.\n");
		printf ("replaceall <find> <target>  - Replace every occurrence of <find> with <target>\n");
		printf ("                              in the current document.\n");
		printf ("replacenext <find> <target> - Replace the next occurrence of <find> with <target>\n");
		printf ("                              in the current document.\n");
		printf ("inserttext <target>         - Insert <target> at the current point in the\n");
		printf ("                              document.\n");
		printf ("delete <args>               - Delete <args> characters at the current point\n");
		printf ("                              in the document.\n");
		printf ("replacenext <find> <target> - Replace the next occurrence of <find> with <target>\n");
		printf ("                              in the current document.\n");
		printf ("movept <arg>                - Move the current point to another location in\n");
		printf ("                              the current document.\n");
		printf ("                              Options for arg are: BOD,EOD,BOP,EOP,BOS,EOS,\n");
		printf ("                              BOL,EOL,BOW,+num,-num,num\n");
		printf ("selectstart                 - Start a selection at the current point\n");
		printf ("selectclear                 - Clear the current selection.\n");
		printf ("findnext <target>           - Find the next occurrence of target and select it.\n");
		printf ("save <filename>             - Save the current document.\n");
		printf ("                              If filename is omitted the file is saved to its\n");
		printf ("                              original name.\n");
		printf ("                              Otherwise the extension of the filename is used\n");
		printf ("                              to determine the format of the file.\n");
		printf ("converttotext <src> <dest>  - Convert the file given in <src> to the plain text\n");
		printf ("                              file named <dest>.\n");
		printf ("convert <src> <dest> <type> - Convert the file given in <src> to the file named\n");
		printf ("                              <dest>. The type of conversion is given by the\n");
		printf ("                              third parameter (abw,html,odt, etc.).\n");
		printf ("writepid <file>             - Write the PID of this process to the file <file>\n");
		printf ("server <error file>         - This is being run as remote process. Write an\n");
		printf ("                              error file on error.\n");
		printf ("previewpng <document> <preview.png> <width> <height> - Create a PNG preview of\n");
		printf ("                              <document> with name <preview.png> of <width>\n");
		printf ("                              pixels wide and <height> pixels in height.\n");
		printf ("visualedit                  - Popup a visual window and edit the file or just\n");
		printf ("                              preview what you've done.\n");
		printf ("                              Close the window when finished.\n");

		return 0;
	}

	else
	{
		if (ev_EditMethod_exists (pCom0->utf8_str ()))
		{
			UT_UTF8String calldata;

			for (i = 1; i < count; i++)
			{
				const UT_UTF8String *pComm = pToks->getNthItem (i);
				calldata += *pComm;
			}

			printf ("EditMethod %s exists. Calling with %s\n",
					pCom0->utf8_str (), calldata.utf8_str ());
			if (ev_EditMethod_invoke
				(pCom0->utf8_str (), calldata.utf8_str ()))
				return 0;

			return -1;
		}
		else
			printf ("EditMethod %s does not exist.\n", pCom0->utf8_str ());
	}

	return -1;
}

/*!
 * Load the document identified by the path sPathToDoc into Abiword
 */
bool AbiCommand::loadDocument(UT_UTF8String & sPathToDoc)
{
  //
  // Load in a document
  //

      PD_Document *pDoc = new PD_Document ();
  
      UT_Error error = pDoc->readFromFile (sPathToDoc.utf8_str (), IEFT_Unknown);

      if (error != UT_OK)
      {
           UNREFP (pDoc);
	   printf ("Error loading %s error %d \n", sPathToDoc.utf8_str (),error);
	   return false;
      }
      replaceDocument (pDoc);
      m_pCurFile->assign (sPathToDoc.utf8_str ());
      return true;
}

/*!
 * Create a new Document
 */
bool AbiCommand::newDocument(void)
{
	//
	// New document
	//

  PD_Document *pDoc = new PD_Document ();

  UT_Error error = pDoc->newDocument ();
  
  if (error != UT_OK)
  {
      UNREFP (pDoc);
      printf ("Error creating new document error %d \n", error);
      return false;;
  }

  replaceDocument (pDoc);
  m_pCurFile->assign ("");

  return true;
}


/*!
 * Return a pointer to the current document
 */
PD_Document * AbiCommand::getCurrentDocument(void)
{
  return m_pCurDoc;
}

//
// This method calls the method defined in ap_EditMethod.cpp via it's
// name with the current nullgraphics view as the controlling view.
//
bool
AbiCommand::invoke (const char *pszCommand)
{
	const EV_EditMethod *pEM =
		m_pApp->getEditMethodContainer ()->findEditMethodByName (pszCommand);

	if (pEM == NULL)
		return false;

	return pEM->Fn (m_pCurView, static_cast < EV_EditMethodCallData * >(NULL));
}

//
// Viewdoc. Popup an abiword window on the current document.
//
bool
AbiCommand::viewDoc (void)
{
	m_bViewDoc = true;	
	invoke ("newWindow");

	while (m_pCurFrame && m_pCurFrame->getViewNumber () > 0)
		nullUpdate ();

	return true;
}

//
// Move the insertion point to various places.
//
bool
AbiCommand::movePoint (const UT_GenericVector<const UT_UTF8String*> * pToks)
{
	if (m_pCurView != NULL)
	{
		const UT_UTF8String *pTarget = pToks->getNthItem (1);
		FV_DocPos docpos = FV_DOCPOS_BOB;
		bool bRelMove = false;
		bool bAbsMove = false;
		UT_sint32 amt = 0;

		if (g_ascii_strcasecmp (pTarget->utf8_str (), "BOD") == 0)
			docpos = FV_DOCPOS_BOD;
		else if (g_ascii_strcasecmp (pTarget->utf8_str (), "EOD") == 0)
			docpos = FV_DOCPOS_EOD;
		else if (g_ascii_strcasecmp (pTarget->utf8_str (), "BOB") == 0)
			docpos = FV_DOCPOS_BOB;
		else if (g_ascii_strcasecmp (pTarget->utf8_str (), "EOB") == 0)
			docpos = FV_DOCPOS_EOB;
		else if (g_ascii_strcasecmp (pTarget->utf8_str (), "BOP") == 0)
			docpos = FV_DOCPOS_BOP;
		else if (g_ascii_strcasecmp (pTarget->utf8_str (), "EOP") == 0)
			docpos = FV_DOCPOS_EOP;
		else if (g_ascii_strcasecmp (pTarget->utf8_str (), "BOL") == 0)
 			docpos = FV_DOCPOS_BOL;
		else if (g_ascii_strcasecmp (pTarget->utf8_str (), "EOL") == 0)
			docpos = FV_DOCPOS_EOL;
		else if (g_ascii_strcasecmp (pTarget->utf8_str (), "BOS") == 0)
			docpos = FV_DOCPOS_BOS;
		else if (g_ascii_strcasecmp (pTarget->utf8_str (), "EOS") == 0)
			docpos = FV_DOCPOS_EOS;
		else if (g_ascii_strcasecmp (pTarget->utf8_str (), "BOW") == 0)
			docpos = FV_DOCPOS_BOW;
		else if (*(pTarget->utf8_str ()) == '+'
				 || *(pTarget->utf8_str ()) == '-')
		{
			bRelMove = true;
			amt = atoi (pTarget->utf8_str ());
		}
		else if (atoi (pTarget->utf8_str ()) != 0)
		{
			bAbsMove = true;
			amt = atoi (pTarget->utf8_str ());
		}
		else
			return false;

		if (bRelMove && amt != 0)
		{
			bool bForward = (amt > 0);

			static_cast < FV_View * >(m_pCurView)->cmdCharMotion (bForward, amt);
			return true;
		}

		if (bAbsMove && amt != 0)
		{
			PT_DocPosition posBOD;
			PT_DocPosition posEOD;
			PT_DocPosition pos = static_cast < PT_DocPosition > (amt);

			static_cast < FV_View * >(m_pCurView)->getEditableBounds (true, posEOD);
			static_cast < FV_View * >(m_pCurView)->getEditableBounds (false, posBOD);

			if (amt >= static_cast<UT_sint32>(posBOD) && amt <= static_cast<UT_sint32>(posEOD))
				static_cast < FV_View * >(m_pCurView)->setPoint (pos);
			else
				return false;
		}
		else if (amt < 0)
			return false;

		static_cast < FV_View * >(m_pCurView)->moveInsPtTo (docpos);

		return true;
	}

	return false;
}

//
// Replace every instance of the string in token 1 with the string in
// token 2
//
bool
AbiCommand::replaceAll (const UT_GenericVector<const UT_UTF8String*> * pToks)
{
	if (m_pCurView != NULL)
	{
		const UT_UTF8String *pFind = pToks->getNthItem (1);
		const UT_UTF8String *pReplace = pToks->getNthItem (2);
		UT_UCSChar *pUCSFind =
			reinterpret_cast < UT_UCSChar * >(UT_calloc (pFind->size () + 1, sizeof (UT_UCSChar)));
		UT_UCSChar *pUCSReplace =	reinterpret_cast < UT_UCSChar *	>(UT_calloc (pReplace->size () + 1, sizeof (UT_UCSChar)));

		UT_UCS4_strcpy_char (pUCSFind, pFind->utf8_str ());
		UT_UCS4_strcpy_char (pUCSReplace, pReplace->utf8_str ());
		static_cast < FV_View * >(m_pCurView)->findSetStartAtInsPoint ();
		static_cast < FV_View * >(m_pCurView)->findSetFindString (pUCSFind);
		static_cast < FV_View * >(m_pCurView)->findSetReplaceString (pUCSReplace);
		static_cast < FV_View * >(m_pCurView)->findSetMatchCase (true);
		static_cast < FV_View * >(m_pCurView)->findReplaceAll ();
		FREEP (pUCSFind);
		FREEP (pUCSReplace);

		return true;
	}

	return false;
}

//
// Insert the text on the command line into the document at the current
// Point.
//
bool
AbiCommand::insertText (const UT_GenericVector<const UT_UTF8String*> * pToks)
{
	if (m_pCurView != NULL && pToks->getItemCount () > 1)
	{
		const UT_UTF8String *pText = pToks->getNthItem (1);
		UT_UCS4Char *pUCSText =
			static_cast < UT_UCS4Char * >(UT_calloc (pText->size () + 1, sizeof (UT_UCS4Char)));
		UT_UCS4_strcpy_char (pUCSText, pText->utf8_str ());
		static_cast < FV_View * >(m_pCurView)->cmdCharInsert (pUCSText, pText->size ());
		FREEP (pUCSText);

		return true;
	}

	return false;
}

//
// Delete the text at the current point according to the argument on the 
// command line.
//
bool
AbiCommand::deleteText (const UT_GenericVector<const UT_UTF8String*> * pToks)
{
	if ((m_pCurView != NULL) && (pToks->getItemCount() > 1))
	{
		const UT_UTF8String *pCom1 = pToks->getNthItem (1);
		UT_sint32 count = atoi (pCom1->utf8_str ());

		static_cast < FV_View * >(m_pCurView)->cmdCharDelete ((count > 0), count);

		return true;
	}
	return false;
}

//
// Replace the next instance of the string in token 1 with the string in
// token 2
//
bool
AbiCommand::replaceNext (const UT_GenericVector<const UT_UTF8String*> * pToks)
{
	if (m_pCurView != NULL)
	{
		const UT_UTF8String *pFind = pToks->getNthItem (1);
		const UT_UTF8String *pReplace = pToks->getNthItem (2);
		UT_UCSChar *pUCSFind =
			reinterpret_cast < UT_UCSChar *	>(UT_calloc (pFind->size () + 1, sizeof (UT_UCSChar)));
		UT_UCSChar *pUCSReplace =
			reinterpret_cast < UT_UCSChar *	>(UT_calloc (pReplace->size () + 1, sizeof (UT_UCSChar)));
		bool bEOD = false;

		UT_UCS4_strcpy_char (pUCSFind, pFind->utf8_str ());
		UT_UCS4_strcpy_char (pUCSReplace, pReplace->utf8_str ());

		static_cast < FV_View * >(m_pCurView)->findSetFindString (pUCSFind);
		static_cast < FV_View * >(m_pCurView)->findSetReplaceString (pUCSReplace);
		static_cast < FV_View * >(m_pCurView)->findSetMatchCase (true);
		static_cast < FV_View * >(m_pCurView)->findReplace (bEOD);
		FREEP (pUCSFind);
		FREEP (pUCSReplace);

		return (!bEOD);
	}

	return false;
}

//
// Print the current documents to the files listed on the command line
//
bool
AbiCommand::printFiles (const UT_GenericVector<const UT_UTF8String*> * pToks)
{
	UT_return_val_if_fail(m_pCurDoc, false);
	XAP_DialogFactory * pDialogFactory
		= static_cast<XAP_DialogFactory *>(m_pCurFrame->getDialogFactory());

	XAP_Dialog_Print * pDialog
		= static_cast<XAP_Dialog_Print *>(pDialogFactory->requestDialog(XAP_DIALOG_ID_PRINT));
	pDialog->setPreview(false);
	for (UT_sint32 i = 1; i < pToks->getItemCount (); i++)
	{
		const UT_UTF8String *pPrinter = pToks->getNthItem (i);
		
		// pPrinter is a printer name, and "-" is our special name for the default printer.

		if(strcmp(pPrinter->utf8_str(), "-") != 0) 
		{
		     pDialog->PrintDirectly(m_pCurFrame, NULL, NULL);

		}
		else
		{
		     pDialog->PrintDirectly(m_pCurFrame, pPrinter->utf8_str(), NULL);
		}
		GR_Graphics * pGraphics = pDialog->getPrinterGraphicsContext();
		pDialog->releasePrinterGraphicsContext(pGraphics);
	}
	pDialogFactory->releaseDialog(pDialog);
	return true;
}

void
AbiCommand::nullUpdate (void)
{
	if (m_bViewDoc)
	{
		UT_uint32 i = 0;

		for (i = 0; i < 5; i++)
			gtk_main_iteration ();
	}
}

//
// Doc loaded OK, delete the old stuff, put in the new stuff
//
bool
AbiCommand::replaceDocument (PD_Document * pDoc)
{
	//
	// Delete the current document.
	//
	deleteCurrentDoc ();

	//
	// Put the new document in place.
	//
	m_pCurDoc = pDoc;

	m_pCurFrame = new AP_UnixFrame();
	UT_UTF8String extension (".bak~");

	m_pCurFrame->setAutoSaveFileExt (extension.utf8_str ());
	GR_UnixNullGraphicsAllocInfo ai;

	m_pG = (UnixNull_Graphics *) m_pApp->newGraphics (ai);

	m_pLayout = new FL_DocLayout (m_pCurDoc, static_cast < GR_Graphics * >(m_pG));
	m_pCurView = new FV_View (m_pApp, m_pCurFrame, m_pLayout);
	m_pCurFrame->setView (static_cast < AV_View * >(m_pCurView));
	m_pCurFrame->setDoc (static_cast < AD_Document * >(m_pCurDoc));
	m_pLayout->fillLayouts ();
	static_cast < FV_View * >(m_pCurView)->setPoint (2);

	return true;
}

