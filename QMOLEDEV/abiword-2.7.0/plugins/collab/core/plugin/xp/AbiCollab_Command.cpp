/*
 * AbiCollab- Code to enable the modification of remote documents.
 * Copyright (C) 2007 by One Laptop Per Child
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

#include <vector>

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>

#include <glib.h>

#include "xap_App.h"
#include "xap_Frame.h"

#ifndef WIN32
#include "ap_UnixApp.h"
#include "ap_UnixFrame.h"

#else
// any windows stuff needed here?

#endif
#include "gr_DrawArgs.h"

#ifdef ABICOLLAB_HANDLER_FAKE
#include <backends/fake/xp/FakeAccountHandler.h>
#endif
#include <session/xp/AbiCollabSessionManager.h>
#include <session/xp/DiskSessionRecorder.h>
#include "AbiCollab_Command.h"

AbiCollab_Command::AbiCollab_Command(const UT_UTF8String& argv)
	: m_argv(argv)
{
	XAP_App* pApp = XAP_App::getApp ();
	
	#ifndef WIN32
	pApp->getGraphicsFactory()->registerAsDefault(GRID_UNIX_NULL, true);
	#else
	pApp->getGraphicsFactory()->registerAsDefault(GRID_WIN32_UNISCRIBE, true);
	#endif
}


AbiCollab_Command::~AbiCollab_Command()
{
}

bool AbiCollab_Command::execute()
{
	int _argc = 0;
	char **_argv = NULL;

	if (g_shell_parse_argv (m_argv.utf8_str(), &_argc, &_argv, NULL))
	{
		if(_argc == 0)
		{
			fprintf(stderr, "Usage: abiword --plugin \"AbiWord Collaboration\" <action> [action arguments]\n");
			return false;
		}
		
		UT_UTF8String cmd = _argv[0];
		
		if (cmd == "regression")
		{
			if (_argc == 2)
			{
				UT_DEBUGMSG(("Starting regression test\n"));
				return _doCmdRegression(_argv[1]);
			}
			else
			{
				fprintf(stderr, "Usage: abiword --plugin \"AbiWord Collaboration\" regression <recorded abicollab session>\n");
				return false;
			}
		}
		else if (cmd == "debug" || cmd == "debugstep")
		{
			if (_argc == 3)
			{
				UT_DEBUGMSG(("Starting abicollab in debugging mode\n"));
				return _doCmdDebug(_argv[1], _argv[2], cmd == "debugstep");
			}
			else
			{
				fprintf(stderr, "Usage: abiword --plugin \"AbiWord Collaboration\" <debug|debugstep> <recorded abicollab server session> <recorded abicollab client session>\n");
				return false;
			}
		}
		else
		{
			fprintf(stderr, "Usage: abiword --plugin \"AbiWord Collaboration\" <action> [action arguments]\n");
			return false;
		}
	}
	
	return false;
}

bool AbiCollab_Command::_doCmdRegression(const UT_UTF8String& sSessionFile)
{
	UT_UNUSED(sSessionFile);
	UT_DEBUGMSG(("_doCmdRegression() - sSessionFile: %s\n", sSessionFile.utf8_str()));
	
#ifdef ABICOLLAB_HANDLER_FAKE
	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	UT_return_val_if_fail(pManager, false);	
	
	char *uri = UT_go_filename_to_uri(sSessionFile.utf8_str());
	
	FakeAccountHandler* pHandler = new FakeAccountHandler(uri, NULL);
	UT_return_val_if_fail(pHandler->initialize(NULL), false);
	pManager->addAccount(pHandler);
	bool res =  pHandler->process();
	pHandler->cleanup();
	pManager->destroyAccount(pHandler);

	g_free(uri);
	return res;
#else
	fprintf(stderr, "Can't run the abicollab regression test: the \"fake\" abiword backend is disabled\n");
	return false;
#endif
}

bool AbiCollab_Command::_doCmdDebug(const UT_UTF8String& sServerSessionFile, const UT_UTF8String& sClientSessionFile, bool bSingleStep)
{
	UT_UNUSED(sServerSessionFile);
	UT_UNUSED(sClientSessionFile);
	UT_DEBUGMSG(("_doCmdDebug() - sServerSessionFile: %s, sClientSessionFile: %s\n", sServerSessionFile.utf8_str(), sClientSessionFile.utf8_str()));
	
#ifdef ABICOLLAB_HANDLER_FAKE
	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	UT_return_val_if_fail(pManager, false);	
	
#ifndef WIN32
		// FIXME: this breaks on OSX
		XAP_App::getApp()->getGraphicsFactory()->registerAsDefault(GRID_UNIX_PANGO, true);
#else
		XAP_App::getApp()->getGraphicsFactory()->registerAsDefault(GRID_WIN32_UNISCRIBE, true);
#endif
	
	char *server_uri = UT_go_filename_to_uri(sServerSessionFile.utf8_str());
	char *client_uri = UT_go_filename_to_uri(sClientSessionFile.utf8_str());

	UT_DEBUGMSG(("Creating server debug frame\n"));
	UT_UTF8String sFakeServerSessionId("fake-server-session-id");
	FakeAccountHandler* pServerHandler = new FakeAccountHandler(server_uri, XAP_App::getApp()->newFrame());
	UT_return_val_if_fail(pServerHandler->initialize(&sFakeServerSessionId), false);
	pManager->addAccount(pServerHandler);

	UT_DEBUGMSG(("Creating client debug frame\n"));
	UT_UTF8String sFakeClientSessionId("fake-client-session-id");
	FakeAccountHandler* pClientHandler = new FakeAccountHandler(client_uri, XAP_App::getApp()->newFrame());
	UT_return_val_if_fail(pClientHandler->initialize(&sFakeClientSessionId), false);
	pManager->addAccount(pClientHandler);
	
	while (1)
	{
		UT_sint32 iServerLocalRev;
		UT_sint32 iServerRemoteRev;
		UT_return_val_if_fail(pServerHandler->getCurrentRev(iServerLocalRev, iServerRemoteRev), false);

		UT_sint32 iClientLocalRev;
		UT_sint32 iClientRemoteRev;
		UT_return_val_if_fail(pClientHandler->getCurrentRev(iClientLocalRev, iClientRemoteRev), false);		

		UT_UTF8String msg = UT_UTF8String_sprintf("Current state: iServerLocalRev: %d, iServerRemoteRev: %d, iClientLocalRev: %d, iClientRemoteRev: %d\nPress OK to move to the next syncpoint", 
					iServerLocalRev, iServerRemoteRev, iClientLocalRev, iClientRemoteRev);
		pServerHandler->getFrame()->showMessageBox(msg.utf8_str(), XAP_Dialog_MessageBox::b_O, XAP_Dialog_MessageBox::a_OK);
		
		if (iServerLocalRev == iClientRemoteRev && iServerRemoteRev == iClientLocalRev)
		{
			if (pServerHandler->canStep())
			{
				UT_ASSERT_HARMLESS(pServerHandler->step(iServerLocalRev));
			}
			else if (pClientHandler->canStep())
			{
				UT_ASSERT_HARMLESS(pClientHandler->step(iClientLocalRev));
			}
			else
			{
				UT_DEBUGMSG(("Neither server nor client can step; assuming we are done...\n"));
				break;
			}
			
			UT_ASSERT_HARMLESS(_syncDocs(pServerHandler, pClientHandler, bSingleStep));
		}
		else
			UT_ASSERT_HARMLESS(_syncDocs(pServerHandler, pClientHandler, bSingleStep));
	}

	pServerHandler->getFrame()->showMessageBox("Done replaying collaboration session", XAP_Dialog_MessageBox::b_O, XAP_Dialog_MessageBox::a_OK);
	
	UT_DEBUGMSG(("Cleaning up fake account handlers...\n"));
	pServerHandler->cleanup();
	pManager->destroyAccount(pServerHandler);

	pClientHandler->cleanup();
	pManager->destroyAccount(pClientHandler);

	g_free(client_uri);
	g_free(server_uri);
	return true;
#else
	UT_UNUSED(bSingleStep);
	fprintf(stderr, "Can't run the abicollab in debug mode: the \"fake\" abiword backend is disabled\n");
	return false;
#endif
}

#ifdef ABICOLLAB_HANDLER_FAKE
bool AbiCollab_Command::_syncDocs(FakeAccountHandler* pServerHandler, FakeAccountHandler* pClientHandler, bool bSingleStep)
{
	UT_DEBUGMSG(("AbiCollab_Command::_syncDocs()\n"));

	UT_return_val_if_fail(pServerHandler, false);
	UT_return_val_if_fail(pClientHandler, false);
	
	UT_sint32 iServerLocalRev;
	UT_sint32 iServerRemoteRev;
	UT_return_val_if_fail(pServerHandler->getCurrentRev(iServerLocalRev, iServerRemoteRev), false);	
	
	UT_sint32 iClientLocalRev;
	UT_sint32 iClientRemoteRev;
	UT_return_val_if_fail(pClientHandler->getCurrentRev(iClientLocalRev, iClientRemoteRev), false);
	
	UT_DEBUGMSG(("_syncDocs() - iServerLocalRev: %d, iServerRemoteRev: %d, iClientLocalRev: %d, iClientRemoteRev: %d\n", 
				iServerLocalRev, iServerRemoteRev, iClientLocalRev, iClientRemoteRev));
	
	if (iServerLocalRev > iClientRemoteRev)
	{
		UT_DEBUGMSG(("Forwarding client to remote rev %d\n", iServerLocalRev));
		pClientHandler->stepToRemoteRev(iServerLocalRev);
	}
	else if (iClientLocalRev > iServerRemoteRev)
	{
		UT_DEBUGMSG(("Forwarding server to remote rev %d\n", iClientLocalRev));
		pServerHandler->stepToRemoteRev(iClientLocalRev);
	}
	else
	{
		UT_return_val_if_fail(iServerLocalRev == iClientRemoteRev && iServerRemoteRev == iClientLocalRev, false);
		return true;
	}
	
	return bSingleStep ? true : _syncDocs(pServerHandler, pClientHandler, false);
}
#endif
