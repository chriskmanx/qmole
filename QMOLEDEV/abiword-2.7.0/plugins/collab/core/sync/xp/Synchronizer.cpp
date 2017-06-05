/* AbiCollab - Code to enable the modification of remote documents.
 * Copyright (C) 2007,2008 by Marc Maurer <uwog@uwog.net>
 * Copyright (C) 2007 by Ryan Pavlik <abiryan@ryand.net>
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

#include "Synchronizer.h"
// Danger Will Robinson: This file is icky to look at.
// Massive comment blocks used to aid reading in syntax-highlighting editors.

#include <ut_debugmsg.h>

#ifndef WIN32
static gboolean s_glib_mainloop_callback(GIOChannel * /*channel*/, GIOCondition /*condition*/, Synchronizer* synchronizer)
{
	synchronizer->callMainloop();
	return TRUE;
}
#endif

//////////////////
// WINDOWS STATIC MAGIC
//////////////////

#ifdef WIN32
#ifndef HWND_MESSAGE
#define HWND_MESSAGE ((HWND)(-3))
#endif

#define SYNC_CLASSNAME "AbiCollabSynchronizer"

int Synchronizer::sm_iMessageWindows = 0;
ATOM sm_iClass = 0;

LRESULT CALLBACK Synchronizer::s_wndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam) // Win32-only
{
	Synchronizer * pThis;
	int swlresult;
	switch (msg)
	{
	
	case WM_NCCREATE:
		pThis = (Synchronizer *)((LPCREATESTRUCT(lParam))->lpCreateParams);
		UT_return_val_if_fail(pThis, 0);
		pThis->m_hWnd = hWnd;
		SetLastError(0);
		swlresult=SetWindowLong(hWnd,GWL_USERDATA,long(pThis));
		if (swlresult==0)
		{
			// we might have an error
			int errorcode=GetLastError();
			if (errorcode)
			{
				UT_DEBUGMSG(("Error in setting the WindowLong GWL_USERDATA: %d\n", errorcode));
			}
			
		}
		return 1;
		
	case WM_ABI_SYNCHRONIZER:
		UT_DEBUGMSG(("Received a message in Synchronizer message loop! 0x%x\n", msg));
		pThis = (Synchronizer *)GetWindowLong(hWnd,GWL_USERDATA);
		UT_return_val_if_fail(pThis, 0);
		pThis->callMainloop();
		return 1;

	default:
		UT_DEBUGMSG(("return DefWindowProc for message 0x%x\n", msg));
		// We do not want to handle this message so pass back to Windows
		// to handle it in a default way
		return 0;
	}
}

void Synchronizer::_registerWndClass() // Win32-only
{
 	if (sm_iClass)
 	{
 		UT_DEBUGMSG(("Skipping window class registration\n"));
 		return;
 	}
  
 	AbiCollabSessionManager * pSessionManager = AbiCollabSessionManager::getManager();
 	UT_return_if_fail(pSessionManager);
  
 	HINSTANCE hInstance = pSessionManager->getInstance();
 	UT_return_if_fail(hInstance);
  
 	WNDCLASS wc;
 	wc.style = CS_HREDRAW | CS_VREDRAW;
 	wc.lpfnWndProc = Synchronizer::s_wndProc;
 	wc.cbClsExtra = 0;
 	wc.cbWndExtra = 0;
 	wc.hInstance = hInstance;
 	wc.hIcon = NULL;
 	wc.hCursor = NULL;
 	wc.hbrBackground = NULL;
 	wc.lpszMenuName =  NULL;
 	wc.lpszClassName = SYNC_CLASSNAME;
  
 	sm_iClass = RegisterClass(&wc);
 	UT_return_if_fail(sm_iClass);
 	
 	sm_iMessageWindows = 0;
}

void Synchronizer::_unregisterWndClass() // Win32-only
{
 	UT_DEBUGMSG(("Synchronizer::_unregisterWndClass()\n"));
 	UT_return_if_fail(sm_iClass);
 	
 	if (sm_iMessageWindows > 0)
  	{
 		UT_DEBUGMSG(("%d message windows still exist, skipping unregistering\n", sm_iMessageWindows));
 		return;
  	}
 	
 	AbiCollabSessionManager * pManager = AbiCollabSessionManager::getManager();
 	UT_return_if_fail(pManager);
 
 	HINSTANCE hInstance = pManager->getInstance();
 	UT_return_if_fail(hInstance);
 
 	UT_DEBUGMSG(("Unregistrating message window class\n"));
 	UT_return_if_fail(UnregisterClass(SYNC_CLASSNAME, hInstance));
 	sm_iClass = 0;
}

#endif

//////////////////
// CONSTRUCTORS
//////////////////

#ifdef WIN32
Synchronizer::Synchronizer(boost::function<void ()>  signalhandler) // Win32 Implementation
	: m_signalhandler(signalhandler),
 	m_hWnd(0)
{
	UT_DEBUGMSG(("Synchronizer()\n"));
	AbiCollabSessionManager * pSessionManager = AbiCollabSessionManager::getManager();
	UT_return_if_fail(pSessionManager);

 	HINSTANCE hInstance = pSessionManager->getInstance();
 	UT_return_if_fail(hInstance);

	_registerWndClass();
	
	// HWND_MESSAGE as parent HWND is Win2k/xp/vista only - replaced with 0
	// (also HWND_MESSAGE doesn't compile in MinGW, weird bug.  --RP 8 August 2007)
	
	m_hWnd = CreateWindow(SYNC_CLASSNAME,
			"AbiCollab",
			0,
			CW_USEDEFAULT,
			SW_HIDE,
			CW_USEDEFAULT,
			CW_USEDEFAULT,
			HWND_MESSAGE,
			NULL,
			hInstance,
			(void *) this
		);
	UT_DEBUGMSG(("Created message window: HWND 0x%x\n", m_hWnd));
	switch ((int)m_hWnd)
	{
		case NULL:
			UT_DEBUGMSG(("Win32 error: %d.\n", GetLastError()));
			break;
		default:
			sm_iMessageWindows++;
			break;
			// ok!
	};
}

#else

Synchronizer::Synchronizer(boost::function<void ()> signalhandler) // Unix Implementation
	: m_signalhandler(signalhandler)
{
	UT_DEBUGMSG(("Synchronizer()\n"));
	// on unix, we use the self-pipe trick to signal the glib main loop
	fdr = -1;
	fdw = -1;

	int pfd[2];
	if (pipe(pfd) == -1)
	{
		UT_DEBUGMSG(("pipe error!\n"));
		// FIXME: throw an exception here, don't just bail out!
		exit(EXIT_FAILURE);
	}
	else
	{
		fdr = pfd[0];
		fdw = pfd[1];

		// setup the glib main loop integration
		io_channel = g_io_channel_unix_new(fdr);
		io_channel_watch_id = g_io_add_watch(io_channel, G_IO_IN, (GIOFunc)s_glib_mainloop_callback, this);
	}
}
#endif

// end constructors
//////////////////

//////////////////
// DESTRUCTORS
//////////////////

#ifdef WIN32
Synchronizer::~Synchronizer() // Win32 Implementation
{
	UT_DEBUGMSG(("~Synchronizer()\n"));
	// destroy our window
	if (m_hWnd)
	{
		DestroyWindow(m_hWnd);
		m_hWnd = 0;
		sm_iMessageWindows--;
	}
	else
	{
		UT_DEBUGMSG(("AbiCollab Synchronizer Window already destroyed!\n"));
	}
	
	// Attempt to unregister class - it will check to make sure we're the last one out.
	_unregisterWndClass();
}

#else

Synchronizer::~Synchronizer() // Unix Implementation
{
	UT_DEBUGMSG(("~Synchronizer()\n"));
	if (fdr != -1)
		close(fdr);
	if (fdw != -1)
		close(fdw);

	g_source_remove(io_channel_watch_id);
	//g_io_channel_shutdown(io_channel, TRUE, NULL);
	g_io_channel_unref(io_channel);
	io_channel_watch_id = 0;
}
#endif
// end destructors
//////////////////

//////////////////
// SIGNAL
//////////////////
#ifdef WIN32
void Synchronizer::signal() // Win32 Implementation
{
	UT_DEBUGMSG(("Synchronizer::signal()\n"));
	// send a message to the main loop
	int result = PostMessage(m_hWnd, WM_ABI_SYNCHRONIZER, 0, 0);
	UT_ASSERT(result != 0);
}

#else

void Synchronizer::signal() // Unix Implementation
{
	UT_DEBUGMSG(("Signalling the main loop\n"));
	unsigned char _signal = 0xff;
	if (write(fdw, &_signal, 1) != 1)
	{
		UT_DEBUGMSG(("Error signaling main loop!\n"));
	}
}
#endif



//////////////////
// CONSUME
//////////////////
void Synchronizer::_consume()
{
#ifdef WIN32
	// void on win32
#else
	// Unix Implementation
	char _signal = 0;
	if (read(fdr, &_signal, 1) != 1)
	{
		UT_DEBUGMSG(("Error signaling main loop!\n"));
	}
#endif
}
