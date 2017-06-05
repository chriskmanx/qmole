/* AbiCollab - Code to enable the modification of remote documents.
 * Copyright (C) 2007-2008 by Marc Maurer <uwog@uwog.net>
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

#ifndef __SYNCHRONIZER__
#define __SYNCHRONIZER__

#include <boost/function.hpp>
#include <ut_assert.h>

class Synchronizer;

#ifdef WIN32
// Windows implementation requirements
#define WM_ABI_SYNCHRONIZER WM_USER+15
#include <windows.h>
#include <xp/AbiCollabSessionManager.h>
#else
// Unix implementation requirements
#include <unistd.h>
#include <glib.h>
#endif

class Synchronizer
{
public:
#ifdef WIN32
	// Windows-only static stuff
	static bool sm_bClassRegistered;
	static int sm_iMessageWindows;
	
	static LRESULT CALLBACK s_wndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam);
	static void _registerWndClass();
	static void _unregisterWndClass();
#endif

	// XP prototypes
	Synchronizer(boost::function<void ()> signalhandler);
	virtual ~Synchronizer();
	
	void signal();

	void callMainloop()
	{
		_consume();
		m_signalhandler();
	}

private:
	void _consume();

//////////////////
// PRIVATE DATA
//////////////////
// XP members
	boost::function<void ()> m_signalhandler;

#ifdef WIN32
	HWND m_hWnd;
#else
	int fdr;
	int fdw;
	GIOChannel* io_channel;
	guint io_channel_watch_id;
#endif
};

#endif /* __SYNCHRONIZER__ */
