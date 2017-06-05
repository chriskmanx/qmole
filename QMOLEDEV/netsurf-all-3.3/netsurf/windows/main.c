/*
 * Copyright 2011 Vincent Sanders <vince@simtec.co.uk>
 *
 * This file is part of NetSurf, http://www.netsurf-browser.org/
 *
 * NetSurf is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License.
 *
 * NetSurf is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "utils/config.h"

#include <limits.h>
#include <stdbool.h>
#include <windows.h>

#include "utils/utils.h"
#include "utils/log.h"
#include "utils/messages.h"
#include "utils/filepath.h"
#include "utils/file.h"
#include "utils/nsurl.h"
#include "utils/nsoption.h"
#include "desktop/browser.h"
#include "desktop/gui_fetch.h"
#include "desktop/netsurf.h"

#include "windows/findfile.h"
#include "windows/drawable.h"
#include "windows/download.h"
#include "windows/localhistory.h"
#include "windows/gui.h"

static char **respaths; /** resource search path vector. */

char *options_file_location;

/**
 * Cause an abnormal program termination.
 *
 * \note This never returns and is intended to terminate without any cleanup.
 *
 * \param error The message to display to the user.
 */
static void die(const char *error)
{
	exit(1);
}

static nsurl *gui_get_resource_url(const char *path)
{
	char buf[PATH_MAX];
	nsurl *url = NULL;

	netsurf_path_to_nsurl(filepath_sfind(respaths, buf, path), &url);

	return url;
}

/**
 * Ensures output logging stream is available
 */
static bool nslog_ensure(FILE *fptr)
{
	/* mwindows compile flag normally invalidates standard io unless
	 *  already redirected 
	 */
	if (_get_osfhandle(fileno(fptr)) == -1) {
		AllocConsole();
		freopen("CONOUT$", "w", fptr);
	}
	return true;
}

/**
 * Set option defaults for windows frontend
 *
 * @param defaults The option table to update.
 * @return error status.
 */
static nserror set_defaults(struct nsoption_s *defaults)
{
	/* Set defaults for absent option strings */

	/* ensure homepage option has a default */
	nsoption_setnull_charp(homepage_url, strdup(NETSURF_HOMEPAGE));

	return NSERROR_OK;
}



/**
 * Entry point from operating system
 **/
int WINAPI
WinMain(HINSTANCE hInstance, HINSTANCE hLastInstance, LPSTR lpcli, int ncmd)
{
	char **argv = NULL;
	int argc = 0, argctemp = 0;
	size_t len;
	LPWSTR *argvw;
	char *messages;
	nserror ret;
	const char *addr;
	nsurl *url;
	struct netsurf_table win32_table = {
		.browser = win32_browser_table,
		.window = win32_window_table,
		.clipboard = win32_clipboard_table,
		.download = win32_download_table,
		.fetch = win32_fetch_table,
		.file = win32_file_table,
		.utf8 = win32_utf8_table,
	};
	win32_fetch_table->get_resource_url = gui_get_resource_url;

	ret = netsurf_register(&win32_table);
	if (ret != NSERROR_OK) {
		die("NetSurf operation table registration failed");
	}

	if (SLEN(lpcli) > 0) {
		argvw = CommandLineToArgvW(GetCommandLineW(), &argc);
	}

	setbuf(stderr, NULL);

	/* Construct a unix style argc/argv */
	argv = malloc(sizeof(char *) * argc);
	while (argctemp < argc) {
		len = wcstombs(NULL, argvw[argctemp], 0) + 1;
		if (len > 0) {
			argv[argctemp] = malloc(len);
		}

		if (argv[argctemp] != NULL) {
			wcstombs(argv[argctemp], argvw[argctemp], len);
			/* alter windows-style forward slash flags to
			 * hyphen flags.
			 */
			if (argv[argctemp][0] == '/')
				argv[argctemp][0] = '-';
		}
		argctemp++;
	}

	respaths = nsws_init_resource("${APPDATA}\\NetSurf:${HOME}\\.netsurf:${NETSURFRES}:${PROGRAMFILES}\\NetSurf\\NetSurf\\:"NETSURF_WINDOWS_RESPATH);


	options_file_location = filepath_find(respaths, "preferences");

	/* initialise logging - not fatal if it fails but not much we
	 * can do about it 
	 */
	nslog_init(nslog_ensure, &argc, argv);

	/* user options setup */
	ret = nsoption_init(set_defaults, &nsoptions, &nsoptions_default);
	if (ret != NSERROR_OK) {
		die("Options failed to initialise");
	}
	nsoption_read(options_file_location, NULL);
	nsoption_commandline(&argc, argv, NULL);

	/* common initialisation */
	messages = filepath_find(respaths, "messages");
	ret = netsurf_init(messages, NULL);
	free(messages);
	if (ret != NSERROR_OK) {
		free(options_file_location);
		LOG(("NetSurf failed to initialise"));
		return 1;
	}

	ret = nsws_create_main_class(hInstance);
	ret = nsws_create_drawable_class(hInstance);
	ret = nsws_create_localhistory_class(hInstance);

	nsoption_set_bool(target_blank, false);

	nsws_window_init_pointers(hInstance);

	/* If there is a url specified on the command line use it */
	if (argc > 1) {
		addr = argv[1];
	} else if (nsoption_charp(homepage_url) != NULL) {
		addr = nsoption_charp(homepage_url);
	} else {
		addr = NETSURF_HOMEPAGE;
	}

	LOG(("calling browser_window_create"));

	ret = nsurl_create(addr, &url);
	if (ret == NSERROR_OK) {
		ret = browser_window_create(BW_CREATE_HISTORY,
					      url,
					      NULL,
					      NULL,
					      NULL);
		nsurl_unref(url);

	}
	if (ret != NSERROR_OK) {
		warn_user(messages_get_errorcode(ret), 0);
	} else {
		win32_run();
	}

	netsurf_exit();

	free(options_file_location);

	return 0;
}
