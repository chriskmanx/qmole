/*
 * AbiCollab - Code to enable the modification of remote documents.
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

#include <string>
#include <string.h>

#include "ut_debugmsg.h"
#include "AbiCollab_Regression.h"

bool AbiCollab_Regression::execute()
{
	std::vector<std::string> files;
	_findRegressionFiles(files);
	
	for (std::vector<std::string>::const_iterator cit = files.begin(); cit != files.end(); cit++)
	{
	}
	return true;
}

#ifndef WIN32

#include <dirent.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

/*!
 * returns all found regression test files
 */
void AbiCollab_Regression::_findRegressionFiles(std::vector<std::string>& files)
{
	UT_DEBUGMSG(("AbiCollab_Regression::_findRegressionFiles()\n"));
	const char* targetDir = "/home/uwog/t";
	
	const char* prefix = "AbiCollabRegressionTest-";
	size_t prefixLen = strlen(prefix);
	
	struct dirent** namelist;
	int n = scandir( targetDir, &namelist, NULL, alphasort );
	UT_DEBUGMSG(("got %d files in %s\n", n, targetDir));
	for (int i=0; i<n; ++i)
	{
		UT_DEBUGMSG(("considering %s\n", namelist[i]->d_name));
		
		// construct full name so we can stat this node
		std::string fullname = targetDir;
		fullname += '/';
		fullname += namelist[i]->d_name;
		
		// stat node
		struct stat details;
		if (stat( fullname.c_str(), &details ) == 0)
		{
			// check if it is a file
			if (!(S_ISDIR(details.st_mode)))
			{
				// if it is a session file
				if (strncmp( namelist[i]->d_name, prefix, prefixLen ) == 0)
				{
					files.push_back( fullname );
				}
			}
		}
		
		// cleanup
		free(namelist[i]);
	}
	// cleanup
	free(namelist);
}
# else

// TODO: needs win32 implementation using FindFirstFile
void AbiCollab_Regression::_findRegressionFiles(std::vector<std::string>& files)
{
}

#endif
