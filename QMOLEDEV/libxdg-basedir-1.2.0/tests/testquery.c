/* Copyright (c) 2007 Mark Nevill
 * 
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <basedir.h>
#include <basedir_fs.h>

void printAndFreeString(const char *string)
{
	printf("%s\n", string);
	free((char*)string);
}

void printAndFreeStringList(const char * const *strings)
{
	const char * const *item;
	for (item = strings; *item; ++item)
	{
		printf("%s\n", *item);
		free((char*)*item);
	}
	free((const char **)strings);
}

int main(int argc, char *argv[])
{
	if (argc < 3)
		return 1;
	char *datatype = argv[1];
	char *querytype = argv[2];
	if (strcmp(datatype, "data") == 0)
	{
		if (strcmp(querytype, "home") == 0)
			printAndFreeString(xdgDataHome(NULL));
		else if (strcmp(querytype, "dirs") == 0)
			printAndFreeStringList(xdgDataDirectories(NULL));
		else if (strcmp(querytype, "search") == 0)
			printAndFreeStringList(xdgSearchableDataDirectories(NULL));
		else if (strcmp(querytype, "find") == 0 && argc == 4)
			printAndFreeString(xdgDataFind(argv[3], NULL));
		else
			return 1;
	}
	else if (strcmp(datatype, "config") == 0)
	{
		if (strcmp(querytype, "home") == 0)
			printAndFreeString(xdgConfigHome(NULL));
		else if (strcmp(querytype, "dirs") == 0)
			printAndFreeStringList(xdgConfigDirectories(NULL));
		else if (strcmp(querytype, "search") == 0)
			printAndFreeStringList(xdgSearchableConfigDirectories(NULL));
		else if (strcmp(querytype, "find") == 0 && argc == 4)
			printAndFreeString(xdgConfigFind(argv[3], NULL));
		else
			return 1;
	}
	else if (strcmp(datatype, "cache") == 0)
	{
		if (strcmp(querytype, "home") == 0)
			printAndFreeString(xdgCacheHome(NULL));
		else
			return 1;
	}
	else if (strcmp(datatype, "runtime") == 0)
	{
		if (strcmp(querytype, "directory") == 0)
		{
			char *rd = xdgRuntimeDirectory(NULL);
			if (!rd) printf("(null)\n");
			else printAndFreeString(rd);
		}
		else
			return 1;
	}
	else
		return 1;
	return 0;
}
