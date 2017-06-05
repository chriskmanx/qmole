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
#include <basedir_fs.h>

void printAndFreeStrings(char * strings)
{
	char * ptr = strings;
	if (!strings) return;
	while (*ptr)
	{
		printf("%s\n", ptr);
		ptr += strlen(ptr)+1;
	}
	free(strings);
}

int main(int argc, char* argv[])
{
	int ret = 0;
	xdgHandle handle;
	if (!xdgInitHandle(&handle)) return 1;
	if (argc == 2)
	{
		printf("xdgDataFind:\n");
		printAndFreeStrings(xdgDataFind(argv[1], &handle));
		printf("xdgConfigFind:\n");
		printAndFreeStrings(xdgConfigFind(argv[1], &handle));
	}
	else if (argc == 3)
	{
		if (strcmp(argv[1], "--data") == 0)
			printAndFreeStrings(xdgDataFind(argv[2], &handle));
		else if (strcmp(argv[1], "--config") == 0)
			printAndFreeStrings(xdgConfigFind(argv[2], &handle));
		else
			ret = 2;
	}
	else
		ret = 2;

	xdgWipeHandle(&handle);
	return ret;
}
