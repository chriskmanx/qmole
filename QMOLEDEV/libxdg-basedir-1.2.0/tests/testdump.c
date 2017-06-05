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

#include <basedir.h>
#include <stdio.h>

int main(int argc, char* argv[])
{
	const char * const * item;
	xdgHandle handle;
	if (!xdgInitHandle(&handle)) return 1;
	printf("${XDG_DATA_HOME:-$HOME/.local/share}=%s\n", xdgDataHome(&handle));
	printf("${XDG_CONFIG_HOME:-$HOME/.config}=%s\n", xdgConfigHome(&handle));
	printf("${XDG_DATA_DIRS:-/usr/local/share/:/usr/share/}=");
	for (item = xdgDataDirectories(&handle); *item; item++)
		printf("%s%c", *item, (item[1] ? ':' : '\n'));
	printf("${XDG_DATA_HOME:-$HOME/.local/share}:${XDG_DATA_DIRS:-/usr/local/share/:/usr/share/}=");
	for (item = xdgSearchableDataDirectories(&handle); *item; item++)
		printf("%s%c", *item, (item[1] ? ':' : '\n'));
	printf("${XDG_CONFIG_DIRS:-/etc/xdg}=");
	for (item = xdgConfigDirectories(&handle); *item; item++)
		printf("%s%c", *item, (item[1] ? ':' : '\n'));
	printf("${XDG_CONFIG_HOME:-$HOME/.config}:${XDG_CONFIG_DIRS:-/etc/xdg}=");
	for (item = xdgSearchableConfigDirectories(&handle); *item; item++)
		printf("%s%c", *item, (item[1] ? ':' : '\n'));
	printf("${XDG_CACHE_HOME:-$HOME/.cache}=%s\n", xdgCacheHome(&handle));
	xdgWipeHandle(&handle);
	return 0;
}
