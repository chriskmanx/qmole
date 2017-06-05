/* w32_reg.c  - Posix emulation layer for Sylpheed (Claws)
 *
 * This file is part of w32lib.
 *
 * w32lib is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * w32lib is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * For more information and a list of changes, see w32lib.h
 */

#include <windows.h>
#include "utils.h"
#include "w32lib.h"

static HKEY get_root_key_from_str(char *parent)
{
	HKEY rootKey = NULL;
	if (!parent || !strlen(parent))
		rootKey = HKEY_CURRENT_USER ;
	else if (!strcmp(parent, "HKCR") || !strcmp(parent,"HKEY_CLASSES_ROOT"))
		rootKey = HKEY_CLASSES_ROOT ;
	else if (!strcmp(parent, "HKCU") || !strcmp(parent,"HKEY_CURRENT_USER"))
		rootKey = HKEY_CURRENT_USER ;
	else if (!strcmp(parent, "HKLM") || !strcmp(parent,"HKEY_LOCAL_MACHINE"))
		rootKey = HKEY_LOCAL_MACHINE ;
	else if (!strcmp(parent, "HKU")  || !strcmp(parent,"HKEY_USERS"))
		rootKey = HKEY_USERS ;
	else if (!strcmp(parent, "HKCC") || !strcmp(parent,"HKEY_CURRENT_CONFIG"))
		rootKey = HKEY_CURRENT_CONFIG ;
	return rootKey;

}

int write_w32_registry_string( char *parent, char *section, char *value, char *data )
{
	HKEY hKey, rootKey;
	int ret;

	rootKey = get_root_key_from_str(parent);
	ret = RegCreateKeyEx(rootKey, section, 0, NULL, 
		REG_OPTION_NON_VOLATILE, KEY_WRITE, NULL, &hKey, NULL);
	if (ret != ERROR_SUCCESS) {
		debug_print("can't write key %s\\%s: %d\n", parent, section, ret);
		return -1;
	}
	ret = RegSetValueEx(hKey, value, 0, REG_SZ, (LPVOID)data, strlen(data)+1);
	if (ret != ERROR_SUCCESS) {
		RegCloseKey(hKey);
		debug_print("can't write key %s\\%s: %d\n", parent, section, ret);
		return -1;
	}
	RegCloseKey(hKey);
	return 0;
}

int write_w32_registry_dword( char *parent, char *section, char *value, int data )
{
	HKEY hKey, rootKey;
	int ret;

	rootKey = get_root_key_from_str(parent);
	ret = RegCreateKeyEx(rootKey, section, 0, NULL, 
		REG_OPTION_NON_VOLATILE, KEY_WRITE, NULL, &hKey, NULL);
	if (ret != ERROR_SUCCESS) {
		debug_print("can't write key %s\\%s: %d\n", parent, section, ret);
		return -1;
	}
	ret = RegSetValueEx(hKey, value, 0, REG_DWORD, (LPBYTE)&data, sizeof(data));
	if (ret != ERROR_SUCCESS) {
		RegCloseKey(hKey);
		debug_print("can't write key %s\\%s: %d\n", parent, section, ret);
		return -1;
	}
	RegCloseKey(hKey);
	return 0;
}

char *read_w32_registry_string( char *parent, char *section, char *key )
{
	HKEY hKey, rootKey;
	char *str;
	int ret;

	char buf[ MAX_PATH ];
	DWORD bufsiz = sizeof( buf );

	rootKey = get_root_key_from_str(parent);

        if (!rootKey) 
          return NULL;

	str = NULL;
	ret = RegOpenKeyEx( rootKey, section, 0, KEY_READ, &hKey );
	if ( ERROR_SUCCESS == ret ){
		ret = RegQueryValueEx( hKey, key, 0, NULL, 
				(LPBYTE)buf, &bufsiz );
		if ( ERROR_SUCCESS == ret ){
			str = strdup( buf );
		}
		RegCloseKey( hKey );
	}
	return str;
}

char *get_content_type_from_registry_with_ext( char *ext )
{
	HKEY hKey, parent;
	int ret;
	char buf[ MAX_PATH ];
	DWORD bufsiz;
	char *section, *key, *value;

	if (ext == NULL)
		return NULL;

	// parent	: HKEY_CLASSES_ROOT
	// section	: ".txt"
	parent = HKEY_CLASSES_ROOT;
        section = malloc ( 1 + strlen (ext) + 1);
        if (!section)
          return NULL;
        *section = '.';
        strcpy (section+1, ext);

	value = NULL;
	while ( 1 ) {
		ret = RegOpenKeyEx( parent, section, 0, KEY_READ, &hKey );
		if ( ERROR_SUCCESS != ret ) {
			// If section is not found...
			value = NULL;
			break;
		}

		// key		: "Content Type"
		key = "Content Type";
		bufsiz = sizeof( buf );
		ret = RegQueryValueEx( hKey, key, 0, NULL, (LPBYTE)buf, &bufsiz );
		if ( ERROR_SUCCESS == ret ) {
			// If value is found!
			RegCloseKey( hKey );
			value = strdup( buf );
			break;
		}

		key = "";
		bufsiz = sizeof( buf );
		ret = RegQueryValueEx( hKey, key, 0, NULL, (LPBYTE)buf, &bufsiz );
		if ( ERROR_SUCCESS != ret ) {
			RegCloseKey( hKey );
			value = NULL;
			break;
		}

		RegCloseKey( hKey );
		free( section );
		section = strdup( buf );
                break; //XXX:tm-gtk2
	}

	free( section );
	return value;
}
