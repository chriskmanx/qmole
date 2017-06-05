////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Register application to be started at boot time (Win32 only code)
////////////////////////////////////////////////////////////////////////////

#include "bootstart.h"

BOOL RunProgramAtBoot(LPCTSTR lpValueName, LPCTSTR lpAppPath, BOOL bSet)
{
    HKEY keyRun = NULL;

    //open registry key with read/write access
    LONG lRes=RegOpenKeyEx( HKEY_LOCAL_MACHINE,
        "Software\\Microsoft\\Windows\\CurrentVersion\\Run",
        (DWORD) 0, KEY_ALL_ACCESS, &keyRun);

    if(ERROR_SUCCESS==lRes) 
    {
        if(bSet){
            //register for boot startup
            lRes=RegSetValueEx(keyRun,
                lpValueName, (DWORD) 0, REG_SZ,
                (CONST BYTE *) lpAppPath, (DWORD) (_tcslen(lpAppPath)+1)*sizeof(TCHAR) );
        }
        else{
            //unregister boot startup (remove registry key)
            lRes=RegDeleteValue(keyRun, lpValueName);
        }
    }
    RegCloseKey(keyRun);
    return (ERROR_SUCCESS==lRes);
}

BOOL IsBootKeySet(LPCTSTR lpValueName)
{
    HKEY keyRun = NULL;

    //open registry key with read/write access
    LONG lRes=RegOpenKeyEx( HKEY_LOCAL_MACHINE,
        "Software\\Microsoft\\Windows\\CurrentVersion\\Run",
        (DWORD) 0, KEY_ALL_ACCESS, &keyRun);

    if(ERROR_SUCCESS==lRes) 
    {
        DWORD dwValueSize = 0;
        lRes=RegQueryValueEx(keyRun, lpValueName,
                NULL, NULL,    NULL, &dwValueSize);
        //TOFIX could check if the the value matches app path
    }
    RegCloseKey(keyRun);
    return (ERROR_SUCCESS==lRes);
}

