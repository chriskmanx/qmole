////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Register application to be started at boot time (Win32 only code)
////////////////////////////////////////////////////////////////////////////

#ifndef _BOOTSTART
#define _BOOTSTART

#include <windows.h>
#include <tchar.h>

// Register program to be started at BOOT time!
//
//PARAMETERS:
// LPCTSTR lpValueName  // name of the value to set (can be anything, but must be unique for this app)
// LPCTSTR lpAppPath    // FULL PATH to APP
//
//RETURNS: TRUE on success

BOOL IsBootKeySet(LPCTSTR lpValueName);
BOOL RunProgramAtBoot(LPCTSTR lpValueName, LPCTSTR lpAppPath, BOOL bSet=TRUE);

#endif //_BOOTSTART

