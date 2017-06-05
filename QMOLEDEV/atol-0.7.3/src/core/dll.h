////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: DynamicLib class handles use of .DLL (Windows) / .so (UNIX)
//       dynamic libraries (load at runtime, execute functions, ...)
////////////////////////////////////////////////////////////////////////////

#ifndef DYNAMICLIB_H
#define DYNAMICLIB_H

#include <stdlib.h>

#ifdef _WIN32
 #define _WINSOCKAPI_   // Prevent inclusion of winsock.h in windows.h
 #include <windows.h>
 #include <winbase.h>
#else
 #include <dlfcn.h>
#endif

#include <string>   //string class

#ifdef _WIN32
 #define DLL_HANDLE HMODULE
 #define DLL_FUNCT  FARPROC
#else
 #define DLL_HANDLE void *
 #define DLL_FUNCT  void *
#endif

class CDll{
public:
    CDll();
    ~CDll();

    //copy constructor
    CDll(const CDll &that){ operator =(that); }
    void operator =(const CDll &that);

    DLL_HANDLE Detach();

    inline bool IsValid(){ return m_hLibrary != NULL; }
    bool Load(const char *path);
    void Free();

    DLL_FUNCT GetFunction(const char *name);
    bool QueryFunction(const char *name);

    int GetErrCode();

protected:
    DLL_HANDLE  m_hLibrary; // library handle
	std::string m_strPath;
};

#endif // DYNAMICLIB_H



