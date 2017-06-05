////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: DynamicLib class handles use of .DLL (Windows) / .so (UNIX)
//       dynamic libraries (load at runtime, execute functions, ...)
////////////////////////////////////////////////////////////////////////////

#include "dll.h"

CDll::CDll()
{
    m_hLibrary = NULL;
}

CDll::~CDll()
{
    //Free();
}

DLL_HANDLE CDll::Detach()
{
	DLL_HANDLE hLib = m_hLibrary;
	m_hLibrary = NULL;
	return hLib;
}

void CDll::operator =(const CDll &that)
{
    if(this == &that)   //don't copy yourself
        return;

    if(IsValid())
        Free();

    //copy data
    m_strPath = that.m_strPath;
    m_hLibrary = that.m_hLibrary;
}

bool CDll::Load(const char *path)
{
#ifdef _WIN32
    m_hLibrary = LoadLibrary(path);

	//DWORD dwError = GetLastError();
	//TRACE("Error %d\n", GetLastError());
#else
    m_hLibrary = dlopen(path, RTLD_LAZY); //UNIX
#endif

    m_strPath = path;   //store path

//	CString strMsg;
//	strMsg.Format("Error: %d", GetLastError());
//	AfxMessageBox(strMsg);

    return IsValid();
}

void CDll::Free()
{
    if(IsValid())
    {
     #ifdef _WIN32
         FreeLibrary(m_hLibrary);
     #else
         dlclose(m_hLibrary);
     #endif
        m_hLibrary = NULL;
    }
}

bool CDll::QueryFunction(const char *name)
{
    return (GetFunction(name) != NULL);
}

DLL_FUNCT CDll::GetFunction(const char *name)
{
    if(IsValid())
    {
        // find address of function
        DLL_FUNCT fptr;
#ifdef _WIN32
        fptr = GetProcAddress(m_hLibrary, name);
#else
        fptr = dlsym(m_hLibrary, name);
#endif
        return fptr;
    }

    return NULL;
}

int CDll::GetErrCode()
{
#ifdef _WIN32
    return GetLastError();
#else
    return 0;   //TOFIX errno?
#endif
}

