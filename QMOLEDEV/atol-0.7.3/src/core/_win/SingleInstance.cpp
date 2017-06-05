////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: object of this class should be alive as long as program lives
//		 so that instance can be detected (by using named mutex object)
////////////////////////////////////////////////////////////////////////////

#include "SingleInstance.h"

CSingleInstance::CSingleInstance(const char *szName)
{
	m_bAlreadyExists = false;

	// try to create the named mutex
    m_hMutex = CreateMutex(NULL, FALSE, szName);
	if (GetLastError() == ERROR_ALREADY_EXISTS)
		m_bAlreadyExists = true;
}

CSingleInstance::~CSingleInstance()
{
	//after this we won't be able to detect this instance
    if (m_hMutex != NULL) {
        ReleaseMutex(m_hMutex);
		CloseHandle(m_hMutex);
    }
}

bool CSingleInstance::ProgramAlreadyStarted()
{
	return m_bAlreadyExists;
}


