////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#include "ProxyLayer.h"
#include <stdio.h>
#include <stdarg.h>

CProxyLayer::CProxyLayer()
{
	m_pNextLayer	 = NULL;
	m_pfnTrace		 = NULL;
	m_dwTraceData	 = 0;
}

CProxyLayer::~CProxyLayer()
{
}

bool CProxyLayer::AddNextLayer(CProxyLayer *pLayer)
{
	//if some layer was already set as our next layer in chain,
	//new one will push it more far away from this layer
	if(NULL != m_pNextLayer)
		pLayer = m_pNextLayer;

	//assign as next in chain
	m_pNextLayer = pLayer;

	return true;
}

void CProxyLayer::SetTraceCallback(TRACE_FN pfTrace, unsigned long dwData)
{
	m_pfnTrace		= pfTrace;
	m_dwTraceData	= dwData;
}

void CProxyLayer::Trace(int nType, const char *fmt, ...)
{
	if(NULL != m_pfnTrace)
	{
		//format message
		char szBuffer[1024];

		va_list valist;
		va_start(valist, fmt);
		/*int nRes =*/ vsprintf(szBuffer, fmt, valist);
		va_end(valist);

		//send formated message through registered callback function
		m_pfnTrace(szBuffer, nType, m_dwTraceData);

		//TRACE("Trace: %s\n", szBuffer);
	}
}

