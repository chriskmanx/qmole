////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Classes to define file view selection position
////////////////////////////////////////////////////////////////////////////

#include "Selection.h"

#ifndef min
#define min(x,y) (((x)<(y))? (x):(y))
#endif
#ifndef max
#define max(x,y) (((x)>(y))? (x):(y))
#endif

void Swap(CMarker &a, CMarker &b)
{
	CMarker tmp(a);
	a = b;
	b = tmp;
}

CMarker::CMarker()
{
	line	= 0;
	column	= 0;
}

CMarker::CMarker(int l, int c) : line(l), column(c)
{
}

CMarker::~CMarker()
{
}

CMarker::CMarker(const CMarker &other)
{
	operator = (other);
}

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CSelection::CSelection()
{
	Clear();
}

CSelection::CSelection(const CMarker &ptStart, const CMarker &ptEnd)
{
	SetSelection(ptStart, ptEnd);
}
	
CSelection::CSelection(const CSelection &sel)
{
	SetSelection(sel.m_ptStart, sel.m_ptEnd);
}

CSelection::~CSelection()
{
}

void CSelection::Clear()
{
	m_ptStart.line	= -1;
}

void CSelection::SetSelectionStart(CMarker pt)
{
	m_ptStart = pt;
}

void CSelection::SetSelectionEnd(CMarker pt)
{
	m_ptEnd	= pt;
}

void CSelection::Normalize()
{
	if(m_ptEnd < m_ptStart)
		Swap(m_ptStart, m_ptEnd);
}

bool CSelection::AnySelection()
{
	if(m_ptStart.line != -1 && (m_ptStart != m_ptEnd))
		return true;
	return false;
}

bool CSelection::LineIntersection(int nLine, int &nStart, int &nStop)
{
	if(!AnySelection())
		return false;

	Normalize();

	if(nLine == m_ptStart.line)
	{
		nStart = m_ptStart.column;
		if(nLine == m_ptEnd.line)
			nStop = m_ptEnd.column;
		else
			nStop = -1;	//overlaps until the end of the line
		
		return true;
	}
	else if(nLine == m_ptEnd.line)
	{
		nStart = 0;
		nStop  = m_ptEnd.column;
		return true;
	}
	else if(m_ptStart.line < nLine && nLine < m_ptEnd.line)
	{
		nStart = 0;
		nStop  = -1;		//until the end of the line
		return true;
	}

	return false;
}

void CSelection::SwapPoints()
{
	Swap(m_ptStart, m_ptEnd);
}

void CSelection::SetSelection(CMarker ptStart, CMarker ptEnd)
{
	m_ptStart = ptStart;
	m_ptEnd	= ptEnd;
}

bool CSelection::Difference(const CSelection &sel, int &nFrom, int &nTo)
{
	if (sel.m_ptStart == m_ptStart)
	{
		nFrom = min(sel.m_ptEnd.line, m_ptEnd.line);
		nTo	  = max(sel.m_ptEnd.line, m_ptEnd.line);
	}
	else if (sel.m_ptEnd == m_ptEnd)
	{
		nFrom = min(sel.m_ptStart.line, m_ptStart.line);
		nTo	  = max(sel.m_ptStart.line, m_ptStart.line);
	}
	else
	{
		nFrom = min(sel.m_ptStart.line, m_ptStart.line);
		nTo	  = max(sel.m_ptEnd.line, m_ptEnd.line);	
	}

	return true;
}
