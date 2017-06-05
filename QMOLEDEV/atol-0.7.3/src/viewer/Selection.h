////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Classes to define file view selection position
////////////////////////////////////////////////////////////////////////////

#ifndef SELECTION_H__
#define SELECTION_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

class CMarker
{
public:
	CMarker();
	CMarker(int l, int c);
	CMarker(const CMarker &other);
	virtual ~CMarker();

	inline void operator =  (const CMarker &other);
	inline bool operator <  (const CMarker &other) const;
	inline bool operator == (const CMarker &other) const;
	inline bool operator != (const CMarker &other) const;

	/*unsigned*/ int	line;
	unsigned int	column;
};

void CMarker::operator =(const CMarker &other)
{
	line	= other.line;
	column	= other.column;
}

bool CMarker::operator < (const CMarker &other) const
{
	return (line < other.line || (line == other.line && column < other.column));
}

bool CMarker::operator == (const CMarker &other) const
{
	return (line == other.line && column == other.column);
}

bool CMarker::operator != (const CMarker &other) const
{
	return (line != other.line || column != other.column);
}

//TOFIX CTextSelection ili CScreenSelection
class CSelection
{
public:
	CSelection();
	CSelection(const CMarker &ptStart, const CMarker &ptEnd);
	CSelection(const CSelection &sel);
	virtual ~CSelection();

	void Clear();
	void SetSelectionStart(CMarker pt);
	void SetSelectionEnd(CMarker pt);

	bool AnySelection();
	bool LineIntersection(int nLine, int &nStart, int &nStop);
	void Normalize();
	void SwapPoints();
	
	void SetSelection(CMarker ptStart, CMarker ptEnd);
	bool Difference(const CSelection &sel, int &nFrom, int &nTo);

	CMarker	m_ptStart;
	CMarker	m_ptEnd;
	bool	m_bTopStart;
};

#endif // SELECTION_H__
