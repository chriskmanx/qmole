////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Defines a selection range inside a file
////////////////////////////////////////////////////////////////////////////

#ifndef _FILESELECTION_H__
#define _FILESELECTION_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

class CFileSelection  
{
public:
	CFileSelection();
	CFileSelection(const CFileSelection &other){
		operator =(other);
	}

	virtual ~CFileSelection();

	void operator = (const CFileSelection &other){
		m_nStart	= other.m_nStart;
		m_nEnd		= other.m_nEnd;
	}

	//__int64 m_nStart;
	//__int64 m_nEnd;
	int m_nStart;
	int m_nEnd;
};

#endif 
