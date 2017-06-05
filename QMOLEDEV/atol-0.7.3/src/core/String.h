////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implements portable string class
//////////////////////////////////////////////////////////////////////////// 

#ifndef STRING_H__INCLUDED_
#define STRING_H__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#if _MSC_VER > 1000
 #pragma warning (disable: 4786)
#endif
#include <string>

class String : public std::string
{
public:
	String();
	virtual ~String();

	String(const char *szData);

	void operator += (const char *a);
	void operator += (char a);
	char &operator [](int nIdx){ return at(nIdx); };

//	void operator =(const char *a){ assign(a); } ;
//	operator =(String &a){ *this = a.c_str(); } ;

	int  Length() const { return size(); };
	bool IsEmpty() const { return empty(); };

	void Empty();
	void Append(const char *szText);

	char GetAt(int nIdx) const { return at(nIdx); };

	String Left(int nCount) const;
	String Right(int nCount) const;
	String Mid(int nFirst, int nCount = 1000000) const;

	void Printf(const char *fmt, ...);
	void Replace(const char *szFrom, const char *szTo);

	void Lower();
	void Upper();

	int  Find(const char *szString, bool bForward = true) const;
	int  CmpNoCase(const char *szString) const;
	int  Cmp(const char *szString) const;

	operator const char*() const { return c_str(); }
};

String operator +(String &szStr1, const char *szStr2);

#endif // STRING_H__INCLUDED_
