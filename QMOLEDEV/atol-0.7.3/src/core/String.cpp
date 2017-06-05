////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implements portable string class
//////////////////////////////////////////////////////////////////////////// 

#include "String.h"
#include <stdio.h>
#include <stdarg.h>
#include <glib.h>

#ifdef _WIN32
 #define vsnprintf _vsnprintf
#else
 #include <ctype.h>
#endif

String::String()
{
}

String::~String()
{
}

String::String(const char *szData) 
{
	if(NULL != szData)
		assign(szData); 
}

void String::operator += (const char *a)
{
	if(NULL != a)
		append(a);
}

void String::operator += (char a)
{
	append(1, a);
}
 
void String::Printf(const char *fmt, ...)
{
	char szBuffer[1024] ="";
	
	va_list ap;
	va_start(ap, fmt);
	vsnprintf(szBuffer, sizeof(szBuffer), fmt, ap);
	operator=(szBuffer);
}

void String::Replace(const char *szFrom, const char *szTo)
{
	//TOFIX
	int nPos = -1;
	while(-1 != (nPos = find(szFrom)))
		replace(nPos, strlen(szFrom), szTo);
}

String String::Left(int nCount)  const
{
	return substr(0, nCount).c_str();
}

String String::Right(int nCount)  const
{
	return substr(size()-nCount, nCount).c_str();
}

String String::Mid(int nFirst, int nCount)  const
{
	return substr(nFirst, nCount).c_str();
}

int  String::Find(const char *szString, bool bForward) const
{
	if(bForward)
		return find(szString);
	else
		return find_last_of(szString);
}

int  String::CmpNoCase(const char *szString) const
{
#ifdef _WIN32
	return _stricmp(c_str(), szString);
#else
	return g_ascii_strcasecmp(c_str(), szString);
#endif
}

int  String::Cmp(const char *szString) const
{
	return strcmp(c_str(), szString);
}

void String::Empty()
{
	erase(begin(), end());
}

void String::Append(const char *szText)
{
	append(szText);
}

void String::Lower()
{
	for(unsigned int i=0; i<size(); i++)
		at(i) = tolower(at(i));
}

void String::Upper()
{
	for(unsigned int i=0; i<size(); i++)
		at(i) = toupper(at(i));
}

/*
String operator +(String &szStr1, String &szStr2)
{
	String res(szStr1);
	res += szStr2;
	return res;
}
*/
String operator +(String &szStr1, const char *szStr2)
{
	String res(szStr1);
	res += szStr2;
	return res;
}
