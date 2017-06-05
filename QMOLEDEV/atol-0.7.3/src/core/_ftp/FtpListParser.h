////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#ifndef FTPLISTPARSER_H__
#define FTPLISTPARSER_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "../VfsListing.h"

#include <vector>
#include <string>
typedef std::vector<std::string> CStrList;

// TOFIX suport to parse multiple LIST reply formats (+auto detect)
// TOFIX remember/cache parse format for given site after success

class CFtpListParser  
{
public:
	CFtpListParser();
	virtual ~CFtpListParser();

	void		SetSystemType(const char *szName);
	const char *GetSystemType(){ return m_szSystem; };
	bool		ParseList(CStrList &list, VfsListing &out);

protected:
	bool ParseUnix(CStrList &list, VfsListing &out);
	bool ParseWinNT(CStrList &list, VfsListing &out);
	bool ParseVMS(CStrList &list, VfsListing &out);

	char m_szSystem[128];
};

#endif // FTPLISTPARSER_H__
