////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#ifndef CONNECTIONINFOLIST_H__
#define CONNECTIONINFOLIST_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <vector>
#include "FtpInfo.h"
#include "IniFile.h"

class CNodeInfo : public CFtpInfo
{
public:
	CNodeInfo();
	CNodeInfo(const CNodeInfo &a){ operator=(a); };

	void operator =(const CNodeInfo &a);
	bool operator <(const CNodeInfo &a);

	int	m_nID;			
	int	m_nPID;			//parent ID
	bool	m_bIsFolder;
	bool	m_bExpanded;
	long	m_hItem;		//tree node ID
};

class CConnectionInfoList : public std::vector<CNodeInfo>
{
public:
	CConnectionInfoList();
	virtual ~CConnectionInfoList();

// Operations
public:
	int  GetMaxID();
	bool LoadDefault();
	void SaveDefault();
	void Dump();

// Implementation
public:
	void CalcIniFile(String &strPath);
	void SetPassword(const char *szPass){ m_strEncPassword = szPass; };
	bool LoadFromIni(const char *szFileName);
	bool SaveToIni(const char *szFileName);

	bool LoadFromIni(IniFile &ini);
	bool SaveToIni(IniFile &ini);

	void Sort();

	int  Find(long hItem);
	int  Find(int nID);
	int  Find(const char *szName);
	void Remove(int nIdx);
	void Swap(int i, int j);

protected:
	std::string m_strEncPassword;
};

#endif // CONNECTIONINFOLIST_H__
