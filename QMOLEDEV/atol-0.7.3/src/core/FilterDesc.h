////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Class stores information of single file list filter.
//         It is used for list selection/filtering/file searching.    
////////////////////////////////////////////////////////////////////////////

#ifndef FILTERDESC_H__
#define FILTERDESC_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "VfsItem.h"
#include "FindInFile.h"
#include <vector>

//global filter flags - sections used
#define FILTER_MatchName        0x0001
#define FILTER_MatchSize        0x0002
#define FILTER_MatchDate        0x0004
#define FILTER_MatchAttr        0x0008
#define FILTER_MatchContents    0x0020

//additional flags
#define FILTER_SkipDirMatch        0x0040
#define FILTER_DateFrom            0x0080
#define FILTER_DateTo            0x0100
#define FILTER_DateAge            0x0200


//TOFIX rename to FileFilter
class FilterDesc  
{
public:
	FilterDesc();
	virtual ~FilterDesc();

	bool Match(const VfsItem &item);    //does item matches the filter?

	//filter setting
	void Clear();
	void AddGroupFlags(int nFlags);
	void RemoveGroupFlags(int nFlags);

	void SetNameGroup(const String &strNameShowPtrn, const String &strNameHidePtrn);
	void SetContentsGroup(const String &strContents, bool bCaseSensitive = true);
	void SetAttrGroup(const int nAttrShow, const int nAttrHide);
	void SetSizeGroup(INT64 nSize, int nRelation, int nUnit);

	void AddDateGroup(int nSubflag, int nAmount, int nOperator, int nUnit);
	void AddToNameGroup(const String &strWild, bool bShowPtrn = true);
	void RemoveFromNameGroup(const String &strWild, bool bShowPtrn = true);
	int  FindNameWildcard(const String &strWild, bool bShowPtrn = true) const;

	bool IsEmpty(){ return (0 == m_nGroups); }

	String GetDescription();

	unsigned short   GetAttrShow(){ return m_nShowAttr; }    //"must have" attributes
	unsigned short   GetAttrHide(){ return m_nHideAttr; }    //"must not have" attributes

protected:
	bool MatchName(const VfsItem &item) const;
	bool MatchSize(const VfsItem &item) const;
	bool MatchDate(const VfsItem &item) const;
	bool MatchAttr(const VfsItem &item) const;
	bool MatchContents(const VfsItem &item);
	bool MatchName(const String &strName, const String &strWild) const;
    
public:
	String m_strTitle;        //filter title

protected:
	int m_nGroups;                //see global flags above

	//file name wildcard lists tokenized from ";" delimited strings
	std::vector<String> m_lstNameShow;
	std::vector<String> m_lstNameHide;

	//file size matching
	int     m_nSizeOperator;    //0, 1, 2 => <, =, >
	int     m_nSizeUnit;        //0, 1, 2, 3 => B, kB, MB, GB
	INT64	m_nSizeAmount;

	//file date matching
	int     m_nDateType;        //modify/created/last access
	int     m_dwDateFlags;      //lower, upper, age
	time_t  m_dateFrom;
	time_t  m_dateUntil;
	int     m_nAgeOperator;     //0, 1, 2 => <, =, >
	int     m_nAgeAmount;       //0, 1, 2, 3 => hours, days, weeks, months, years
	int     m_nAgeUnit;

	//file attribute matching
	unsigned short   m_nShowAttr;        //"must have" attributes
	unsigned short   m_nHideAttr;        //"must not have" attributes

	//content matching data
	String m_strContents;        //TOFIX later - multiple contents (must have + must not have)
	bool     m_bCaseSensitive;
	//bool     m_bASCII;         //DOS character set

private:
	FindInFile m_finder;
};

#endif // FILTERDESC_H__


