////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Filter description information
////////////////////////////////////////////////////////////////////////////

#include "FilterDesc.h"
#include <time.h>
#include "util.h"
#include "../support.h"
#include "PathName.h"

FilterDesc::FilterDesc()
{
	m_nGroups = 0;    //filter is initially empty
	m_nShowAttr = 0;
	m_nHideAttr = 0;
	m_dwDateFlags = 0;
}

FilterDesc::~FilterDesc()
{
}

void FilterDesc::Clear()
{
	m_nGroups = 0;
	//TOFIX clear other data
}

void FilterDesc::AddGroupFlags(int nFlags)
{
	m_nGroups |= nFlags;
}

void FilterDesc::RemoveGroupFlags(int nFlags)
{
	m_nGroups &= ~nFlags;
}

void FilterDesc::SetNameGroup(const String &strNameShowPtrn, const String &strNameHidePtrn)
{
	if(strNameShowPtrn.IsEmpty() && strNameHidePtrn.IsEmpty())
	{
		//remove name filtering
		m_nGroups &= ~(FILTER_MatchName);
	}
	else
	{
		m_nGroups |= FILTER_MatchName;

		//tokenize each group of wildcard patterns into the list
		Tokenize(strNameShowPtrn, m_lstNameShow);
		Tokenize(strNameHidePtrn, m_lstNameHide);
	}
}

int FilterDesc::FindNameWildcard(const String &strWild, bool bShowPtrn) const
{
	unsigned int i;
	unsigned int nMax = (bShowPtrn) ? m_lstNameShow.size() : m_lstNameHide.size();
	for(i=0; i<nMax; i++)
	{
		if(bShowPtrn){
			if(m_lstNameShow[i] == strWild)
				return i;
		}
		else{
			if(m_lstNameHide[i] == strWild)
				return i;
		}
	}
	return -1;
}

void FilterDesc::AddToNameGroup(const String &strWild, bool bShowPtrn)
{
	if(FindNameWildcard(strWild, bShowPtrn) >= 0)
		return;    //wildcard pattern is already in the list

	if(bShowPtrn)
		m_lstNameShow.push_back(strWild);
	else
		m_lstNameHide.push_back(strWild);

	//TOFIX refresh flags
	AddGroupFlags(FILTER_MatchName);
}

void FilterDesc::RemoveFromNameGroup(const String &strWild, bool bShowPtrn)
{
	int nPos = FindNameWildcard(strWild, bShowPtrn);
	if(nPos < 0)
		return;    //wildcard pattern is not in the list

	if(bShowPtrn)
		m_lstNameShow.erase(m_lstNameShow.begin()+nPos);
	else
		m_lstNameHide.erase(m_lstNameHide.begin()+nPos);

	//TOFIX refresh flags
}

void FilterDesc::SetContentsGroup(const String &strContents, bool bCaseSensitive)
{
	if(strContents.IsEmpty())
	{
		//remove name filtering
		m_nGroups &= ~(FILTER_MatchContents);
	}
	else
	{
		m_nGroups |= FILTER_MatchContents;
		m_strContents     = strContents;
		m_bCaseSensitive = bCaseSensitive;

		//init finder object
		m_finder.Clear();
		if(!m_bCaseSensitive)
			m_finder.SetScanStyle(FS_CASE_INSENSITIVE);
		m_finder.SetSearchPattern(m_strContents);
	}
}

void FilterDesc::SetSizeGroup(INT64 nSize, int nRelation, int nUnit)
{
	m_nGroups |= FILTER_MatchSize;
	m_nSizeOperator    = nRelation;    //0, 1, 2, 3 => 0, <, =, >
	m_nSizeUnit        = nUnit;        //kB, MB, ...?
	m_nSizeAmount    = nSize;
}

void FilterDesc::SetAttrGroup(const int nAttrShow, const int nAttrHide)
{
	m_nGroups |= FILTER_MatchAttr;
	m_nShowAttr = nAttrShow;
	m_nHideAttr = nAttrHide;
}

void FilterDesc::AddDateGroup(int nSubflag, int nAmount, int nOperator, int nUnit)
{
	m_nGroups |= FILTER_MatchDate;
	m_dwDateFlags |= nSubflag;

	if(nSubflag & FILTER_DateFrom)
	{
		m_dateFrom = nAmount;
	}
	else if(nSubflag & FILTER_DateTo)
	{
		m_dateUntil = nAmount;
	}
	else if(nSubflag & FILTER_DateAge)
	{
		m_nAgeAmount    = nAmount;
		m_nAgeOperator  = nOperator;
		m_nAgeUnit      = nUnit;
	}
}

//does item matches the filter?
bool FilterDesc::Match(const VfsItem &item)
{
	if(0 == m_nGroups)
		return true;    //no filtering - empty filter

	if(MatchName(item) &&
	   MatchSize(item) &&
	   MatchDate(item) &&
	   MatchAttr(item) &&
	   MatchContents(item))
	{
		return true;
	}

	return false;
}

bool FilterDesc::MatchName(const VfsItem &item) const
{
	if(m_nGroups & FILTER_MatchName)
	{
		//check if directory skipping activated
		if(m_nGroups & FILTER_SkipDirMatch)
			if(item.IsDir())
				return true;

		//skip matching ".." item
		if(item.IsDots())
			return true;

		//match file name agaings a list of wildcard patterns
		String strItem = item.GetName();
		unsigned int i;

		for(i=0; i<m_lstNameHide.size(); i++)
			if(MatchName(strItem, m_lstNameHide[i]))
				return false;    //matches one of the patterns that forbid display

		bool bAnyMatch = false;
		unsigned int nCount = m_lstNameShow.size();
		if(nCount > 0)
		{
			for(i=0; i<nCount; i++)
			{
				if(MatchName(strItem, m_lstNameShow[i])){
					bAnyMatch = true;
					break;
				}
			}
			if(!bAnyMatch)
				return false;    //doesn't match any pattern required for display
		}
	}

	return true;
}

bool FilterDesc::MatchSize(const VfsItem &item) const
{
    if(m_nGroups & FILTER_MatchSize)
    {
        if(!item.IsDir())    //directory is not matched by size
        {
            INT64 nSizeInUnits = 0;
            switch(m_nSizeUnit){
             case 0: //byte(s)
                 nSizeInUnits = item.m_nSize;
                 break;
             case 1: //kB    
                 nSizeInUnits = item.m_nSize / 1024;
                 break;
             case 2: //MB
                 nSizeInUnits = item.m_nSize / 1048576; //1024*1024;
                 break;
             case 3: //GB
                 nSizeInUnits = item.m_nSize / 1073741824; //1024*1024*1024;
                 break;
            }

            switch(m_nSizeOperator){
             case 0: //<
                if(nSizeInUnits >= m_nSizeAmount)
                    return false;
                break;
             case 1: //=
                 if(nSizeInUnits != m_nSizeAmount)
                    return false;
                 break;
             case 2: //>
                 if(nSizeInUnits <= m_nSizeAmount)
                    return false;
                 break;
            }
        }
    }

    return true;
}

bool FilterDesc::MatchDate(const VfsItem &item) const
{
    if(m_nGroups & FILTER_MatchDate)
    {
        //TOFIX support for different date times; if other not available use modified time
        //        int m_nDateType;        //modify/created/last access
        if(m_dwDateFlags & FILTER_DateFrom)
        {
            if(item.m_nLastModDate < m_dateFrom)
                return false;
        }

        if(m_dwDateFlags & FILTER_DateTo)
        {
            if(item.m_nLastModDate > m_dateUntil)
                return false;
        }

        if(m_dwDateFlags & FILTER_DateAge)
        {
            //calculate difference of current time and item time
            time_t spanSec = time(NULL);
			spanSec -= item.m_nLastModDate; 

			int nAmountInUnits = 0;
/* TOFIX
            wxTimeSpan timeSpan(0, 0, spanSec, 0);
            switch (m_nAgeUnit){
                case 0:    //hours
                    nAmountInUnits = timeSpan.GetHours(); break;
                case 1: //days
                    nAmountInUnits = timeSpan.GetDays(); break;
                case 2:    //weeks
                    nAmountInUnits = timeSpan.GetWeeks(); break;
                case 3: //months
                    nAmountInUnits = timeSpan.GetHours();
                    //TOFIX approximated? - use dates directly instead of span
                    nAmountInUnits = timeSpan.GetWeeks() / 4; break;
                case 4:  //years
                    //TOFIX approximated? - use dates directly instead of span
                    nAmountInUnits = timeSpan.GetDays() / 365; break;
            }
*/
            switch (m_nAgeOperator){
                case 0:    //<
                    if(nAmountInUnits >= m_nAgeAmount)
                        return false;
                    break;
                case 1: //=
                    if(nAmountInUnits != m_nAgeAmount)
                        return false;
                    break;
                case 2: //>
                    if(nAmountInUnits <= m_nAgeAmount)
                        return false;
                    break;
            }
        }
    }

    return true;
}

bool FilterDesc::MatchAttr(const VfsItem &item) const
{
    if(m_nGroups & FILTER_MatchAttr)
    {
        //ensure all "must have" attributes exist
        if(m_nShowAttr != (item.m_nAttrib & m_nShowAttr))
            return false;

        //ensure no "must NOT have" attributes exist
        if(0 != (item.m_nAttrib & m_nHideAttr))
            return false;
    }

    return true;
}

//grep file for given contents
bool FilterDesc::MatchContents(const VfsItem &item)
{
    //TOFIX only for local files (Vfs_Local)!!!
    if(m_nGroups & FILTER_MatchContents)
    {
        if(!m_strContents.IsEmpty())
        {
            //NOTE: directory does not have text content
            if(item.IsDir())
                return false;

			String strName(item.m_strPath);
			PathName::EnsureTerminated(strName);
			strName += item.GetName();

            if(m_finder.SetScanFile(strName))
            {
                int nPos = m_finder.Search();
                m_finder.Close();
                return (-1 != nPos);
            }
            else
                return false;
        }
    }

    return true;
}

String FilterDesc::GetDescription()
{
    //TOFIX define filter string description (must not be too long - used in selection info contrl)
    if(!m_strTitle.IsEmpty())
        return m_strTitle;
    else
    {
        return _("Untitled");

        /*String strResult;
        if(m_nGroups & FILTER_MatchName){
            strResult += m_lstNameShow;
        }

        //TOFIX other marked with "..."
        return strResult;
        */
    }
}

bool FilterDesc::MatchName(const String &strName, const String &strWild) const
{
	//NOTE: matching filename against pattern list
	//(previously tokenized using ";" divider)
#ifdef _WIN32
	return fnmatch(strWild.c_str(), strName.c_str(), false, true);
#else
	return fnmatch(strWild.c_str(), strName.c_str(), true, false);
#endif
}


