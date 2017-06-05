////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: BrowseHistoryList implementation
////////////////////////////////////////////////////////////////////////////

#include "BrowseHistoryList.h"

const unsigned int BrowseHistoryList::nMaxHistorySize = 20;

BrowseHistoryList::BrowseHistoryList()
{
}

BrowseHistoryList::~BrowseHistoryList()
{
}

void BrowseHistoryList::Clear()
{
	m_lstForward.clear();
	m_lstBackward.clear();
	m_strCurrent.erase();
}

void BrowseHistoryList::Push(const char *szPath)
{
	std::string strNew(szPath);

	//if path valid and new
	if(!strNew.empty() && strNew != m_strCurrent)
	{
		if(!m_strCurrent.empty())
			m_lstBackward.push_back(m_strCurrent);

		//keep maximal length of history list
		if(m_lstBackward.size() > nMaxHistorySize)
			m_lstBackward.erase(m_lstBackward.begin());

		m_strCurrent = szPath;
		m_lstForward.clear();
	}
}

bool BrowseHistoryList::CanMovePrev()
{
	return (m_lstBackward.size()>0);
}

bool BrowseHistoryList::CanMoveNext()
{
	return (m_lstForward.size()>0);
}

std::string BrowseHistoryList::MovePrev()
{
	int nSizeBackward = m_lstBackward.size();
	if(nSizeBackward>0)
	{
		std::string strPath;
		strPath = m_lstBackward[nSizeBackward-1];

		//backward path is now mowed forward
		m_lstBackward.pop_back();
		m_lstForward.push_back(m_strCurrent);

		m_strCurrent = strPath;
		return strPath;
	}

	return std::string();
}

std::string BrowseHistoryList::MoveNext()
{
	int nSizeForward = m_lstForward.size();
	if(nSizeForward>0)
	{
		std::string strPath;
		strPath = m_lstForward[nSizeForward-1];
        
		m_lstForward.pop_back();
		m_lstBackward.push_back(m_strCurrent);

		m_strCurrent = strPath;
		return strPath;
	}

	return std::string();
}

void BrowseHistoryList::Move(int nSteps, bool bBackwards)
{
	for(int i=0; i<nSteps; i++)
	{
		if(bBackwards)
			MovePrev();
		else
			MoveNext();
	}
}

