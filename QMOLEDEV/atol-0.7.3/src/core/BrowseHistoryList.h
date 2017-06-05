////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: This is a list of a single panel browsing history (storing visited paths).
//         List can be traveled forward and backward. 
////////////////////////////////////////////////////////////////////////////

#ifndef BROWSEHISTORY_H__
#define BROWSEHISTORY_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#if _MSC_VER > 1000
  #pragma warning (disable: 4786)
#endif
#include <vector>
#include <string>

class BrowseHistoryList
{
    static const unsigned int nMaxHistorySize;

public:
    BrowseHistoryList();
    virtual ~BrowseHistoryList();

    void Clear();
    void Push(const char *szPath);

    //time travel functions
    bool CanMovePrev();
    bool CanMoveNext();

    std::string MovePrev();
    std::string MoveNext();

    void Move(int nSteps, bool bBackwards);

public:
    std::vector<std::string> m_lstBackward;
    std::vector<std::string> m_lstForward;
    std::string              m_strCurrent;
};

#endif // BROWSEHISTORY_H__

