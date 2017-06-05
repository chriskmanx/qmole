////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: VfsSelection is a class that stores selection within a single Vfs
//         (selection is hierarchical, it can be expanded to include items within
//          selected subbdirectores recursively)
////////////////////////////////////////////////////////////////////////////

#ifndef VFSSELECTION_H__
#define VFSSELECTION_H__

#include "VfsItem.h"
#include "VfsListing.h"
#include <vector>

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "types.h"

class VfsSelectionItem : public VfsItem
{
public:
    VfsSelectionItem(){};
    ~VfsSelectionItem(){};

    VfsSelectionItem(const VfsItem &other){
        operator =(other);
    }

    void operator =(const VfsItem &other){
        m_lstSubItems.clear();
        VfsItem::operator =(other);
    }

    void operator =(const VfsSelectionItem &other){
        VfsItem::operator =( dynamic_cast<const VfsItem &>(other) );
        m_lstSubItems = other.m_lstSubItems;
    }

    bool operator ==(const VfsSelectionItem &other){
        return VfsItem::operator ==(other);
    }

    std::vector<VfsSelectionItem> m_lstSubItems;
};

// selection tree - can be expanded/unexpanded(only 1st level filled)
class VfsSelection  
{
public:
    VfsSelection();
    virtual ~VfsSelection();

    void operator = (const VfsListing &a);

    void   Clear();
    bool   IsEmpty(){ return (0 == m_lstRootItems.size()); }

    int    GetRootCount(){ return m_lstRootItems.size(); }

    int    GetTotalCount();
    int    GetTotalCountRecursive(VfsSelectionItem *pItem);

    INT64  GetTotalSize();
    INT64  GetTotalSizeRecursive(VfsSelectionItem *pItem);

	void   RemoveRootDirs();
	void   RemoveRootFiles();

#ifdef _DEBUG
    void Dump();
#endif

    std::vector<VfsSelectionItem> *Find(const char *szPath);
    VfsItem *Insert(const char *szPath);
    VfsSelectionItem *InsertUnder(VfsSelectionItem &item, VfsSelectionItem *pParent);

    std::vector<VfsSelectionItem> m_lstRootItems;
};

#endif // VFSSELECTION_H__

