////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: VfsItem is a class that stores information on a single virtual file system item
////////////////////////////////////////////////////////////////////////////

#ifndef VFSITEM_H
#define VFSITEM_H

#include "String.h"
#include <sys/types.h>
#include "types.h"
#include "../../plugins/src/file_attrib.h" //portable file attribute flags

class VfsItem
{
public:
	VfsItem();
	VfsItem(const VfsItem& a);
	virtual ~VfsItem() {}
    
	void operator =(const VfsItem& a);
	bool operator ==(const VfsItem& a){ return m_strName == a.m_strName; };

	void CalcExt();        //TOFIX solve through SetName

	bool IsDir() const;
	bool IsDots() const;
	bool IsLink() const;

	String GetTitle() const;    // name without ext
	String GetName()  const;    // name + dot + ext
	String GetExt()   const;	  // dot + ext	
	String GetSize()  const;
	String GetDate()  const;
	String GetAttr()  const;
	String GetPath()  const { return m_strPath; };
	String GetExtTitle() const; //ext without dot

	void SetName(const char *szName);
	void Clear();

public:
	String m_strPath;            // absolute directory path (used in file search)
	String m_strName;            // name + dot + ext (using local/disc encoding)

	short     m_nExtPos;
	time_t    m_nLastModDate;
	INT64     m_nSize;
	unsigned short m_nAttrib;
	short    m_nIconIdx;	//TOFIX not used yet
};

#endif

