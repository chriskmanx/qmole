////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Class for searching for a string within a given file.
////////////////////////////////////////////////////////////////////////////

#ifndef FINDINFILE_H_
#define FINDINFILE_H_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

// search using Boyer-Moore-Horspool-Sunday string search algorithm
// some of the code borrowed from SFL library (www.imatix.com)

#include <stdio.h>

#ifndef _BYTE
 #define _BYTE unsigned char
#endif

//possible find styles
#define FS_CASE_INSENSITIVE    1

class FindInFile
{
public:
    FindInFile();
    virtual ~FindInFile();

    //TOFIX Get... functions, more styles?, debug timing
    void SetScanStyle(int dwStyle);

    void SetSearchPattern(const char *szText);
    void SetSearchPattern(const char *szBinary, int nSize);

    bool SetScanFile(const char *szFile);
    bool SetScanBuffer(const char *szBuffer, int nSize);
    
    long Search(long nStartPos = 0);    //tofix int64

    void Close();
    void Clear();

    long SearchMem(long nStartPos);
    long SearchFile(long nStartPos);

protected:
    void BuildShiftTable();

protected:
    int    m_dwStyle;

    const _BYTE *m_pszPattern;
    int            m_nPtrnSize;    
    const _BYTE *m_pszBlock;
    int            m_nBlkSize;    

    FILE    *m_pFile;
    size_t     m_shift[256];
};

#endif // FINDINFILE_H_

