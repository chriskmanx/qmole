////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: FindInFile impelmentation
////////////////////////////////////////////////////////////////////////////

#include "FindInFile.h"
#include "String.h"
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

//file search buffer size
#define BUF_SIZE 8*1024

FindInFile::FindInFile()
{
    m_dwStyle        = 0;
    m_pszPattern    = NULL;
    m_nPtrnSize        = 0;
    m_pFile            = NULL;
}

FindInFile::~FindInFile()
{
    Close();
}

void FindInFile::SetScanStyle(int dwStyle)
{
    //NOTE: you should NOT change style once the pattern is set 
    //        (since that functions used current style setting)
    //ASSERT(NULL == m_pszPattern && 0 == m_nPtrnSize);

    m_dwStyle |= dwStyle;
}

void FindInFile::SetSearchPattern(const char *szText)
{
    m_pszPattern = (const _BYTE *)szText;
    m_nPtrnSize     = strlen(szText);    //TOFIX if NULL

    BuildShiftTable();
}

void FindInFile::SetSearchPattern(const char *szBinary, int nSize)
{
    m_pszPattern = (const _BYTE *)szBinary;
    m_nPtrnSize     = nSize;

    BuildShiftTable();
}

// To ensure the pattern is not inadvertently chopped up, 
// BMG_Patlen - 1 bytes is always moved to the start of the buffer
// The next time we fill the buffer we fill it with BUFSIZ - (BMG_Patlen - 1) bytes.
bool FindInFile::SetScanFile(const char *szFile)
{
    Close();

    String strName(szFile);
    //m_mapFile.MapFile(strName, TRUE);    //read-only
    m_pFile = fopen(szFile, "rb");
    
    //m_pszBlock    = (const _BYTE *)m_mapFile.Open();
    //m_nBlkSize    = m_mapFile.GetLength();

    return (NULL != m_pFile);
}

bool FindInFile::SetScanBuffer(const char *szBuffer, int nSize)
{
    Close();

    m_pszBlock    = (const _BYTE *)szBuffer;
    m_nBlkSize    = nSize;
    return (m_pszBlock != NULL && m_nBlkSize > 0);
}

//tofix int64
long FindInFile::SearchMem(long nStartPos)
{
    int
        match_size;                 //  Size of matched part
    const _BYTE
        *match_base = NULL,         //  Base of match of pattern
        *match_ptr  = NULL,         //  Point within current match
        *limit      = NULL;         //  Last potiental match point
    const _BYTE
        *block   = m_pszBlock,        //  Concrete pointer to block data
        *pattern = m_pszPattern;    //  Concrete pointer to search value

    //ASSERT (block);                 //  Expect non-NULL pointers, but
    //ASSERT (pattern);               //  fail gracefully if not debugging

    if (block == NULL || pattern == NULL)
        return -1;

    //  Pattern must be smaller or equal in size to string
    if (m_nBlkSize < m_nPtrnSize)
        return -1;                  //  Otherwise it's not found

    if (m_nPtrnSize == 0)           //  Empty patterns match at start
        return 0;

    //  Search for the block, each time jumping up by the amount             
    //  computed in the shift table                                          

    limit = block + (m_nBlkSize - m_nPtrnSize + 1);
    //ASSERT (limit > block);

    //NOTE: two versions: case sensitive and case insensitive version
    if(m_dwStyle & FS_CASE_INSENSITIVE)
    {
        for (match_base = block;
             match_base < limit;
             match_base += m_shift [ tolower(*(match_base + m_nPtrnSize)) ])
        {
            match_ptr  = match_base;
            match_size = 0;

            // Compare pattern until it all matches, or we find a difference
            while (tolower(*match_ptr++) == tolower(pattern [match_size++]))
            {
                //ASSERT (match_size <= m_nPtrnSize && match_ptr == (match_base + match_size));

                // If we found a match, return the start address
                if (match_size >= m_nPtrnSize)
                  return (match_base - m_pszBlock);
            }
         }
    }
    else
    {
        for (match_base = block;
             match_base < limit;
             match_base += m_shift [ *(match_base + m_nPtrnSize) ])
        {
            match_ptr  = match_base;
            match_size = 0;

            // Compare pattern until it all matches, or we find a difference
            while (*match_ptr++ == pattern [match_size++])
            {
                //ASSERT (match_size <= m_nPtrnSize && match_ptr == (match_base + match_size));

                // If we found a match, return the start address
                if (match_size >= m_nPtrnSize)
                  return (match_base - m_pszBlock);
            }
        }

    }

    return -1;    // Found nothing
}

void FindInFile::BuildShiftTable()
{
    //  Build the shift table unless we're continuing a previous search      

    //  The shift table determines how far to shift before trying to match
    //  again, if a match at this point fails.  If the byte after where the
    //  end of our pattern falls is not in our pattern, then we start to
    //  match again after that byte; otherwise we line up the last occurence
    //  of that byte in our pattern under that byte, and try match again.
    for (int i = 0; i < 256; i++)
        m_shift[i] = m_nPtrnSize + 1;

    if(m_dwStyle & FS_CASE_INSENSITIVE)
    {
        //case insensitive version
        for (int i = 0; i < m_nPtrnSize; i++)
            m_shift[(_BYTE) tolower(m_pszPattern[i])] = m_nPtrnSize - i;
    }
    else
    {
        //case sensitive version
        for (int i = 0; i < m_nPtrnSize; i++)
            m_shift[(_BYTE) m_pszPattern[i]] = m_nPtrnSize - i;
    }
}

void FindInFile::Close()
{
    //m_mapFile.Close();
    if(m_pFile)
    {
        fclose(m_pFile);
        m_pFile = NULL;
    }
}

void FindInFile::Clear()
{
    m_pszPattern = NULL;
    m_nPtrnSize  = 0;
    m_pszBlock   = NULL;
    m_nBlkSize   = 0;

    Close();
}

//TOFIX progress, abort, test, ...
long FindInFile::SearchFile(long nStartPos)
{
    //ASSERT(NULL == m_pFile);
    if(NULL == m_pFile)
        return -1;

    _BYTE *pBuffer = new _BYTE [BUF_SIZE];
    if(pBuffer)
    {
        if(nStartPos > 0)
            if(-1 != fseek(m_pFile, nStartPos, SEEK_SET))
                return -1;    // failed to set initial position

        m_pszBlock    = (const _BYTE *)pBuffer;

        int nRead = fread(pBuffer, 1, BUF_SIZE, m_pFile);
        while(nRead > 0)
        {
            m_nBlkSize    = nRead;

            //parse buffer
            long nPos = SearchMem(0);
            if(nPos > -1){
                delete [] pBuffer; 
                return nPos;    //something found
            }

            //keep last N chars in the buffer
            memmove(pBuffer, pBuffer+m_nPtrnSize, m_nPtrnSize);

            //refill the buffer
            nRead = fread(pBuffer+m_nPtrnSize, 1, BUF_SIZE-m_nPtrnSize, m_pFile);
            if(nRead > 0)
                nRead += m_nPtrnSize;
        }

        delete [] pBuffer;
    }

    return -1;
}

long FindInFile::Search(long nStartPos)
{
    if(m_pFile)
        return SearchFile(nStartPos);
    else
        return SearchMem(nStartPos);
}


