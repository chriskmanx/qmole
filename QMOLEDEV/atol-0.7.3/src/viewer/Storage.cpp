////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: For file viewer, handles file I/O and returns formatted lines (txt,bin,hex)
////////////////////////////////////////////////////////////////////////////

#include "Storage.h"

#ifndef _WIN32
 #include <ctype.h>
#endif

#ifndef min
 #define min(a,b) ((a)<(b))?(a):(b)
#endif
#ifndef max
 #define max(a,b) ((a)>(b))?(a):(b)
#endif
#ifndef UCHAR
 #define UCHAR unsigned char
#endif

class LineCountThread : public Thread
{
public:
	LineCountThread(){ m_pObj = NULL; };
	void MainMethod(){ if(m_pObj){ m_pObj->TextLineCounter(); } };
	FormatTxt *m_pObj;
};

char *txtfind (
		const char *string,            /*  String containing data           */
		const char *pattern );          /*  Pattern to search for            */

void * memfind_rb (
		const void   *in_block,     /*  Block containing data            */
		const size_t block_size,    /*  Size of block in bytes           */
		const void   *in_pattern,   /*  Pattern to search for            */
		const size_t pattern_size,  /*  Size of pattern block            */
		size_t       *shift,        /*  Shift table (search buffer)      */
		bool         *repeat_find); /*  TRUE: search buffer already init */

//reverse find -> search from end to start
void * memrfind_rb (
		const void   *in_block,     /*  Block containing data            */
		const size_t block_size,    /*  Size of block in bytes           */
		const void   *in_pattern,   /*  Pattern to search for            */
		const size_t pattern_size,  /*  Size of pattern block            */
		size_t       *shift,        /*  Shift table (search buffer)      */
		bool         *repeat_find); /*  TRUE: search buffer already init */

CStorage::CStorage()
{
	m_nFileOffset = 0;		//file pos
	m_nBuffSize   = 0;	
	m_bBinaryFile = false;
	m_bUnicodeText= false;

	m_fmtTxt.m_pData = this;
	m_fmtBin.m_pData = this;
	m_fmtHex.m_pData = this;
	//m_fmtUnicode.m_pData = this;
	m_nDrawMode = DRAW_TEXT;

	m_pFmt	= &m_fmtTxt;
	m_pThread = NULL;
}

CStorage::~CStorage()
{
	Close();
}

bool CStorage::GetLineRaw(int nLine, LPBYTE &pBuffer, int &nLineSize)
{
	//ASSERT(NULL != m_pFmt);
	return m_pFmt->GetLineRaw(nLine, pBuffer, nLineSize);
}

bool CStorage::Open(const char *szPath)
{
	//close previous document
	if(m_File.IsOpen())
		m_File.Close();

	//FIX: FILE_SHARE_DELETE flag causes file open to fail on Win98
	if(m_File.Open(szPath))
	{
		m_strFilePath	= szPath;
		m_nFileSize		= m_File.GetSize();
		m_nFileOffset	= 0;	//file pos

		CheckDataType();
		
		if(m_bUnicodeText)
			SetDrawMode(DRAW_UNICODE);
		else if(m_bBinaryFile)
			SetDrawMode(DRAW_BIN); 

		//start lengthy operation in the separate thread
		m_fmtTxt.m_bAbortThread = false;
		m_pThread = new LineCountThread;
		m_pThread->m_pObj = &m_fmtTxt;
		m_pThread->Run();

		FillBuffer(0);
		return true;
	}
	else
	{
		//CString strMsg;
		//strMsg.Format("Error = %d\n", exc.m_lOsError);
		//AfxMessageBox(strMsg);
		//TRACE("Error = %d\n", exc.m_lOsError);
		//TOFIX System error message
	}

	return false;
}

int CStorage::GetLineCount()
{
	//ASSERT(NULL != m_pFmt);
	return m_pFmt->GetLineCount();
}

bool CStorage::GetLineFormated(int nLine, LPBYTE &pszLine, int &nSize)
{
	//ASSERT(NULL != m_pFmt);
	return m_pFmt->GetLineFormated(nLine, pszLine, nSize);
}

INT64 CStorage::FileOffsetFromScreenPos(const CMarker &pos)
{
	//ASSERT(NULL != m_pFmt);
	return m_pFmt->ScreenToOffset(pos);
}

CMarker CStorage::ScreenPosFromFileOffset(INT64 nOffset)
{
	//ASSERT(NULL != m_pFmt);
	//ASSERT(0 <= nOffset && nOffset <= m_nFileSize);
	return m_pFmt->OffsetToScreen(nOffset);
}

/*  ---------------------------------------------------------------------[<]-
    Function: txtfind

    Synopsis: Searches for a case-insensitive text pattern in a string
    using the Boyer-Moore-Horspool-Sunday algorithm.  The string and
    pattern are null-terminated strings.  Returns a pointer to the first
    occurance of the pattern if found within the string, or NULL if the 
    pattern was not found.  Will match strings irrespective of case.  
    To match exact strings, use strfind().  Will not work on multibyte 
    characters.  

    Reentrant.

    Examples:
    char *result;

    result = txtfind ("AbracaDabra", "cad");
    if (result)
        puts (result);
    ---------------------------------------------------------------------[>]-*/
char *txtfind (
		const char *string,            /*  String containing data           */
		const char *pattern )          /*  Pattern to search for            */
{
    int
        shift [256];                    /*  Shift distance for each value    */
    size_t
        string_size,
        pattern_size,
        byte_nbr,                       /*  Distance through block           */
        match_size,                     /*  Size of matched part             */
        limit;                          /*  Last potiental match point       */
    const char
        *match_ptr       = NULL;

    //ASSERT (string);                    /*  Expect non-NULL pointers, but    */
    //ASSERT (pattern);                   /*  fail gracefully if not debugging */
    if (string == NULL || pattern == NULL)
        return (NULL);

    string_size  = strlen (string);
    pattern_size = strlen (pattern);

    /*  Pattern must be smaller or equal in size to string                   */
    if (string_size < pattern_size)
        return (NULL);                  /*  Otherwise it cannot be found     */

    if (pattern_size == 0)              /*  Empty string matches at start    */
        return (char *)string;

    /*  Build the shift table                                                */

    /*  The shift table determines how far to shift before trying to match   */
    /*  again, if a match at this point fails.  If the byte after where the  */
    /*  end of our pattern falls is not in our pattern, then we start to     */
    /*  match again after that byte; otherwise we line up the last occurence */
    /*  of that byte in our pattern under that byte, and try match again.    */

    for (byte_nbr = 0; byte_nbr < 256; byte_nbr++)
        shift [byte_nbr] = pattern_size + 1;

    for (byte_nbr = 0; byte_nbr < pattern_size; byte_nbr++)
        shift [(UCHAR) tolower (pattern [byte_nbr])] = pattern_size - byte_nbr;

    /*  Search for the string.  If we don't find a match, move up by the     */
    /*  amount we computed in the shift table above, to find location of     */
    /*  the next potiental match.                                            */
    limit = string_size - pattern_size + 1;
    //ASSERT (limit > 0);

    for (byte_nbr = 0;
         byte_nbr < limit;
         byte_nbr += shift [(UCHAR) tolower (string [byte_nbr + pattern_size])])
      {
        //ASSERT (byte_nbr >= 0 && byte_nbr < (string_size - pattern_size) + 1);

        // If the first byte matches, compare rest of pattern
        if (tolower (string [byte_nbr]) == tolower (*pattern))
        {
            match_ptr  = string + byte_nbr + 1;
            match_size = 1;

            do{
				//  Loop invarients
                //ASSERT (match_size > 0    && match_size <= pattern_size);
                //ASSERT (match_ptr != NULL && match_ptr > string 
                //                          && match_ptr <= string+string_size);
				//ASSERT (match_ptr == (string + byte_nbr + match_size));

                //  If all matched, return pointer to start of match
                if (match_size == pattern_size)
                    return ((char *) string + byte_nbr);

                //ASSERT (match_size < pattern_size && match_ptr  < string+string_size);
              
			}while (tolower (*match_ptr++) == tolower (pattern [match_size++]));
        }

		//ASSERT (byte_nbr + pattern_size <= string_size);
    }

    return (NULL);                      /*  Found nothing                    */
}



/*  ---------------------------------------------------------------------[<]-
    Function: memfind_rb

    Synopsis: Searches for a pattern in a block of memory using the Boyer-
    Moore-Horspool-Sunday algorithm.  The block and pattern may contain any
    values; you must explicitly provide their lengths.  Returns a pointer to
    the pattern if found within the block, or NULL if the pattern was not
    found.  On the first search with a given pattern, *repeat_find should
    be FALSE.  It will be set to TRUE after the shift table is initialised,
    allowing the initialisation phase to be skipped on subsequent searches.
    shift must point to an array big enough to hold 256 (8**2) size_t values.

    Original algorithm published by BOYER, R., and S. MOORE. 1977. "A 
    Fast String Searching Algorithm." CACM, 20, 762-72.  Simplifications 
    by HORSPOOL, R. N. 1980.  "Practical Fast Searching in Strings." 
    Software - Practice and Experience, 10, 501-06.  Further improvements 
    by HUME, A., and D. M. SUNDAY. 1991.  "Fast String Searching." AT&T 
    Bell Labs Computing Science Technical Report No. 156.  Finally, 
    implemented in C by P. Hintjens.

    This function is meant to handle binary data, for repeated searches
    for the same pattern.  If you need to search strings, use the 
    strfind_r() or strfind_rb() functions.  If you wish to search for a
    pattern only once consider using memfind_r().

    Reentrant.
    ---------------------------------------------------------------------[>]-*/

typedef unsigned char byte;

void * memfind_rb (
		const void   *in_block,     /*  Block containing data            */
		const size_t block_size,    /*  Size of block in bytes           */
		const void   *in_pattern,   /*  Pattern to search for            */
		const size_t pattern_size,  /*  Size of pattern block            */
		size_t       *shift,        /*  Shift table (search buffer)      */
		bool         *repeat_find)  /*  TRUE: search buffer already init */
{
    size_t	byte_nbr,                       /*  Distance through block           */
			match_size,                     /*  Size of matched part             */
			limit;
    
	const byte *match_ptr = NULL;
    
	const byte
        *block     = (byte *)in_block,  /*  Concrete pointer to block data   */
        *pattern   = (byte *)in_pattern;/*  Concrete pointer to search value */

    //ASSERT (block);                     /*  Expect non-NULL pointers, but    */
    //ASSERT (pattern);                   /*  fail gracefully if not debugging */
    //ASSERT (shift);                     /*  NULL repeat_find => is false     */
    if (block == NULL || pattern == NULL || shift == NULL)
	    return (NULL);

    /*  Pattern must be smaller or equal in size to string                   */
    if (block_size < pattern_size)
        return (NULL);                  /*  Otherwise it's not found         */

    if (pattern_size == 0)              /*  Empty patterns match at start    */
        return ((void *)block);

    /*  Build the shift table unless we're continuing a previous search      */

    /*  The shift table determines how far to shift before trying to match   */
    /*  again, if a match at this point fails.  If the byte after where the  */
    /*  end of our pattern falls is not in our pattern, then we start to     */
    /*  match again after that byte; otherwise we line up the last occurence */
    /*  of that byte in our pattern under that byte, and try match again.    */

    if (!repeat_find || !*repeat_find)
      {
        for (byte_nbr = 0; byte_nbr < 256; byte_nbr++)
            shift [byte_nbr] = pattern_size + 1;
        for (byte_nbr = 0; byte_nbr < pattern_size; byte_nbr++)
            shift [(byte) tolower(pattern [byte_nbr])] = pattern_size - byte_nbr;	//TOFIX tolower added for case insensitive search

        if (repeat_find)
            *repeat_find = true;
      }


    /*  Search for the block, each time jumping up by the amount             */
    /*  computed in the shift table                                          */
    limit = block_size - pattern_size + 1;
    //ASSERT (limit > 0);

    for (byte_nbr = 0;
         byte_nbr < limit;
         byte_nbr += shift [tolower(block [byte_nbr + pattern_size])])	//TOFIX tolower for case insensitive
    {
        //ASSERT (byte_nbr >= 0 && byte_nbr < (block_size - pattern_size) + 1);

        /*  If the first byte matches, compare rest of pattern               */
        if (tolower(block [byte_nbr]) == tolower(*pattern))//TOFIX tolower for case insensitive
          {
            match_ptr  = block + byte_nbr + 1;
            match_size = 1;

            do{
				/*  Loop invarients                                          */
                //ASSERT (match_size > 0    && match_size <= pattern_size);
                //ASSERT (match_ptr != NULL && match_ptr > block
                //                          && match_ptr <= block+block_size);
				//ASSERT (match_ptr == (block + byte_nbr + match_size));


                /*  If we found a match, return the start address            */
                if (match_size == pattern_size)
                    return (void*)(block + byte_nbr);

                //ASSERT (match_size < pattern_size && match_ptr  < block+block_size);
            
			}while (tolower(*match_ptr++) == tolower(pattern [match_size++]));//TOFIX tolower for case insensitive

          }

		  //ASSERT (byte_nbr + pattern_size <= block_size);
      }

    return (NULL);                      /*  Found nothing                    */
}

typedef unsigned char byte;

void * memrfind_rb (
		const void   *in_block,     /*  Block containing data            */
		const size_t block_size,    /*  Size of block in bytes           */
		const void   *in_pattern,   /*  Pattern to search for            */
		const size_t pattern_size,  /*  Size of pattern block            */
		size_t       *shift,        /*  Shift table (search buffer)      */
		bool         *repeat_find)  /*  TRUE: search buffer already init */
{
	unsigned int	byte_nbr,                       /*  Distance through block           */
		match_size,                     /*  Size of matched part             */
		limit;
    
	const byte *match_ptr = NULL;
    
	const byte
        *block     = (byte *)in_block,  /*  Concrete pointer to block data   */
        *pattern   = (byte *)in_pattern;/*  Concrete pointer to search value */

    //ASSERT (block);                     /*  Expect non-NULL pointers, but    */
    //ASSERT (pattern);                   /*  fail gracefully if not debugging */
    //ASSERT (shift);                     /*  NULL repeat_find => is false     */
    if (block == NULL || pattern == NULL || shift == NULL)
	    return (NULL);

    /*  Pattern must be smaller or equal in size to string                   */
    if (block_size < pattern_size)
        return (NULL);                  /*  Otherwise it's not found         */

    if (pattern_size == 0)              /*  Empty patterns match at start    */
        return ((void *)block);

    /*  Build the shift table unless we're continuing a previous search      */

    /*  The shift table determines how far to shift before trying to match   */
    /*  again, if a match at this point fails.  If the byte after where the  */
    /*  end of our pattern falls is not in our pattern, then we start to     */
    /*  match again after that byte; otherwise we line up the last occurence */
    /*  of that byte in our pattern under that byte, and try match again.    */

    if (!repeat_find || !*repeat_find)
      {
        for (byte_nbr = 0; byte_nbr < 256; byte_nbr++)
            shift [byte_nbr] = pattern_size + 1;
        for (byte_nbr = 0; byte_nbr < pattern_size; byte_nbr++)
            shift [(byte) tolower(pattern [byte_nbr])] = pattern_size - byte_nbr;

        if (repeat_find)
            *repeat_find = true;
      }


    /*  Search for the block, each time jumping up by the amount             */
    /*  computed in the shift table                                          */
    limit = pattern_size - 1;
    //ASSERT (limit > 0);

    for (byte_nbr = block_size - 1;
         byte_nbr > limit;
         byte_nbr -= shift [tolower(block [byte_nbr - pattern_size])] )
      {
        //ASSERT (byte_nbr >= 0 && byte_nbr < (block_size - pattern_size) + 1);

        /*  If the first byte matches, compare rest of pattern               */
        if (tolower(block [byte_nbr]) == tolower(*pattern))
          {
            match_ptr  = block + byte_nbr - 1;
            match_size = 1;

            do{
				//  Loop invarients
                //ASSERT (match_size > 0    && match_size <= pattern_size);
                //ASSERT (match_ptr != NULL && match_ptr > block
                //                          && match_ptr <= block+block_size);
				//ASSERT (match_ptr == (block + byte_nbr - match_size));

                /*  If we found a match, return the start address            */
                if (match_size == pattern_size)
                    return (void*)(match_ptr+1);

                //ASSERT (match_size < pattern_size && match_ptr  < block+block_size);
            
			}while (tolower(*match_ptr--) == tolower(pattern [match_size++]));
          }

		  //ASSERT (byte_nbr + pattern_size <= block_size);
      }

    return (NULL);                      /*  Found nothing                    */
}

int CStorage::Find(const char *szPattern, int nLen, int nStartFrom, bool bForward)
{
	if(!m_File.IsOpen())
		return -1;
	
	if(nStartFrom < 0)
		nStartFrom = 0;

	unsigned int nOffset = nStartFrom;

	size_t shift[256];  //  Shift distance for each value
	bool bShiftTableInit = false;
	void *pszRes = NULL;

	//we use overlapped regions so that we don't miss words
	//on the memory region/block borders
	const unsigned int nBlockStep = READ_CHUNK_SIZE - nLen;
	//ASSERT(nBlockStep > 0);

	if(bForward)
	{
		//read file in chunks of size READ_CHUNK_SIZE
		while(1)
		{
			FillBuffer(nOffset);
			pszRes = memfind_rb (
						m_szBuffer,
						min(m_nFileSize - nOffset, READ_CHUNK_SIZE),
						szPattern,
						nLen,
						shift,
						&bShiftTableInit);
			
			if(NULL != pszRes)
				break;

			nOffset += nBlockStep;
			if(nOffset >= m_nFileSize)
				break;
		}
	}
	else
	{
		//read file in chunks of size READ_CHUNK_SIZE
		while(1)
		{
			unsigned int nOldOffset = nOffset;
			
			nOffset	-= min(nOffset, nBlockStep);
			if(nOffset == nOldOffset)
				break;

			FillBuffer(nOffset);
			pszRes = memrfind_rb (
						m_szBuffer,
						max(nOldOffset-nOffset, 0),
						szPattern,
						nLen,
						shift,
						&bShiftTableInit);
				
			if(NULL != pszRes){
				//nOffset -= ((LPBYTE)pszRes - m_szBuffer);	//set last search offset at the start of data
				break;
			}
		}
	}

	if(NULL == pszRes)
		return -1;

	return ((LPBYTE)pszRes - m_szBuffer) + nOffset;  //return lst used offset
}

bool CStorage::IsTextMode()
{
	return (&m_fmtTxt == m_pFmt);
}

bool CStorage::IsBinMode()
{
	return (&m_fmtBin == m_pFmt);
}

bool CStorage::IsHexMode()
{
	return (&m_fmtHex == m_pFmt);
}

bool CStorage::IsUnicodeMode()
{
	return false;
	//return (&m_fmtUnicode == m_pFmt);
}

bool CStorage::FillBuffer(UINT64 nOffset)
{
	if(m_File.IsOpen())
	{
		if(nOffset < m_nFileSize)
		{
			m_File.Seek(nOffset);
			m_nBuffSize = m_File.Read((char *)m_szBuffer, READ_CHUNK_SIZE);
			m_nFileOffset = nOffset;
			return true;
		}
	}
	return false;
}

bool CStorage::SetDrawMode(int nMode)
{
	switch(nMode){
	 case DRAW_TEXT:
		if(!IsTextMode()){
			m_nDrawMode = DRAW_TEXT;
			m_pFmt = &m_fmtTxt;
			return true;
		}
		break;
	 case DRAW_HEX:
		if(!IsHexMode()){
			m_nDrawMode = DRAW_HEX;
			m_pFmt = &m_fmtHex;
			return true;
		}
		break;
	 case DRAW_BIN:
	 	if(!IsBinMode()){
			m_nDrawMode = DRAW_BIN;
			m_pFmt = &m_fmtBin;
			return true;
		}
		break;
	 case DRAW_UNICODE:
	 	if(!IsUnicodeMode()){
			m_nDrawMode = DRAW_UNICODE;
			//m_pFmt = &m_fmtUnicode;
			return true;
		}
		break;
	 //default:
		//ASSERT(false);	//invalid mode
	}

	return false;
}

int CStorage::GetViewMode()
{
	return m_nDrawMode;
} 

//check if the file is binary or unicode text (checks only first 8k of data)
void CStorage::CheckDataType()
{
	m_bBinaryFile  = false;
	m_bUnicodeText = false;

	if( m_nFileSize>0 &&
		FillBuffer(0) && 
		m_nBuffSize>0)
	{
		//check for unicode first
		if(0 == memcmp(m_szBuffer, "\xFF\xFE", 2))
			m_bUnicodeText = true;

		//check for binary
		INT64 nBinaryCount = 0;

		for(unsigned int i=0; i<m_nBuffSize; i++)
		{
			int cByte = m_szBuffer[i];
			
			if(!isprint(cByte) &&
				cByte != '\r'  &&
				cByte != '\n'  &&
				cByte != '\t')
			{
				//TRACE("Lister: Binary char 0x%x at offset 0x%x\n", cByte, i);
				nBinaryCount ++;
				//m_bBinaryFile = true;
				//break;
			}
		}

		//file is binary if at least x% of buffer characters are non-printable
#define MIN_PERCENT_BIN 2
		if(nBinaryCount > MIN_PERCENT_BIN * m_nBuffSize/100)
			m_bBinaryFile = true;
	}
}

void CStorage::Close()
{
	if(m_File.IsOpen())
		m_File.Close();

	m_nFileSize		= 0;
	m_nFileOffset	= 0;
	m_bBinaryFile	= false;
	m_bUnicodeText	= false;
	m_strFilePath.Empty();

	if(NULL != m_pThread){
		m_fmtTxt.m_bAbortThread = true;
		m_pThread->Wait();
		delete m_pThread;
		m_pThread = NULL;
	}

}

bool CStorage::IsOpen()
{
	return m_File.IsOpen();
}
