// Base64.cpp: implementation of the CBase64 class.
// Author: Wes Clyburn (clyburnw@enmu.edu)
// 
// This code was practically stolen from:
// Dr. Dobb's Journal, September 1995, 
// "Mime and Internet Mail", by Tim Kientzle
//////////////////////////////////////////////////////////////////////
// WARNING: this class DOES NOT pad  with '=' as per RFC 1521
//    to the 4byte border (when encoding).
//////////////////////////////////////////////////////////////////////

#include "Base64.h"
#include "../debug.h"

// Static Member Initializers
//

// The 7-bit alphabet used to encode binary information
String CBase64::m_sBase64Alphabet = 
 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

int CBase64::m_nMask[] = { 0, 1, 3, 7, 15, 31, 63, 127, 255 };

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CBase64::CBase64()
{
}

CBase64::~CBase64()
{
}
/*
String CBase64::Encode(LPCTSTR szEncoding, int nSize)
{
	String sOutput = _T( "" );
	int nNumBits = 6;
	UINT nDigit;
	int lp = 0;

	ASSERT( szEncoding != NULL );
	if( szEncoding == NULL )
		return sOutput;
	m_szInput = szEncoding;
	m_nInputSize = nSize;

	m_nBitsRemaining = 0;
	nDigit = read_bits( nNumBits, &nNumBits, lp );
	while( nNumBits > 0 )
	{
		sOutput += m_sBase64Alphabet[ (int)nDigit ];
		nDigit = read_bits( nNumBits, &nNumBits, lp );
	}
	// Pad with '=' as per RFC 1521
	while( sOutput.Length() % 4 != 0 )
	{
		sOutput += '=';
	}
	return sOutput;
}
*/
// Matt Spaulding (1999/01/22) 
String CBase64::Encode(LPCTSTR szEncoding, int nSize)
{
	ASSERT( szEncoding != NULL );
	ASSERT( nSize > 0 );  //don't send empty strings (or empty files)
	if( szEncoding == NULL || 0 == nSize )
		return String("");
	
	char *sOutput = new char[nSize*2+1];  //lazy/safe calculation
	int nNumBits = 6;
	UINT nDigit;
	int pos=0, target=BYTES_PER_LINE, lp = 0;
	String result;
	
	m_szInput = szEncoding;
	m_nInputSize = nSize;
	
	m_nBitsRemaining = 0;
	nDigit = read_bits( nNumBits, &nNumBits, lp );
	while( nNumBits > 0 )
	{
		sOutput[pos++] = m_sBase64Alphabet[ (int)nDigit ];
		nDigit = read_bits( nNumBits, &nNumBits, lp );
		
		if(pos==target)
		{
			sOutput[pos++] = '\r';
			sOutput[pos++] = '\n';
			target=pos+BYTES_PER_LINE;       // Not including CR/LF, already handled
		}
	}
	
	sOutput[pos]=0;
	result=sOutput;
	delete [] sOutput;

	return result;
}

// The size of the output buffer must not be less than
// 3/4 the size of the input buffer. For simplicity,
// make them the same size.
int CBase64::Decode(LPCTSTR szDecoding, LPTSTR szOutput)
{
	String sInput;
	int c, lp =0;
	int nDigit;
	int nDecode[ 256 ];

	ASSERT( szDecoding != NULL );
	ASSERT( szOutput != NULL );
	if( szOutput == NULL )
		return 0;
	if( szDecoding == NULL )
		return 0;
	sInput = szDecoding;
	if( sInput.Length() == 0 )
		return 0;

	// Build Decode Table
	//
	int i;
	for( i = 0; i < 256; i++ ) 
		nDecode[i] = -2; // Illegal digit
	for( i=0; i < 64; i++ )
	{
		nDecode[ (int) m_sBase64Alphabet[ i ] ] = i;
		nDecode[ m_sBase64Alphabet[ i ] | 0x80 ] = i; // Ignore 8th bit
		nDecode[ (int)'=' ] = -1; 
		nDecode[ '=' | 0x80 ] = -1; // Ignore MIME padding char
	}

	// Clear the output buffer
	memset( szOutput, 0, sInput.Length() + 1 );
	
	m_lBitStorage = 0;		//(IVO) somebody failed to initialize
	m_nBitsRemaining =0;	//(IVO) 

	// Decode the Input
	//
	for( lp = 0, i = 0; lp < sInput.Length(); lp++ )
	{
		c = sInput[ lp ];
		nDigit = nDecode[ c & 0x7F ];
		if( nDigit < -1 ) 
		{
			return 0;
		} 
		else if( nDigit >= 0 ) 
			// i (index into output) is incremented by write_bits()
			write_bits( nDigit & 0x3F, 6, szOutput, i );
    }	
	return i;
}


UINT CBase64::read_bits(int nNumBits, int * pBitsRead, int& lp)
{
    ULONG lScratch;
    while( ( m_nBitsRemaining < nNumBits ) && 
		   ( lp < m_nInputSize ) ) 
	{
		int c = m_szInput[ lp++ ];
        m_lBitStorage <<= 8;
        m_lBitStorage |= (c & 0xff);
		m_nBitsRemaining += 8;
    }
    if( m_nBitsRemaining < nNumBits ) 
	{
		lScratch = m_lBitStorage << ( nNumBits - m_nBitsRemaining );
		*pBitsRead = m_nBitsRemaining;
		m_nBitsRemaining = 0;
    } 
	else 
	{
		lScratch = m_lBitStorage >> ( m_nBitsRemaining - nNumBits );
		*pBitsRead = nNumBits;
		m_nBitsRemaining -= nNumBits;
    }
    return (UINT)lScratch & m_nMask[nNumBits];
}


void CBase64::write_bits(UINT nBits,
						 int nNumBits,
						 LPTSTR szOutput,
						 int& i)
{
	UINT nScratch;

	m_lBitStorage = (m_lBitStorage << nNumBits) | nBits;
	m_nBitsRemaining += nNumBits;
	while( m_nBitsRemaining > 7 ) 
	{
		nScratch = m_lBitStorage >> (m_nBitsRemaining - 8);
		szOutput[ i++ ] = nScratch & 0xFF;
		m_nBitsRemaining -= 8;
	}
}

