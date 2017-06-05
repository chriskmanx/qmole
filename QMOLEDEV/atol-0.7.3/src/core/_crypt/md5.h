////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implements MD5 hashing algorithm
////////////////////////////////////////////////////////////////////////////

/*
  Revision from MD5 original code, you can digest BINARY file

  Usage:
	char buffer[11] = "MyPassword";

	CMD5 md5;
	md5.MD5Init();
	md5.MD5Update(buffer,   5);	//allows updating piece by piece
	md5.MD5Update(buffer+5, 5);	//allows updating piece by piece
	md5.MD5Final();

	char *digest = md5.GetDigest();
*/

#ifndef MD5_H__INCLUDED_
#define MD5_H__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

// UINT4 defines a four byte word
typedef unsigned long int UINT4;

class CMD5 
{
public:
	// MD5 context
	typedef struct {
		unsigned long int state[4]; // state (ABCD)
		unsigned long int count[2]; // number of bits, modulo 2^64 (lsb first)
		unsigned char buffer[64];   // input buffer
	} MD5_CTX;
	
public:
	CMD5();
	~CMD5();

	void MD5Init();
	void MD5Update( unsigned char *input,		// input block
					unsigned int inputLen);		// length of input block
	void MD5Final();

	const char* GetDigest();
	const unsigned char* GetDigestBinary();		//FIX: MIRO

protected:
	void MD5Transform(UINT4 [4], unsigned char [64]);
	void Encode(unsigned char *, UINT4 *, unsigned int);
	void Decode(UINT4 *, unsigned char *, unsigned int);
	
private:
	MD5_CTX m_context;
	unsigned char m_digest[16]; //the numerical value of the digest
	char  m_digestString[33]; 
};

#endif

