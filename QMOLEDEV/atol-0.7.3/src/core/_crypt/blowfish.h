////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: C++ class implementation of the BLOWFISH encryption algorithm
//       Important: encode/decode buffer size should be divisible with 8 (blowfish block size)
////////////////////////////////////////////////////////////////////////////

// _THE BLOWFISH ENCRYPTION ALGORITHM_
// by Bruce Schneier
// Revised code--3/20/94
// Converted to C++ class 5/96, Jim Conger
// - fixed deleting array in destructor (Miroslav Rajcic)
// - fixed DWORD definition (was invalid for 64bit build) (Miroslav Rajcic)

#ifndef BLOWFISH_H_INCLUDED_
#define BLOWFISH_H_INCLUDED_

#define MAXKEYBYTES 	56		// 448 bits max
#define NPASS           16		// SBox passes

#define DWORD  		unsigned int
#define WORD  		unsigned short
#define BYTE  		unsigned char

class CBlowFish
{
public:
	CBlowFish ();
	~CBlowFish ();

	void 		Initialize (BYTE key[], int keybytes);
	DWORD		GetOutputLength (DWORD lInputLong);
	DWORD		Encode (BYTE * pInput, BYTE * pOutput, DWORD lSize);
	void		Decode (BYTE * pInput, BYTE * pOutput, DWORD lSize);
	int			GetBlockSize(){ return 8; };

private:
	DWORD 		* PArray;
	DWORD		(* SBoxes)[256];
	void 		Blowfish_encipher (DWORD *xl, DWORD *xr);
	void 		Blowfish_decipher (DWORD *xl, DWORD *xr);
};

#endif //BLOWFISH_H_INCLUDED_

