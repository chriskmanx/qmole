////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implements .Z archive unpacking
////////////////////////////////////////////////////////////////////////////

#ifndef DECOMPRESS_H_
#define DECOMPRESS_H_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

/*
 * compress.c - File compression ala IEEE Computer, June 1984.
 *
 * Authors:	Spencer W. Thomas	(decvax!harpo!utah-cs!utah-gr!thomas)
 *		Jim McKie		(decvax!mcvax!jim)
 *		Steve Davies		(decvax!vax135!petsd!peora!srd)
 *		Ken Turkowski		(decvax!decwrl!turtlevax!ken)
 *		James A. Woods		(decvax!ihnp4!ames!jaw)
 *		Joe Orost		(decvax!vax135!petsd!joe)
 *
 * modified for MS-DOS, decompression only, by B.D. Ripley, 1/90
 *		b.d.ripley@uk.ac.strath.vaxa
 * modified for Win32, decompression only by Siarhei Zharski 08/2000
 *      zharik@gmx.net
 */

#include "../plugin_defs.h"

#include <stdio.h>
#include <string>
#ifdef _WIN32
 #include <windows.h>
#endif

// a code_int must be able to hold 2**BITS values of type int, and also -1
typedef int		code_int;

#ifdef SIGNED_COMPARE_SLOW
 typedef unsigned long int count_int;
 typedef unsigned short int count_short;
#else
 typedef long int count_int;
#endif

#ifdef NO_UCHAR
 typedef char char_type;
#else
 typedef unsigned char char_type;
#endif /* UCHAR */


class CDecompress
{
public:
	CDecompress();
	virtual ~CDecompress();

	bool Decompress(const char *szArchive, const char *szOutFile);
	int	 GetUnpackSize(const char *szArchive);

	//progress support
	tProcessDataProc m_pfnProgress;
	std::string m_strEntry;
	long m_dwUserData;

protected:
	bool decompress(int &nUnpackSize);
	code_int getcode();

	FILE *m_pArchive;
	FILE *m_pOutFile;

	int n_bits;					/* number of bits/code */
	int maxbits;			/* user settable max # bits/code */
	code_int maxcode;			/* maximum code, given n_bits */
	code_int maxmaxcode;	/* should NEVER generate this code */

	char_type *htab;
	unsigned short *codetab;
	count_int fsize, hsize;

	char_type *de_stack;

	code_int free_ent;			/* first unused entry */
	int exit_stat;

	int nomagic;	/* Use a 3-byte magic number header, unless old file */
	int zcat_flg;	/* Write output on m_pOutFile, suppress messages */
	int quiet;		/* don't tell me about compression */

	/*
	 * block compression parameters -- after all codes are used up,
	 * and compression rate changes, start over.
	 */
	int block_compress;
	int clear_flg;
	long int ratio;
};

#endif // DECOMPRESS_H_
