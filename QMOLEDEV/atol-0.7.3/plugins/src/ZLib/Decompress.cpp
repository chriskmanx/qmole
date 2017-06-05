////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implements .Z archive unpacking
////////////////////////////////////////////////////////////////////////////

#include "Decompress.h"

/*
 * Compress - data compression program
 */

#define BITS   16
#define HSIZE  1L<<BITS

char_type magic_header[] = { "\037\235" };	/* 1F 9D */

/* Defines for third byte of header */
#define BIT_MASK	0x1f
#define BLOCK_MASK	0x80
/* Masks 0x40 and 0x20 are free.  I think 0x20 should mean that there is
   a fourth header byte (for expansion).
*/
#define INIT_BITS 9			/* initial number of bits/code */

#define tab_prefixof(i)		codetab[i]
#define tab_suffixof(i)		htab[i]
#define MAXCODE(n_bits)	((1L << (n_bits)) - 1)

/*
 * the next two codes should not be changed lightly, as they must not
 * lie within the contiguous general code space.
 */
#define FIRST	257	/* first free entry */
#define	CLEAR	256	/* table clear output code */

char_type rmask[9] = {0x00, 0x01, 0x03, 0x07, 0x0f, 0x1f, 0x3f, 0x7f, 0xff};

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#ifdef _WIN32
 #include <io.h>
#endif
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifndef min
  #define min(x,y) (((x)<(y))?(x):(y))
#endif
//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CDecompress::CDecompress()
{
	m_pArchive = NULL;
	m_pOutFile = NULL;

	maxbits = BITS;			/* user settable max # bits/code */
	maxmaxcode = 1L << BITS;	/* should NEVER generate this code */
	free_ent = 0;			/* first unused entry */
	exit_stat = 0;
	nomagic = 0;	/* Use a 3-byte magic number header, unless old file */
	zcat_flg = 0;	/* Write output on m_pOutFile, suppress messages */
	quiet = 1;		/* don't tell me about compression */
	block_compress = BLOCK_MASK;
	clear_flg = 0;
	ratio = 0;

	m_pfnProgress = NULL;
}

CDecompress::~CDecompress()
{
}

//same as real decompression except opening output file
int	 CDecompress::GetUnpackSize(const char *szArchive)
{
#define ERR_SIZE 0
	struct stat statbuf;

    if (maxbits < INIT_BITS) maxbits = INIT_BITS;
    if (maxbits > BITS) maxbits = BITS;
    maxmaxcode = 1L << maxbits;

	// Open input file
	m_pArchive = fopen(szArchive, "rb");
	if (NULL == m_pArchive)
		return ERR_SIZE;

	stat(szArchive, &statbuf);
	fsize = statbuf.st_size;

	// Check the magic number
	if (nomagic == 0) 
	{
		if ((getc(m_pArchive) != (magic_header[0] & 0xFF)) ||
			(getc(m_pArchive) != (magic_header[1] & 0xFF)))
		{
			fprintf(stderr, "%s: not in compressed format\n", szArchive);
			fclose(m_pArchive);
			m_pArchive = NULL;
			return ERR_SIZE;
		}
		maxbits = getc(m_pArchive);	/* set -b from file */
		block_compress = maxbits & BLOCK_MASK;
		maxbits &= BIT_MASK;
		maxmaxcode = 1L << maxbits;
		if(maxbits > BITS)
		{
			fprintf(stderr,	"%s: compressed with %d bits, can only handle %d bits\n", szArchive, maxbits, BITS);
			fclose(m_pArchive);
			m_pArchive = NULL;
			return ERR_SIZE;
		}
	}

    hsize = min(HSIZE, fsize); /* cannot have more codes than file size */
    htab = (char_type *)calloc(hsize, sizeof(char_type));
    if (htab == NULL)
	{
		fprintf(stderr,"Not enough memory\n");
		fclose(m_pArchive);
		m_pArchive = NULL;
		return ERR_SIZE;
	}
    codetab = (unsigned short *)calloc(hsize, sizeof(unsigned short));
    if(codetab == NULL) {
		fprintf(stderr, "Not enough memory\n");
		free(htab);
		fclose(m_pArchive);
		m_pArchive = NULL;
		return ERR_SIZE;
	}
    de_stack = (char_type *)malloc(8000);

	int nUnpackSize;
	bool bSuccess = decompress(nUnpackSize);

	fclose(m_pArchive);
	m_pArchive = NULL;

	if(!bSuccess)
		nUnpackSize = ERR_SIZE;

	free(htab);
	free(codetab);
	free(de_stack);

    return nUnpackSize;
}

/*****************************************************************
 * TAG( main )
 *
 * Algorithm from "A Technique for High Performance Data Compression",
 * Terry A. Welch, IEEE Computer Vol 17, No 6 (June 1984), pp 8-19.
 *
 * Algorithm:
 * 	Modified Lempel-Ziv method (LZW).  Basically finds common
 * substrings and replaces them with a variable size code.  This is
 * deterministic, and can be done on the fly.  Thus, the decompression
 * procedure needs no input table, but tracks the way the table was built.
 */
bool CDecompress::Decompress(const char *szArchive, const char *szOutFile)
{
	struct stat statbuf;

    if (maxbits < INIT_BITS) maxbits = INIT_BITS;
    if (maxbits > BITS) maxbits = BITS;
    maxmaxcode = 1L << maxbits;

	// Open input file
	m_pArchive = fopen(szArchive, "rb");
	if (NULL == m_pArchive)
		return false;

	stat(szArchive, &statbuf);
	fsize = statbuf.st_size;

	// Check the magic number
	if (nomagic == 0)
	{
		if ((getc(m_pArchive) != (magic_header[0] & 0xFF)) ||
			(getc(m_pArchive) != (magic_header[1] & 0xFF)))
		{
			fprintf(stderr, "%s: not in compressed format\n", szArchive);
			fclose(m_pArchive);
			m_pArchive = NULL;
			return false;
		}
		maxbits = getc(m_pArchive);	/* set -b from file */
		block_compress = maxbits & BLOCK_MASK;
		maxbits &= BIT_MASK;
		maxmaxcode = 1L << maxbits;
		if(maxbits > BITS)
		{
			fprintf(stderr,	"%s: compressed with %d bits, can only handle %d bits\n", szArchive, maxbits, BITS);
			fclose(m_pArchive);
			m_pArchive = NULL;
			return false;
		}
	}

	//struct tm *ptm =gmtime(&statbuf.st_mtime);

	// open output file
	m_pOutFile = fopen(szOutFile, "wb");
	if (NULL == m_pOutFile) 
	{
		fclose(m_pArchive);
		m_pArchive = NULL;
		return false;
	}

    hsize = min(HSIZE, fsize); /* cannot have more codes than file size */
    htab = (char_type *)calloc(hsize, sizeof(char_type));
    if (htab == NULL)
	{
		fprintf(stderr,"Not enough memory\n");
		fclose(m_pArchive);
		m_pArchive = NULL;
		fclose(m_pOutFile);
		m_pOutFile = NULL;
		return false;
	}
    codetab = (unsigned short *)calloc(hsize, sizeof(unsigned short));
    if(codetab == NULL) {
		fprintf(stderr, "Not enough memory\n");
		free(htab);
		fclose(m_pArchive);
		m_pArchive = NULL;
		fclose(m_pOutFile);
		m_pOutFile = NULL;
		return false;
	}
    de_stack = (char_type *)malloc(8000);

	int nUnpackSize;
	bool bSuccess = decompress(nUnpackSize);

	printf("finished decompressing %s\n",szOutFile);

	fclose(m_pArchive);
	m_pArchive = NULL;
	fclose(m_pOutFile);
	m_pOutFile = NULL;

	if(!bSuccess)
	#ifdef __WIN32
		DeleteFile(szOutFile);
	#else
		remove(szOutFile);
	#endif

	free(htab);
	free(codetab);
	free(de_stack);

    return true;
}


/*
 * Decompress m_pArchive to m_pOutFile.  This routine adapts to the codes in the
 * file building the "string" table on-the-fly; requiring no table to
 * be stored in the compressed file.  The tables used herein are shared
 * with those of the compress() routine.  See the definitions above.
 */

bool CDecompress::decompress(int &nUnpackSize)
{
    register char_type *stackp;
    register code_int finchar;
    register code_int code, oldcode, incode;

	nUnpackSize = 2;	//account for header size (already read)

    /*
     * As above, initialize the first 256 entries in the table.
     */
    maxcode = MAXCODE(n_bits = INIT_BITS);
    for ( code = 255; code >= 0; code-- ) {
		tab_prefixof(code) = 0;
		tab_suffixof(code) = (char_type)code;
    }
    free_ent = ((block_compress) ? FIRST : 256 );

    finchar = oldcode = getcode();
    if(oldcode == -1)	/* EOF already? */
		return true;	/* Get out of here */
	
	if(m_pOutFile){
		putc( (char)finchar, m_pOutFile );		/* first code must be 8 bits = char */
		if(ferror(m_pOutFile))		/* Crash if can't write */
			return false;
	}

    stackp = de_stack;

    while ( (code = getcode()) > -1 ) 
	{

	if ( (code == CLEAR) && block_compress ) {
	    for ( code = 255; code >= 0; code-- )
		tab_prefixof(code) = 0;
	    clear_flg = 1;
	    free_ent = FIRST - 1;
	    if ( (code = getcode ()) == -1 )	/* O, untimely death! */
			break;
	}
	incode = code;
	/*
	 * Special case for KwKwK string.
	 */
	if ( code >= free_ent ) {
        *stackp++ = finchar;
	    code = oldcode;
	}

	/*
	 * Generate output characters in reverse order
	 */
#ifdef SIGNED_COMPARE_SLOW
	while ( ((unsigned long)code) >= ((unsigned long)256) ) {
#else
	while ( code >= 256 ) {
#endif
	    *stackp++ = tab_suffixof(code);
	    code = tab_prefixof(code);
	}
	*stackp++ = finchar = tab_suffixof(code);

		/*
		 * And put them out in forward order
		 */
		do{
			if(m_pOutFile)
				putc ( *--stackp, m_pOutFile );
			else
				--stackp;

			nUnpackSize ++;
		}
		while ( stackp > de_stack );

		if(m_pOutFile && m_pfnProgress)
		{
			int nRes = m_pfnProgress(m_strEntry.c_str(), nUnpackSize, m_dwUserData);
			if(0 == nRes)
				return false; //abort requested
		}

		/*
		 * Generate the new entry.
		 */
		if ( (code=free_ent) < maxmaxcode ) {
			tab_prefixof(code) = (unsigned short)oldcode;
			tab_suffixof(code) = finchar;
			free_ent = code+1;
		}
		/*
		 * Remember previous code.
		 */
		oldcode = incode;
    }
	if(m_pOutFile){
		fflush( m_pOutFile );
		if(ferror(m_pOutFile))
			return false;
	}

	return true;
}

/*****************************************************************
 * TAG( getcode )
 *
 * Read one code from the standard input.  If EOF, return -1.
 * Inputs:
 * 	m_pArchive
 * Outputs:
 * 	code or -1 is returned.
 */

code_int CDecompress::getcode() 
{
    register code_int code;
    static long int offset = 0, size = 0;
    static char_type buf[BITS];
    register int r_off, bits;
    register char_type *bp = buf;

    if ( clear_flg > 0 || offset >= size || free_ent > maxcode ) {
	/*
	 * If the next entry will be too big for the current code
	 * size, then we must increase the size.  This implies reading
	 * a new buffer full, too.
	 */
	if ( free_ent > maxcode ) {
	    n_bits++;
	    if ( n_bits == maxbits )
		maxcode = maxmaxcode;	/* won't get any bigger now */
	    else
		maxcode = MAXCODE(n_bits);
	}
	if ( clear_flg > 0) {
    	    maxcode = MAXCODE (n_bits = INIT_BITS);
	    clear_flg = 0;
	}
	size = fread( buf, 1, n_bits, m_pArchive );
	if ( size <= 0 )
	    return -1;			/* end of file */
	offset = 0;
	/* Round size down to integral number of codes */
	size = (size << 3) - (n_bits - 1);
    }
    r_off = offset;
    bits = n_bits;
	/*
	 * Get to the first byte.
	 */
	bp += (r_off >> 3);
	r_off &= 7;
	/* Get first part (low order bits) */
#ifdef NO_UCHAR
	code = ((*bp++ >> r_off) & rmask[8 - r_off]) & 0xff;
#else
	code = (*bp++ >> r_off);
#endif /* NO_UCHAR */
	bits -= (8 - r_off);
	r_off = 8 - r_off;		/* now, offset into code word */
	/* Get any 8 bit parts in the middle (<=1 for up to 16 bits). */
	if ( bits >= 8 ) {
#ifdef NO_UCHAR
	    code |= (*bp++ & 0xff) << r_off;
#else
	    code |= *bp++ << r_off;
#endif /* NO_UCHAR */
	    r_off += 8;
	    bits -= 8;
	}
	/* high order bits. */
	code |= (*bp & rmask[bits]) << r_off;
    offset += n_bits;
/* Turbo C sign extends, so correct this bug (?) */
#if BITS > 15
    code &= 0xffff;
#endif
    return code;
}
