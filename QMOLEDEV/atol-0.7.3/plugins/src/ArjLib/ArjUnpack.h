////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implements .arj archive unpacking
////////////////////////////////////////////////////////////////////////////

#ifndef ARJUNPACK_H__
#define ARJUNPACK_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#if _MSC_VER > 1000
 #pragma warning (disable : 4786)
#endif 

#include "unarj.h"
#ifdef _WIN32
 #include <windows.h>
#endif
#include <string>
#include "../plugin_defs.h"

class CArjUnpack  
{
public:
	CArjUnpack();
	virtual ~CArjUnpack();

	bool Open(const char *szArchive);
	void Close();

	void ResetEntry();
	bool NextEntry();
	bool Unpack(const char *szEntry, const char *szOutFile);

	const char * GetEntryName(){ return filename; }
	int			 GetEntrySize(){ return origsize; }

	//progress support
	tProcessDataProc m_pfnProgress;
	std::string m_strEntry;
	long m_dwUserData;

protected:
	bool m_bFirstEntry;
	char m_szOutName[FNAME_MAX];

protected:
	int  extract(const char *szOutFile = NULL);

	void  make_crctable(void);
	void  crc_buf(char *str, int len);
	void  strparity(uchar *p);
	FILE  *fopen_msg(char *name, char *mode);
	int   fget_byte(FILE *f);
	uint  fget_word(FILE *f);
	ulong fget_longword(FILE *f);
	void  fread_crc(uchar *p, int n, FILE *f);
	void  decode_path(char *name);
	void  get_date_str(char *str, ulong tstamp);
	int   parse_path(char *pathname, char *path, char *entry);
	void  strncopy(char *to, char *from, int len);
	uint  get_word(void);
	ulong get_longword(void);
	long  find_header(FILE *fd);
	int   read_header(int first, FILE *fd, char *name);
	void  skip(void);
	void  unstore(void);
	int   check_flags(void);
	int   test(void);
	uint  ratio(long a, long b);
	void  list_start(void);
	void  list_arc(int count);
	void  execute_cmd(void);

	void   strlower OF((char *str));
	void   strupper OF((char *str));
	voidp  *malloc_msg OF((int size));
	void   disp_clock OF((void));
	void   error OF((char *fmt, char *arg));
	void   fillbuf OF((int n));
	ushort getbits OF((int n));
	void   fwrite_txt_crc OF((uchar *p, int n));
	void   init_getbits OF((void));

	//huffman decode
	void   make_table(int nchar, uchar *bitlen, int tablebits, ushort *table, int tablesize);
	void   read_pt_len(int nn, int nbit, int i_special);
	void   read_c_len(void);
	ushort decode_c(void);
	ushort decode_p(void);
	void   decode_start(void);
	short  decode_ptr(void);
	short  decode_len(void);
	void   decode OF((void));
	void   decode_f OF((void));

protected:
	UCRC   crc;
	FILE   *arcfile;
	FILE   *outfile;
	ushort bitbuf;
	long   compsize;
	long   origsize;
	uchar  subbitbuf;
	uchar  header[HEADERSIZE_MAX];
	char   arc_name[FNAME_MAX];
	int    command;
	int    bitcount;
	int    file_type;
	int    no_output;
	int    error_count;

	/* Local variables */
	char   filename[FNAME_MAX];
	char   comment[COMMENT_MAX];
	char   *hdr_filename;
	char   *hdr_comment;

	ushort headersize;
	uchar  first_hdr_size;
	uchar  arj_nbr;
	uchar  arj_x_nbr;
	uchar  host_os;
	uchar  arj_flags;
	short  method;
	uint   file_mode;
	ulong  time_stamp;
	short  entry_pos;
	ushort host_data;
	uchar  *get_ptr;
	UCRC   file_crc;
	UCRC   header_crc;

	long   first_hdr_pos;
	long   torigsize;
	long   tcompsize;

	int    clock_inx;

	UCRC   crctable[UCHAR_MAX + 1];


	//huffman decode variables
	uchar  *text;

	short  getlen;
	short  getbuf;

	ushort left[2 * NC - 1];
	ushort right[2 * NC - 1];
	uchar  c_len[NC];
	uchar  pt_len[NPT];

	ushort c_table[CTABLESIZE];
	ushort pt_table[PTABLESIZE];
	ushort blocksize;
};

#endif // ARJUNPACK_H__
