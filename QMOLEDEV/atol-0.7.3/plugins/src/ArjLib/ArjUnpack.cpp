////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implements .arj archive unpacking
////////////////////////////////////////////////////////////////////////////

#include "ArjUnpack.h"

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <string>

bool MatchPaths(const char *szPath1, const char *szPath2);

static	char   *writemode[2]  = { "wb",  "w" };

#define get_crc()       get_longword()
#define fget_crc(f)     fget_longword(f)

#define setup_get(PTR)  (get_ptr = (PTR))
#define get_byte()      ((uchar)(*get_ptr++ & 0xff))

#define BUFFERSIZE      4096
#define ASCII_MASK      0x7F
#define CRCPOLY         0xEDB88320L
#define UPDATE_CRC(r,c) r=crctable[((uchar)(r)^(uchar)(c))&0xff]^(r>>CHAR_BIT)

/* Local functions */

//unarj.c

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CArjUnpack::CArjUnpack()
{
	text = NULL;
}

CArjUnpack::~CArjUnpack()
{
}

bool CArjUnpack::Open(const char *szArchive)
{
	strncpy(arc_name, szArchive, sizeof(arc_name));
    arcfile = fopen_msg(arc_name, "rb");
	return (NULL != arcfile);
}

void CArjUnpack::Close()
{
	if(arcfile)
		fclose(arcfile);
	arcfile = NULL;
}

void CArjUnpack::ResetEntry()
{
	m_bFirstEntry = true;
}

bool CArjUnpack::NextEntry()
{
	if(m_bFirstEntry)
	{
		m_bFirstEntry = false;

		file_seek(arcfile, 0, SEEK_SET);

		first_hdr_pos = find_header(arcfile);
		if (first_hdr_pos < 0)
			return false; //error(M_NOTARJ, arc_name);

		file_seek(arcfile, first_hdr_pos, SEEK_SET);
		if (!read_header(1, arcfile, arc_name))
			return false; //error(M_BADCOMNT, "");
	}
	else
		skip();	//jump to the start of next header

	if(read_header(0, arcfile, arc_name))
		return true;

	return false;
}

bool CArjUnpack::Unpack(const char *szEntry, const char *szOutFile)
{
	//find correct entry to unpack
	bool bFound = false;
	ResetEntry();
	while(NextEntry())
	{
		if(MatchPaths(szEntry, GetEntryName()))
		{
			bFound = true;
			break;
		}
	}

	//not found
	if(!bFound)
		return false;

	//now start extracting
	if(extract(szOutFile))
		return true;

	return false;
}

//unarj.c

int CArjUnpack::extract(const char *szOutFile)
{
    char name[FNAME_MAX];

    if (check_flags())
    {
        error_count++;
        return 0;
    }

    no_output = 0;

	if(!szOutFile)
	{
		if (command == 'E')
			strcpy(name, &filename[entry_pos]);
		else
		{
			strcpy(name, DEFAULT_DIR);
			strcat(name, filename);
		}

		if (host_os != OS)
			default_case_path(name);

		if (file_exists(name))
		{
			printf(M_FEXISTS, name);
			printf(M_SKIPPED, name);
			skip();
			error_count++;
			return 0;
		}

		szOutFile = name;
	}

    outfile = file_open((char *)szOutFile, writemode[file_type & 1]);
    if (outfile == NULL)
    {
        printf(M_CANTOPEN, name);
        putchar('\n');
        skip();
        error_count++;
        return 0;
    }

	//store file name
	strcpy(m_szOutName, szOutFile);

    printf(M_EXTRACT, name);
    if (host_os != OS && file_type == BINARY_TYPE)
        printf(M_DIFFHOST);
    printf("  ");

    crc = CRC_MASK;

    if (method == 0)
        unstore();
    else if (method == 1 || method == 2 || method == 3)
        decode();
    else if (method == 4)
        decode_f();
	if(outfile)
		fclose(outfile);

    set_ftime_mode(name, time_stamp, file_mode, (uint) host_os);

    if ((crc ^ CRC_MASK) == file_crc)
        printf(M_CRCOK);
    else
    {
        printf(M_CRCERROR);
        error_count++;
    }
    return 1;
}


/* Functions */

void CArjUnpack::make_crctable()
{
    uint i, j;
    UCRC r;

    for (i = 0; i <= UCHAR_MAX; i++)
    {
        r = i;
        for (j = CHAR_BIT; j > 0; j--)
        {
            if (r & 1)
                r = (r >> 1) ^ CRCPOLY;
            else
                r >>= 1;
        }
        crctable[i] = r;
    }
}

void CArjUnpack::crc_buf(char *str, int len)
{
    while (len--)
        UPDATE_CRC(crc, *str++);
}

void CArjUnpack::disp_clock()
{
    //static char clock_str[4] = { '|', '/', '-', '\\' };
    //printf("(%c)\b\b\b", clock_str[clock_inx]);
    //clock_inx = (clock_inx + 1) & 0x03;

	//progress report
	if(m_pfnProgress)
	{
		int nRes = m_pfnProgress(m_strEntry.c_str(), ftell(outfile), m_dwUserData);
		if(0 == nRes)	//abort request
		{
			fclose(arcfile);
			arcfile = NULL;

			//close and remove partial file
			fclose(outfile);
			outfile = NULL;
			remove(m_szOutName);
		}
	}
}

void CArjUnpack::error(char *fmt, char *arg)
{
    putc('\n', stdout);
    printf(fmt, arg, error_count);
    putc('\n', stdout);
//    exit(EXIT_FAILURE);
}

void CArjUnpack::strparity(uchar *p)
{
    while (*p)
    {
        FIX_PARITY(*p);
        p++;
    }
}

FILE *CArjUnpack::fopen_msg(char *name, char *mode)
{
    FILE *fd;

    fd = file_open(name, mode);
    if (fd == NULL)
        return NULL;//error(M_CANTOPEN, name);
    return fd;
}

int CArjUnpack::fget_byte(FILE *f)
{
    int c;
    if ((c = getc(f)) == EOF)
        return EOF;//error(M_CANTREAD, "");
    return c & 0xFF;
}

uint CArjUnpack::fget_word(FILE *f)
{
    uint b0, b1;

    b0 = fget_byte(f);
    b1 = fget_byte(f);
    return (b1 << 8) + b0;
}

ulong CArjUnpack::fget_longword(FILE *f)
{
    ulong b0, b1, b2, b3;

    b0 = fget_byte(f);
    b1 = fget_byte(f);
    b2 = fget_byte(f);
    b3 = fget_byte(f);
    return (b3 << 24) + (b2 << 16) + (b1 << 8) + b0;
}

void CArjUnpack::fread_crc(uchar *p, int n, FILE *f)
{
    n = file_read((char *)p, 1, n, f);
    origsize += n;
    crc_buf((char *)p, n);
}

void CArjUnpack::fwrite_txt_crc(uchar *p, int n)
{
    uchar c;

    crc_buf((char *)p, n);
    if (no_output)
        return;

    if (file_type == TEXT_TYPE)
    {
        while (n--)
        {
            c = *p++;
            if (host_os != OS)
            {
                FIX_PARITY(c);
            }
            if (putc((int) c, outfile) == EOF)
                return; //error(M_CANTWRIT, "");
        }
    }
    else
    {
        if (file_write((char *)p, 1, n, outfile) != n)
            return; //error(M_CANTWRIT, "");
    }
}

void CArjUnpack::init_getbits()
{
    bitbuf = 0;
    subbitbuf = 0;
    bitcount = 0;
    fillbuf(2 * CHAR_BIT);
}

void CArjUnpack::fillbuf(int n)  /* Shift bitbuf n bits left, read n bits */
{
    bitbuf = (bitbuf << n) & 0xFFFF;  /* lose the first n bits */
    while (n > bitcount)
    {
        bitbuf |= subbitbuf << (n -= bitcount);
        if (compsize != 0)
        {
            compsize--;
            subbitbuf = (uchar) getc(arcfile);
        }
        else
            subbitbuf = 0;
        bitcount = CHAR_BIT;
    }
    bitbuf |= subbitbuf >> (bitcount -= n);
}

ushort CArjUnpack::getbits(int n)
{
    ushort x;

    x = bitbuf >> (2 * CHAR_BIT - n);
    fillbuf(n);
    return x;
}

void CArjUnpack::decode_path(char *name)
{
    for ( ; *name; name++)
    {
        if (*name == ARJ_PATH_CHAR)
            *name = PATH_CHAR;
    }
}

void CArjUnpack::get_date_str(char *str, ulong tstamp)
{
    sprintf(str, "%04u-%02u-%02u %02u:%02u:%02u",
           ts_year(tstamp), ts_month(tstamp), ts_day(tstamp),
           ts_hour(tstamp), ts_min(tstamp), ts_sec(tstamp));
}

int CArjUnpack::parse_path(char *pathname, char *path, char *entry)
{
    char *cptr, *ptr, *fptr;
    short pos;

    fptr = NULL;
    for (cptr = PATH_SEPARATORS; *cptr; cptr++)
    {
        if ((ptr = strrchr(pathname, *cptr)) != NULL &&
                (fptr == NULL || ptr > fptr))
            fptr = ptr;
    }
    if (fptr == NULL)
        pos = 0;
    else
        pos = fptr + 1 - pathname;
    if (path != NULL)
    {
       strncpy(path, pathname, pos);
       path[pos] = NULL_CHAR;
    }
    if (entry != NULL)
       strcpy(entry, &pathname[pos]);
    return pos;
}

void CArjUnpack::strncopy(char *to, char *from, int len)
{
    int i;

    for (i = 1; i < len && *from; i++)
        *to++ = *from++;
    *to = NULL_CHAR;
}

void CArjUnpack::strlower(char *s)
{
    while (*s)
    {
        *s = (char) tolower(*s);
        s++;
    }
}

void CArjUnpack::strupper(char *s)
{
    while (*s)
    {
        *s = (char) toupper(*s);
        s++;
    }
}

voidp *CArjUnpack::malloc_msg(int size)
{
    char *p;

    if ((p = (char *)xmalloc(size)) == NULL)
        return NULL; //error(M_NOMEMORY, "");
    return (voidp *)p;
}

uint CArjUnpack::get_word()
{
    uint b0, b1;

    b0 = get_byte();
    b1 = get_byte();
    return (b1 << 8) + b0;
}

ulong CArjUnpack::get_longword()
{
    ulong b0, b1, b2, b3;

    b0 = get_byte();
    b1 = get_byte();
    b2 = get_byte();
    b3 = get_byte();
    return (b3 << 24) + (b2 << 16) + (b1 << 8) + b0;
}

long CArjUnpack::find_header(FILE *fd)
{
    long arcpos, lastpos;
    int c;

    arcpos = file_tell(fd);
    file_seek(fd, 0L, SEEK_END);
    lastpos = file_tell(fd) - 2;
    if (lastpos > MAXSFX)
        lastpos = MAXSFX;
    for ( ; arcpos < lastpos; arcpos++)
    {
        file_seek(fd, arcpos, SEEK_SET);
        c = fget_byte(fd);
        while (arcpos < lastpos)
        {
            if (c != HEADER_ID_LO)  /* low order first */
                c = fget_byte(fd);
            else if ((c = fget_byte(fd)) == HEADER_ID_HI)
                break;
            arcpos++;
        }
        if (arcpos >= lastpos)
            break;
        if ((headersize = fget_word(fd)) <= HEADERSIZE_MAX)
        {
            crc = CRC_MASK;
            fread_crc(header, (int) headersize, fd);
            //if ((crc ^ CRC_MASK) == fget_crc(fd))
            {
                file_seek(fd, arcpos, SEEK_SET);
                return arcpos;
            }
        }
    }
    return -1;          /* could not find a valid header */
}

int CArjUnpack::read_header(int first, FILE *fd, char *name)
{
    ushort extheadersize, header_id;

    header_id = fget_word(fd);
    if (header_id != HEADER_ID)
    {
        if (first)
            return 0;//error(M_NOTARJ, name);
        else
            return 0;//error(M_BADHEADR, "");
    }

    headersize = fget_word(fd);
    if (headersize == 0)
        return 0;               /* end of archive */
    if (headersize > HEADERSIZE_MAX)
        return 0;//error(M_BADHEADR, "");

    crc = CRC_MASK;
    fread_crc(header, (int) headersize, fd);
    header_crc = fget_crc(fd);
    //if ((crc ^ CRC_MASK) != header_crc)
    //    return 0; //error(M_HEADRCRC, "");

    setup_get(header);
    first_hdr_size = get_byte();
    arj_nbr = get_byte();
    arj_x_nbr = get_byte();
    host_os = get_byte();
    arj_flags = get_byte();
    method = get_byte();
    file_type = get_byte();
    (void)get_byte();
    time_stamp = get_longword();
    compsize = get_longword();
    origsize = get_longword();
    file_crc = get_crc();
    entry_pos = get_word();
    file_mode = get_word();
    host_data = get_word();

    if (origsize < 0 || compsize < 0)
        return 0;//error(M_HEADRCRC, "");

    hdr_filename = (char *)&header[first_hdr_size];
    strncopy(filename, hdr_filename, sizeof(filename));
    if (host_os != OS)
        strparity((uchar *)filename);
    if ((arj_flags & PATHSYM_FLAG) != 0)
        decode_path(filename);

    hdr_comment = (char *)&header[first_hdr_size + strlen(hdr_filename) + 1];
    strncopy(comment, hdr_comment, sizeof(comment));
    if (host_os != OS)
        strparity((uchar *)comment);

    /* if extheadersize == 0 then no CRC */
    /* otherwise read extheader data and read 4 bytes for CRC */

    while ((extheadersize = fget_word(fd)) != 0)
        file_seek(fd, (long) (extheadersize + 4), SEEK_CUR);

    return 1;                   /* success */
}

void CArjUnpack::skip()
{
    file_seek(arcfile, compsize, SEEK_CUR);
}

void CArjUnpack::unstore()
{
    int n;
    long pos;
    char *buffer;

    buffer = (char *)malloc_msg(BUFFERSIZE);
    pos = file_tell(arcfile);
    disp_clock();
    n = (int)(BUFFERSIZE - (pos % BUFFERSIZE));
    n = compsize > (long)n ? n : (int)compsize;
    while (compsize > 0)
    {
        if (file_read(buffer, 1, n, arcfile) != n)
            return;//error(M_CANTREAD, "");
        disp_clock();
        compsize -= n;
        fwrite_txt_crc((uchar *)buffer, n);
        n = compsize > BUFFERSIZE ? BUFFERSIZE : (int)compsize;
    }
    free(buffer);
}

int CArjUnpack::check_flags()
{
    if (arj_x_nbr > ARJ_X_VERSION)
    {
        printf(M_UNKNVERS, arj_x_nbr);
        printf(M_SKIPPED, filename);
        skip();
        return -1;
    }
    if ((arj_flags & GARBLE_FLAG) != 0)
    {
        printf(M_ENCRYPT);
        printf(M_SKIPPED, filename);
        skip();
        return -1;
    }
    if (method < 0 || method > MAXMETHOD || (method == 4 && arj_nbr == 1))
    {
        printf(M_UNKNMETH, method);
        printf(M_SKIPPED, filename);
        skip();
        return -1;
    }
    if (file_type != BINARY_TYPE && file_type != TEXT_TYPE)
    {
        printf(M_UNKNTYPE, file_type);
        printf(M_SKIPPED, filename);
        skip();
        return -1;
    }
    return 0;
}

int CArjUnpack::test()
{
    if (check_flags())
        return 0;

    no_output = 1;
    printf(M_TESTING, filename);
    printf("  ");

    crc = CRC_MASK;

    if (method == 0)
        unstore();
    else if (method == 1 || method == 2 || method == 3)
        decode();
    else if (method == 4)
        decode_f();

    if ((crc ^ CRC_MASK) == file_crc)
        printf(M_CRCOK);
    else
    {
        printf(M_CRCERROR);
        error_count++;
    }
    return 1;
}

uint CArjUnpack::ratio(long a, long b)
{
   int i;

   for (i = 0; i < 3; i++)
       if (a <= LONG_MAX / 10)
           a *= 10;
       else
           b /= 10;
   if ((long) (a + (b >> 1)) < a)
   {
       a >>= 1;
       b >>= 1;
   }
   if (b == 0)
       return 0;
   return (uint) ((a + (b >> 1)) / b);
}

void CArjUnpack::list_start()
{
    printf("Filename       Original Compressed Ratio DateTime modified CRC-32   AttrBTPMGVX\n");
    printf("------------ ---------- ---------- ----- ----------------- -------- -----------\n");
}

void CArjUnpack::list_arc(int count)
{
    uint r;
    int garble_mode, path_mode, volume_mode, extfil_mode, ftype, bckf_mode;
    char date_str[20], fmode_str[10];
    static char mode[5] = { 'B', 'T', '?', 'D', 'V' };
    static char pthf[2] = { ' ', '+' };
    static char pwdf[2] = { ' ', 'G' };  /* plain, encrypted */
    static char volf[2] = { ' ', 'V' };
    static char extf[2] = { ' ', 'X' };
    static char bckf[2] = { ' ', '*' };

    if (count == 0)
        list_start();

    garble_mode = ((arj_flags & GARBLE_FLAG) != 0);
    volume_mode = ((arj_flags & VOLUME_FLAG) != 0);
    extfil_mode = ((arj_flags & EXTFILE_FLAG) != 0);
    bckf_mode   = ((arj_flags & BACKUP_FLAG) != 0);
    path_mode   = (entry_pos > 0);
    r = ratio(compsize, origsize);
    torigsize += origsize;
    tcompsize += compsize;
    ftype = file_type;
    if (ftype != BINARY_TYPE && ftype != TEXT_TYPE && ftype != DIR_TYPE &&
            ftype != LABEL_TYPE)
        ftype = 3;
    get_date_str(date_str, time_stamp);
    strcpy(fmode_str, "    ");
    if (host_os == OS)
        get_mode_str(fmode_str, (uint) file_mode);
    if (strlen(&filename[entry_pos]) > 12)
        printf("%-12s\n             ", &filename[entry_pos]);
    else
        printf("%-12s ", &filename[entry_pos]);
    printf("%10ld %10ld %u.%03u %s %08lX %4s%c%c%c%u%c%c%c\n",
        origsize, compsize, r / 1000, r % 1000, &date_str[2], file_crc,
        fmode_str, bckf[bckf_mode], mode[ftype], pthf[path_mode], method,
        pwdf[garble_mode], volf[volume_mode], extf[extfil_mode]);
}

void CArjUnpack::execute_cmd()
{
    int file_count;
    char date_str[22];
    uint r;

    first_hdr_pos = 0;
    time_stamp = 0;
    first_hdr_size = FIRST_HDR_SIZE;

    //arcfile = fopen_msg(arc_name, "rb");

    printf(M_PROCARC, arc_name);

    first_hdr_pos = find_header(arcfile);
    if (first_hdr_pos < 0)
        return;//error(M_NOTARJ, arc_name);
    file_seek(arcfile, first_hdr_pos, SEEK_SET);
    if (!read_header(1, arcfile, arc_name))
        return;//error(M_BADCOMNT, "");
    get_date_str(date_str, time_stamp);
    printf(M_ARCDATE, date_str);
    if (arj_nbr >= ARJ_M_VERSION)
    {
        get_date_str(date_str, (ulong) compsize);
        printf(M_ARCDATEM, date_str);
    }
    printf("\n");

    file_count = 0;
    while (read_header(0, arcfile, arc_name))
    {
        switch (command)
        {
        case 'E':
        case 'X':
            if (extract())
                file_count++;
            break;
        case 'L':
            list_arc(file_count++);
            skip();
            break;
        case 'T':
            if (test())
                file_count++;
            break;
        }
    }

    if (command == 'L')
    {
        printf("------------ ---------- ---------- ----- -----------------\n");
        r = ratio(tcompsize, torigsize);
        printf(" %5d files %10ld %10ld %u.%03u %s\n",
            file_count, torigsize, tcompsize, r / 1000, r % 1000, &date_str[2]);
    }
    else
        printf(M_NBRFILES, file_count);

    fclose(arcfile);
}

// decode.c - huffman decode

/* Huffman decode routines */

void CArjUnpack::make_table(int nchar, uchar  *bitlen, int tablebits, ushort *table, int tablesize)
{
    ushort count[17], weight[17], start[18], *p;
    uint i, k, len, ch, jutbits, avail, nextcode, mask;

    for (i = 1; i <= 16; i++)
        count[i] = 0;
    for (i = 0; (int)i < nchar; i++)
        count[bitlen[i]]++;

    start[1] = 0;
    for (i = 1; i <= 16; i++)
        start[i + 1] = start[i] + (count[i] << (16 - i));
    if (start[17] != (ushort) (1 << 16))
        return;//error(M_BADTABLE, "");

    jutbits = 16 - tablebits;
    for (i = 1; (int)i <= tablebits; i++)
    {
        start[i] >>= jutbits;
        weight[i] = 1 << (tablebits - i);
    }
    while (i <= 16)
    {
        weight[i] = 1 << (16 - i);
        i++;
    }

    i = start[tablebits + 1] >> jutbits;
    if (i != (ushort) (1 << 16))
    {
        k = 1 << tablebits;
        while (i != k)
            table[i++] = 0;
    }

    avail = nchar;
    mask = 1 << (15 - tablebits);
    for (ch = 0; (int)ch < nchar; ch++)
    {
        if ((len = bitlen[ch]) == 0)
            continue;
        k = start[len];
        nextcode = k + weight[len];
        if ((int)len <= tablebits)
        {
            if (nextcode > (uint)tablesize)
                return;//error(M_BADTABLE, "");
            for (i = start[len]; i < nextcode; i++)
                table[i] = ch;
        }
        else
        {
            p = &table[k >> jutbits];
            i = len - tablebits;
            while (i != 0)
            {
                if (*p == 0)
                {
                    right[avail] = left[avail] = 0;
                    *p = avail++;
                }
                if (k & mask)
                    p = &right[*p];
                else
                    p = &left[*p];
                k <<= 1;
                i--;
            }
            *p = ch;
        }
        start[len] = nextcode;
    }
}

void CArjUnpack::read_pt_len(int nn, int nbit, int i_special)
{
    int i, n;
    short c;
    ushort mask;

    n = getbits(nbit);
    if (n == 0)
    {
        c = getbits(nbit);
        for (i = 0; i < nn; i++)
            pt_len[i] = 0;
        for (i = 0; i < 256; i++)
            pt_table[i] = c;
    }
    else
    {
        i = 0;
        while (i < n)
        {
            c = bitbuf >> (13);
            if (c == 7)
            {
                mask = 1 << (12);
                while (mask & bitbuf)
                {
                    mask >>= 1;
                    c++;
                }
            }
            fillbuf((c < 7) ? 3 : (int)(c - 3));
            pt_len[i++] = (uchar)c;
            if (i == i_special)
            {
                c = getbits(2);
                while (--c >= 0)
                    pt_len[i++] = 0;
            }
        }
        while (i < nn)
            pt_len[i++] = 0;
        make_table(nn, pt_len, 8, pt_table, PTABLESIZE);  /* replaced sizeof */
    }
}

void CArjUnpack::read_c_len()
{
    short i, c, n;
    ushort mask;

    n = getbits(CBIT);
    if (n == 0)
    {
        c = getbits(CBIT);
        for (i = 0; i < NC; i++)
            c_len[i] = 0;
        for (i = 0; i < CTABLESIZE; i++)
            c_table[i] = c;
    }
    else
    {
        i = 0;
        while (i < n)
        {
            c = pt_table[bitbuf >> (8)];
            if (c >= NT)
            {
                mask = 1 << (7);
                do
                {
                    if (bitbuf & mask)
                        c = right[c];
                    else
                        c = left[c];
                    mask >>= 1;
                } while (c >= NT);
            }
            fillbuf((int)(pt_len[c]));
            if (c <= 2)
            {
                if (c == 0)
                    c = 1;
                else if (c == 1)
                    c = getbits(4) + 3;
                else
                    c = getbits(CBIT) + 20;
                while (--c >= 0)
                    c_len[i++] = 0;
            }
            else
                c_len[i++] = (uchar)(c - 2);
        }
        while (i < NC)
            c_len[i++] = 0;
        make_table(NC, c_len, 12, c_table, CTABLESIZE);  /* replaced sizeof */
    }
}

ushort CArjUnpack::decode_c()
{
    ushort j, mask;

    if (blocksize == 0)
    {
        blocksize = getbits(16);
        read_pt_len(NT, TBIT, 3);
        read_c_len();
        read_pt_len(NP, PBIT, -1);
    }
    blocksize--;
    j = c_table[bitbuf >> 4];
    if (j >= NC)
    {
        mask = 1 << (3);
        do
        {
            if (bitbuf & mask)
                j = right[j];
            else
                j = left[j];
            mask >>= 1;
        } while (j >= NC);
    }
    fillbuf((int)(c_len[j]));
    return j;
}

ushort CArjUnpack::decode_p()
{
    ushort j, mask;

    j = pt_table[bitbuf >> (8)];
    if (j >= NP)
    {
        mask = 1 << (7);
        do
        {
            if (bitbuf & mask)
                j = right[j];
            else
                j = left[j];
            mask >>= 1;
        } while (j >= NP);
    }
    fillbuf((int)(pt_len[j]));
    if (j != 0)
    {
        j--;
        j = (1 << j) + getbits((int)j);
    }
    return j;
}

void CArjUnpack::decode_start()
{
    blocksize = 0;
    init_getbits();
}

void CArjUnpack::decode()
{
    short i;
    short j;
    short c;
    short r;
    long count;

#ifdef KEEP_WINDOW
    if (text == (uchar *) NULL)
        text = (uchar *)malloc_msg(DDICSIZ);
#else
    text = (uchar *)malloc_msg(DDICSIZ);
#endif

    disp_clock();
    decode_start();
    count = 0;
    r = 0;

    while (count < origsize)
    {
        if ((c = decode_c()) <= UCHAR_MAX)
        {
            text[r] = (uchar) c;
            count++;
            if (++r >= DDICSIZ)
            {
                r = 0;
                disp_clock();
                fwrite_txt_crc(text, DDICSIZ);
            }
        }
        else
        {
            j = c - (UCHAR_MAX + 1 - THRESHOLD);
            count += j;
            i = decode_p();
            if ((i = r - i - 1) < 0)
                i += DDICSIZ;
            if (r > i && r < DDICSIZ - MAXMATCH - 1)
            {
                while (--j >= 0)
                    text[r++] = text[i++];
            }
            else
            {
                while (--j >= 0)
                {
                    text[r] = text[i];
                    if (++r >= DDICSIZ)
                    {
                        r = 0;
                        disp_clock();
                        fwrite_txt_crc(text, DDICSIZ);
                    }
                    if (++i >= DDICSIZ)
                        i = 0;
                }
            }
        }
    }
    if (r != 0)
        fwrite_txt_crc(text, r);

#ifndef KEEP_WINDOW
    free((char *)text);
#endif
}

/* Macros */

#define BFIL {getbuf|=bitbuf>>getlen;fillbuf(CODE_BIT-getlen);getlen=CODE_BIT;}
#define GETBIT(c) {if(getlen<=0)BFIL c=(getbuf&0x8000)!=0;getbuf<<=1;getlen--;}
#define BPUL(l) {getbuf<<=l;getlen-=l;}
#define GETBITS(c,l) {if(getlen<l)BFIL c=(ushort)getbuf>>(CODE_BIT-l);BPUL(l)}

short CArjUnpack::decode_ptr()
{
    short c;
    short width;
    short plus;
    short pwr;

    plus = 0;
    pwr = 1 << (STRTP);
    for (width = (STRTP); width < (STOPP) ; width++)
    {
        GETBIT(c);
        if (c == 0)
            break;
        plus += pwr;
        pwr <<= 1;
    }
    if (width != 0)
        GETBITS(c, width);
    c += plus;
    return c;
}

short CArjUnpack::decode_len()
{
    short c;
    short width;
    short plus;
    short pwr;

    plus = 0;
    pwr = 1 << (STRTL);
    for (width = (STRTL); width < (STOPL) ; width++)
    {
        GETBIT(c);
        if (c == 0)
            break;
        plus += pwr;
        pwr <<= 1;
    }
    if (width != 0)
        GETBITS(c, width);
    c += plus;
    return c;
}

void CArjUnpack::decode_f()
{
    short i;
    short j;
    short c;
    short r;
    short pos;
    long count;

#ifdef KEEP_WINDOW
    if (text == (uchar *) NULL)
        text = (uchar *)malloc_msg(DDICSIZ);
#else
    text = (uchar *)malloc_msg(DDICSIZ);
#endif

    disp_clock();
    init_getbits();
    getlen = getbuf = 0;
    count = 0;
    r = 0;

    while (count < origsize)
    {
        c = decode_len();
        if (c == 0)
        {
            GETBITS(c, CHAR_BIT);
            text[r] = (uchar)c;
            count++;
            if (++r >= DDICSIZ)
            {
                r = 0;
                disp_clock();
                fwrite_txt_crc(text, DDICSIZ);
            }
        }
        else
        {
            j = c - 1 + THRESHOLD;
            count += j;
            pos = decode_ptr();
            if ((i = r - pos - 1) < 0)
                i += DDICSIZ;
            while (j-- > 0)
            {
                text[r] = text[i];
                if (++r >= DDICSIZ)
                {
                    r = 0;
                    disp_clock();
                    fwrite_txt_crc(text, DDICSIZ);
                }
                if (++i >= DDICSIZ)
                    i = 0;
            }
        }
    }
    if (r != 0)
        fwrite_txt_crc(text, r);

#ifndef KEEP_WINDOW
    free((char *)text);
#endif
}

