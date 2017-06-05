/* $Id: e2p_crypt.c 2840 2013-10-24 10:02:23Z tpgww $

Copyright (C) 2007-2013 tooar <tooar@emelfm2.net>

This file is part of emelFM2.
emelFM2 is free software; for the most part you can redistribute it
and/or modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 3, or
(at your option) any later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

/**
@file plugins/e2p_crypt.c
@brief plugin for encrypting or decrypting selected items
The encryption process is Salsa20/12 (see www.ecrypt.eu.org/stream/salsa20pf.html)
Code is based on public-domain salsa20-ref.c version 20051118 by D.J. Bernstein.
For compatibility with previous version of this plugin, decryption using the
formerly-used method derived from tinycrypt v.0.4 remains effective. That's based
on ARC4 (see http://en.wikipedia.org/wiki/RC4) with some tweaks by the author of
tinycrypt:
 - reduced nonce value to about 20 bytes ("160 bits should be enough")
 - reduced discarded bytes to 512 ("there's no evidence on the net that more
   are needed")
 - simplified mixing of the password and nonce values with the key, since
   throwing away the first 512 bytes mixes them up anyway
Any [de]compression will, by default, be peformed by an external library
(depending on what is available via dlopen()) or it may be done by built-in
minilzo, the mini subset of the fast LZO real-time data compression library. Such
built-in processing is only enabled by a build-time define E2_MINICRYPT
*/

/*TODO
en/de-compression
  single
  handle counter in new custom name
  ?m[un]map (void *address, size_t length, int protect, int flags, int filedes, off_t offset)
  buffered I/O if not processing whole file at once, can't be done with compression ?
  pthread_testcancel ();	//swap threads, cancel if instructed WHERE RELEVANT
  library access for [de]compression
  check - links ok ?

LZO supports in-place decompression, so sniff or reallocate loaded buffer NO - fails
in-place decompressing instead of duplicate buffers seems to NOT work
max size of lzo buffer lzo_unt = size_t? = ?
LZO streaming?
LZMA/7-zip compression supports streaming ?

UI
error handling - warnings
entry latency sometimes
crashes sometimes during dialog setup (maybe more to do with de-cryption?)
entry text scrolled out of entry window sometimes
some dialog-button choices have a BGL problem, if hide dialog after user-choice ?
*/

//use Salsa20 instead of ARC4 (DO NOT CHANGE THIS)
#define E2_NEWCRYPT

//enable internal mini-LZO for en/de-compression, instead of external lib
//#define E2_MINICRYPT

#include "emelfm2.h"
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <pthread.h>
#include <dlfcn.h>
#include "e2_plugins.h"
#include "e2_password_dialog.h"
#include "e2_task.h"
#include "e2_icons.h"
#include "e2_filelist.h"

//signature component, must match 'core' of this file name and likewise for corresponding icon file name 
#define ANAME "crypt"

#ifdef _LARGEFILE_SOURCE	//if we cater for "large" files (DEFAULT)
# define csize_t guint64
# define SIZESHIFT 56		//64-8
# define NONCE_LENGTH 24	//HEADER_LENGTH1 - sizeof(csize_t) (>=20)
# define HEADER_LENGTH1 32	//multiple of 8 >= (20 + sizeof(csize_t))
#else //if we cater only for files up to 2 GB (NOT the default)
# define csize_t guint32
# define SIZESHIFT 24		//32-8
# define NONCE_LENGTH 20
# define HEADER_LENGTH1 24	//NONCE_LENGTH + sizeof(csize_t) for flags
#endif

#define KEY_LENGTH 256 //for ARC4, hash table size KEY_LENGTH MUST be 256
//"conservative" approach would use much greater size e.g. KEY_LENGTH * 10 or 12
//#define DISCARD_BYTES 768 // KEY_LENGTH * 3
#define DISCARD_BYTES 512 // KEY_LENGTH * 2 this is recommended by TC author
#define SWAP(a,b) temp=a;a=b;b=temp;

//bitflags in csize_t stored in after nonce in encrypted file
//(don't change or remove any existing flag!)
enum
{
	E2_CFLAGNONE     = 0,
	E2_CFLAGCOMPRESS = 1,		//file is compressed (and one of the type-flags is set)
	E2_CFLAGSIZE     = 1 << 4,	//original file size stored as csize_t
	E2_CFLAGNAME     = 1 << 5,	//original file name stored as localised-text
	E2_CFLAGINFO     = 1 << 6,	//original file data stored as localised-text name plus statbuf
	E2_CFLAGVALIDATE = 1 << 7,	//determine and check crc of file and decomressed file size
	//bits 10-12 reserved for interface version number, 0..7
	E2_CFLAGINTLZ    = 1 << 16,	//file compressed with internal mini lzo
	E2_CFLAGLZO      = 1 << 17,	//file compressed with external liblzo
	E2_CFLAGZ        = 1 << 18,	//file compressed with external libz
	E2_CFLAGBZ2      = 1 << 19,	//file compressed with external libbz2
	E2_CFLAGLZMA     = 1 << 20	//file compressed with external liblzma NOT WORKING
};
//for [de]compression library checking
#define E2_CFLAGLIBMASK 0x1f0000
//for nonce-located status data about type of en/de-cryption
#define E2_CRYPTVERSION 1	//1 = salsa20 initial release
//for interface version NOTE this is stored in flags, so available only
//before encryption and after decryption
#define E2_FLAGVERSION 0
#define E2_GETFLAGVERSION(s) ((s>>10)&7)
#define E2_SETFLAGVERSION(s,n) s|=((n&7)<<10);

#ifndef E2_MINICRYPT
//FOR LZO COMPRESSION
#define LZO1X_1_MEM_COMPRESS 16384*sizeof(guchar *)
//for dlopen() usage
# define LZO_COMPRESSFUNC "lzo1x_1_compress"
# define LZO_DECOMPRESSFUNC "lzo1x_decompress_safe"
# define GZIP_COMPRESSFUNC "compress2"
# define GZIP_DECOMPRESSFUNC "uncompress"
# define BZIP2_COMPRESSFUNC "BZ2_bzBuffToBuffCompress"
# define BZIP2_DECOMPRESSFUNC "BZ2_bzBuffToBuffDecompress"
//# define LZMA_COMPRESSFUNC "LzmaCompressFIXME"
//# define LZMA_DECOMPRESSFUNC "LzmaUnCompressFIXME"

//the preferred order for automatically choosing a compression library, if present
//NOTE config data now used instead of auto choice
#define COMPRESS_CHOICE1 E2_CFLAGLZO
#define COMPRESS_CHOICE2 E2_CFLAGZ
#define COMPRESS_CHOICE3 E2_CFLAGBZ2
//#define COMPRESS_CHOICE4 E2_CFLAGLZMA
#endif

//FOR LIBZ [DE]COMPRESSION
//compression levels
enum
{
	Z_DEFAULT_COMPRESSION = -1,
	Z_NO_COMPRESSION,
	Z_BEST_SPEED,
	Z_BEST_COMPRESSION = 9
};

//FOR LIBBZIP2 [DE]COMPRESSION

//FOR LIBLZMA [DE]COMPRESSION
//#define SZ_OK ?

typedef struct _E2P_CryptOpts
{
	gboolean en_name_same;	//encrypted file name = same as onencrypted name
	gboolean en_name_suffix;//encrypted file name has user-specified suffix (if any)
	gboolean en_name_custom;//encrypted file name = user-specified
	gboolean en_name_embed;	//store filename in encrypted file CHECKME used ?
	gboolean en_properties_embed;	//store filename and statbuf in encrypted file
	gboolean de_name_same;		//decrypted file name = same as encrypted name
	gboolean de_name_stored;	//decrypted file name = embedded original name, if any
	gboolean de_name_suffix;	//decrypted file name omits user-specified suffix (if any)
	gboolean de_name_custom;	//decrypted file name = user-specified
	gboolean de_props_stored;	//decrypted file other properties = original file
	gboolean compress;	//compress file before encryption
	gboolean validate;	//crc data before encrypt, check it after decyrypt
	gboolean backup;	//preserve any file with same name as specified for the [de]crypted file
	gboolean preserve;	//preserve the file to be [de]crypted, with alternate name if appropriate
	gboolean recurse;	//recursively process all files in any selected dir
	gboolean walklinks;	//process link targets

	gboolean decryptmode;//TRUE = decrypt, FALSE = encrypt
	gboolean permission;//for transferring whether it's ok to make the change
	gboolean multisrc;	//TRUE when processing > 1 item in loop
	gboolean owrite;	//don't ask to confirm overwrites
	gboolean ignore_suffix; //don't worry about incorrect suffix when decompressing

	gchar *en_suffix;	//user-specified suffix, freeable utf-8
	gchar *en_name;		//user-specified name, freeable utf-8
	gchar *de_suffix;	//user-specified suffix, freeable utf-8
	gchar *de_name;		//user-specified name, freeable utf-8
	gchar *plain_pw;	//store for plaintext password ptr utf-8
	const gchar *localpath;	//copy of dialog-function arg, or substitute during treewalk
#ifndef E2_MINICRYPT
	csize_t compresslibflags;
	gpointer libhandle;	//result from latest dlopen(), need to dlcose() when done
#endif
	struct stat *statptr;
#ifdef E2_VFS
	PlaceInfo *spacedata;
#endif
	GList *dirdata;		//list of E2_Dirent's for dirs yet to be processed
} E2P_CryptOpts;

typedef struct _E2P_CryptDlgRuntime
{
	GtkWidget *dialog;	//main dialog widget
	E2P_CryptOpts *opts;
	E2_PWDataRuntime *pwrt;	//data struct for password-related widgets
	gboolean dlgopen;	//the following widgets exist
	GtkWidget *mode_btn;		//radio button for en/de-crypt mode

	GtkWidget *encryptbox;		//vbox with encryption-specific widgets
	GtkWidget *en_name_btn_same;
	GtkWidget *en_name_btn_suffix;
	GtkWidget *en_name_btn_custom;
	GtkWidget *en_name_suffix_entry;
	GtkWidget *en_name_custom_entry;
	GtkWidget *en_name_embed_btn; //CHECKME use this ? c.f. de_name_btn_stored
	GtkWidget *confirmbox;
	GtkWidget *en_properties_embed_btn;
	GtkWidget *compress_btn;
	GtkWidget *validate_btn;

	GtkWidget *decryptbox;		//vbox with decryption-specific widgets
	GtkWidget *de_name_btn_same;
	GtkWidget *de_name_btn_stored;
	GtkWidget *de_name_btn_suffix;
	GtkWidget *de_name_btn_custom;
	GtkWidget *de_name_suffix_entry;
	GtkWidget *de_name_custom_entry;
	GtkWidget *recurse_btn;
	GtkWidget *backup_btn;
	GtkWidget *preserve_btn;
	GtkWidget *linktarget_btn;
	GtkWidget *properties_btn;

//	GtkWidget *all_btn;	//for [de]sensitisizing apply-to-all choice
	GtkWidget *ok_btn;
//	DialogButtons result;
	gboolean result;	//TRUE when accepting the dialog response according to user's choice
} E2P_CryptDlgRuntime;

//session-static parameters
static PluginIface iface;

static E2P_CryptOpts session_opts =
{
	FALSE,	//encrypted file name = same as onencrypted name
	TRUE,	//encrypted file name has user-specified suffix (if any)
	FALSE,	//encrypted file name = user-specified
	FALSE,	//store filename in encrypted file	CHECKME relevant ?
	FALSE,	//store filename and statbuf in encrypted file
	FALSE,	//decrypted file name = same as encrypted name
	TRUE,	//decrypted file name = embedded original name (if any)
	TRUE,	//decrypted file name omits user-specified suffix (if any)
	FALSE,	//decrypted file name = user-specified

	TRUE,	//reinstate any properties of original file other than its name
	TRUE,	//compress file before encryption
	TRUE,	//crc data before encrypt, check it after decyrypt
	TRUE,	//preserve any file with same name as specified for the [de]crypted file
	TRUE,	//preserve the file to be [de]crypted, with alternate name if appropriate
	FALSE,	//recursively process all files in any selected dir
	TRUE,	//process link targets
/*rest are all default values, FALSE or NULL
	FALSE,	//decrypt mode
	FALSE,	//permission
	FALSE,	//>1 item
	FALSE,	//overwrite
	FALSE,	//ignore decomp suffix

	NULL,	//encryption suffix, initialised to ".enc"
	NULL,	//custom name for encryption
	NULL,	//decryption suffix, initialised to ".enc"
	NULL,	//custom name for decryption
	NULL,	//password
	NULL,	//item path
#ifndef E2_MINICRYPT
	0,		//compresslibflags;
	NULL,	//libhandle
#endif
	NULL	//stat buffer ptr
*/
};

//CHECKME are these ok to be static for all usage in session ??
//gboolean lzma_init_done = FALSE; //ditto for lzma SHOULD BE DONE ONCE ONLY, NOT EACH USAGE

#ifndef E2_MINICRYPT
//comression-library names used for config labels and error messages. Do not translate
static const gchar *libnames [] =
{
	"LZO",	//config option 0
	"GZIP",	//1
	"BZIP2",//2
//	"LZMA",	//3
	NULL
};
#endif

csize_t compresslib = E2_CFLAGNONE;

#ifdef E2_MINICRYPT
# if E2_DEBUG_LEVEL > 2
#  define LZO_DEBUG
# endif
/* get mini-lzo headers and code
For convenience, this file merges all relevant files */
# include "e2p_crypt_lzo.source"
/* If the merge is not available, minilzo.c can be included instead. The
files lzodefs.h lzoconf.h minilzo.h will also need to be available, and if
built as is, it wlll result in a bogus plugin minilzo.so */
//# include "minilzo.c"
#endif

static gboolean _e2p_task_docryptQ (E2_ActionTaskData *qed);
static csize_t _e2pcr_compress_buffer (gpointer filebuffer, /*size_t*/ gulong filebuffersize,
	gpointer *compressedbuffer
#ifndef E2_MINICRYPT
	, csize_t *usedlib, gpointer *libhandle
#endif
);
static gboolean _e2pcr_write_buffer (VPATH *localpath, gint descriptor,
	gpointer filebuffer, /*ssize_t*/ gulong filebuffersize);	//CHECKME gulong size ?
static gboolean _e2pcr_read_file (VPATH *localpath, gpointer *filebuffer,
	/*ssize_t*/ gulong filebuffersize);
static csize_t _e2pcr_decompress_buffer (gpointer filebuffer,
	/*ssize_t*/ gulong filebuffersize, csize_t originalfilesize,
	csize_t modeflags, gpointer *decompressedbuffer
#ifndef E2_MINICRYPT
	, csize_t *lastlib, gpointer *libhandle
#endif
	);
static gboolean _e2pcr_wipe_buffer (gpointer buffer, size_t buffersize, guint times);
static gboolean _e2pcr_flush_file (VPATH *localpath,
#ifdef E2_NEWCRYPT
	guint32 hashes[]
#else
	guint8 hashes[]
#endif
);

  /***************/
 /** utilities **/
/***************/

#ifdef E2_NEWCRYPT
/**
@brief calculate crc-32 of contents of @a buffer
This should move to utils if/when vfs plugin is implemented, that also uses this
function
@param buffer pointer to content to be summed
@param len no. of bytes to be summed

@return the crc
*/
static guint32 _e2pcr_getcrc32 (const guchar *buffer, size_t len)
{
/*	Standards          : ISO 3309, ITU-T V.42, ANSI X3.66, IEEE 802.3, FIPS PUB 71
	Initializing value : ffffffff
	Finalizing value   : ffffffff
	Polynomial value   : 04c11db7 (mirror value = edb88320)
	Polynomial         : x^32 + x^26 + x^23 + x^22 + x^16 + x^12 + x^11 +
						 x^10 + x^8 + x^7 + x^5 + x^4 + x^2 + x + 1
	This is derived from lib_crc v.1.15, by Lammert Bies
*/
	static gboolean init_table = FALSE;
	static guint32 crc_table [256];
	guint32 crc, long_c;
	const guchar *end;
	guint i, j;
	guint8 indx;

	if (!init_table)
	{	//do this just once
		for (i = 1; i < 256; i++)	//crc_tab[0] is always 0
		{
			crc = (guint32)i;
			for (j = 0; j < 8; j++)
			{
				if (crc & 0x1)
					crc = (crc >> 1) ^ 0xedb88320;
				else
					crc = crc >> 1;
			}
			crc_table [i] = crc;
		}
		init_table = TRUE;
	}

	crc = 0xffffffff;

	end = buffer + len;
	while (buffer < end)
	{
		long_c = ((guint32)*buffer) & 0xff ;
		indx = (crc ^ long_c) & 0xff;
		crc = (crc >> 8) ^ (crc_table [indx]);
		buffer++;
	}

	crc ^= 0xffffffff;

	return crc;
}

#ifndef USE_GLIB2_16
/* adapted from the SHA256 implementation in glib 2.16 */
#define GET_UINT32(n,b,i)               G_STMT_START{   \
    (n) = ((guint32) (b)[(i)    ] << 24)                \
        | ((guint32) (b)[(i) + 1] << 16)                \
        | ((guint32) (b)[(i) + 2] <<  8)                \
        | ((guint32) (b)[(i) + 3]      ); } G_STMT_END

#define PUT_UINT32(n,b,i)               G_STMT_START{   \
    (b)[(i)    ] = (guint8) ((n) >> 24);                \
    (b)[(i) + 1] = (guint8) ((n) >> 16);                \
    (b)[(i) + 2] = (guint8) ((n) >>  8);                \
    (b)[(i) + 3] = (guint8) ((n)      ); } G_STMT_END

#define SHR(x,n)        ((x & 0xffffffff) >> n)
#define ROTR(x,n)       (SHR (x,n) | (x << (32 - n)))

#define S0(x) (ROTR (x, 7) ^ ROTR (x,18) ^  SHR (x, 3))
#define S1(x) (ROTR (x,17) ^ ROTR (x,19) ^  SHR (x,10))
#define S2(x) (ROTR (x, 2) ^ ROTR (x,13) ^ ROTR (x,22))
#define S3(x) (ROTR (x, 6) ^ ROTR (x,11) ^ ROTR (x,25))

#define F0(x,y,z) ((x & y) | (z & (x | y)))
#define F1(x,y,z) (z ^ (x & (y ^ z)))

#define R(t) (W[t] = S1(W[t -  2]) + W[t -  7] + S0(W[t - 15]) + W[t - 16])

#define P(a,b,c,d,e,f,g,h,x,K)          G_STMT_START {  \
        temp1 = h + S3(e) + F1(e,f,g) + K + x;          \
        temp2 = S2(a) + F0(a,b,c);                      \
        d += temp1; h = temp1 + temp2; } G_STMT_END

#define SHA256_DATASIZE   64
#define SHA256_DIGEST_LEN 32

typedef struct
{
	guint32 buf[8];
	guint32 bits[2];
	guint8 data[SHA256_DATASIZE];
	guchar digest[SHA256_DIGEST_LEN];
} Sha256sum;

static void
sha256_transform (guint32      buf[8],
                  guint8 const data[SHA256_DATASIZE])
{
	guint32 temp1, temp2, W[64];
	guint32 A, B, C, D, E, F, G, H;

	GET_UINT32 (W[0],  data,  0);
	GET_UINT32 (W[1],  data,  4);
	GET_UINT32 (W[2],  data,  8);
	GET_UINT32 (W[3],  data, 12);
	GET_UINT32 (W[4],  data, 16);
	GET_UINT32 (W[5],  data, 20);
	GET_UINT32 (W[6],  data, 24);
	GET_UINT32 (W[7],  data, 28);
	GET_UINT32 (W[8],  data, 32);
	GET_UINT32 (W[9],  data, 36);
	GET_UINT32 (W[10], data, 40);
	GET_UINT32 (W[11], data, 44);
	GET_UINT32 (W[12], data, 48);
	GET_UINT32 (W[13], data, 52);
	GET_UINT32 (W[14], data, 56);
	GET_UINT32 (W[15], data, 60);

	A = buf[0];
	B = buf[1];
	C = buf[2];
	D = buf[3];
	E = buf[4];
	F = buf[5];
	G = buf[6];
	H = buf[7];

	P (A, B, C, D, E, F, G, H, W[ 0], 0x428a2f98);
	P (H, A, B, C, D, E, F, G, W[ 1], 0x71374491);
	P (G, H, A, B, C, D, E, F, W[ 2], 0xb5c0fbcf);
	P (F, G, H, A, B, C, D, E, W[ 3], 0xe9b5dba5);
	P (E, F, G, H, A, B, C, D, W[ 4], 0x3956c25b);
	P (D, E, F, G, H, A, B, C, W[ 5], 0x59f111f1);
	P (C, D, E, F, G, H, A, B, W[ 6], 0x923f82a4);
	P (B, C, D, E, F, G, H, A, W[ 7], 0xab1c5ed5);
	P (A, B, C, D, E, F, G, H, W[ 8], 0xd807aa98);
	P (H, A, B, C, D, E, F, G, W[ 9], 0x12835b01);
	P (G, H, A, B, C, D, E, F, W[10], 0x243185be);
	P (F, G, H, A, B, C, D, E, W[11], 0x550c7dc3);
	P (E, F, G, H, A, B, C, D, W[12], 0x72be5d74);
	P (D, E, F, G, H, A, B, C, W[13], 0x80deb1fe);
	P (C, D, E, F, G, H, A, B, W[14], 0x9bdc06a7);
	P (B, C, D, E, F, G, H, A, W[15], 0xc19bf174);

	P (A, B, C, D, E, F, G, H, R(16), 0xe49b69c1);
	P (H, A, B, C, D, E, F, G, R(17), 0xefbe4786);
	P (G, H, A, B, C, D, E, F, R(18), 0x0fc19dc6);
	P (F, G, H, A, B, C, D, E, R(19), 0x240ca1cc);
	P (E, F, G, H, A, B, C, D, R(20), 0x2de92c6f);
	P (D, E, F, G, H, A, B, C, R(21), 0x4a7484aa);
	P (C, D, E, F, G, H, A, B, R(22), 0x5cb0a9dc);
	P (B, C, D, E, F, G, H, A, R(23), 0x76f988da);
	P (A, B, C, D, E, F, G, H, R(24), 0x983e5152);
	P (H, A, B, C, D, E, F, G, R(25), 0xa831c66d);
	P (G, H, A, B, C, D, E, F, R(26), 0xb00327c8);
	P (F, G, H, A, B, C, D, E, R(27), 0xbf597fc7);
	P (E, F, G, H, A, B, C, D, R(28), 0xc6e00bf3);
	P (D, E, F, G, H, A, B, C, R(29), 0xd5a79147);
	P (C, D, E, F, G, H, A, B, R(30), 0x06ca6351);
	P (B, C, D, E, F, G, H, A, R(31), 0x14292967);
	P (A, B, C, D, E, F, G, H, R(32), 0x27b70a85);
	P (H, A, B, C, D, E, F, G, R(33), 0x2e1b2138);
	P (G, H, A, B, C, D, E, F, R(34), 0x4d2c6dfc);
	P (F, G, H, A, B, C, D, E, R(35), 0x53380d13);
	P (E, F, G, H, A, B, C, D, R(36), 0x650a7354);
	P (D, E, F, G, H, A, B, C, R(37), 0x766a0abb);
	P (C, D, E, F, G, H, A, B, R(38), 0x81c2c92e);
	P (B, C, D, E, F, G, H, A, R(39), 0x92722c85);
	P (A, B, C, D, E, F, G, H, R(40), 0xa2bfe8a1);
	P (H, A, B, C, D, E, F, G, R(41), 0xa81a664b);
	P (G, H, A, B, C, D, E, F, R(42), 0xc24b8b70);
	P (F, G, H, A, B, C, D, E, R(43), 0xc76c51a3);
	P (E, F, G, H, A, B, C, D, R(44), 0xd192e819);
	P (D, E, F, G, H, A, B, C, R(45), 0xd6990624);
	P (C, D, E, F, G, H, A, B, R(46), 0xf40e3585);
	P (B, C, D, E, F, G, H, A, R(47), 0x106aa070);
	P (A, B, C, D, E, F, G, H, R(48), 0x19a4c116);
	P (H, A, B, C, D, E, F, G, R(49), 0x1e376c08);
	P (G, H, A, B, C, D, E, F, R(50), 0x2748774c);
	P (F, G, H, A, B, C, D, E, R(51), 0x34b0bcb5);
	P (E, F, G, H, A, B, C, D, R(52), 0x391c0cb3);
	P (D, E, F, G, H, A, B, C, R(53), 0x4ed8aa4a);
	P (C, D, E, F, G, H, A, B, R(54), 0x5b9cca4f);
	P (B, C, D, E, F, G, H, A, R(55), 0x682e6ff3);
	P (A, B, C, D, E, F, G, H, R(56), 0x748f82ee);
	P (H, A, B, C, D, E, F, G, R(57), 0x78a5636f);
	P (G, H, A, B, C, D, E, F, R(58), 0x84c87814);
	P (F, G, H, A, B, C, D, E, R(59), 0x8cc70208);
	P (E, F, G, H, A, B, C, D, R(60), 0x90befffa);
	P (D, E, F, G, H, A, B, C, R(61), 0xa4506ceb);
	P (C, D, E, F, G, H, A, B, R(62), 0xbef9a3f7);
	P (B, C, D, E, F, G, H, A, R(63), 0xc67178f2);

	buf[0] += A;
	buf[1] += B;
	buf[2] += C;
	buf[3] += D;
	buf[4] += E;
	buf[5] += F;
	buf[6] += G;
	buf[7] += H;
}

static void
sha256_sum_init (Sha256sum *sha256)
{
	sha256->buf[0] = 0x6a09e667;
	sha256->buf[1] = 0xbb67ae85;
	sha256->buf[2] = 0x3c6ef372;
	sha256->buf[3] = 0xa54ff53a;
	sha256->buf[4] = 0x510e527f;
	sha256->buf[5] = 0x9b05688c;
	sha256->buf[6] = 0x1f83d9ab;
	sha256->buf[7] = 0x5be0cd19;

	sha256->bits[0] = sha256->bits[1] = 0;
}

static void
sha256_sum_update (Sha256sum    *sha256,
                   const guchar *buffer,
                   gsize         length)
{
	guint32 left, fill;
	const guint8 *input = buffer;

	if (length == 0)
		return;

	left = sha256->bits[0] & 0x3F;
	fill = 64 - left;

	sha256->bits[0] += length;
	sha256->bits[0] &= 0xffffffff;

	if (sha256->bits[0] < length)
		sha256->bits[1]++;

	if (left > 0 && length >= fill)
	{
		memcpy ((sha256->data + left), input, fill);

		sha256_transform (sha256->buf, sha256->data);
		length -= fill;
		input += fill;

		left = 0;
	}

	while (length >= SHA256_DATASIZE)
	{
		sha256_transform (sha256->buf, input);

		length -= SHA256_DATASIZE;
		input += SHA256_DATASIZE;
	}

	if (length)
		memcpy (sha256->data + left, input, length);
}

static void
sha256_sum_close (Sha256sum *sha256)
{
	guint32 last, padn;
	guint32 high, low;
	guint8 msglen[8];
	guint8 sha256_padding[SHA256_DATASIZE] =
	{
	 0x80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
	};

	high = (sha256->bits[0] >> 29) | (sha256->bits[1] <<  3);
	low  = (sha256->bits[0] <<  3);

	PUT_UINT32 (high, msglen, 0);
	PUT_UINT32 (low, msglen, 4);

	last = sha256->bits[0] & 0x3F;
	padn = (last < 56) ? (56 - last) : (120 - last);

	sha256_sum_update (sha256, sha256_padding, padn);
	sha256_sum_update (sha256, msglen, 8);

	PUT_UINT32 (sha256->buf[0], sha256->digest,  0);
	PUT_UINT32 (sha256->buf[1], sha256->digest,  4);
	PUT_UINT32 (sha256->buf[2], sha256->digest,  8);
	PUT_UINT32 (sha256->buf[3], sha256->digest, 12);
	PUT_UINT32 (sha256->buf[4], sha256->digest, 16);
	PUT_UINT32 (sha256->buf[5], sha256->digest, 20);
	PUT_UINT32 (sha256->buf[6], sha256->digest, 24);
	PUT_UINT32 (sha256->buf[7], sha256->digest, 28);
}

#undef SHR
#undef ROTR
#undef S0
#undef S1
#undef S2
#undef S3
#undef F0
#undef F1
#undef R
#undef P
#undef PUT_UINT32
#undef GET_UINT32
#endif //ndef USE_GLIB2_16
/**
@brief calculate sha-256 hash of contents of @a data
This should maybe move to utils ?
@param data pointer to content to be summed
@param length no. of bytes to be hashed
@param result >=32-byte array to hold the result

@return
*/
static void _e2pcr_getsha256 (const guchar *data, size_t length, guint8 result[32])
{
#ifdef USE_GLIB2_16
	GChecksum *summer = g_checksum_new (G_CHECKSUM_SHA256);
	gsize digest_len = 256;	//bits
	g_checksum_update (summer, data, length);
	g_checksum_get_digest (summer, result, &digest_len);
	g_checksum_free (summer);
#else
	//use local code, lifted from glib 2.16
	Sha256sum context;
	sha256_sum_init (&context);
	sha256_sum_update (&context, data, length);
	sha256_sum_close (&context);
	guint i;
	for (i = 0; i < 32; i++)
		result[i] = context.digest[i];
#endif
}
/**
@brief get a 256-bit hash from @a pw
This is not meant to be any more secure than a 32-bit hash, merely to be
moderately distinctive and not bound to utf-8 bytes in UI-generated @a pw
@param pw a "password" to generate the key, 0-terminated sequence of bytes
@param keybuffer pointer to >=32-byte array to hold 256-bit key

@return
*/
static void _e2pcr_getkey256 (const guchar *pw, guint8 keybuffer [32])
{
	size_t len = (size_t)strlen ((gchar *)pw);
	if (len == 0)
	{
		//any known padding here will do, nothing is secure
		guint8 empty[3] = { 253, 126, 49 };
		pw = (const guchar *)empty;
		len = 3;
	}
	_e2pcr_getsha256 (pw, len, keybuffer);
}
#endif //def E2_NEWCRYPT

/**
@brief get a random value in range 0 .. 255

@return the value
*/
static guint8 _e2pcr_getrandom (void)
{
	guint8 retval;
	E2_FILE *randFile = e2_fs_open_stream
#if defined(__linux__) || defined(__solaris__) || defined(darwin) || defined(__OpenBSD__) || defined(AIX)
	//CHECKME which other OS's ?
	("/dev/urandom", "r");
#else
	("/dev/random", "r");
#endif
	if (randFile != NULL)
	{
		retval = getc (randFile);
		e2_fs_close_stream (randFile);
	}
	else
	{
		printd (DEBUG, "cannot open random number source");
		retval = (guint8)((gulong)&compresslib >> 8);
	}
	return retval;
}
/**
@brief construct a temporary itemname by adding a suffix to @a localpath

@param localpath absolute path of item to be processed, localised string
@param custom string to append to @a localpath, localised string

@return newly-allocated, localised, path string comprising the temp name
*/
static gchar *_e2pcr_get_tempname (VPATH *localpath, gchar *custom)
{
	gchar *temppath, *s;
	guint i = 0;
	E2_ERR_DECLARE
#ifdef E2_VFS
	VPATH tdata;
	tdata.spacedata = localpath->spacedata;
#endif
	while (TRUE)
	{
		temppath = g_strdup_printf ("%s%s~%d", VPSTR(localpath), custom, i);
		if (i == 0)
		{	//first try without any "~N" suffix
			s = strrchr (temppath, '~');
			*s = '\0';
		}
#ifdef E2_VFS
		tdata.path = temppath;
		if (e2_fs_access2 (&tdata E2_ERR_PTR()) && E2_ERR_IS (ENOENT))
#else
		if (e2_fs_access2 (temppath E2_ERR_PTR()) && E2_ERR_IS (ENOENT))
#endif
		{
			E2_ERR_CLEAR
			break;
		}
		E2_ERR_CLEAR
		g_free (temppath);
		i++;
	}
	return temppath;
}
/**
@brief in an endian-independant fashion, store unencrypted data corresponding to @a value starting from @a datastart

@param datastart pointer to first of a series of unencrypted bytes to store

@return
*/
static void _e2pcr_store (csize_t value, guchar *datastart)
{
	guint i;
	//stored byte-order is low ... high
    for (i = 0; i < sizeof (csize_t); i++)
	{
		*datastart++ = (guchar) value;
		value >>= 8;
    }
}
/**
@brief in an endian-independant fashion, construct a csize_t from data stored starting from @a datastart

@param datastart pointer to first of a series of decrypted stored bytes to process

@return the stored value
*/
static csize_t _e2pcr_read_store (guchar *datastart)
{
	csize_t value = 0;
	csize_t grabber;	//needs to be bigger than SIZESHIFT to prevent warning
	guint i;
	//retrieved byte-order is low ... high
    for (i = 0; i < sizeof (csize_t); i++)
	{
		grabber = *datastart++;
		value = (value >> 8) | (grabber << SIZESHIFT);
    }
	return value;
}
/**
@brief check whether the user wlll allow an existing item to be over-written
This is not done in main task loop, as we need to handle custom names set in
the dialog or stored in a file being decompressed
This expects BGL to be open/off
@param localpath absolute path of item to check, localised string
@param multisrc TRUE if the current item is part of a multi-item selection

@return OK if there is no conflict, or code corresponding to the user's choice in the o/w dialog
*/
static DialogButtons _e2pcr_ow_check (VPATH *localpath, gboolean multisrc)
{
	return (e2_fs_access2 (localpath E2_ERR_NONE()) == 0) ?
		e2_dialog_ow_check (NULL, localpath, (multisrc) ? BOTHALL : NONE):
		OK;
}
/**
@brief run warning dialog for confirmation
@param prompt prompt string
@param multi this is part of a multi-file task
@return button code returned by the dialog
*/
static DialogButtons _e2pcr_dialog_warning (gchar *prompt, gboolean multi)
{
	DialogButtons retval;
	CLOSEBGL
	GtkWidget *dialog = e2_dialog_create (STOCK_NAME_DIALOG_WARNING, prompt,
		_("confirm"), DUMMY_RESPONSE_CB, NULL);
	OPENBGL

	E2_Button no_btn;
	e2_button_derive (&no_btn, &E2_BUTTON_NO,
			(multi) ? BTN_NO_SKIP : BTN_NO_CANCEL);

	//set default button to 'no'
	no_btn.showflags |= E2_BTN_DEFAULT;

	//block until user selects
	if (multi)
		retval = e2_dialog_show (dialog, NULL,
			E2_DIALOG_BLOCKED | E2_DIALOG_CLOSELOCK | E2_DIALOG_FREE | E2_DIALOG_MULTI,
			&E2_BUTTON_CANCEL, &E2_BUTTON_APPLYTOALL, &no_btn, &E2_BUTTON_APPLY, NULL);
	else
		retval = e2_dialog_show (dialog, NULL,
			E2_DIALOG_BLOCKED | E2_DIALOG_CLOSELOCK | E2_DIALOG_FREE,
			&no_btn, &E2_BUTTON_APPLY, NULL);
	return retval;
}

  /****************************/
 /* en/de-cryption functions */
/****************************/

#ifndef E2_MINICRYPT
/**
@brief setup things for decompression, according to @a flags
A pointer to library function for compressing or decompressing a buffer will be
set to relevant function from liblzo, libz or libbz2, according to which lib is
wanted and if it's available
If a library requires initialisation before use, that's done here
@param flags the flags recorded in a file being decompressed, in particular,
  the type of compression library
@param lastlib store for lib-in-use flag (may have been used for alternate operation)
@param getcompress TRUE to get compress function, FALSE for decompress
@param libhandle store for handle to dlopen'd library
@param libfunc store for desired function pointer

@return TRUE if no error happens
*/
static gboolean _e2pcr_check_lib (csize_t flags, csize_t *lastlib,
	gboolean getcompress, gpointer *libhandle, gpointer *libfunc)
{
	printd (DEBUG, "masked file lib-flags %0x", flags & E2_CFLAGLIBMASK);

	gboolean retval;
	gint (*init_func) ();
	gpointer thishandle;

	retval = FALSE;
	if (flags & E2_CFLAGLZO)	//want to use LZO
	{
		if (*lastlib & E2_CFLAGLZO) //lib is already interrogated
		{
			thishandle = *libhandle;
			printd (DEBUG, "LZO decompression library used before");
		}
		else
			thishandle = dlopen ("liblzo2.so.2", RTLD_LAZY | RTLD_LOCAL
#ifdef RTLD_DEEPBIND
			| RTLD_DEEPBIND
#endif
		);

		if (thishandle != NULL)
		{
			printd (DEBUG, "LZO decompression library opened");
			init_func = dlsym (thishandle, "__lzo_init_v2");	//a #define in lzoconf.h
			if (init_func != NULL)
			{
//				if (!lzo_init_done)//CHECKME needed every time ?
//				{
					//this hack is not much better than no init at all !
					gint result = init_func (2,
								sizeof(gshort),
								sizeof(gint),
								sizeof(glong),
								sizeof(guint32),
								sizeof(guint),
								sizeof(guchar *), //dict_t
								sizeof(gchar *),
								sizeof(gpointer),
								-1	//sizeof(lzo_callback_t));
								);
//					if (result == 0)
//						lzo_init_done = TRUE;
//					else
					if (result != 0)
						printd (DEBUG, "Cannot init compression using LZO lib");
//				}

//				if (lzo_init_done)
				if (result == 0)
				{
					*libfunc = dlsym (thishandle,
						(getcompress) ? LZO_COMPRESSFUNC : LZO_DECOMPRESSFUNC);
					if (*libfunc != NULL)
					{
						*lastlib &= ~E2_CFLAGLIBMASK;
						*lastlib |= E2_CFLAGLZO;
						if (*libhandle != NULL && *libhandle != thishandle)
							dlclose (*libhandle);
						*libhandle = thishandle;
						retval = TRUE;
						printd (DEBUG, "using LZO lib");
					}
				}

				if (!(*lastlib & E2_CFLAGLZO))
				{
//					lzo_init_done = FALSE;
					dlclose (thishandle);
					if (*libhandle == thishandle)
						*libhandle = NULL;
				}
			}
		}
#ifdef DEBUG_MESSAGES
		else //thishandle == NULL
			printd (DEBUG, "LZO decompression library missing");
#endif
	}
	else if (flags & E2_CFLAGZ)
	{
		if (*lastlib & E2_CFLAGZ) //lib is already interrogated
			thishandle = *libhandle;
		else
			thishandle = dlopen ("libz.so.1", RTLD_LAZY | RTLD_LOCAL
#ifdef RTLD_DEEPBIND
			| RTLD_DEEPBIND
#endif
		);

		if (thishandle != NULL)
		{
			*libfunc = dlsym (thishandle,
				(getcompress) ? GZIP_COMPRESSFUNC : GZIP_DECOMPRESSFUNC);
			if (*libfunc != NULL)
			{
				*lastlib &= ~E2_CFLAGLIBMASK;
				*lastlib |= E2_CFLAGZ;
				if (*libhandle != NULL && *libhandle != thishandle)
					dlclose (*libhandle);
				*libhandle = thishandle;
				retval = TRUE;
				printd (DEBUG, "using ZLIB lib");
			}
			else
			{
				dlclose (thishandle);
				if (*libhandle == thishandle)
					*libhandle = NULL;
			}
		}
#ifdef DEBUG_MESSAGES
		else	//thishandle == NULL
			printd (DEBUG, "ZLIB decompression library gone missing");
#endif
	}
	else if (flags & E2_CFLAGBZ2)
	{
		if (*lastlib & E2_CFLAGBZ2) //lib is already interrogated
			thishandle = *libhandle;
		else
			thishandle = dlopen ("libbz2.so.1", RTLD_LAZY | RTLD_LOCAL
#ifdef RTLD_DEEPBIND
			| RTLD_DEEPBIND
#endif
		);

		if (thishandle != NULL)
		{
			*libfunc = dlsym (thishandle,
				(getcompress) ? BZIP2_COMPRESSFUNC : BZIP2_DECOMPRESSFUNC);
			if (*libfunc != NULL)
			{
				*lastlib &= ~E2_CFLAGLIBMASK;
				*lastlib |= E2_CFLAGBZ2;
				if (*libhandle != NULL && *libhandle != thishandle)
					dlclose (*libhandle);
				*libhandle = thishandle;
				retval = TRUE;
				printd (DEBUG, "using BZ2 lib");
			}
			else
			{
				dlclose (thishandle);
				if (*libhandle == thishandle)
					*libhandle = NULL;
			}
		}
#ifdef DEBUG_MESSAGES
		else //thishandle == NULL
			printd (DEBUG, "BZ2LIB decompression library gone missing");
#endif
	}
/*NOT WORKING
	else if (flags & E2_CFLAGLZMA)
	{
		if (*lastlib & E2_CFLAGLZMA) //lib is already interrogated
			thishandle = *libhandle;
		else
			thishandle = dlopen ("liblzma.so.0", RTLD_LAZY | RTLD_LOCAL
#ifdef RTLD_DEEPBIND
			| RTLD_DEEPBIND
#endif
	);

		if (thishandle != NULL)
		{
			if (!lzma_init_done)
			{
				init_func = dlsym (thishandle, "lzma_init");	//a #define in lzma/init.h
				if (init_func != NULL)
				{
					lzma_init_done = init_func ();
				}
			}

			if (lzma_init_done)
			{
				*libfunc = dlsym (thishandle,
					(getcompress) ? LZMA_COMPRESSFUNC : LZMA_DECOMPRESSFUNC);
				if (*libfunc != NULL)
				{
					*lastlib &= ~E2_CFLAGLIBMASK;
					*lastlib |= E2_CFLAGLZMA;
					if (*libhandle != NULL && *libhandle != thishandle)
						dlclose (*libhandle);
					*libhandle = thishandle;
					printd (DEBUG, "using LZMA lib");
				}
				else
				{
					dlclose (thishandle);
					if (*libhandle == thishandle)
						*libhandle = NULL;
				}
			}
		}
#ifdef DEBUG_MESSAGES
		else //thishandle == NULL
				printd (DEBUG, "LZMA decompression library missing");
#endif
	}
*/
	return retval;
}
#endif

#ifdef E2_NEWCRYPT
//salsa20 stuff
#define U32V(v) ((guint32)(v) & 0xFFFFFFFF)
#define ROTL32(v,n) (U32V((v) << (n)) | ((v) >> (32 - (n))))
#define ROTATE(v,c) (ROTL32(v,c))
#define XOR(v,w) ((v) ^ (w))
#define PLUS(v,w) (U32V((v) + (w)))
#define PLUSONE(v) (PLUS((v),1))
#define SWAP32(v) ((ROTL32(v,8) & 0x00FF00FF) | (ROTL32(v,24) & 0xFF00FF00))
#define U32TO32_LITTLE(v) SWAP32(v)
#define U8TO32_LITTLE(p) U32TO32_LITTLE(((guint32*)(p))[0])
#define U32TO8_LITTLE(p,v) (((guint32*)(p))[0] = U32TO32_LITTLE(v))

/**
@brief setup hash array @a input (apart from the IV members) based on @a key
The code is for a 256-bit key. Used in setup for encryption or deccyption.
For example, this sequence is acceptable:
 _e2pcr_keysetup();
 _e2pcr_ivsetup();
 _e2pcr_encrypt_bytes();

 _e2pcr_ivsetup();
 _e2pcr_encrypt_bytes();

@param hashes array of numbers to be used for hashing
@param key buffer >= 32 bytes long containing password-derived 256-bit key

@return
*/
void _e2pcr_keysetup (guint32 hashes[16], const guint8 *key)
{
	const gchar *constants = "expand 32-byte k";	//DO NOT CHANGE THIS
	//hashes[6] .. [10] are setup one-or-more times in _e2pcr_ivsetup()
	hashes[1]  = U8TO32_LITTLE(key);
	hashes[2]  = U8TO32_LITTLE(key + 4);
	hashes[3]  = U8TO32_LITTLE(key + 8);
	hashes[4]  = U8TO32_LITTLE(key + 12);
	hashes[11] = U8TO32_LITTLE(key + 16);
	hashes[12] = U8TO32_LITTLE(key + 20);
	hashes[13] = U8TO32_LITTLE(key + 24);
	hashes[14] = U8TO32_LITTLE(key + 28);
	hashes[0]  = U8TO32_LITTLE(constants);
	hashes[5]  = U8TO32_LITTLE(constants + 4);
	hashes[10] = U8TO32_LITTLE(constants + 8);
	hashes[15] = U8TO32_LITTLE(constants + 12);
}
/**
@brief setup IV and loop-counter in @a hashes
After having called _e2pcr_keysetup(), this function may be called any number
of times in order to encrypt/decrypt different messages with the same key but
different @a iv

@param hashes array of numbers to be used for hashing
@param iv pointer to 8-byte buffer to use for "public" initial-value (aka nonce)

@return
*/
void _e2pcr_ivsetup (guint32 hashes[16], const guint8 *iv)
{
	hashes[6] = U8TO32_LITTLE(iv);
	hashes[7] = U8TO32_LITTLE(iv + 4);
	//hashes[8] and [9] comprise a 64-bit loop-counter, initialized before each usage
	hashes[8] = 0;
	hashes[9] = 0;
}
/**
@brief encrypt or decrypt buffer at @a original whose length is @a bytes

Note: decryption must mirror encryption in blocksize and number of calls to
this function, because the original hashes array and a mangled variation of it
are both needed, so most of the mangled array is restarted afresh at each call

@param hashes array of guints to use for hashing
@param original pointer to buffer holding material to be converted
@param converted pointer to buffer to hold converted message, may be same as @a original
@param bytes message length, in bytes

@return
*/
static void _e2pcr_crypt_bytes (guint32 hashes[16],
		const guint8 *original, guint8 *converted, size_t bytes)
{
	if (bytes != 0)
	{
		gint i;
		guint32 x[16];
		//get working copy of hashes, which can be re-mangled for each loop
		for (i = 0; i < 16; i++)
			x[i] = hashes[i];

		//loop through buffer, processing up to 64 bytes per loop
		for (;;)
		{
			guint8 output[64];
			const guint8 *sp;
			guint8 *dp, *op;

			//hashes[8] and [9] comprise a 64-bit loop-counter, set to 0 before start
			if (++hashes[8] == 0 && ++hashes[9] == 0)
			{
				//CHECKME rollover-and-continue (after 2^70 bytes) risks something ?
				//if so, FIXME pre-check for size-limit exceeded HOW ?
				//return;
			}

			for (i = 12; i > 0; i -= 2)
			{
				x [4] = XOR(x[ 4],ROTATE(PLUS(x[ 0],x[12]), 7));
				x [8] = XOR(x[ 8],ROTATE(PLUS(x[ 4],x[ 0]), 9));
				x[12] = XOR(x[12],ROTATE(PLUS(x[ 8],x[ 4]),13));
				x [0] = XOR(x[ 0],ROTATE(PLUS(x[12],x[ 8]),18));
				x [9] = XOR(x[ 9],ROTATE(PLUS(x[ 5],x[ 1]), 7));
				x[13] = XOR(x[13],ROTATE(PLUS(x[ 9],x[ 5]), 9));
				x [1] = XOR(x[ 1],ROTATE(PLUS(x[13],x[ 9]),13));
				x [5] = XOR(x[ 5],ROTATE(PLUS(x[ 1],x[13]),18));
				x[14] = XOR(x[14],ROTATE(PLUS(x[10],x[ 6]), 7));
				x [2] = XOR(x[ 2],ROTATE(PLUS(x[14],x[10]), 9));
				x [6] = XOR(x[ 6],ROTATE(PLUS(x[ 2],x[14]),13));
				x[10] = XOR(x[10],ROTATE(PLUS(x[ 6],x[ 2]),18));
				x [3] = XOR(x[ 3],ROTATE(PLUS(x[15],x[11]), 7));
				x [7] = XOR(x[ 7],ROTATE(PLUS(x[ 3],x[15]), 9));
				x[11] = XOR(x[11],ROTATE(PLUS(x[ 7],x[ 3]),13));
				x[15] = XOR(x[15],ROTATE(PLUS(x[11],x[ 7]),18));
				x [1] = XOR(x[ 1],ROTATE(PLUS(x[ 0],x[ 3]), 7));
				x [2] = XOR(x[ 2],ROTATE(PLUS(x[ 1],x[ 0]), 9));
				x [3] = XOR(x[ 3],ROTATE(PLUS(x[ 2],x[ 1]),13));
				x [0] = XOR(x[ 0],ROTATE(PLUS(x[ 3],x[ 2]),18));
				x [6] = XOR(x[ 6],ROTATE(PLUS(x[ 5],x[ 4]), 7));
				x [7] = XOR(x[ 7],ROTATE(PLUS(x[ 6],x[ 5]), 9));
				x [4] = XOR(x[ 4],ROTATE(PLUS(x[ 7],x[ 6]),13));
				x [5] = XOR(x[ 5],ROTATE(PLUS(x[ 4],x[ 7]),18));
				x[11] = XOR(x[11],ROTATE(PLUS(x[10],x[ 9]), 7));
				x [8] = XOR(x[ 8],ROTATE(PLUS(x[11],x[10]), 9));
				x [9] = XOR(x[ 9],ROTATE(PLUS(x[ 8],x[11]),13));
				x[10] = XOR(x[10],ROTATE(PLUS(x[ 9],x[ 8]),18));
				x[12] = XOR(x[12],ROTATE(PLUS(x[15],x[14]), 7));
				x[13] = XOR(x[13],ROTATE(PLUS(x[12],x[15]), 9));
				x[14] = XOR(x[14],ROTATE(PLUS(x[13],x[12]),13));
				x[15] = XOR(x[15],ROTATE(PLUS(x[14],x[13]),18));
			}
			for (i = 0; i < 16; ++i)
				x[i] = PLUS(x[i], hashes[i]);
			for (i = 0; i < 16; ++i)
				U32TO8_LITTLE (output + 4 * i, x[i]);

			sp = original;
			dp = converted;
			op = output;
			if (bytes > 64)
				original += 64;
			else
				original += bytes;

			while (sp < original)
			{
				*dp++ = (*sp) ^ (*op);
				sp++;
				op++;
			}
			if (bytes > 64)
			{
				bytes -= 64;
				converted += 64;
				//bump workarray to match loop-counter
				x[8] = hashes[8];
				x[9] = hashes[9];
			}
			else
				break;
		}
	}
}

#undef U32V
#undef ROTL32
#undef ROTATE
#undef XOR
#undef PLUS
#undef PLUSONE
#undef SWAP32
#undef U32TO32_LITTLE
#undef U8TO32_LITTLE
#undef U32TO8_LITTLE

#endif //def E2_NEWCRYPT

/**
@brief initialize hash key for ARC4 decryption
Unlike with tinycrypt, the "discard bytes" process starts at hashes[0], not hashes[1],
and at the end of that process tha i-index is 255, not 0
@param hashes array of hash-bytes
@param password the en/de-cryption password string (UTF-8)
@param nonce pointer to start of "nonce" string

@return the "seed" (j-index) of the hashkey to use for the next operation
*/
static guint8 _e2pcr_init_key (guint8 hashes[256], const gchar *password,
	const guchar *nonce)
{
	guint indx;
	guint8 i, j, temp;
	const gchar *p, *pe;

	//initialize the key;
    for (indx = 0; indx < KEY_LENGTH; indx++)
        hashes[indx] = (guint8)indx;
	//mangle the key from the password, as done in ARC4
	i = j = 0;
	p = password;
	pe = password + strlen (password);	//use end-marker in case password is empty
	for (indx = 0; indx < KEY_LENGTH; indx++)
	{
		if (p >= pe)
			p = password;

		j += hashes[(guint8)indx] + *p++;
		SWAP(hashes[(guint8)indx], hashes[j]);
	}
	//mangle the key from the "public" random bytes in the nonce
	//CHECKME why not KEY_LENGTH repetitions ?
	for (indx = 0; indx < NONCE_LENGTH; indx++)
	{
//this is the way it was done in the initial release of the plugin
		j += hashes[i] + *nonce++;
		SWAP(hashes[i], hashes[j]);
		i++;	//CHECKME want this before j update ?
	}
	//"discard bytes" = mangle some more to increase chance of all array-members
	//being swapped
	i = 255;	//do hashes[0] first
	j = 0;
#if 0
//NOTE this is NOT compatible with original release of the crypt plugin
	gint retries = 0;
remangle:
#endif
	for (indx = 0; indx < DISCARD_BYTES; indx++) //DISCARD_BYTES is a multiple of KEY_LENGTH
	{
		i++;
		j += hashes[i];
		SWAP(hashes[i], hashes[j]);
	}
#if 0
//NOTE this is NOT compatible with original release of the crypt plugin
//CHECKME require 0 same-vlues ?
	guint sames = 0;
	temp = 0;
	while (TRUE)
	{
		if (hashes[temp] == temp)
		{
			if (++sames > 1)
			{
				if (++retries < 6)
				{
					printd (DEBUG, "more mangle needed");
					goto remangle;
				}
				printd (DEBUG, "exausted mangle loop");
				break;
			}
		}
		if (++temp == 0)
		{
			printd (DEBUG, "all hash values are changed");
			break;
		}
	}
#endif
	return j;
}
/**
@brief setup nonce to use for encryption (and later, decryption)
Nonce may include a signature to hint about encryption mode (the original plugin
using ARC4 requires the whole nonce for initialisation, salsa20 does not need it all)
@param noncebuffer array of chars in which to store the nonce, must be sized >= NONCE_LENGTH

@return
*/
static void _e2pcr_init_nonce (guint8 noncebuffer[NONCE_LENGTH])
{
	//fill nonce with random bytes
	E2_FILE *randFile = e2_fs_open_stream
#if defined(__linux__) || defined(__solaris__) || defined(darwin) || defined(__OpenBSD__) || defined(AIX)
	//CHECKME which other OS's ?
	("/dev/urandom", "r");
#else
	("/dev/random", "r");
#endif
	guint8 *bufferp = noncebuffer;
	guint i;
	if (randFile != NULL)
	{
		for (i = 0; i < NONCE_LENGTH; i++)
			*bufferp++ = getc (randFile);
		e2_fs_close_stream (randFile);
	}
	else
	{
		printd (DEBUG, "cannot open random number source");
		guint8 c = (guint8)((gulong)&bufferp >> 8);
		*bufferp++ = c;
		for (i = 1; i < NONCE_LENGTH; i++)
		{
			if (i % 2)
				*bufferp++ = c + i;
			else if (i % 3)
				*bufferp++ = (c + i)/2;
			else
				*bufferp++ = (c - i)/2;
		}
	}

#ifdef E2_NEWCRYPT
	//store a signature in the nonce, to indicate what type of cryption is in use
	guint32 crc = _e2pcr_getcrc32 (noncebuffer, NONCE_LENGTH - sizeof(csize_t));
	//crc += E2_CRYPTVERSION;
	_e2pcr_store ((csize_t)crc, noncebuffer + NONCE_LENGTH - sizeof(csize_t));
#endif
}
/**
@brief encrypt or decrypt @a filebuffer using ARC4
Buffer contents are XOR-ed with the user's munged key
@param hashes array of key bytes
@param iseed pointer to store for i-index for manipulating @a hashes
@param jseed pointer to store for j-index for manipulating @a hashes
@param filebuffer the buffer to be processed
@param filebuffersize byte-size of @a filebuffer

@return
*/
static void _e2pcr_crypt_buffer (guint8 hashes[256],
	guint8 *iseed, guint8 *jseed, gpointer filebuffer, csize_t filebuffersize)
{
	guchar *filebufferp, *filebufferend;
	guint8 i, j, temp;

	filebufferend = filebuffer + filebuffersize;
	i = *iseed;
	j = *jseed;

	for (filebufferp = filebuffer; filebufferp < filebufferend; filebufferp++)
	{
		guint8 k;
		k = hashes[i];
		j += k;
//this is like the way it was done in the initial release of the plugin (but it had an unbracketed pointer)
		(*filebufferp) ^= hashes[(guint8)(k + hashes[j])];
		SWAP(hashes[i], hashes[j]);
		i++;
	}
	*iseed = i;
	*jseed = j;
}
/**
@brief finalise naming of en/de-crypted file
This is same for en- and de-cryption

@param localpath localised path of the item that's been processed
@param temppath localised path of interim temp file holding the processed results
@param newpath localised path of final name that's different from @a localpath
@param same_name TRUE when the processed file ends up as @a localpath
@param options pointer to operation data struct

@return TRUE if all was done as required
*/
static gboolean _e2pcr_finalise_item (VPATH *localpath, VPATH *temppath, VPATH *newpath,
	gboolean same_name, E2P_CryptOpts *options)
{
#ifdef E2_VFS
	VPATH otherpath;
	otherpath.spacedata = localpath->spacedata;
#endif
	gboolean success;
	gchar *tmp;
	if (same_name)
	{
		if (options->backup)
		{
			//no need to check for permission - that's done in the dialog
			tmp = _e2pcr_get_tempname (localpath, "-original");	//ascii, & don't bother with translation
#ifdef E2_VFS
			otherpath.path = tmp;
			success = e2_task_backend_rename (localpath, &otherpath);
#else
			success = e2_task_backend_rename (localpath, tmp);
#endif
			g_free (tmp);
			if (!success)
				return FALSE;
		}

		if (!e2_task_backend_rename (temppath, localpath))
			return FALSE;
	}
	else	//final name != original name
	{
		if (!e2_fs_access (newpath, F_OK E2_ERR_NONE()))
		{
			if (options->backup)
			{
				if (!e2_fs_access (newpath, W_OK E2_ERR_NONE()))
				{
					tmp = _e2pcr_get_tempname (newpath, "-original");	//ascii, & don't bother to translate
#ifdef E2_VFS
					otherpath.path = tmp;
					success = e2_task_backend_rename (newpath, &otherpath);
#else
					success = e2_task_backend_rename (newpath, tmp);
#endif
					g_free (tmp);
					if (!success)
					{
						//can't change original file
						//CHECKME get & use a tempname for localpath?
						return FALSE;
					}
				}
			}
			else
			{
				if (e2_option_bool_get ("confirm-overwrite"))
				{
					DialogButtons choice = _e2pcr_ow_check (newpath, FALSE);
					if (choice != OK)
						return FALSE;
				}
				e2_task_backend_delete (newpath);
			}
		}

		if (!e2_task_backend_rename (temppath, newpath))
			return FALSE;
	}

	return TRUE;
}
/**
@brief encrypt file @a localpath
Because a glib function is used for allocating buffer memory, its size is limited
to G_MAXUINT32 on a 32-bit platform
Any error message expects BGL open
@a newname is freed, if non-NULL
@param localpath localised path of item being processed, also reflected in @a dir and @a oldname
@param dir directory in which the item is located, absolute localised path
@param oldname name of item being processed, localised string
@param newname new name of item after processing, NULL if @a use_same_name is TRUE
@param use_same_name TRUE to give encrypted file same name as original
@param check whether to confirm overwrites when renaming
@param options pointer to crypt options data

@return code indicating choice or success
*/
static DialogButtons _e2pcr_encrypt1 (VPATH *localpath,
	const gchar *dir, const gchar *oldname, gchar *newname,
	gboolean use_same_name, gboolean check, E2P_CryptOpts *options)
{
	gint fdesc;
	gsize filebuffersize, compressedbuffersize;	//limited to 2^32 on 32-bit platform
	gboolean onwards;
	gpointer filebuffer, compressedbuffer;
	gchar *temppath;
	E2_ERR_DECLARE
	struct stat sb;

	if (e2_fs_stat (localpath, &sb E2_ERR_NONE()))
	{
		printd (DEBUG, "cannpt stat the file");
bad_open:
#ifdef E2_VFS
		e2_fs_set_error_from_errno (&E2_ERR_NAME);
#endif
		e2_fs_error_local (_("Cannot read '%s'"),
			localpath E2_ERR_MSGL());
		E2_ERR_CLEAR
		return NO;
	}

	if (sb.st_size == 0)
	{	//error or empty file, probably not error as file was stat'd ok
		return CANCEL;
	}
	//read local files uses glib gsize (size_t) for buffer-allocation byte-size
	if (sb.st_size > G_MAXUINT32 && sizeof (gsize) <= 4)
	{
		//FIXME more-informative warning for this
		goto bad_open;
	}
	//CHECKME mmap() file if possible, but then need separate buffer to crypt it
	filebuffersize = (gsize) sb.st_size;
	if (!_e2pcr_read_file (localpath, &filebuffer, filebuffersize))
	{
		goto bad_open;
	}

	if (options->compress)
	{
		compressedbuffersize = _e2pcr_compress_buffer (
		filebuffer, filebuffersize, &compressedbuffer
#ifndef E2_MINICRYPT
			, &options->compresslibflags, &options->libhandle
#endif
		);
		if (compressedbuffersize == 0)
		{
#ifdef E2_VFS
			e2_fs_set_error_from_errno (&E2_ERR_NAME);
#endif
			e2_fs_error_local (_("Compression failed for file %s"),
				localpath E2_ERR_MSGL());
			E2_ERR_CLEAR
//			_e2pcr_wipe_buffer (filebuffer, filebuffersize);
			g_free (filebuffer); //CHECKME wipe it ?
			return NO;	//or CANCEL ?
		}
	}
	else
	{	//warnings prevention only
		compressedbuffersize = 0;
		compressedbuffer = NULL;
	}

	temppath = e2_utils_get_tempname (VPSTR (localpath));
#ifdef E2_VFS
	VPATH tdata = { temppath, localpath->spacedata };
#endif
	//descriptor for blockwize writing encryped file
	fdesc = e2_fs_safeopen (temppath, O_CREAT | O_WRONLY, S_IWUSR | S_IRUSR);
	if (fdesc < 0)
	{
#ifdef E2_VFS
		e2_fs_set_error_from_errno (&E2_ERR_NAME);
#endif
		e2_fs_error_local (_("Cannot open '%s' for writing"),
#ifdef E2_VFS
		&tdata E2_ERR_MSGL());
#else
		temppath E2_ERR_MSGL());
#endif
		E2_ERR_CLEAR
//		_e2pcr_wipe_buffer (filebuffer, filebuffersize);
		g_free (filebuffer); //CHECKME wipe it ?
		g_free (temppath);
		if (compressedbuffer != NULL)
		{
//NEEDED?	_e2pcr_wipe_buffer (compressedbuffer, compressedbuffersize);
			free (compressedbuffer);	//not g_free
		}
		return NO;
	}

/* FILE LAYOUT
  |<     headlen        >|
  | NONCE |     DATA     |        File           | CRC |
          |<               crc len              >|
          |<datacryptlen>|
          |<                filecryptlen              >|

   DATA LAYOUT (indicated in flags)
  |flags|0-terminated name or nothing|statbuf or nothing| Oa or nothing |

  Oa == csize_t holding uncompresed File length, regardless of whether compressed
	during encryption, BUT it's not used if statbuf is stored (statbuf.st_size will do)
  CRC == csize_t holding 32-bit crc of unencrypted content from data to start of
	CRC, or random garbage if verification is not wanted

  Setup during encryption:
	1. compress original file if appropriate
	2. setup nonce and data and Oa
	3. crypt data and Oa
	4. crc data and Oa  and File and store at CRC, or store garbage at CRC
	6. encrypt all after nonce
  Unraveling this during decryption:
	1. decrypt all file after nonce
	2. decrypt again N bytes from end of nonce, N big enough for all possibilities
	3. interpret flags at start of this, to get other relevant parameters
	4. if relevant, compare *CRC with crc content over crclen, fail if no match
	5. decompress File if relevant
	6. compare original length if was compressed
*/

	//determine data length and set corresponding flags
	guint headlen = HEADER_LENGTH1;	//always need enough for nonce and immediately following flags
	csize_t flags = (options->compress) ?
#ifdef E2_MINICRYPT
		E2_CFLAGCOMPRESS | E2_CFLAGINTLZ : E2_CFLAGNONE;
#else
		E2_CFLAGCOMPRESS | (options->compresslibflags & E2_CFLAGLIBMASK) : E2_CFLAGNONE;
#endif
#ifdef E2_NEWCRYPT
	if (options->validate)
		flags |= E2_CFLAGVALIDATE;
	E2_SETFLAGVERSION (flags,E2_FLAGVERSION) //store interface version, for decryption
#endif
//	printd (DEBUG, "versioned file lib-flags %0x", flags);
//	printd (DEBUG, "masked file lib-flags %0x", flags & E2_CFLAGLIBMASK);
	if (options->en_name_embed)
	{
		flags |= E2_CFLAGNAME;
		headlen += strlen (oldname) + 1;
	}
	if (options->en_properties_embed)
	{
		flags |= E2_CFLAGINFO;
		//a whole FileInfo may be bigger than this
		headlen += sizeof(struct stat);
	}
	else	//don't need another size, already in properties
		if (options->compress)
	{
		flags |= E2_CFLAGSIZE;
		headlen += sizeof (csize_t);
	}
	//required header size is known now, we can populate it
	guint8 headerbuffer[headlen];

	_e2pcr_init_nonce (headerbuffer);	//public nonce at start of header
	_e2pcr_store (flags, headerbuffer + NONCE_LENGTH);

	gint nlen = 0;
	if (options->en_name_embed)
	{
		nlen = strlen (oldname) + 1;
		g_strlcpy ((gchar *)headerbuffer + HEADER_LENGTH1, oldname, nlen);
	}
	if (options->en_properties_embed)
		memcpy (headerbuffer + HEADER_LENGTH1 + nlen, &sb, sizeof (struct stat));
	else if (options->compress)
		_e2pcr_store (filebuffersize, headerbuffer + HEADER_LENGTH1 + nlen);

#ifdef E2_NEWCRYPT
	guint32 hashes[16];
	guint8 mainkey[40];
	guint8 datakey[32];

	 //from password, create 256-bit key at start of main key buffer
	_e2pcr_getkey256 ((guchar*)options->plain_pw, mainkey);
	//"privatise" 8 bytes of nonce into remainder of main key
	_e2pcr_keysetup (hashes, mainkey);
	_e2pcr_ivsetup (hashes, mainkey);
	_e2pcr_crypt_bytes (hashes, headerbuffer, mainkey + 32, 8);

	//create "independent" key for obscuring discoverable content at known positions
	_e2pcr_getsha256 (mainkey, 40, datakey);
	_e2pcr_keysetup (hashes, datakey);
	_e2pcr_ivsetup (hashes, datakey);

	_e2pcr_crypt_bytes (hashes,
						headerbuffer + NONCE_LENGTH, headerbuffer + NONCE_LENGTH,
						headlen - NONCE_LENGTH);

	//setup to encrypt all buffer except nonce, in one pass, to match the decrypt process
	guint32 crc;
	gpointer writebuf;
	gsize writebufsize;	//== size_t, platform-dependent

	if (flags & E2_CFLAGCOMPRESS)
	{
		writebufsize = compressedbuffersize + headlen + sizeof (csize_t);
		writebuf = realloc (compressedbuffer, writebufsize);
		if (writebuf != NULL)
		{
			//make space for header
			memmove (writebuf + headlen, writebuf, compressedbuffersize);
			memmove (writebuf, headerbuffer, headlen);
			if (options->validate)
			{
				//record checksum
				crc = _e2pcr_getcrc32 (writebuf + NONCE_LENGTH,
							headlen - NONCE_LENGTH + compressedbuffersize);
				printd (DEBUG, "calc crc for %d bytes = %x", headlen - NONCE_LENGTH + compressedbuffersize, crc);
				_e2pcr_store ((csize_t)crc, writebuf + headlen + compressedbuffersize);
			}
			//if no validation, the content of the CRC will be whatever it happens to be
			compressedbuffer = writebuf;	//for cleanup later
			onwards = TRUE;
		}
		else
			onwards = FALSE;
	}
	else //not compressed
	{
		writebufsize = filebuffersize + headlen + sizeof (csize_t);
		writebuf = realloc (filebuffer, writebufsize);
		if (writebuf != NULL)
		{
			memmove (writebuf + headlen, writebuf, filebuffersize);
			memmove (writebuf, headerbuffer, headlen);
			if (options->validate)
			{
				crc = _e2pcr_getcrc32 (writebuf + NONCE_LENGTH,
						headlen - NONCE_LENGTH +  filebuffersize);
				printd (DEBUG, "calc crc for %d bytes = %x", headlen - NONCE_LENGTH + compressedbuffersize, crc);
				_e2pcr_store ((csize_t)crc, writebuf + headlen + filebuffersize);
			}
			filebuffer = writebuf;
			onwards = TRUE;
		}
		else
			onwards = FALSE;
	}

	if (onwards)
	{
		//crypt content using the first-created key
		_e2pcr_keysetup (hashes, mainkey);
		_e2pcr_ivsetup (hashes, mainkey);
		_e2pcr_crypt_bytes (hashes, (guint8 *)writebuf + NONCE_LENGTH,
			(guint8 *)writebuf + NONCE_LENGTH, writebufsize - NONCE_LENGTH);

# ifdef E2_VFS
		onwards = _e2pcr_write_buffer (&tdata,
# else
		onwards = _e2pcr_write_buffer (temppath,
# endif
			fdesc, writebuf, writebufsize);
	}
	else
	{
		//FIXME memory failure message
		CHECKALLOCATEDWARNT (NULL,)
	}
#endif	//def E2_NEWCRYPT

	//fs rename operation may be not reliably atomic (ext4)
	if (onwards && e2_fs_writeflush (fdesc) != 0)
		onwards = FALSE;

	e2_fs_safeclose (fdesc);

	if (onwards)
	{
		gchar *newpath = (newname == NULL) ? NULL : g_build_filename (dir, newname, NULL);
#ifdef E2_VFS
		VPATH ddata = { newpath, localpath->spacedata };
#endif
		onwards = _e2pcr_finalise_item (localpath,
#ifdef E2_VFS
		&tdata, &ddata,
#else
		temppath, newpath,
#endif
			use_same_name, options);
		if (newpath != NULL)
			g_free (newpath);
	}

	_e2pcr_wipe_buffer (filebuffer, filebuffersize, 1);
	g_free (filebuffer);
	if (compressedbuffer != NULL)
	{
		//CHECKME maybe compressed, encytped, maybe paged, should buffer be wiped ?
		//_e2pcr_wipe_buffer (compressedbuffer, compressedbuffersize, 1);
		free (compressedbuffer);	//not g_free
	}
	g_free (temppath);
	if (newname != NULL)
		g_free (newname);

	if (onwards && !options->preserve)
	{
		//wipe & delete unencrypted original file
		onwards = _e2pcr_flush_file (localpath, hashes);
	}

	return ((onwards) ? OK : NO);
}
/**
@brief decrypt file @a localpath

Any error message expects BGL open

@param localpath localised path of item being processed, also reflected in @a dir and @a oldname
@param dir directory in which the item is located, absolute localised path
@param oldname name of item being processed, localised string
@param newname new name of item after processing, NULL if @a use_same_name is TRUE
@param use_same_name TRUE to give decrypted file same name as original
@param check whether to confirm overwrites when renaming
@param options pointer to crypt options data

@return code indicating choice or NO for error
*/
static DialogButtons _e2pcr_decrypt1 (VPATH *localpath,
	const gchar *dir, const gchar *oldname, gchar *newname,
	gboolean use_same_name, gboolean check, E2P_CryptOpts *options)
{
	gboolean onwards;
	E2_ERR_DECLARE
	struct stat sb;

	if (e2_fs_stat (localpath, &sb E2_ERR_NONE()))
	{
		printd (DEBUG, "cannot stat the file");
bad_open:
#ifdef E2_VFS
		e2_fs_set_error_from_errno (&E2_ERR_NAME);
#endif
		e2_fs_error_local (_("Cannot read '%s'"),
			localpath E2_ERR_MSGL());
		E2_ERR_CLEAR
		return NO;
	}
	if (sb.st_size == 0)
	{	//error or empty file, probably not error as file was stat'd ok
		return CANCEL;
	}
	//read local files uses glib gsize (size_t) for buffer-allocation byte-size
	if (sb.st_size > G_MAXUINT32 && sizeof (gsize) <= 4)
	{
		//FIXME more-informative warning for this
		goto bad_open;
	}
	//file reading is size-limited to gulong (was ssize_t)
	gsize filebuffersize = (gsize) sb.st_size;
	gpointer filebuffer;

	if (!_e2pcr_read_file (localpath, &filebuffer, filebuffersize))
	{
		goto bad_open;
	}

	//see comment in _e2pcr_encrypt1() about salsa-processed file layout

	csize_t flags;
	guint8 *workheader;
	//several variables needed only for deprecated v.0 ARC4 process
	guint8 iseed;
	guint8 jseed;
	guint8 ahashes[KEY_LENGTH];
#ifdef E2_NEWCRYPT
	guint32 shashes[16];
	gint iversion;

	//get a 'hint' from the (un-encrypted) nonce about the type of cryption used
	guint32 crc = _e2pcr_getcrc32 (filebuffer, NONCE_LENGTH - sizeof(csize_t));
	flags = _e2pcr_read_store (filebuffer + NONCE_LENGTH - sizeof(csize_t));
	if (flags == (csize_t)crc)
		iversion = 1;
	else
		iversion = 0;
othermode:
	if (iversion == 1)	//try salsa20 first
	{
		guint8 mainkey[40];
		guint8 datakey[32];
		//this is the maximum data-size that may be present:
		//flags + stored filename + stored properties
		guint8 recoveredheader [sizeof (csize_t) + NAME_MAX + 1 + sizeof (struct stat)];
		gint recoverlen = MIN (sb.st_size - NONCE_LENGTH, sizeof (recoveredheader));

		 //from password, create 256-bit key at start of main key buffer
		_e2pcr_getkey256 ((guchar*)options->plain_pw, mainkey);

		_e2pcr_keysetup (shashes, mainkey);
		_e2pcr_ivsetup (shashes, mainkey);
		_e2pcr_crypt_bytes (shashes, (guint8 *)filebuffer + NONCE_LENGTH,
			(guint8 *)filebuffer + NONCE_LENGTH, (size_t) filebuffersize - NONCE_LENGTH);
		//"privatise" 8 bytes of nonce into remainder of main key
		_e2pcr_ivsetup (shashes, mainkey);
		_e2pcr_crypt_bytes (shashes, filebuffer, mainkey + 32, 8);
		//create "independent" key for recovering discoverable content at known positions
		_e2pcr_getsha256 (mainkey, 40, datakey);
		_e2pcr_keysetup (shashes, datakey);
		_e2pcr_ivsetup (shashes, datakey);
		//get it into a safe place, in case it's shorter than the maximum
		_e2pcr_crypt_bytes (shashes,
							filebuffer + NONCE_LENGTH, recoveredheader, (size_t)recoverlen);
		workheader = recoveredheader;
	}
	else //if (iversion == 0)	//arc4, original, deprecated
	{
#endif
		//setup hashes using password and nonce at start of file
		iseed = 0;
		jseed = _e2pcr_init_key (ahashes, options->plain_pw, filebuffer);

		//decrypt rest of file
		_e2pcr_crypt_buffer (ahashes, &iseed, &jseed, filebuffer + NONCE_LENGTH,
				filebuffersize - NONCE_LENGTH);
		//if reading the whole file, check the trailing signature
		//since it's supposed to be 0, endian-ness doesn't matter
		flags = *((csize_t *) (filebuffer + filebuffersize - sizeof (csize_t)));
		if (flags != 0)
		{
			g_free (filebuffer);	//original is encrypted, no need to wipe
			printd (DEBUG, "bad password");
			e2_fs_error_simple (_("Wrong password for %s"), localpath);
			return NO;
		}
		workheader = filebuffer + NONCE_LENGTH;
#ifdef E2_NEWCRYPT
	}
#endif

#ifdef E2_VFS
	VPATH ddata;
	ddata.spacedata = localpath->spacedata;
#endif

	flags = _e2pcr_read_store (workheader);
	//interpret flags
	if (iversion == 1)	//salsa20
	{
		if (flags & E2_CFLAGVALIDATE)
		{
			//crc needs to be done on obscured data
			crc = _e2pcr_getcrc32 (filebuffer + NONCE_LENGTH,
								filebuffersize - NONCE_LENGTH - sizeof(csize_t));
			printd (DEBUG, "calc crc for %d bytes = %x", filebuffersize - NONCE_LENGTH - sizeof(csize_t) , crc);
			csize_t getcrc = _e2pcr_read_store (filebuffer + filebuffersize - sizeof (csize_t));
			printd (DEBUG, "stored crc = %x", getcrc);
//			if ((csize_t)crc != _e2pcr_read_store (filebuffer + filebuffersize - sizeof(csize_t)))
			if ((guint32)getcrc != crc)
			{
				printd (DEBUG, "bad password");
				iversion = 0;	//try again with the other form of decryption
				printd (DEBUG, "try again using old decryption mode");
				//FIXME this will fail due to bad decyrpt of filebuffer
				goto othermode; //FIXME need to get file again
//				g_free (filebuffer);	//original is encrypted, no need to wipe
//				e2_fs_error_simple (_("Wrong password for %s"), localpath);
//				return NO;
			}
		}
	}

	csize_t originalFileLength;
	const gchar *stored_name;
	const struct stat *stored_sb;

	gint hlen = sizeof (csize_t);	//header data will always be long enough for flags, at least

	if (flags & E2_CFLAGNAME)
	{
		stored_name = (const gchar *)(workheader + hlen);
		if (iversion == 0)
			hlen += NAME_MAX + 1;	//old version stored a FileInfo, which begins with this
		else
			hlen += strlen (stored_name) + 1;
	}
	else
		stored_name = NULL;

	stored_sb = NULL;
	if (flags & E2_CFLAGINFO)
	{
		stored_sb = (struct stat *)(workheader + hlen);
		hlen += sizeof(struct stat);
		originalFileLength = (csize_t)stored_sb->st_size;
	}
	else	//don't need size as well as properties
		if (flags & E2_CFLAGSIZE)
	{
		originalFileLength = _e2pcr_read_store (workheader + hlen);
		hlen += sizeof (csize_t);
	}
	else
	{
		originalFileLength = filebuffersize - NONCE_LENGTH - hlen - sizeof (csize_t);
	}
	//originalFileLength and real header size are known now

	//determine whether decrypted file name should change
	gboolean free_new_name;
	if (options->de_name_stored)
	{	//user wants to use a stored name, if any
		if (stored_name == NULL)
		{	//but there isn't one
			if (newname == NULL)
			{	//nor is this available
				gchar *s = strstr (oldname, ".enc");
				if (s != NULL && *(s+4) == '\0')
					s = g_strndup (oldname, s-oldname);
				else
					s = g_strconcat (oldname, "-decrypted", NULL);
				free_new_name = strcmp (oldname, s);
				if (free_new_name)
					newname = s;
				else
				{
					g_free (s);
					newname = (gchar *)oldname;
				}
			}
			else
				free_new_name = FALSE;	//just use provided newname instead
		}
		else //there is a stored name
		{
			free_new_name = strcmp (oldname, stored_name);
			if (free_new_name)
				newname = g_strdup (stored_name);
		}
	}
	else
		free_new_name = FALSE;
	//overwrite check if relevant
	if (check &&
		!(use_same_name //no point in warning about re-use of same name
		 || !strcmp (oldname, newname)))
	{
		gchar *checkpath = g_build_filename (dir, newname, NULL);
#ifdef E2_VFS
		ddata.path = checkpath;
		DialogButtons choice = _e2pcr_ow_check (&ddata, options->multisrc);
#else
		DialogButtons choice = _e2pcr_ow_check (checkpath, options->multisrc);
#endif
		g_free (checkpath);
		if (choice == YES_TO_ALL)
			options->owrite = FALSE;
		else if (choice == CANCEL || choice == NO_TO_ALL)
		{
			g_free (filebuffer);
			if (free_new_name)
				g_free (newname);
			return choice;
		}
	}

	gboolean use_stored_props =
		(options->de_props_stored	//user wants to use stored properties
		 && (flags & E2_CFLAGINFO));//file header has stored props

	guint skiplen = NONCE_LENGTH + hlen;
//	if (iversion == 1)	//salsa20
//		memcpy (filebuffer + NONCE_LENGTH, workheader, hlen);	//put real data back
	gpointer uncompressedbuffer;
	csize_t uncompressedbuffersize;

	gboolean compressed = flags & E2_CFLAGCOMPRESS;
	if (compressed)
	{
		uncompressedbuffersize =
			_e2pcr_decompress_buffer (filebuffer + skiplen,
				filebuffersize - skiplen - sizeof (csize_t),
				originalFileLength, flags,
				&uncompressedbuffer
#ifndef E2_MINICRYPT
				, &options->compresslibflags, &options->libhandle
#endif
		);

		if (uncompressedbuffersize == 0)
		{
			printd (DEBUG, "decompression failed");
			e2_fs_error_simple (_("Error decompressing file %s"), localpath);
			if (free_new_name)
				g_free (newname);
			g_free (filebuffer);
			//CHECKME other cleanups
			return CANCEL;
		}
	}

	gchar *temppath = e2_utils_get_tempname (VPSTR (localpath));
#ifdef E2_VFS
	VPATH tdata = { temppath, localpath->spacedata };
#endif
	if (compressed)
	{
#ifdef E2_VFS
		onwards = e2_fs_set_file_contents (&tdata,
#else
		onwards = e2_fs_set_file_contents (temppath,
#endif
			uncompressedbuffer, uncompressedbuffersize,
			S_IWUSR | S_IRUSR E2_ERR_PTR());

//NEEDED? _e2pcr_wipe_buffer (uncompressedbuffer, uncompressedbuffersize, 1);
		free (uncompressedbuffer);
	}
	else
	{
		onwards = e2_fs_set_file_contents (
#ifdef E2_VFS
			&tdata,
#else
			temppath,
#endif
			filebuffer + skiplen, filebuffersize - skiplen - sizeof (csize_t),
			S_IWUSR | S_IRUSR E2_ERR_PTR());
	}

	gchar *newpath = (newname == NULL) ? NULL : g_build_filename (dir, newname, NULL);
	if (onwards)
	{
#ifdef E2_VFS
		ddata.path = newpath;
#endif
		onwards = _e2pcr_finalise_item (localpath,
#ifdef E2_VFS
		&tdata, &ddata,
#else
		temppath, newpath,
#endif
			use_same_name, options);
	}

	if (onwards && use_stored_props)
	{	//decrypting with stored data
#ifndef E2_VFS
		const gchar *p;
#endif
		if (use_same_name)
		{
#ifdef E2_VFS
			ddata.path = localpath->path;
#else
			p = VPCSTR (localpath);
#endif
		}
		else
		{
//			ddata.path = newpath already
#ifndef E2_VFS
			p = newpath;
#endif
		}
		if (onwards && stored_sb != NULL &&
#ifdef E2_VFS
			!e2_task_backend_chown (&ddata, stored_sb->st_uid, stored_sb->st_gid, FALSE))
#else
			!e2_task_backend_chown (p, stored_sb->st_uid, stored_sb->st_gid, FALSE))
#endif
				onwards = FALSE;
		if (onwards &&
#ifdef E2_VFS
			e2_fs_chmod (&ddata, stored_sb->st_mode & ALLPERMS E2_ERR_NONE()))
#else
			e2_fs_chmod (p, stored_sb->st_mode & ALLPERMS E2_ERR_NONE()))
#endif
				onwards = FALSE;
		if (onwards && stored_sb != NULL)
		{
			struct utimbuf tb;
			tb.modtime = stored_sb->st_mtime;
			tb.actime = stored_sb->st_atime;
#ifdef E2_VFS
			if (e2_fs_utime (&ddata, &tb E2_ERR_NONE()))
#else
			if (e2_fs_utime (p, &tb E2_ERR_NONE()))
#endif
				onwards = FALSE;
		}
	}

	g_free (filebuffer); //no need for cleanup of crypted stuff
	g_free (temppath);
	if (newpath != NULL)
		g_free (newpath);
	if (newname != NULL)
		g_free (newname);	//CAREFUL maybe in caller too

	if (onwards && !options->preserve)
	{
		//delete encrypted original file
		onwards = e2_task_backend_delete (localpath);
	}

	return ((onwards) ? OK : NO);
}

  /*****************************/
 /** buffer & file functions **/
/*****************************/
/**
@brief compress @a filebuffer using using mini-lzo or some other supported library
Any error message expects BGL open
Size of the buffer is implicitly limited to ULONG_MAX.
After use, the compressed buffer should be free'd not g_free'd
@param filebuffer pointer to buffer to compress
@param filebuffersize no. of bytes to compress in @a filebuffer
@param compressedbuffer store for pointer newly-allocated buffer holding compressed bytes
@param localpath pointer to path data for any error message
#ifndef E2_MINICRYPT
@param usedlib store for flags indicating which compression library is used
@param libhandle store for handle to dlopen'd lib, for repeated use if any
#endif

@return size of compressed buffer, 0 on error
*/
static csize_t _e2pcr_compress_buffer (gpointer filebuffer, /*size_t*/ gulong filebuffersize,
	gpointer *compressedbuffer
#ifndef E2_MINICRYPT
	, csize_t *usedlib, gpointer *libhandle
#endif
)
{
#ifdef E2_MINICRYPT
//	if (!lzo_init_done)
//	{
		if (lzo_init() != LZO_E_OK)
		{
//			msg = _("Cannot initialize compression process");
//			CLOSEBGL
//			e2_output_print_error (msg, FALSE);
//			OPENBGL
			printd (WARN, "Cannot initialize compression process");
			return 0;
		}
//		lzo_init_done = TRUE;
//	}

	csize_t compressedbuffersize = filebuffersize + (filebuffersize >> 6) + 19;
	*compressedbuffer = malloc (compressedbuffersize);	//not g_try_malloc, that's limited to gulong
#if (CHECKALLOCATEDWARNT)
	CHECKALLOCATEDWARNT (*compressedbuffer, return 0;)
#else
	if (*compressedbuffer == NULL)
	{
		//FIXME handle error
		return 0;
	}
#endif
	/* Work-memory needed for compression. CHECKME there may be some alignment issue with this */
	gpointer workmem = g_try_malloc (LZO1X_1_MEM_COMPRESS);

#if (CHECKALLOCATEDWARNT)
	CHECKALLOCATEDWARNT (workmem, free(FIXME);return 0;)
#else
	if (workmem == NULL)
	{
		//FIXME handle error
		free (*compressedbuffer);
		return 0;
	}
#endif
	lzo_uint newbuffersize;
	gint result = lzo1x_1_compress
		(filebuffer, filebuffersize, *compressedbuffer, &newbuffersize, workmem);

	g_free (workmem);

	if (result == LZO_E_OK)
	{
		workmem = realloc (*compressedbuffer, newbuffersize);
		if (workmem != NULL)
			*compressedbuffer = workmem;
		return (csize_t) newbuffersize;
	}
#else
	printd (DEBUG, "compress file buffer");

	csize_t libflags;
	csize_t compressedbuffersize;
	gint (*compress_buf) ();
	gchar *msg;

	//decide preferred compression library from those available
	gint libnum = e2_option_sel_get ("compress-library");
	switch (libnum)
	{
		case 0:
			libflags = E2_CFLAGLZO;
			break;
		case 1:
			libflags = E2_CFLAGZ;
			break;
		case 2:
			libflags = E2_CFLAGBZ2;
			break;
/*LZMA not working yet
		case 3:
			libflags = E2_CFLAGLZMA;
			break;
*/
		default:
			libflags = 0; //trigger abort
			break;
	}

	if (!(libflags & compresslib) ||
			!_e2pcr_check_lib (libflags, usedlib, TRUE, libhandle, (gpointer *)&compress_buf))
	{
		msg = g_strdup_printf (
			_("Requested %s compression library not found or not compatible"), libnames[libnum]);
		CLOSEBGL
		e2_output_print_error (msg, TRUE);
		OPENBGL
		return 0;
	}
/* AUTOMATIC SELECTION
	gint libnum = 0;
	while (TRUE)
	{
		switch (libnum)
		{
			case 0:
				libflags = COMPRESS_CHOICE1;
				break;
			case 1:
				libflags = COMPRESS_CHOICE2;
				break;
			case 2:
				libflags = COMPRESS_CHOICE3;
				break;
/ *LZMA not working yet
			case 3:
				libflags = COMPRESS_CHOICE4;
				break;
* /
			default:
				msg = _("No supported compression library");
				CLOSEBGL
				e2_output_print_error (msg, FALSE);
				OPENBGL
				return 0;
		}

		if ((libflags & compresslib) &&
			_e2pcr_check_lib (libflags, usedlib, TRUE, libhandle, (gpointer *)&compress_buf))
			break;
		else
		{
#ifdef DEBUG_MESSAGES
			switch (libflags & E2_CFLAGLIBMASK)
			{
				case E2_CFLAGLZO:
					msg = libnames[0];
					break;
				case E2_CFLAGZ:
					msg = libnames[1];
					break;
				case E2_CFLAGBZ2:
					msg = libnames[2];
					break;
//				case E2_CFLAGLZMA:
//					msg = libnames[3];
//					break;
				default:	//should never happen
					msg = "";
					break;
			}
			printd (DEBUG, "No usable %s compression-library", msg);
#endif
			libnum++;
		}
	}
*/

	if (libflags & E2_CFLAGLZO)
		compressedbuffersize = filebuffersize + (filebuffersize >> 6) + 19;
	else if (libflags & E2_CFLAGZ)
		compressedbuffersize = (filebuffersize * 1.001 + 20 + 8) / 8 * 8;
	else //if (libflags & E2_CFLAGBZ2)
		compressedbuffersize = (filebuffersize * 1.01 + 600 + 8) / 8 * 8;
//	else //if (libflags & E2_CFLAGLZMA)
//		compressedbuffersize = (filebuffersize * 1);	//FIXME;

	*compressedbuffer = malloc (compressedbuffersize);	//not g_try_malloc, that's limited to gulong
	CHECKALLOCATEDWARNT (*compressedbuffer, return 0;)

	if (libflags & E2_CFLAGLZO)
	{
		printd (DEBUG, "compress using LZO");
		gpointer workmem = g_try_malloc (LZO1X_1_MEM_COMPRESS);
		CHECKALLOCATEDWARNT (workmem, )
		if (workmem != NULL)
		{
			guint compressedlen;
			gint res = compress_buf (filebuffer, (guint) filebuffersize, *compressedbuffer,
				&compressedlen, workmem);
			g_free (workmem);
			if (res == 0)
			{
				workmem = realloc (*compressedbuffer, compressedlen);
				if (workmem != NULL)
					*compressedbuffer = workmem;
				return (csize_t) compressedlen;
			}
		}
	}
	else if (libflags & E2_CFLAGZ)
	{
		printd (DEBUG, "compress using ZLIB");
		gulong compressedlen = (gulong) compressedbuffersize;
		if (compress_buf (*compressedbuffer, &compressedlen, filebuffer,
			(gulong) filebuffersize, Z_BEST_SPEED) == 0)
		{
			gpointer workmem = realloc (*compressedbuffer, compressedlen);
			if (workmem != NULL)
				*compressedbuffer = workmem;
			return (csize_t) compressedlen;
		}
	}
	else //if (libflags & E2_CFLAGBZ2)
	{
		printd (DEBUG, "compress using BZ2");
		guint compressedlen = (guint) compressedbuffersize;
		if (compress_buf (*compressedbuffer, &compressedlen, filebuffer,
			(guint) filebuffersize, 2, 0, 30) == 0)
		{
			gpointer workmem = realloc (*compressedbuffer, compressedlen);
			if (workmem != NULL)
				*compressedbuffer = workmem;
			return (csize_t) compressedlen;
		}
	}
/*NOT WORKING
	else //if (libflags & E2_CFLAGLZMA)
	{
		printd (DEBUG, "compress using LZMA");
		guint compressedlen = (guint) compressedbuffersize;
		size_t outPropsSize = 5;
		guint8 outProps[5]; //CHECKME last 4 of these for dictionary size, may need to be 8 for 64-bits

 		if (compress_buf (*compressedbuffer, &compressedlen,
					filebuffer, filebuffersize,
					&outProps, &outPropsSize, // *outPropsSize must be = 5
					-1,	//gint level, 0 <= level <= 9, default = 5
					0,	//unsigned dictSize,  default = (1 << 24)
					-1, //gint lc, 0 <= lc <= 8, default = 3
					-1, //gint lp, 0 <= lp <= 4, default = 0
					-1, //gint pb, 0 <= pb <= 4, default = 2
					-1, //gint fb, 5 <= fb <= 273, default = 32
					-1, //gint numThreads 1 or 2, default = 2
					) == SZ_OK)
		{
			gpointer workmem = realloc (*compressedbuffer, compressedlen);
			if (workmem != NULL)
				*compressedbuffer = workmem;
			return (csize_t) compressedlen;
		}
	}
*/
#endif
	printd (WARN, "Compression failed");
	free (*compressedbuffer);
	*compressedbuffer = NULL;
	return 0;
}
/**
@brief decompress buffer using mini-lzo or some other supported library
Size of the de-compress buffer is implicitly limited to ULONG_MAX
Size of decompressed file is checked, if relevant.
After use, the compressed buffer should be free'd not g_free'd
@param filebuffer pointer to buffer to decompress
@param filebuffersize no. of bytes in @a filebuffer
@param originalfilesize expected length of decompressed buffer
#ifndef E2_MINICRYPT
@param csize_t libflags which decompression mode to use
#endif
@param decompressedbuffer store for pointer to newly-allocated buffer holding
    @a originalFileLength decompressed bytes
@param localpath pointer to path data for any error message
#ifndef E2_MINICRYPT
@param usedlib store for flags indicating which decompression library is used
@param libhandle store for handle to dlopen'd lib, for repeated use if any
#endif

@return size of uncompressed buffer, 0 on error
*/
static csize_t _e2pcr_decompress_buffer (gpointer filebuffer,
	/*size_t*/ gulong filebuffersize, csize_t originalfilesize,
	csize_t modeflags, gpointer *decompressedbuffer
#ifndef E2_MINICRYPT
	, csize_t *usedlib, gpointer *libhandle
#endif
	)
{
#ifdef E2_MINICRYPT
//	if (!lzo_init_done)
//	{
		if (lzo_init() != LZO_E_OK)
		{
			//FIXME handle error
//			msg = _("Cannot initialize decompression process");
//			CLOSEBGL
//			e2_output_print_error (msg, FALSE);
//			OPENBGL
			printd (WARN, "Cannot initialize decompression process");
			return 0;
		}
//		lzo_init_done = TRUE;
//	}
#else
	gchar *msg;
	if ((modeflags & E2_CFLAGLIBMASK) == 0)
	{
		msg = _("Unknown compression library");
		CLOSEBGL
		e2_output_print_error (msg, FALSE);
		OPENBGL
		return 0;
	}
	gint (*decompress_buf) ();
	if (!_e2pcr_check_lib (modeflags & E2_CFLAGLIBMASK, usedlib, FALSE,
			libhandle, (gpointer *)&decompress_buf))
	{
		const gchar *arg;
		switch (modeflags & E2_CFLAGLIBMASK)
		{
			case E2_CFLAGLZO:
				arg = libnames[0];
				break;
			case E2_CFLAGZ:
				arg = libnames[1];
				break;
			case E2_CFLAGBZ2:
				arg = libnames[2];
				break;
//			case E2_CFLAGLZMA:
//				arg = libnames[3];
//				break;
			default:	//should never happen
				arg = "";
				break;
		}
		msg = (*arg != '\0') ?
			g_strdup_printf (_("No %s compression library"), arg) :
			_("Unknown compression library");
		CLOSEBGL
		e2_output_print_error (msg, (*arg != '\0'));
		OPENBGL

		return 0;
	}
#endif

	*decompressedbuffer = malloc (originalfilesize);	//not g_try_malloc
#if (CHECKALLOCATEDWARNT)
	CHECKALLOCATEDWARNT (*decompressedbuffer, return 0;)
#else
	if (*decompressedbuffer == NULL)
		return 0;
#endif
#ifdef E2_MINICRYPT
	lzo_uint decompressedlen;
	gint result = lzo1x_decompress_safe
		(filebuffer, filebuffersize, *decompressedbuffer, &decompressedlen, NULL);

	if (result == LZO_E_OK &&
		(decompressedlen == originalfilesize
#ifdef E2_NEWCRYPT
		|| (modeflags & E2_CFLAGVALIDATE) == 0)
#endif
		)
		return (csize_t) decompressedlen;
	printd (DEBUG, "minilzo de-compression error %d", result);
#else
	if (modeflags & E2_CFLAGLZO)	//(E2_CFLAGLZO | E2_CFLAGINTLZ));
	{
		printd (DEBUG, "de-compressing using LZO lib");
		guint decompressedlen = (guint) originalfilesize;
		if (decompress_buf (filebuffer, (guint) filebuffersize,
			*decompressedbuffer, &decompressedlen, NULL //gpointer wrkmem NOT USED
			) == 0 &&
			((csize_t) decompressedlen == originalfilesize
#ifdef E2_NEWCRYPT
			|| (modeflags & E2_CFLAGVALIDATE) == 0)
#endif
			)
		{
			return (csize_t) decompressedlen;
		}
		printd (DEBUG, "but that failed - original size %d decompressed to %d", originalfilesize, decompressedlen);
	}
	else if (modeflags & E2_CFLAGZ)
	{
		printd (DEBUG, "de-compressing using ZLIB lib");
		gulong decompressedlen = (gulong) originalfilesize;
		if (decompress_buf (*decompressedbuffer, &decompressedlen, filebuffer,
			(gulong) filebuffersize) == 0 &&
			((csize_t) decompressedlen == originalfilesize
#ifdef E2_NEWCRYPT
			|| (modeflags & E2_CFLAGVALIDATE) == 0)
#endif
			)
		{
			return (csize_t) decompressedlen;
		}
		printd (DEBUG, "but that failed - original size %d decompressed to %d", originalfilesize, decompressedlen);
	}
	else if (modeflags & E2_CFLAGBZ2)
	{
		printd (DEBUG, "de-compressing using BZ2 lib");
		guint decompressedlen = (guint) originalfilesize;
		if (decompress_buf (*decompressedbuffer, &decompressedlen, filebuffer,
			(guint) filebuffersize, 0, 0) == 0 &&
			((csize_t) decompressedlen == originalfilesize
#ifdef E2_NEWCRYPT
			|| (modeflags & E2_CFLAGVALIDATE) == 0)
#endif
			)
		{
			return (csize_t) decompressedlen;
		}
		printd (DEBUG, "but that failed - original size %d decompressed to %d", originalfilesize, decompressedlen);
	}
/*	else if (modeflags & E2_CFLAGLZMA)
	{
		printd (DEBUG, "de-compressing using LZMA lib");
		gint result = decompress_buf (*decompressedbuffer, &decompressedlen,
			filebuffer, &filebuffersize,
			const unsigned char *props, size_t propsSize);
		if (result == SZ_OK &&
		((csize_t) decompressedlen == originalfilesize
#ifdef E2_NEWCRYPT
		|| (modeflags & E2_CFLAGVALIDATE) == 0)
#endif
			)
		{
			return (csize_t) decompressedlen;
		}
		printd (DEBUG, "but that failed - original size %d decompressed to %d", originalfilesize, decompressedlen);
	}
*/
#endif
	//FIXME handle error
	free (*decompressedbuffer);
	*decompressedbuffer = NULL;
	return 0;
}
/**
@brief fill @a buffer with the contents of some file from $PATH
This is an alternative to storing some sequence of data that is readily
recognisable as over-written data
Expects BGL to be open on arrival here
@param buffer pointer to buffer to be overwritten
@param buffersize size of @a buffer
@param times the number of times to wipe

@return TRUE if the process was completed
*/
static gboolean _e2pcr_wipe_buffer (gpointer buffer, size_t buffersize, guint times)
{
	gboolean retval = FALSE;
	gchar *sep;
	gchar *execpath = (gchar *)g_getenv ("PATH");
	if (execpath == NULL)
	{
		sep = NULL;
		execpath = "/bin";
	}
	else
	{
		sep = strchr (execpath, ':');	//ascii scan ok
		if (sep != NULL)
			execpath = g_strndup (execpath, sep-execpath);
		//FIXME preserve execpath so that later members can be used
	}
#ifdef E2_VFS
	VPATH ddata = { execpath, NULL };	//files in $PATH must be local
	GList *entries = (GList *)e2_fs_dir_foreach (&ddata,
#else
	GList *entries = (GList *)e2_fs_dir_foreach (execpath,
#endif
		E2_DIRWATCH_NO,	//local = fast read
		NULL, NULL, NULL E2_ERR_NONE());

	if (E2DREAD_FAILED (entries))
	{
		//FIXME try another dir in PATH, or ...
		//FIXME warn user
//		e2_fs_error_simple (
//			_("You do not have authority to read %s"), execpath);
		if (sep != NULL)
			g_free (execpath);
		return FALSE;
	}
	guint count = g_list_length (entries);
	while (times > 0)
	{
		guint8 c;
restart:
		c = _e2pcr_getrandom ();
		guint first = count * c / 256;
		guint i = 0;
		gchar *filename, *filepath = NULL;
		GList *member;
reloop:
		for (member = g_list_nth (entries, first); member != NULL; member = member->next)
		{
			filename = (gchar *)member->data;
			if (strcmp (filename, ".."))
			{
				filepath = g_build_filename (execpath, filename, NULL);
#ifdef E2_VFS
				ddata.path = filepath;
				if (!e2_fs_access (&ddata, R_OK E2_ERR_NONE()))
#else
				if (!e2_fs_access (filepath, R_OK E2_ERR_NONE()))
#endif
					break;
				g_free (filepath);
			}
			filepath = NULL;

			if (++i == count);
			{
				//try with next dir from PATH or ...
				printd (DEBUG, "cannot find a file for data source");
				//FIXME warn user
//				e2_fs_error_simple (
//					_("You do not have authority to read anything in %s"), execpath);
				goto cleanup;
			}
		}
		if (member == NULL && i < count)
		{	//reached end of list, cycle back to start
			first = 0;
			goto reloop;
		}
		if (filepath == NULL)
			goto cleanup;

		E2_ERR_DECLARE
		gint fdesc = e2_fs_safeopen (filepath, O_RDONLY, 0);
		if (fdesc < 0)
		{
			printd (DEBUG, "cannot open data source file");
			goto restart;	//try with another file from list
/*  or ...
#ifdef E2_VFS
			e2_fs_set_error_from_errno (E2_ERR_NAME);
#endif
			e2_fs_error_local (_("Cannot open '%s' for reading"), filepath E2_ERR_MSGL());
			E2_ERR_CLEAR
			goto cleanup;
*/
		}

		struct stat sb;
#ifdef E2_VFS
		e2_fs_stat (&ddata, &sb E2_ERR_NONE());
#else
		e2_fs_stat (filepath, &sb E2_ERR_NONE());
#endif
		csize_t masksize = (csize_t) sb.st_size;
		ssize_t n_read;

		if (masksize >= buffersize)
		{
			n_read = e2_fs_read (fdesc, buffer, buffersize E2_ERR_PTR());
			if (n_read < buffersize)
			{
//#ifdef E2_VFS
//				e2_fs_set_error_from_errno (&E2_ERR_NAME);
//#endif
//				e2_fs_error_local (_("Error reading file %s"), localpath E2_ERR_MSGL());
				E2_ERR_CLEAR
				//FIXME handle shortfall
			}
		}
		else
		{	//mask-file is smaller than the buffer, read repeatedly until buffer is full
			csize_t readsofar = 0;
			guchar *readPtr = buffer;
			while (readsofar < buffersize)
			{
				n_read = e2_fs_read (fdesc, readPtr, masksize E2_ERR_PTR());
				if (n_read < masksize)
				{
//#ifdef E2_VFS
//					e2_fs_set_error_from_errno (&E2_ERR_NAME);
//#endif
//					e2_fs_error_local (_("Error reading file %s"), localpath E2_ERR_MSGL());
					E2_ERR_CLEAR
					//FIXME handle shortfall
				}
				lseek (fdesc, 0, SEEK_SET);	//FIXME vfs
				readsofar += masksize;
				readPtr += masksize;
				if (readsofar > (buffersize - masksize))
					masksize = buffersize - readsofar;
			}
		}

		//FIXME page buffer to disk, to mask any swap storage

		e2_fs_safeclose (fdesc);
		times--;
	}
	retval = TRUE;
cleanup:
	if (sep != NULL)
		g_free (execpath);
	e2_list_free_with_data (&entries);

	return retval;
}
/**
@brief read some or all of a file into @a filebuffer
This allows full or partial writing of a file, to support streaming and
otherwise-segemented input
Any error message expects BGL to be open
@param localpath localised name of item being read, used only for error messages
@param filebuffer pointer to store for address of allocated buffer
@param filebuffersize no. of bytes to read into @a filebuffer

@return TRUE if requested no. of bytes were read
*/
static gboolean _e2pcr_read_file (VPATH *localpath, gpointer *filebuffer,
	/*size_t*/ gulong filebuffersize)
{
	if (filebuffersize > 0)
	{
		E2_ERR_DECLARE
		/*ssize_t*/ gulong nread;
		if (!e2_fs_get_file_contents (localpath, filebuffer, &nread,
			FALSE E2_ERR_PTR()) || nread < filebuffersize)
		{
			printd (DEBUG, "cannot read whole file");
			//FIXME handle error
#ifdef E2_VFS
			e2_fs_set_error_from_errno (&E2_ERR_NAME);
#endif
			e2_fs_error_local (_("Error reading file %s"), localpath E2_ERR_MSGL());
			E2_ERR_CLEAR
			return FALSE;
		}
	}
	return TRUE;
}
/**
@brief write @a buffer out to storage
This allows full or partial writing of a file, to support streaming and
otherwise-segemented output
Any error message expects BGL to be open
@param localpath localised name of item being read, used only for error messages
@param descriptor file descriptor
@param buffer store for pointer to buffer holding data to write
@param buffersize size of @a filebuffer, > 0

@return TRUE if the write was completed
*/
static gboolean _e2pcr_write_buffer (VPATH *localpath, gint descriptor,
	gpointer buffer, /*size_t*/ gulong buffersize)
{
	if (buffersize > 0)
	{
		E2_ERR_DECLARE
		ssize_t bytes_written = e2_fs_write (descriptor, buffer,
			buffersize E2_ERR_PTR());
		if ((gulong)bytes_written < buffersize)
		{
#ifdef E2_VFS
			e2_fs_set_error_from_errno (&E2_ERR_NAME);
#endif
			e2_fs_error_local (_("Error writing file %s"), localpath E2_ERR_MSGL());
			E2_ERR_CLEAR
			return FALSE;
		}
	}
	return TRUE;
}
/**
@brief overwrite and delete @a localpath with the contents of some file from /bin
Any error message here expects BGL to be open
@param localpath absolute path of item to be processed, localised string
@param hashes the hash array used for en/de-cryption

@return TRUE if the process was completed
*/
static gboolean _e2pcr_flush_file (VPATH *localpath,
#ifdef E2_NEWCRYPT
	guint32 hashes[]
#else
	guint8 hashes[]
#endif
)
{
	E2_ERR_DECLARE
	struct stat sb;
	if (e2_fs_stat (localpath, &sb E2_ERR_PTR()))
	{
		e2_fs_error_local (_("Cannot get current data for %s"),
			localpath E2_ERR_MSGL());
		E2_ERR_CLEAR
		return FALSE;
	}
	if (sb.st_size == 0)
		return TRUE;

	guint8 randomval = _e2pcr_getrandom ();//fudge the size
	csize_t wipesize = (csize_t) sb.st_size + (csize_t) randomval;

	//find a buffer up to 64 times file's block-size
	csize_t buffersize = sb.st_blksize * 64;
	while (buffersize > wipesize)
		buffersize /= 2;
	if (buffersize < wipesize && buffersize < sb.st_blksize)
		buffersize = wipesize;
	gpointer buffer;
	while ((buffer = malloc (buffersize)) == NULL)
	{
		if (buffersize < sb.st_blksize)
		{
			CLOSEBGL
			e2_utils_show_memory_message ();
			OPENBGL
			return FALSE;
		}
		buffersize /= 2;
	}
	//open file for writing without truncation
	gint fdesc = e2_fs_safeopen (VPCSTR (localpath), O_RDWR | O_NONBLOCK, 0);
	if (fdesc < 0)
	{
		g_free (buffer);
#ifdef E2_VFS
		e2_fs_set_error_from_errno (&E2_ERR_NAME);
#endif
		e2_fs_error_local (_("Cannot open '%s' for writing"), localpath E2_ERR_MSGL());
		E2_ERR_CLEAR
		return FALSE;
	}

	gboolean retval = FALSE;
//	flockfile (outputFile);
	if (buffersize == wipesize)
	{
		if (!_e2pcr_wipe_buffer (buffer, buffersize, 2)
			|| !_e2pcr_write_buffer (localpath, fdesc, buffer, buffersize))
		{
			//FIXME error message
//			e2_fs_error_simple (
//				_("You do not have authority to modify %s"), (*iterator)->filename);
			goto cleanup;
		}
	}
	else
	{
		csize_t writesofar = 0;
		csize_t bsize = buffersize;
		while (writesofar < wipesize)
		{
			if (_e2pcr_wipe_buffer (buffer, bsize, 2)
				&& _e2pcr_write_buffer (localpath, fdesc, buffer, bsize))
			{
				writesofar += bsize;
				if (writesofar > (wipesize - buffersize))
					bsize = wipesize - writesofar;
			}
			else
			{
				//FIXME error message
//				e2_fs_error_simple (
//					_("You do not have authority to modify %s"), (*iterator)->filename);
				goto cleanup;
			}
		}
	}

	e2_fs_writeflush (fdesc); //should never fail

	retval = TRUE;
cleanup:
	g_free (buffer);
//	funlockfile (outputFile);
	e2_fs_safeclose (fdesc);

	if (retval)
	{
		//rename it (which changes ctime to now)
		gchar *s = _e2pcr_get_tempname (localpath, "ABCDE");
		gchar *t = strrchr (s, G_DIR_SEPARATOR);
		t++;
#ifdef E2_NEWCRYPT
		guint8 iv[8];	//don't care about initial value
		_e2pcr_ivsetup (hashes, iv);
		_e2pcr_crypt_bytes (hashes, (guint8 *)t, (guint8 *)t, (size_t) strlen (t));
#else
		guint8 iseed = randomval;
		guint8 jseed = (guint8) randomval * 2;
		_e2pcr_crypt_buffer (hashes, &iseed, &jseed, t, (csize_t) strlen (t));
#endif
		guchar *p = (guchar *) t;
		while (*p != '\0')
		{
			if (*p < '0')
				*p += '0';
			else
			{
				while (*p > 0x7e)
					*p -= 0x10;
			}
			p += sizeof(gchar);
		}
#ifdef E2_VFS
		VPATH ddata = { s, localpath->spacedata };
		e2_task_backend_move (localpath, &ddata);
#else
		e2_task_backend_move (localpath, s);
#endif
		//mask file m, atimes - random dates in past year
		time_t now = time (NULL);
		struct utimbuf tb;
		tb.modtime = now - 365 * 24 * 3600 * (time_t) randomval / 256;
		randomval = _e2pcr_getrandom ();
		tb.actime = now - 365 * 24 * 3600 * (time_t) randomval / 256;
		while (tb.actime < tb.modtime)
			tb.actime += 3600;
#ifdef E2_VFS
		e2_fs_utime (&ddata, &tb E2_ERR_NONE());
#else
		e2_fs_utime (s, &tb E2_ERR_NONE());
#endif
		//delete
#ifdef E2_VFS
		e2_task_backend_delete (&ddata);
#else
		e2_task_backend_delete (s);
#endif
		g_free (s);
	}

	return retval;
}
/**
@brief encrypt or decrypt file @a localpath
@a localpath may be target of a link, whose path is given in options
Any error message here expects BGL to be open
@param localpath absolute path of item to process, localised string
@param options pointer to process-parameters data

@return OK or YES_TO_ALL if successful, CANCEL or NO_TO_ALL if user chooses those, NO after error
*/
static DialogButtons _e2pcr_crypt1 (VPATH *localpath, E2P_CryptOpts *options)
{
	gchar *s, *newname = NULL;	//warning prevention
	gchar *dir = g_path_get_dirname (VPSTR (localpath));
	gchar *oldname = g_path_get_basename (VPSTR (localpath));
	DialogButtons retval;
	gboolean use_same_name = FALSE;
	gboolean check = (options->backup || options->owrite) ?
		FALSE : e2_option_bool_get ("confirm-overwrite");
#ifdef E2_VFS
	VPATH ddata;
	ddata.spacedata = localpath->spacedata;
#endif

	if (options->decryptmode)	//doing decryption
	{
		//check naming arrangements specified by dialog flags & widgets
		gboolean use_stored_name = FALSE;
		if (options->de_name_same)	//decrypted file name = same as encrypted name
			use_same_name = TRUE;
		else
		{
			if (options->de_name_suffix)
			{
				newname = F_FILENAME_TO_LOCALE (options->de_suffix);
				if (*newname != '\0' && g_str_has_suffix (oldname, newname))
				{
					use_same_name = FALSE;
					gint len = strlen (newname);
					F_FREE (newname, options->de_suffix);
					newname = g_strdup (oldname);
					*(newname + strlen (newname) - len) = '\0';
				}
				else
				{
					if (*newname != '\0' && !options->ignore_suffix)
					{
						//ask user what to do
						gchar *utf = F_FILENAME_FROM_LOCALE (VPSTR (localpath));
						s = g_strdup_printf (
							_("%s does not end with \"%s\".\nProcess this file anyway?"),
							utf, options->de_suffix);
						retval = _e2pcr_dialog_warning (s, options->multisrc);
						F_FREE (utf, VPSTR (localpath));
						g_free (s);
						switch (retval)
						{
							case YES_TO_ALL:
								options->ignore_suffix = TRUE;
							case OK:
								break;
							//case NO_TO_ALL:
//CHECKME consider a flag for stopping, instead of the current spaghetti to provoke a stop
							//case CANCEL:
							default:
								F_FREE (newname, options->de_suffix);
								g_free (dir);
								g_free (oldname);
								return retval;
						}
					}
					use_same_name = TRUE;
					F_FREE (newname, options->de_suffix);
				}
			}
			else if (options->de_name_custom)
			{
				if (*options->de_name != '\0')
				{
					newname = D_FILENAME_TO_LOCALE (options->de_name);
					use_same_name = !strcmp (oldname, newname);
					if (use_same_name)
						g_free (newname);
				}
				else
					use_same_name = TRUE;
			}
			else	//last choice is stored name (which may, later, turn out to be N/A or same)
			{
				use_stored_name = TRUE;
//				use_same_name = TRUE;	//nothing to cleanup afterwards
			}
			//newname now clear or g_freeable
		}
		if (check
			&& !use_same_name	//no point in warning about re-use of same name
			&& !use_stored_name	//we already know the name of the processed item
			)
		{
			s = g_build_filename (dir, newname, NULL);
#ifdef E2_VFS
			ddata.path = s;
			retval = _e2pcr_ow_check (&ddata, options->multisrc);
#else
			retval = _e2pcr_ow_check (s, options->multisrc);
#endif
			g_free (s);
			if (retval == YES_TO_ALL)
				options->owrite = FALSE;
			else if (retval == CANCEL || retval == NO_TO_ALL)
			{
				g_free (dir);
				g_free (oldname);
//				if (!use_same_name)
					g_free (newname);
				return retval;
			}
		}

		retval = _e2pcr_decrypt1 (localpath, dir, oldname, newname,
				use_same_name, check, options);
	}
	else	//doing encryption
	{
		//check naming arrangements specified by dialog flags & widgets
		if (options->en_name_same)	//encrypted file name = same as original name
			use_same_name = TRUE;
		else
		{
			if (options->en_name_suffix)
			{
				if (*options->en_suffix != '\0')
				{
					use_same_name = FALSE;
					s = F_FILENAME_TO_LOCALE (options->en_suffix);
					newname = e2_utils_strcat (oldname, s);
					F_FREE (s, options->en_suffix);
				}
				else
					use_same_name = TRUE;
			}
			else if (options->en_name_custom)
			{
				if (*options->en_name != '\0')
				{
					newname = D_FILENAME_TO_LOCALE (options->en_name);
					use_same_name = !strcmp (oldname, newname);
					if (use_same_name)
						g_free (newname);
				}
				else
					use_same_name = TRUE;
			}
			else
				use_same_name = TRUE;	//should never get here
		}

		if (check
			&& !use_same_name)	//no point in warning about re-use of same name
		{
			s = g_build_filename (dir, newname, NULL);
#ifdef E2_VFS
			ddata.path = s;
			retval = _e2pcr_ow_check (&ddata, options->multisrc);
#else
			retval = _e2pcr_ow_check (s, options->multisrc);
#endif
			g_free (s);
			if (retval == YES_TO_ALL)
				options->owrite = FALSE;
			else if (retval == CANCEL || retval == NO_TO_ALL)
			{
				g_free (dir);
				g_free (oldname);
//				if (!use_same_name)
					g_free (newname);
				return retval;
			}
		}

		retval = _e2pcr_encrypt1 (localpath, dir, oldname, newname,
			use_same_name, check, options);

	}	//end of encryption-specific section

	g_free (dir);
	g_free (oldname);
//	if (!use_same_name) cleared downstream
//		g_free (newname);

	return retval;
}
/**
@brief callback function for recursive directory processing
This is called for each non-directory item in the directory to be processed.
Treewalk is breadth-first, not physical
@param localpath absolute path of item to copy, localised string
@param statptr pointer to struct stat with info about @a localpath
@param status code from the walker, indicating what type of report it is
@param user_data pointer to user-specified data

@return E2TW_CONTINUE on success, others as appropriate
*/
static E2_TwResult _e2pcr_task_twcb_crypt (VPATH *localpath,
	const struct stat *statptr, E2_TwStatus status, E2P_CryptOpts *user_data)
{
	E2_TwResult retval = E2TW_CONTINUE;
	switch (status)
	{
		DialogButtons cryptresult;
		mode_t mode;
		E2_DirEnt *dirfix;
		GList *member;
#ifdef E2_VFS
		VPATH ddata;
#endif

		case E2TW_DP:	//dir completed
			//revert any altered dir permissions
#ifdef E2_VFS
			ddata.spacedata = localpath->spacedata;
#endif
			mode = statptr->st_mode & ALLPERMS;
			for (member = g_list_last (user_data->dirdata); member != NULL; member = member->prev)
			{
				dirfix = member->data;
				if (dirfix != NULL)
				{
					if (!strcmp (dirfix->path, VPSTR (localpath)))
					{
#ifdef E2_VFS
						ddata.path = dirfix->path;
#endif
						if ((mode & ALLPERMS) != dirfix->mode &&
#ifdef E2_VFS
							e2_fs_chmod (&ddata, dirfix->mode E2_ERR_NONE()))
#else
							e2_fs_chmod (localpath, dirfix->mode E2_ERR_NONE()))
#endif
								retval = E2TW_STOP;	//CHECKME - want cleanup of copied file to continue
						g_free (dirfix->path);
						DEALLOCATE (E2_DirEnt, dirfix);
						user_data->dirdata = g_list_delete_link (user_data->dirdata, member);
						break;
					}
				}
//				else //should never happen CHECKME ok when walking list ?
//					user_data->dirdata = g_list_delete_link (user_data->dirdata, member);
			}
			break;
		case E2TW_DM:	//dir, not opened due to different file system (reported upstream)
		case E2TW_DL:	//dir, not opened due to depth limit (reported upstream)
		case E2TW_DNR:	//unreadable dir (for which, error is reported upstream)
						//eventual chmod for this will probably fail, but try anyhow
//CHECKME report continue after problem
//			retval = E2TW_FIXME;
			break;
		case E2TW_DRR:	//dir now readable
		case E2TW_D:
			//ensure dir is writable, if we can
			if (e2_fs_tw_adjust_dirmode (localpath, statptr, (W_OK | X_OK)) == 0)
				//failed to set missing W and/or X perm
				retval = E2TW_SKIPSUB;	//can't process any item in the dir
				//CHECKME does DP report arrive ?
			else
			{
				//add this dir to list of items to revert afterwards
				//CHECKME what if newmode == oldmode ? (omit == scan full list in DP cb)
				dirfix = ALLOCATE (E2_DirEnt);
				CHECKALLOCATEDWARNT (dirfix, result = E2TW_STOP; break;)
				dirfix->path = g_strdup (VPSTR (localpath));
				dirfix->mode = statptr->st_mode & ALLPERMS;
				user_data->dirdata = g_list_append (user_data->dirdata, dirfix);
			}
			break;
		case E2TW_F:	//not directory or link
			if (S_ISREG (statptr->st_mode))
			{
				struct stat sb;
				user_data->localpath = VPSTR (localpath);	//ok to throw away the original
				sb = *statptr;	//get a non-const statbuf
				user_data->statptr = &sb;
				cryptresult = _e2pcr_crypt1 (localpath, user_data);
				if (cryptresult == NO_TO_ALL || cryptresult == NO) //NO == error
					retval = E2TW_STOP;
			}
			break;
		case E2TW_SL:	//symbolic link
			if (user_data->walklinks)
			{
				//get ultimate target of link
				gchar *target = g_strdup (VPSTR (localpath));
				if (e2_fs_walk_link (&target E2_ERR_NONE()))
				{
					struct stat sb;
#ifdef E2_VFS
					ddata.path = target;
					ddata.spacedata = localpath->spacedata;
					if (!e2_fs_stat (&ddata, &sb E2_ERR_NONE()))
#else
					if (!e2_fs_stat (target, &sb E2_ERR_NONE()))
#endif
					{
						user_data->localpath = VPSTR (localpath);	//ok to throw away the original
						user_data->statptr = &sb;
#ifdef E2_VFS
						cryptresult = _e2pcr_crypt1 (&ddata, user_data);
#else
						cryptresult = _e2pcr_crypt1 (target, user_data);
#endif
					}
					else
						cryptresult = NO;
				}
				else
					cryptresult = NO;
				g_free (target);
				if (cryptresult == NO_TO_ALL || cryptresult == NO) //NO == error
					retval = E2TW_STOP;
			}
//		case E2TW_SLN:	//symbolic link targeting non-existent item
//			error message ??
//			retval = E2TW_STOP;
		default:
			break;
	}
	return retval;
}
/**
@brief apply the user's choice to @a localpath
Error messages here and downstream expect BGL open
@param options pointer to process parameters data

@return TRUE if successful
*/
static DialogButtons _e2pcr_apply (E2P_CryptOpts *options)
{
#ifdef E2_VFS
	VPATH ddata;
	ddata.spacedata = options->spacedata;
#endif
	if (S_ISDIR (options->statptr->st_mode))
	{
		if (!options->recurse)
			return CANCEL;
		if ((options->decryptmode && options->de_name_same) ||
			(!options->decryptmode && options->en_name_same))
			return CANCEL;	//CHECKME warning ? ok if only 1 file/link in dir ?
		//the path may be changed in the treewalk
		const gchar *savepath = options->localpath;
		//recursively process dir contents
		//walk-flags for: fix-DNR, no thru-links if appropriate
		E2_TwFlags exec_flags = E2TW_FIXDIR;
		if (!options->walklinks)
			exec_flags |= E2TW_PHYS;

		E2_ERR_DECLARE;

		//CHECKME allow continue after error?
#ifdef E2_VFS
		ddata.path = options->localpath;
		gboolean retval = e2_fs_tw (&ddata,
#else
		gboolean retval = e2_fs_tw ((gchar *)options->localpath,
#endif
			_e2pcr_task_twcb_crypt, options, -1, exec_flags E2_ERR_PTR());

		//normally no leftover dir data, but ensure cleanup ...
		GList *member;
		for (member = g_list_last (options->dirdata); member != NULL; member = member->prev)
		{
			E2_DirEnt *dirfix = member->data;
			if (dirfix != NULL)
			{
#ifdef E2_VFS
				ddata.path = dirfix->path;
				if (e2_fs_chmod (&ddata, dirfix->mode E2_ERR_PTR()))
#else
				if (e2_fs_chmod (dirfix->path, dirfix->mode E2_ERR_PTR()))
#endif
					retval = FALSE;
				g_free (dirfix->path);
				DEALLOCATE (E2_DirEnt, dirfix);
			}
		}

		//handle errors
		if (!retval
			&& E2_ERR_ISNOT(0))	//might be a user-abort, not an error
		{
#ifdef E2_VFS
			ddata.path = savepath;
#endif
			e2_fs_error_local (_("Cannot process all of %s"),
#ifdef E2_VFS
				&ddata E2_ERR_MSGL());
#else
				(gchar *) savepath E2_ERR_MSGL());
#endif
			E2_ERR_CLEAR
		}
		return (retval) ? OK : NO_TO_ALL;	//no need for a simple CANCEL
	}
	else //not a dir
		if (S_ISLNK (options->statptr->st_mode))
	{	//handle links separately, to manage 'look-through'
		if (!options->walklinks)
			return FALSE;
		//get ultimate target and remove any relativity
		DialogButtons cryptresult;
		gchar *target = g_strdup (options->localpath);
		if (e2_fs_walk_link (&target E2_ERR_NONE()))
		{
			struct stat sb;
#ifdef E2_VFS
			ddata.path = target;
			if (!e2_fs_stat (&ddata, &sb E2_ERR_NONE()))
#else
			if (!e2_fs_stat (target, &sb E2_ERR_NONE()))
#endif
			{
				options->localpath = target;	//ok to throw away the original
				options->statptr = &sb;
#ifdef E2_VFS
				cryptresult = _e2pcr_crypt1 (&ddata, options);
#else
				cryptresult = _e2pcr_crypt1 (target, options);
#endif
			}
			else
				cryptresult = NO;
		}
		else
			cryptresult = NO;
		g_free (target);
		return cryptresult;
//		return (_e2pcr_crypt1 (target, options) == OK);
	}
	else	//not dir or link
#ifdef E2_VFS
	{
		ddata.path = options->localpath;
		return (_e2pcr_crypt1 (&ddata, options));
	}
#else
		return (_e2pcr_crypt1 (options->localpath, options));
#endif
}
/**
@brief determine whether intended change is permitted

@param rt pointer to dialog runtime data struct

@return TRUE if change is permitted, or if current item is a dir with W,X permissions
*/
static gboolean _e2pcr_check_permission (E2P_CryptDlgRuntime *rt)
{
	gchar target[PATH_MAX];
	gchar *newpath, *localname, *dir;
	const gchar *utfname, *localpath;
	struct stat *sp;
#ifdef E2_VFS
	VPATH ddata;
	ddata.spacedata = rt->opts->spacedata;
#endif
	localpath = rt->opts->localpath;
	sp = rt->opts->statptr;
restart:
#ifdef E2_VFS
	ddata.path = localpath;
	if (e2_fs_lstat (&ddata, sp E2_ERR_NONE()))
#else
	if (e2_fs_lstat (localpath, sp E2_ERR_NONE()))
#endif
	{
		rt->opts->permission = FALSE;
		return FALSE;
	}
	if (S_ISLNK (sp->st_mode))
	{
		if ((rt->dlgopen &&
			!gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->linktarget_btn)))
			|| !rt->opts->walklinks)
		{
			rt->opts->permission = FALSE;
			return FALSE;
		}
		gchar *checktarget = g_strdup (localpath);
		if (!e2_fs_walk_link (&checktarget E2_ERR_NONE()))
		{
			g_free (checktarget);
			rt->opts->permission = FALSE;
			return FALSE;
		}

		g_strlcpy (target, checktarget, sizeof (target));
		g_free (checktarget);
		localpath = target;
		goto restart;
	}
	if (S_ISDIR (sp->st_mode))
	{
#ifdef E2_VFS
		rt->opts->permission = !e2_fs_access (&ddata, X_OK | W_OK E2_ERR_NONE());
#else
		rt->opts->permission = !e2_fs_access (localpath, X_OK | W_OK E2_ERR_NONE());
#endif
		return rt->opts->permission;
	}

	dir = g_path_get_dirname (localpath);
	//access _always_ traverses links
#ifdef E2_VFS
	ddata.path = dir;
	if (!e2_fs_access (&ddata, X_OK | W_OK E2_ERR_NONE()))
#else
	if (!e2_fs_access (dir, X_OK | W_OK E2_ERR_NONE()))
#endif
	{
		if (rt->opts->decryptmode)
		{
			if (rt->opts->de_name_same)	//decrypted file name = same as encrypted name
#ifdef E2_VFS
			{
				ddata.path = localpath;
				rt->opts->permission = !e2_fs_access (&ddata, W_OK E2_ERR_NONE());
			}
#else
				rt->opts->permission = !e2_fs_access (localpath, W_OK E2_ERR_NONE());
#endif
			else
			{
				if (rt->opts->de_name_suffix)
				{
					utfname = (rt->dlgopen) ?
							gtk_entry_get_text (GTK_ENTRY (rt->de_name_suffix_entry)):
							rt->opts->de_suffix;
					localname = F_FILENAME_TO_LOCALE (utfname);
					newpath = g_strdup (localpath);
					if (*localname != '\0' && g_str_has_suffix (newpath, localname))
						*(newpath + strlen (newpath) - strlen (localname)) = '\0';
#ifdef E2_VFS
					ddata.path = newpath;
					rt->opts->permission = e2_fs_access (&ddata, F_OK E2_ERR_NONE())
					 || !e2_fs_access (&ddata, W_OK E2_ERR_NONE());
#else
					rt->opts->permission = e2_fs_access (newpath, F_OK E2_ERR_NONE())
					 || !e2_fs_access (newpath, W_OK E2_ERR_NONE());
#endif
					F_FREE (localname, utfname);
					g_free (newpath);
				}
				else if (rt->opts->de_name_custom)
				{
					utfname = (rt->dlgopen) ?
							gtk_entry_get_text (GTK_ENTRY (rt->de_name_custom_entry)):
							rt->opts->de_name;
					localname = F_FILENAME_TO_LOCALE (utfname);
					newpath = g_build_filename (dir, localname, NULL);
#ifdef E2_VFS
					ddata.path = newpath;
					rt->opts->permission = e2_fs_access (&ddata, F_OK E2_ERR_NONE())
					 || !e2_fs_access (&ddata, W_OK E2_ERR_NONE());
#else
					rt->opts->permission = e2_fs_access (newpath, F_OK E2_ERR_NONE())
					 || !e2_fs_access (newpath, W_OK E2_ERR_NONE());
#endif
					F_FREE (localname, utfname);
					g_free (newpath);
				}
				else
					rt->opts->permission = TRUE;
			}
		}
		else	//encrypting
		{
			if (rt->opts->en_name_same)	//encrypted file name = same as original
#ifdef E2_VFS
			{
				ddata.path = localpath;
				rt->opts->permission = !e2_fs_access (&ddata, W_OK E2_ERR_NONE());
			}
#else
				rt->opts->permission = !e2_fs_access (localpath, W_OK E2_ERR_NONE());
#endif
			else
			{
				if (rt->opts->en_name_suffix)
				{
					utfname = (rt->dlgopen) ?
							gtk_entry_get_text (GTK_ENTRY (rt->en_name_suffix_entry)):
							rt->opts->en_suffix;
					localname = F_FILENAME_TO_LOCALE (utfname);
					newpath = g_build_filename (localpath, localname, NULL);
#ifdef E2_VFS
					ddata.path = newpath;
					rt->opts->permission = e2_fs_access (&ddata, F_OK E2_ERR_NONE())
					 || !e2_fs_access (&ddata, W_OK E2_ERR_NONE());
#else
					rt->opts->permission = e2_fs_access (newpath, F_OK E2_ERR_NONE())
					 || !e2_fs_access (newpath, W_OK E2_ERR_NONE());
#endif
					F_FREE (localname, utfname);
					g_free (newpath);
				}
				else if (rt->opts->en_name_custom)
				{
					utfname = (rt->dlgopen) ?
							gtk_entry_get_text (GTK_ENTRY (rt->en_name_custom_entry)):
							rt->opts->en_name;
					localname = F_FILENAME_TO_LOCALE (utfname);
					newpath = g_build_filename (dir, localname, NULL);
#ifdef E2_VFS
					ddata.path = newpath;
					rt->opts->permission = e2_fs_access (&ddata, F_OK E2_ERR_NONE())
					 || !e2_fs_access (&ddata, W_OK E2_ERR_NONE());
#else
					rt->opts->permission = e2_fs_access (newpath, F_OK E2_ERR_NONE())
					 || !e2_fs_access (newpath, W_OK E2_ERR_NONE());
#endif
					F_FREE (localname, utfname);
					g_free (newpath);
				}
				else
					rt->opts->permission = TRUE;
			}
		}
	}
	else
		rt->opts->permission = FALSE;

	g_free (dir);
	return rt->opts->permission;
}
/**
@brief determine change-permission and set dialog button-sensitivities accordingly

@param rt pointer to dialog data struct

@return
*/
static void _e2pcr_set_buttons (E2P_CryptDlgRuntime *rt)
{
	gboolean ok, custom, encmode;
	ok = _e2pcr_check_permission (rt);
	//enable or disable button(s)
	if (rt->opts->multisrc)
	{
		if (ok)
		{
			//disable yes-to-all button (if any) when a custom name is to be used
			encmode = //(rt->dlgopen) ?
				gtk_toggle_button_get_active
				(GTK_TOGGLE_BUTTON (rt->mode_btn));// : !rt->opts->decryptmode;
			if (encmode)
				custom = //(rt->dlgopen) ?
				gtk_toggle_button_get_active
					(GTK_TOGGLE_BUTTON (rt->en_name_btn_custom));// : rt->opts->en_name_custom ;
			else
				custom =// (rt->dlgopen) ?
				gtk_toggle_button_get_active
					(GTK_TOGGLE_BUTTON (rt->de_name_btn_custom));// : rt->opts->de_name_custom ;
		}
		else
			custom = FALSE;	//warning prevention
		gtk_dialog_set_response_sensitive (GTK_DIALOG (rt->dialog), E2_RESPONSE_APPLYTOALL, ok & !custom);
	}
	gtk_dialog_set_response_sensitive (GTK_DIALOG (rt->dialog), E2_RESPONSE_APPLY, ok);

	ok = !((!rt->opts->decryptmode && rt->opts->en_name_custom)
		|| (rt->opts->decryptmode && rt->opts->de_name_custom));
	gtk_widget_set_sensitive (rt->recurse_btn, ok);
}
/**
@brief key-release callback

@param entry UNUSED the entry widget where the key was released
@param event pointer to event data struct
@param rt pointer to data struct for the search

@return FALSE always
*/
static gboolean _e2pcr_keyrel_cb (GtkWidget *entry, GdkEventKey *event,
	E2P_CryptDlgRuntime *rt)
{
	NEEDCLOSEBGL
	_e2pcr_set_buttons (rt);
	NEEDOPENBGL
	return FALSE;
}
/**
@brief callback for toggle encryption (mode) button

@param widget toggled encrypt button
@param rt pointer to dialog data struct

@return
*/
static void _e2pcr_toggle_mode_cb (GtkWidget *widget, E2P_CryptDlgRuntime *rt)
{
	gboolean state = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widget));
	NEEDCLOSEBGL
	if (state)
	{
		gtk_widget_hide (rt->decryptbox);
		gtk_widget_show (rt->encryptbox);
		gtk_widget_show (rt->confirmbox);
		gtk_widget_show (rt->compress_btn);
		gtk_widget_show (rt->validate_btn);
		gtk_widget_show (rt->en_name_embed_btn);
		gtk_widget_show (rt->en_properties_embed_btn);
		gtk_widget_hide (rt->properties_btn);
		gtk_widget_set_sensitive (rt->recurse_btn,
			!gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->en_name_btn_custom)));
		e2_button_set_label (rt->ok_btn, _("_Encrypt"));
	}
	else
	{
		gtk_widget_hide (rt->encryptbox);
		gtk_widget_show (rt->decryptbox);
		gtk_widget_hide (rt->confirmbox);
		gtk_widget_hide (rt->compress_btn);
		gtk_widget_hide (rt->validate_btn);
		gtk_widget_hide (rt->en_name_embed_btn);
		gtk_widget_hide (rt->en_properties_embed_btn);
		gtk_widget_show (rt->properties_btn);
		gtk_widget_set_sensitive (rt->recurse_btn,
			!gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->de_name_btn_custom)));
		e2_button_set_label (rt->ok_btn, _("_Decrypt"));
	}
//	gtk_widget_set_sensitive (rt->pwrt->pwentry2, state);
	rt->opts->decryptmode = !state;
	rt->pwrt->confirm = state; //make the password dialog check/not for matches

	//determine and handle permission
	_e2pcr_set_buttons (rt);
	NEEDOPENBGL
}
/**
@brief callback for encryped name toggle buttons

@param widget clicked button
@param rt pointer to dialog data struct

@return
*/
static void _e2pcr_toggle_encname_cb (GtkWidget *widget, E2P_CryptDlgRuntime *rt)
{
	if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widget)))
	{
		NEEDCLOSEBGL
		if (widget == rt->en_name_btn_suffix)
		{
			gtk_widget_set_sensitive (rt->en_name_suffix_entry, TRUE);
			gtk_widget_set_sensitive (rt->en_name_custom_entry, FALSE);
		}
		else if (widget == rt->en_name_btn_custom)
		{
			gtk_widget_set_sensitive (rt->en_name_custom_entry, TRUE);
			gtk_widget_set_sensitive (rt->en_name_suffix_entry, FALSE);
		}
		else //same name
		{
			gtk_widget_set_sensitive (rt->en_name_suffix_entry, FALSE);
			gtk_widget_set_sensitive (rt->en_name_custom_entry, FALSE);
		}
		gtk_widget_set_sensitive (rt->recurse_btn, widget != rt->en_name_btn_custom);
		//determine and handle permission
		_e2pcr_set_buttons (rt);
		NEEDOPENBGL
	}
}
/**
@brief callback for decryped name toggle buttons
Assumes BGL is off/open
@param widget clicked button
@param rt pointer to dialog data struct

@return
*/
static void _e2pcr_toggle_decname_cb (GtkWidget *widget, E2P_CryptDlgRuntime *rt)
{
	if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widget)))
	{
		NEEDCLOSEBGL
		if (widget == rt->de_name_btn_suffix)
		{
			gtk_widget_set_sensitive (rt->de_name_suffix_entry, TRUE);
			gtk_widget_set_sensitive (rt->de_name_custom_entry, FALSE);
		}
		else if (widget == rt->de_name_btn_custom)
		{
			gtk_widget_set_sensitive (rt->de_name_custom_entry, TRUE);
			gtk_widget_set_sensitive (rt->de_name_suffix_entry, FALSE);
		}
		else //same name
		{
			gtk_widget_set_sensitive (rt->de_name_suffix_entry, FALSE);
			gtk_widget_set_sensitive (rt->de_name_custom_entry, FALSE);
		}
		gtk_widget_set_sensitive (rt->recurse_btn, widget != rt->de_name_btn_custom);
		//determine and handle permission
		_e2pcr_set_buttons (rt);
		NEEDOPENBGL
	}
}
/**
@brief handle button click, window-close etc for crypt dialog
This is the callback for "response" signals emitted from @a dialog
@param dialog UNUSED the dialog where the response was generated
@param response the response returned from the dialog
@param rt pointer to data struct for the dialog

@return
*/
static void _e2pcr_response_cb (GtkDialog *dialog, gint response,
	E2P_CryptDlgRuntime *rt)
{
	gboolean finished;
	switch (response)
	{
		case E2_RESPONSE_APPLY:
		case E2_RESPONSE_APPLYTOALL:
			//only end if the password(s) are ok
			NEEDCLOSEBGL
			finished = e2_password_dialog_confirm (rt->pwrt);
			NEEDOPENBGL
			break;
		default:
			finished = TRUE;
			break;
	}

	rt->result = finished;	//confirm or refute that the returned enum is valid

	if (finished)
		e2_password_dialog_backup (rt->pwrt);	//backup static stuff
		//do not cleanup widgets here - done in main code
}
/**
@brief create and run an encryption-change dialog

@param options pointer to options data struct

@return enumerator corresponding to user's choice of action
*/
static DialogButtons _e2pcr_crypt_dialog_run (E2P_CryptOpts *options)
{
	const gchar *localpath = options->localpath;
	struct stat *sp = options->statptr;
#ifdef E2_VFS
	VPATH ddata = { localpath, options->spacedata };
	if (e2_fs_lstat (&ddata, sp E2_ERR_NONE()))
#else
	if (e2_fs_lstat (localpath, sp E2_ERR_NONE()))
#endif
		return CANCEL;
	if (!(S_ISREG (sp->st_mode) || S_ISDIR (sp->st_mode) || S_ISLNK (sp->st_mode)))
		return CANCEL;

	printd (DEBUG, "create crypt dialog");

	E2P_CryptDlgRuntime crt;
	gchar *pw = NULL;
	//create a temporary container from which we can later plunder some widgets
#ifdef USE_GTK3_0
	GtkWidget *tempbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
#else
	GtkWidget *tempbox = gtk_vbox_new (FALSE, 0);
#endif
	//create P/W widgets, with both entries, we'll hide one as appropriate

	crt.pwrt = e2_password_dialog_setup (tempbox, TRUE, NULL, &pw);

	if (crt.pwrt == NULL)
	{
		printd (ERROR, "Not enough memory for password widgets");
		//FIXME warn user about memory error
		gtk_widget_destroy (tempbox);
		return NO_TO_ALL;
	}

	crt.opts = options;

	CLOSEBGL
	crt.dialog = e2_dialog_create (NULL, NULL, _("en/decrypt file"),
		(ResponseFunc)_e2pcr_response_cb, &crt);
	OPENBGL

	GtkWidget *dialog_vbox =
#ifdef USE_GTK2_14
		gtk_dialog_get_content_area (GTK_DIALOG (crt.dialog));
#else
		GTK_DIALOG (crt.dialog)->vbox;
#endif
	//things that go before the password entries
	GString *label_text = g_string_sized_new (NAME_MAX+20);
	gchar *type;
	switch (sp->st_mode & S_IFMT)
	{
		case S_IFDIR:
			type = _("Directory");
			break;
		case S_IFLNK:
			type = _("Symbolic link");
			break;
//		case S_IFREG:
		default:
			type = _("Filename");
			break;
	}
	gchar *name = g_path_get_basename (localpath);
	gchar *utf = F_FILENAME_FROM_LOCALE (name);	//needed for naming
	g_string_printf (label_text, "%s: <b>%s</b>", type, utf);
	if (S_ISLNK (sp->st_mode))
	{
		gchar *target = g_strdup (localpath);
		if (e2_fs_walk_link (&target E2_ERR_NONE()))
		{
			gchar *utfname = F_DISPLAYNAME_FROM_LOCALE (target);
			g_string_append_printf (label_text, _(" to %s"), utfname);
			F_FREE (utfname, target);
		}
		g_free (target);
	}
	e2_widget_add_mid_label (dialog_vbox, label_text->str, 0, TRUE, E2_PADDING); //L, R padding
	GtkWidget *hbox = e2_widget_add_box (dialog_vbox, FALSE, 0, FALSE, TRUE, 0);
	crt.mode_btn = e2_button_add_radio (hbox, _("_encrypt"), NULL,
		!options->decryptmode, FALSE, 0, NULL, NULL);	//set callback later, after widgets created
	GSList *group = gtk_radio_button_get_group (GTK_RADIO_BUTTON (crt.mode_btn));
	e2_button_add_radio (hbox, _("_decrypt"), group, options->decryptmode, FALSE, 0, NULL, NULL);

	hbox = e2_widget_add_box (dialog_vbox, FALSE, 0, FALSE, FALSE, 0);
	//the existing label in tempbox is centred, might as well set a new one
	GtkWidget *wid =
	e2_widget_add_mid_label (hbox, _("Enter password:"), 0, FALSE, E2_PADDING_SMALL); //L, R padding
	GtkSizeGroup *same = gtk_size_group_new (GTK_SIZE_GROUP_HORIZONTAL);
	gtk_size_group_add_widget (same, wid);
	//supply a previously-entered password (but not the confirmation one when encrypting?)
	if (options->plain_pw != NULL)
	{
		gtk_entry_set_text (GTK_ENTRY (crt.pwrt->pwentry1), options->plain_pw);
		gtk_editable_select_region (GTK_EDITABLE (crt.pwrt->pwentry1), 0, -1);
	}
	wid = e2_widget_add_box (hbox, TRUE, E2_PADDING, FALSE, FALSE, 0);
	gtk_widget_reparent (crt.pwrt->pwentry1, wid);

	crt.confirmbox = e2_widget_add_box (dialog_vbox, FALSE, 0, FALSE, FALSE, 0);
	wid =
	e2_widget_add_mid_label (crt.confirmbox, _("Confirm password:"), 0, FALSE, E2_PADDING_SMALL); //L, R padding
	gtk_size_group_add_widget (same, wid);
	g_object_unref (G_OBJECT (same));
	if (options->plain_pw != NULL)
	{
		gtk_entry_set_text (GTK_ENTRY (crt.pwrt->pwentry2), options->plain_pw);
	}
	wid = e2_widget_add_box (crt.confirmbox, TRUE, E2_PADDING, FALSE, FALSE, 0);
	gtk_widget_reparent (crt.pwrt->pwentry2, wid);
	gtk_widget_destroy (tempbox);	//finished with that now

#ifdef USE_GTK3_0
	crt.encryptbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
#else
	crt.encryptbox = gtk_vbox_new (FALSE, 0);
#endif
	crt.en_name_btn_same = e2_button_add_radio (crt.encryptbox,
		_("encrypted file will have _same name"), NULL, options->en_name_same,
		FALSE, 0, NULL, NULL);	//set callback later, after widgets created;
	hbox = e2_widget_add_box (crt.encryptbox, TRUE, 0, FALSE, FALSE, 0);
	group = gtk_radio_button_get_group (GTK_RADIO_BUTTON (crt.en_name_btn_same));
	crt.en_name_btn_suffix = e2_button_add_radio (hbox,
		_("_append this to encrypted file name"), group, options->en_name_suffix,
		FALSE, 0, NULL, NULL);

	//setup all entries in 2 steps to avoid selecting entry text
	crt.en_name_suffix_entry = e2_widget_add_entry (hbox, NULL, TRUE, FALSE);
	gtk_entry_set_text (GTK_ENTRY (crt.en_name_suffix_entry), options->en_suffix);
	gtk_widget_set_size_request (crt.en_name_suffix_entry, 80, -1);
	gtk_widget_set_sensitive (crt.en_name_suffix_entry, options->en_name_suffix);
	//enable permissions change after any change
	g_signal_connect_after (G_OBJECT (crt.en_name_suffix_entry), "key-release-event",
		G_CALLBACK (_e2pcr_keyrel_cb), &crt);

	hbox = e2_widget_add_box (crt.encryptbox, TRUE, 0, FALSE, TRUE, 0);
	group = gtk_radio_button_get_group (GTK_RADIO_BUTTON (crt.en_name_btn_same));
	crt.en_name_btn_custom = e2_button_add_radio (hbox,
		_("encrypted file _name will be"), group, options->en_name_custom,
		FALSE, 0, NULL, NULL);
	crt.en_name_custom_entry = e2_widget_add_entry (hbox, NULL, TRUE, FALSE);
	gtk_entry_set_text (GTK_ENTRY (crt.en_name_custom_entry), utf);
	gtk_widget_set_sensitive (crt.en_name_custom_entry, options->en_name_custom);
#ifdef E2_ASSISTED
	e2_widget_set_label_relations (crt.en_name_btn_custom, crt.en_name_custom_entry);
#endif
	g_signal_connect_after (G_OBJECT (crt.en_name_custom_entry), "key-release-event",
		G_CALLBACK (_e2pcr_keyrel_cb), &crt);

	gtk_box_pack_start (GTK_BOX (dialog_vbox), crt.encryptbox, TRUE, TRUE, 0); //top, bottom padding

#ifdef USE_GTK3_0
	crt.decryptbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
#else
	crt.decryptbox = gtk_vbox_new (FALSE, 0);
#endif
	crt.de_name_btn_same = e2_button_add_radio (crt.decryptbox,
		_("decrypted file will have _same name"), NULL, options->de_name_same,
		FALSE, 0, NULL, NULL);	//set callback later, after widgets created;
	group = gtk_radio_button_get_group (GTK_RADIO_BUTTON (crt.de_name_btn_same));
	crt.de_name_btn_stored = e2_button_add_radio (crt.decryptbox,
		_("decrypted file will have e_mbedded name, if any"), group, options->de_name_stored,
		FALSE, 0, NULL, NULL);
	hbox = e2_widget_add_box (crt.decryptbox, TRUE, 0, FALSE, FALSE, 0);
	group = gtk_radio_button_get_group (GTK_RADIO_BUTTON (crt.de_name_btn_same));
	crt.de_name_btn_suffix = e2_button_add_radio (hbox,
		_("strip this _from end of decrypted file name"), group, options->de_name_suffix,
		FALSE, 0, NULL, NULL);

	crt.de_name_suffix_entry = e2_widget_add_entry (hbox, NULL, TRUE, FALSE);
	gtk_entry_set_text (GTK_ENTRY (crt.de_name_suffix_entry), options->de_suffix);
	gtk_widget_set_size_request (crt.de_name_suffix_entry, 80, -1);
	gtk_widget_set_sensitive (crt.de_name_suffix_entry, options->de_name_suffix);
#ifdef E2_ASSISTED
	e2_widget_set_label_relations (crt.de_name_btn_suffix, crt.de_name_suffix_entry);
#endif
	g_signal_connect_after (G_OBJECT (crt.de_name_suffix_entry), "key-release-event",
		G_CALLBACK (_e2pcr_keyrel_cb), &crt);

	hbox = e2_widget_add_box (crt.decryptbox, TRUE, 0, FALSE, TRUE, 0);
	group = gtk_radio_button_get_group (GTK_RADIO_BUTTON (crt.de_name_btn_same));
	crt.de_name_btn_custom = e2_button_add_radio (hbox,
		_("decrypted file _name will be"), group, options->de_name_custom,
		FALSE, 0, NULL, NULL);
	crt.de_name_custom_entry = e2_widget_add_entry (hbox, NULL, TRUE, FALSE);
	gtk_entry_set_text (GTK_ENTRY (crt.de_name_custom_entry), utf);
	gtk_widget_set_sensitive (crt.de_name_custom_entry, options->de_name_custom);
#ifdef E2_ASSISTED
	e2_widget_set_label_relations (crt.de_name_btn_custom, crt.de_name_custom_entry);
#endif
	g_signal_connect_after (G_OBJECT (crt.de_name_custom_entry), "key-release-event",
		G_CALLBACK (_e2pcr_keyrel_cb), &crt);
	gtk_box_pack_start (GTK_BOX (dialog_vbox), crt.decryptbox, FALSE, FALSE, E2_PADDING); //top, bottom padding

	crt.properties_btn = e2_button_add_toggle (dialog_vbox, TRUE, options->de_props_stored,
		_("restore _properties"), _("apply any stored permissions, owners, dates to decrypted file"),
		TRUE, 0, NULL, NULL);
	hbox = e2_widget_add_box (dialog_vbox, FALSE, 0, FALSE, TRUE, E2_PADDING_SMALL);
#ifdef E2_MINICRYPT
	crt.compress_btn = e2_button_add_toggle (hbox, TRUE, options->compress,
		_("compress"), _("compress file before encryption"), FALSE, 0, NULL, NULL);
#else
	crt.compress_btn = e2_button_add_toggle (hbox, TRUE,
		(options->compress && (compresslib & E2_CFLAGLIBMASK)),
		_("_compress"), _("compress file before encryption"), FALSE, 0, NULL, NULL);
	if (!(compresslib & E2_CFLAGLIBMASK))
	{
		options->compress = FALSE;
		gtk_widget_set_sensitive (crt.compress_btn, FALSE);
	}
#endif
	crt.validate_btn = e2_button_add_toggle (hbox, TRUE, options->validate,
		_("_validate"), _("setup for validation when decrypting"), FALSE, 0, NULL, NULL);
	hbox = e2_widget_add_box (dialog_vbox, FALSE, 0, FALSE, TRUE, E2_PADDING_SMALL);
	crt.en_name_embed_btn = e2_button_add_toggle (hbox, TRUE, options->en_name_embed,
		_("store _filename"), _("store current name for application after decryption"), FALSE, 0, NULL, NULL);
	crt.en_properties_embed_btn = e2_button_add_toggle (hbox, TRUE, options->en_properties_embed,
		_("store _properties"), _("store current permissions etc for application after decryption"), FALSE, 0, NULL, NULL);
	hbox = e2_widget_add_box (dialog_vbox, FALSE, 0, FALSE, TRUE, E2_PADDING_SMALL);
	crt.backup_btn = e2_button_add_toggle (hbox, TRUE, options->backup,
		_("_backup"), _("backup an existing file with the same name as the processed file"), FALSE, 0, NULL, NULL);
	crt.preserve_btn = e2_button_add_toggle (hbox, TRUE, options->preserve,
		_("_keep original"), _("do not remove the original file, after processing it"), FALSE, 0, NULL, NULL);
	hbox = e2_widget_add_box (dialog_vbox, FALSE, 0, FALSE, TRUE, E2_PADDING_SMALL);
	crt.linktarget_btn = e2_button_add_toggle (hbox, TRUE, options->walklinks,
		_("through _links"), _("if file is a symlink, process its target"), FALSE, 0, NULL, NULL);
	crt.recurse_btn = e2_button_add_toggle (hbox, TRUE, options->recurse,
		_("_recurse directories"), NULL, FALSE, 0, NULL, NULL);

	//now it's safe to connect toggle button callbacks
	g_signal_connect (G_OBJECT (crt.mode_btn), "toggled",
		G_CALLBACK (_e2pcr_toggle_mode_cb), &crt);
	g_signal_connect (G_OBJECT (crt.en_name_btn_same), "toggled",
		G_CALLBACK (_e2pcr_toggle_encname_cb), &crt);
	g_signal_connect (G_OBJECT (crt.en_name_btn_suffix), "toggled",
		G_CALLBACK (_e2pcr_toggle_encname_cb), &crt);
	g_signal_connect (G_OBJECT (crt.en_name_btn_custom), "toggled",
		G_CALLBACK (_e2pcr_toggle_encname_cb), &crt);
	g_signal_connect (G_OBJECT (crt.de_name_btn_same), "toggled",
		G_CALLBACK (_e2pcr_toggle_decname_cb), &crt);
	g_signal_connect (G_OBJECT (crt.de_name_btn_suffix), "toggled",
		G_CALLBACK (_e2pcr_toggle_decname_cb), &crt);
	g_signal_connect (G_OBJECT (crt.de_name_btn_custom), "toggled",
		G_CALLBACK (_e2pcr_toggle_decname_cb), &crt);
	g_signal_connect (G_OBJECT (crt.de_name_btn_stored), "toggled",
		G_CALLBACK (_e2pcr_toggle_decname_cb), &crt);

	if (options->decryptmode)
	{
		gtk_widget_show (crt.decryptbox);
		gtk_widget_hide (crt.confirmbox);
		gtk_widget_hide (crt.compress_btn);
		gtk_widget_hide (crt.validate_btn);
		gtk_widget_hide (crt.en_name_embed_btn);
		gtk_widget_hide (crt.en_properties_embed_btn);
	}
	else
	{
		gtk_widget_show (crt.encryptbox);
		gtk_widget_hide (crt.properties_btn);
	}

	E2_Button no_btn;
	if (options->multisrc)
	{
		e2_dialog_set_negative_response (crt.dialog, E2_RESPONSE_NOTOALL);
		e2_dialog_add_defined_button (crt.dialog, &E2_BUTTON_CANCEL);
		e2_dialog_add_defined_button (crt.dialog, &E2_BUTTON_APPLYTOALL);
		e2_button_derive (&no_btn, &E2_BUTTON_NO, BTN_NO_SKIP);
	}
	else
	{
		e2_dialog_set_negative_response (crt.dialog, GTK_RESPONSE_NO);
		e2_button_derive (&no_btn, &E2_BUTTON_NO, BTN_NO_CANCEL);
	}
//	no_btn.showflags |= E2_BTN_DEFAULT;
	e2_dialog_add_defined_button (crt.dialog, &no_btn);

	E2_Button yes_btn;
	yes_btn = E2_BUTTON_YES;
	yes_btn.label = (options->decryptmode) ? _("_Decrypt") : _("_Encrypt");
	yes_btn.showflags &= ~E2_BTN_DEFAULT;

	crt.ok_btn = e2_dialog_add_defined_button (crt.dialog, &yes_btn);
	//set where to focus when confirmation entry is activated
	crt.pwrt->focus = crt.ok_btn;

	_e2pcr_set_buttons (&crt);

	crt.pwrt->confirm = !options->decryptmode; //make the password dialog check/not for matches
	crt.dlgopen = TRUE;

	CLOSEBGL
	gtk_widget_show_all (dialog_vbox);
	NEEDOPENBGL
	_e2pcr_toggle_mode_cb (crt.mode_btn, &crt);
	NEEDCLOSEBGL

	e2_dialog_setup (crt.dialog, app.main_window);
	e2_dialog_run (crt.dialog, NULL, E2_DIALOG_DONT_SHOW_ALL);
	gtk_widget_grab_focus (crt.pwrt->pwentry1);

	crt.result = FALSE;
	DialogButtons retval;
	while (!crt.result)
		retval = e2_dialog_wait (crt.dialog, TRUE, FALSE,
			options->multisrc, TRUE); //CHECKME TRUE maincontext
	OPENBGL

	//remember most of the current dialog data
	options->decryptmode =
		!gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (crt.mode_btn));
	options->en_name_same =		//encrypted file name = same as onencrypted name
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (crt.en_name_btn_same));
	options->en_name_suffix =	//encrypted file name has user-specified suffix (if any)
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (crt.en_name_btn_suffix));
	options->en_name_custom =	//encrypted file name = user-specified
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (crt.en_name_btn_custom));
	options->en_name_embed =	//store filenama in encrypted file
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (crt.en_name_embed_btn));
	options->en_properties_embed =	//store filenama and statbuf in encrypted file
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (crt.en_properties_embed_btn));
	options->de_name_same =		//decrypted file name = same as encrypted name
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (crt.de_name_btn_same));
	options->de_name_stored =	//decrypted file name = embedded original name
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (crt.de_name_btn_stored));
	options->de_name_suffix =	//decrypted file name omits user-specified suffix (if any)
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (crt.de_name_btn_suffix));
	options->de_name_custom =	//decrypted file name = user-specified
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (crt.de_name_btn_custom));
	options->de_props_stored =	//reinstate other properties of original file
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (crt.properties_btn));
	options->compress =	//compress file before encryption
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (crt.compress_btn));
	options->validate =	//validate file when decrypting
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (crt.validate_btn));
	options->backup =	//preserve any file with same name as specified for the [de]crypted file
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (crt.backup_btn));
	options->preserve =	//preserve the file to be [de]crypted, with alternate name if appropriate
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (crt.preserve_btn));
	options->recurse =	//recursively process all files in any selected dir
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (crt.recurse_btn));
	options->walklinks =//process link targets
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (crt.linktarget_btn));

	if (options->en_name != NULL)
		g_free (options->en_name);
	options->en_name = g_strdup (gtk_entry_get_text (GTK_ENTRY (crt.en_name_custom_entry)));	//user-specified suffix, freeable utf-8
	if (options->en_suffix != NULL)
		g_free (options->en_suffix);
	options->en_suffix = g_strdup (gtk_entry_get_text (GTK_ENTRY (crt.en_name_suffix_entry)));	//user-specified suffix, freeable utf-8
	if (options->de_name != NULL)
		g_free (options->de_name);
	options->de_name = g_strdup (gtk_entry_get_text (GTK_ENTRY (crt.de_name_custom_entry)));	//user-specified suffix, freeable utf-8
	if (options->de_suffix != NULL)
		g_free (options->de_suffix);
	options->de_suffix = g_strdup (gtk_entry_get_text (GTK_ENTRY (crt.de_name_suffix_entry)));	//user-specified suffix, freeable utf-8

	//remember password only if a single-item is to be processed,
	//and permission is ok and password is not empty
	if (retval == OK || retval == YES_TO_ALL || (retval == CANCEL && options->multisrc))
	{
		if (pw == NULL || *pw == '\0')
			retval = CANCEL;
		else
		{
			if (retval != CANCEL)
			{
				_e2pcr_check_permission (&crt);
				if (!options->permission)
					retval = CANCEL;
			}
			if (options->plain_pw != NULL)
				g_free (options->plain_pw);
			options->plain_pw = pw;
		}
	}
	if (!(retval == OK || retval == YES_TO_ALL || (retval == CANCEL && options->multisrc)))
	{
		if (options->plain_pw != NULL)
		{
			g_free (options->plain_pw);
			options->plain_pw = NULL;
		}
	}

	g_free (name);
	F_FREE (utf, name);

	if (GTK_IS_DIALOG(crt.dialog))
	{	//not explicitly closed by the user
		CLOSEBGL
		gtk_widget_destroy (crt.dialog);
		OPENBGL
	}
	//do not free p/w string - it's saved at options->plain_pw
	DEALLOCATE (E2_PWDataRuntime, crt.pwrt);
	return retval;
}

/**
@brief encryt or decrypt selected item(s) in active pane
If > 1 item is selected, a dialog is created for each such item in turn (or
until the user chooses stop or apply-to-all)
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if action completed successfully, else FALSE
*/
static gboolean _e2p_task_docrypt (gpointer from, E2_ActionRuntime *art)
{
	return (e2_task_enqueue_task (E2_TASK_CRYPT, art, from,
		_e2p_task_docryptQ, e2_task_refresh_lists));
}
static gboolean _e2p_task_docryptQ (E2_ActionTaskData *qed)
{
	//printd (DEBUG, "task: crypt");
#ifdef E2_VFSTMP
	if (qed->currspace != NULL)	//not a local space
		return FALSE;
#endif

	struct stat sb;	//statbuf for general use
	E2P_CryptOpts options = session_opts;
	options.permission = FALSE;
	options.ignore_suffix = FALSE;
	options.owrite = FALSE;
	options.en_suffix = g_strdup (session_opts.en_suffix);
	options.de_suffix = g_strdup (session_opts.de_suffix);
	options.plain_pw = NULL;	//probably redundant
	options.statptr = &sb;
	options.dirdata = NULL;
#ifdef E2_VFS
	options.spacedata = qed->currspace;
#endif

	GPtrArray *names = qed->names;
	options.multisrc = names->len > 1;
	gchar *curr_local = qed->currdir;
	guint count;
	gboolean all = FALSE;
	E2_SelectedItemInfo **iterator = (E2_SelectedItemInfo **) names->pdata;
	//guess the initial mode
	//too bad if 1st item is a dir with the expected suffix
	gchar *utf = F_FILENAME_TO_LOCALE ((*iterator)->filename);
	options.decryptmode =
		(*session_opts.de_suffix != '\0' && g_str_has_suffix (utf, session_opts.de_suffix));
	F_FREE (utf, (*iterator)->filename);
	GString *path = g_string_sized_new (PATH_MAX);

	e2_task_advise ();
#ifdef E2_REFRESH_DEBUG
	printd (DEBUG, "disable refresh, crypt task");
#endif
	e2_filelist_disable_refresh ();

#ifdef E2_VFS
	VPATH ddata;
	ddata.spacedata = qed->currspace;
#endif

	for (count = 0; count < names->len; count++, iterator++)
	{
		DialogButtons choice;
		//".." entries filtered when names compiled
//FIXME for single-setup: instead of the following, adjust file details in dialog, reset default button
//		gchar *itempath = e2_utils_dircat (curr_view, (*iterator)->filename, TRUE);
		g_string_printf (path, "%s%s", curr_local, (*iterator)->filename); //separator comes with dir
		options.localpath = path->str;
		if (all)
		{
			//check if we have permission to change this item
			E2P_CryptDlgRuntime crt;
			crt.opts = &options;
			crt.dlgopen = FALSE;
			if (_e2pcr_check_permission (&crt))
				choice = OK;
			else
			{
#ifdef E2_VFS
				ddata.path = (*iterator)->filename;
#endif
				e2_fs_error_simple (
					_("You do not have authority to modify %s"),
#ifdef E2_VFS
					&ddata);
#else
					(*iterator)->filename);
#endif
				choice = CANCEL;
			}
		}
		else
		{
#ifdef E2_REFRESH_DEBUG
			printd (DEBUG, "enable refresh, encrypt dialog");
#endif
			e2_filelist_enable_refresh ();  //allow updates while we wait
			*qed->status = E2_TASK_PAUSED;
			choice = _e2pcr_crypt_dialog_run (&options);
			*qed->status = E2_TASK_RUNNING;
#ifdef E2_REFRESH_DEBUG
			printd (DEBUG, "disable refresh, encrypt dialog");
#endif
			e2_filelist_disable_refresh ();
		}

		switch (choice)
		{
		  case YES_TO_ALL:
			all = TRUE;
//			myuid = getuid ();  //do this once, for speed
			choice = OK;
		  case OK:
			if (options.permission)	// && (axs_changes != NULL || def_changes != NULL))
			{
#ifdef E2_INCLIST
				if ((choice = _e2pcr_apply (&options)) == OK)
				{
					//FIXME update line in treeview
				}
#else
# ifdef E2_FAM
				choice = _e2pcr_apply (&options);
# else
				if ((choice = _e2pcr_apply (&options)) == OK)
#  ifdef E2_VFS
				{
					if (ddata.spacedata == NULL)
					{
						ddata.path = qed->currdir;
						e2_fs_touchnow (&ddata E2_ERR_NONE());
					}
				}
#  else
				//make the file-list refresher notice successful change
					e2_fs_touchnow (qed->currdir E2_ERR_NONE());
#  endif
# endif
#endif
/*				if (!all)
				{
					CLEANUPS
				}
*/
			}
		  case CANCEL:
			break;
		  default:
			choice = NO_TO_ALL;  // break flag;
			break;
		}
		if (choice == NO_TO_ALL)
			break;
	}

	//backup relevant last-used option data
	g_free (session_opts.en_suffix);
	g_free (session_opts.de_suffix);
	if (options.en_name != NULL)
	{
		g_free (options.en_name);
		options.en_name = NULL;
	}
	if (options.de_name != NULL)
	{
		g_free (options.de_name);
		options.de_name = NULL;
	}
	if (options.plain_pw != NULL)
	{
		g_free (options.plain_pw);
		options.plain_pw = NULL;
	}
	session_opts = options;

	g_string_free (path, TRUE);
	e2_window_clear_status_message ();
#ifdef E2_REFRESH_DEBUG
	printd (DEBUG, "enable refresh, acl task");
#endif
	e2_filelist_enable_refresh ();

#ifndef E2_MINICRYPT
	if (options.libhandle != NULL)	//disconnect from the compression library
	{
		dlclose (options.libhandle);
		session_opts.libhandle = NULL;
		session_opts.compresslibflags = 0;
//		lzma_init_done = FALSE; //ditto for lzma
	}
#endif

	return TRUE;
}

/**
@brief plugin initialization function, called by main program

@param mode flags enumerating what sort of init to perform

@return Plugin*, with refcount 1 if @a mode included runtime setup and that succeeded
*/
Plugin *init_plugin (E2PInit mode)
{
	const gchar *aname = _("crypt");

	PLUGINIT_ONE_START(_A(6),aname,_e2p_task_docrypt,
		_("_En/decrypt.."),
		_("Encrypt or decrypt selected items"),
		"plugin_"ANAME E2ICONTB)

	session_opts.en_suffix = g_strdup (".enc");	//no translation
	session_opts.de_suffix = g_strdup (".enc");	//no translation

#ifndef E2_MINICRYPT
	gint deflib = -1;
	//detect available compression libs
	gpointer libhandle;
	gint (*initfunc) ();
	//first try, fastest performer, lzo
	if ((libhandle = dlopen ("liblzo2.so.2", RTLD_LAZY | RTLD_LOCAL
#ifdef RTLD_DEEPBIND
		| RTLD_DEEPBIND
#endif
		)) != NULL)
	{
		if (dlsym (libhandle, LZO_COMPRESSFUNC) != NULL)
		{
			if (dlsym (libhandle, LZO_DECOMPRESSFUNC) != NULL)
			{
				initfunc = dlsym (libhandle, "__lzo_init_v2");	//a #define in lzoconf.h
				if (initfunc != NULL)
				{
					//this hack is not much better than no init at all !
					gint result = initfunc (2,
						sizeof(gshort),
						sizeof(gint),
						sizeof(glong),
						sizeof(guint32),
						sizeof(guint),
						sizeof(guchar *), //dict_t
						sizeof(gchar *),
						sizeof(gpointer),
						-1	//sizeof(lzo_callback_t));
						);
					if (result == 0)
					{
//actual usage in another thread, re-init there  lzo_init_done = TRUE;
						compresslib |= E2_CFLAGLZO;	//we have this one
						deflib = 0;
					}
				}
			}
		}
		dlclose (libhandle);
	}
#ifdef DEBUG_MESSAGES
	if (!(compresslib & E2_CFLAGLZO))
		printd (DEBUG, "Cannot [de]compress using LZO lib");
#endif
	//second, intermediate speed, zlib
	if ((libhandle = dlopen ("libz.so.1", RTLD_LAZY | RTLD_LOCAL
#ifdef RTLD_DEEPBIND
		| RTLD_DEEPBIND
#endif
		)) != NULL)
	{
		if (dlsym (libhandle, GZIP_COMPRESSFUNC) != NULL)
		{
			if (dlsym (libhandle, GZIP_DECOMPRESSFUNC) != NULL)
			{
				compresslib |= E2_CFLAGZ;	//and we have this one
				if (deflib == -1)
					deflib = 1;
			}

		}
		dlclose (libhandle);
	}
#ifdef DEBUG_MESSAGES
	if (!(compresslib & E2_CFLAGZ))
		printd (DEBUG, "Cannot [de]compress using libz");
#endif
	//next, libbz2
	if ((libhandle = dlopen ("libbz2.so.1", RTLD_LAZY | RTLD_LOCAL
#ifdef RTLD_DEEPBIND
		| RTLD_DEEPBIND
#endif
		)) != NULL)
	{
		if (dlsym (libhandle, BZIP2_COMPRESSFUNC) != NULL)
		{
			if (dlsym (libhandle, BZIP2_DECOMPRESSFUNC) != NULL)
			{
				compresslib |= E2_CFLAGBZ2;	//and we have this one
				if (deflib == -1)
					deflib = 2;
			}
		}
		dlclose (libhandle);
	}
#ifdef DEBUG_MESSAGES
	if (!(compresslib & E2_CFLAGBZ2))
		printd (DEBUG, "Cannot [de]compress using libbzip2");
#endif
/*	//lzma (7-zip) compression supports streaming, slower compression than
	bzip2, compression ratio and decryption speed between gzip and bzip2
	if ((libhandle = dlopen ("liblzma.so.0", RTLD_LAZY | RTLD_LOCAL
#ifdef RTLD_DEEPBIND
		| RTLD_DEEPBIND
#endif
		)) != NULL)
	{
		if (dlsym (libhandle, LZMA_COMPRESSFUNC) != NULL)
		{
			if (dlsym (libhandle, LZMA_DECOMPRESSFUNC) != NULL)
			{
				compresslib |= E2_CFLAGLZMA;	//and we have this one
				if (deflib == -1)
					deflib = 3;
			}
		}
		dlclose (libhandle);
	}
#ifdef DEBUG_MESSAGES
	if (!(compresslib & E2_CFLAGLZMA))
		printd (DEBUG, "Cannot [de]compress using liblzma");
#endif
*/
	//no flags set in compresslib if nothing found
	printd (DEBUG, "masked lib-flags %0x", compresslib & E2_CFLAGLIBMASK);

	if (deflib == -1)
		deflib = 0;	//no valid lib found, just set the first one
	gchar *group = g_strconcat(_C(34),".",_C(27),":",aname,NULL); //_("plugins.options:crypt"
	E2_OptionSetupExtra ex;
	memset (&ex, 0, sizeof (E2_OptionSetupExtra));
	ex.exsel.def = deflib;
	ex.exsel.values = libnames;
	E2_OptionSet *set =
	e2_plugins_option_register (E2_OPTION_TYPE_SEL, "compress-library",
		group, _("compression type"),
		_("Use this form of compression before encryption"),
		NULL, &ex, E2_OPTION_FLAG_FREEGROUP | E2_OPTION_FLAG_ADVANCED);
	//because plugins are loaded after config data, config options need to
	//get any data from unknown-options data
	e2_option_transient_value_get (set);

#endif //ndef E2_MINICRYPT

	PLUGINIT_ONE_END
}
/**
@brief cleanup transient things for this plugin

@param p pointer to data struct for the plugin

@return TRUE if all cleanups were completed
*/
gboolean clean_plugin (Plugin *p)
{
	PLUGIN_CLEAR_ACTIONS (p)

	if (ret)
	{
		g_free (session_opts.en_suffix);
		g_free (session_opts.de_suffix);
	}
#ifndef E2_MINICRYPT
	return (ret && e2_plugins_option_unregister ("compress-library"));
#else
	return ret;
#endif
}
