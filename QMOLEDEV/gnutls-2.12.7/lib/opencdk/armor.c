/* armor.c - Armor filters
 * Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2007, 2008, 2010
 * Free Software Foundation, Inc.
 *
 * Author: Timo Schulz
 *
 * This file is part of OpenCDK.
 *
 * The OpenCDK library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
 * USA
 *
 * ChangeLog for basic BASE64 code (base64_encode, base64_decode):
 * Original author: Eric S. Raymond (Fetchmail)
 * Heavily modified by Brendan Cully <brendan@kublai.com> (Mutt)
 * Modify the code for generic use by Timo Schulz <twoaday@freakmail.de>
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>

#include "opencdk.h"
#include "main.h"
#include "filters.h"

#ifdef __MINGW32__
#define LF "\r\n"
#define ALTLF "\n"
#else
#define LF "\n"
#define ALTLF "\r\n"
#endif

#define CRCINIT 0xB704CE

#define BAD -1
#define b64val(c) index64[(unsigned int)(c)]

static u32 crc_table[] = {
  0x000000, 0x864CFB, 0x8AD50D, 0x0C99F6, 0x93E6E1, 0x15AA1A, 0x1933EC,
  0x9F7F17,
  0xA18139, 0x27CDC2, 0x2B5434, 0xAD18CF, 0x3267D8, 0xB42B23, 0xB8B2D5,
  0x3EFE2E,
  0xC54E89, 0x430272, 0x4F9B84, 0xC9D77F, 0x56A868, 0xD0E493, 0xDC7D65,
  0x5A319E,
  0x64CFB0, 0xE2834B, 0xEE1ABD, 0x685646, 0xF72951, 0x7165AA, 0x7DFC5C,
  0xFBB0A7,
  0x0CD1E9, 0x8A9D12, 0x8604E4, 0x00481F, 0x9F3708, 0x197BF3, 0x15E205,
  0x93AEFE,
  0xAD50D0, 0x2B1C2B, 0x2785DD, 0xA1C926, 0x3EB631, 0xB8FACA, 0xB4633C,
  0x322FC7,
  0xC99F60, 0x4FD39B, 0x434A6D, 0xC50696, 0x5A7981, 0xDC357A, 0xD0AC8C,
  0x56E077,
  0x681E59, 0xEE52A2, 0xE2CB54, 0x6487AF, 0xFBF8B8, 0x7DB443, 0x712DB5,
  0xF7614E,
  0x19A3D2, 0x9FEF29, 0x9376DF, 0x153A24, 0x8A4533, 0x0C09C8, 0x00903E,
  0x86DCC5,
  0xB822EB, 0x3E6E10, 0x32F7E6, 0xB4BB1D, 0x2BC40A, 0xAD88F1, 0xA11107,
  0x275DFC,
  0xDCED5B, 0x5AA1A0, 0x563856, 0xD074AD, 0x4F0BBA, 0xC94741, 0xC5DEB7,
  0x43924C,
  0x7D6C62, 0xFB2099, 0xF7B96F, 0x71F594, 0xEE8A83, 0x68C678, 0x645F8E,
  0xE21375,
  0x15723B, 0x933EC0, 0x9FA736, 0x19EBCD, 0x8694DA, 0x00D821, 0x0C41D7,
  0x8A0D2C,
  0xB4F302, 0x32BFF9, 0x3E260F, 0xB86AF4, 0x2715E3, 0xA15918, 0xADC0EE,
  0x2B8C15,
  0xD03CB2, 0x567049, 0x5AE9BF, 0xDCA544, 0x43DA53, 0xC596A8, 0xC90F5E,
  0x4F43A5,
  0x71BD8B, 0xF7F170, 0xFB6886, 0x7D247D, 0xE25B6A, 0x641791, 0x688E67,
  0xEEC29C,
  0x3347A4, 0xB50B5F, 0xB992A9, 0x3FDE52, 0xA0A145, 0x26EDBE, 0x2A7448,
  0xAC38B3,
  0x92C69D, 0x148A66, 0x181390, 0x9E5F6B, 0x01207C, 0x876C87, 0x8BF571,
  0x0DB98A,
  0xF6092D, 0x7045D6, 0x7CDC20, 0xFA90DB, 0x65EFCC, 0xE3A337, 0xEF3AC1,
  0x69763A,
  0x578814, 0xD1C4EF, 0xDD5D19, 0x5B11E2, 0xC46EF5, 0x42220E, 0x4EBBF8,
  0xC8F703,
  0x3F964D, 0xB9DAB6, 0xB54340, 0x330FBB, 0xAC70AC, 0x2A3C57, 0x26A5A1,
  0xA0E95A,
  0x9E1774, 0x185B8F, 0x14C279, 0x928E82, 0x0DF195, 0x8BBD6E, 0x872498,
  0x016863,
  0xFAD8C4, 0x7C943F, 0x700DC9, 0xF64132, 0x693E25, 0xEF72DE, 0xE3EB28,
  0x65A7D3,
  0x5B59FD, 0xDD1506, 0xD18CF0, 0x57C00B, 0xC8BF1C, 0x4EF3E7, 0x426A11,
  0xC426EA,
  0x2AE476, 0xACA88D, 0xA0317B, 0x267D80, 0xB90297, 0x3F4E6C, 0x33D79A,
  0xB59B61,
  0x8B654F, 0x0D29B4, 0x01B042, 0x87FCB9, 0x1883AE, 0x9ECF55, 0x9256A3,
  0x141A58,
  0xEFAAFF, 0x69E604, 0x657FF2, 0xE33309, 0x7C4C1E, 0xFA00E5, 0xF69913,
  0x70D5E8,
  0x4E2BC6, 0xC8673D, 0xC4FECB, 0x42B230, 0xDDCD27, 0x5B81DC, 0x57182A,
  0xD154D1,
  0x26359F, 0xA07964, 0xACE092, 0x2AAC69, 0xB5D37E, 0x339F85, 0x3F0673,
  0xB94A88,
  0x87B4A6, 0x01F85D, 0x0D61AB, 0x8B2D50, 0x145247, 0x921EBC, 0x9E874A,
  0x18CBB1,
  0xE37B16, 0x6537ED, 0x69AE1B, 0xEFE2E0, 0x709DF7, 0xF6D10C, 0xFA48FA,
  0x7C0401,
  0x42FA2F, 0xC4B6D4, 0xC82F22, 0x4E63D9, 0xD11CCE, 0x575035, 0x5BC9C3,
  0xDD8538
};

static const char *armor_begin[] = {
  "BEGIN PGP MESSAGE",
  "BEGIN PGP PUBLIC KEY BLOCK",
  "BEGIN PGP PRIVATE KEY BLOCK",
  "BEGIN PGP SIGNATURE",
  NULL
};

static const char *armor_end[] = {
  "END PGP MESSAGE",
  "END PGP PUBLIC KEY BLOCK",
  "END PGP PRIVATE KEY BLOCK",
  "END PGP SIGNATURE",
  NULL
};

static const char *valid_headers[] = {
  "Comment",
  "Version",
  "MessageID",
  "Hash",
  "Charset",
  NULL
};

static char b64chars[] =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

static int index64[128] = {
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 62, -1, -1, -1, 63,
  52, 53, 54, 55, 56, 57, 58, 59, 60, 61, -1, -1, -1, -1, -1, -1,
  -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
  15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, -1, -1, -1, -1, -1,
  -1, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
  41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, -1, -1, -1, -1, -1
};


/* encode a raw binary buffer to a null-terminated base64 strings */
static int
base64_encode (char *out, const byte * in, size_t len, size_t olen)
{
  if (!out || !in)
    {
      gnutls_assert ();
      return CDK_Inv_Value;
    }

  while (len >= 3 && olen > 10)
    {
      *out++ = b64chars[in[0] >> 2];
      *out++ = b64chars[((in[0] << 4) & 0x30) | (in[1] >> 4)];
      *out++ = b64chars[((in[1] << 2) & 0x3c) | (in[2] >> 6)];
      *out++ = b64chars[in[2] & 0x3f];
      olen -= 4;
      len -= 3;
      in += 3;
    }

  /* clean up remainder */
  if (len > 0 && olen > 4)
    {
      byte fragment = 0;
      *out++ = b64chars[in[0] >> 2];
      fragment = (in[0] << 4) & 0x30;
      if (len > 1)
        fragment |= in[1] >> 4;
      *out++ = b64chars[fragment];
      *out++ = (len < 2) ? '=' : b64chars[(in[1] << 2) & 0x3c];
      *out++ = '=';
    }
  *out = '\0';
  return 0;
}


/* Convert '\0'-terminated base64 string to raw byte buffer.
   Returns length of returned buffer, or -1 on error. */
static int
base64_decode (byte * out, const char *in)
{
  size_t len;
  byte digit1, digit2, digit3, digit4;

  if (!out || !in)
    {
      gnutls_assert ();
      return -1;
    }

  len = 0;
  do
    {
      digit1 = in[0];
      if (digit1 > 127 || b64val (digit1) == BAD)
        {
          gnutls_assert ();
          return -1;
        }
      digit2 = in[1];
      if (digit2 > 127 || b64val (digit2) == BAD)
        {
          gnutls_assert ();
          return -1;
        }
      digit3 = in[2];
      if (digit3 > 127 || ((digit3 != '=') && (b64val (digit3) == BAD)))
        {
          gnutls_assert ();
          return -1;
        }
      digit4 = in[3];
      if (digit4 > 127 || ((digit4 != '=') && (b64val (digit4) == BAD)))
        {
          gnutls_assert ();
          return -1;
        }
      in += 4;

      /* digits are already sanity-checked */
      *out++ = (b64val (digit1) << 2) | (b64val (digit2) >> 4);
      len++;
      if (digit3 != '=')
        {
          *out++ = ((b64val (digit2) << 4) & 0xf0) | (b64val (digit3) >> 2);
          len++;
          if (digit4 != '=')
            {
              *out++ = ((b64val (digit3) << 6) & 0xc0) | b64val (digit4);
              len++;
            }
        }
    }
  while (*in && digit4 != '=');

  return len;
}


/* Return the compression algorithm in @r_zipalgo.
   If the parameter is not set after execution,
   the stream is not compressed. */
static int
compress_get_algo (cdk_stream_t inp, int *r_zipalgo)
{
  byte plain[512];
  char buf[128];
  int nread, pkttype;

  *r_zipalgo = 0;
  cdk_stream_seek (inp, 0);
  while (!cdk_stream_eof (inp))
    {
      nread = _cdk_stream_gets (inp, buf, DIM (buf) - 1);
      if (!nread || nread == -1)
        break;
      if (nread == 1 && !cdk_stream_eof (inp)
          && (nread = _cdk_stream_gets (inp, buf, DIM (buf) - 1)) > 0)
        {
          base64_decode (plain, buf);
          if (!(*plain & 0x80))
            break;
          pkttype = *plain & 0x40 ? (*plain & 0x3f) : ((*plain >> 2) & 0xf);
          if (pkttype == CDK_PKT_COMPRESSED && r_zipalgo)
            {
              _gnutls_buffers_log ("armor compressed (algo=%d)\n",
                                   *(plain + 1));
              *r_zipalgo = *(plain + 1);
            }
          break;
        }
    }
  return 0;
}


static int
check_armor (cdk_stream_t inp, int *r_zipalgo)
{
  char buf[4096];
  size_t nread;
  int check;

  check = 0;
  nread = cdk_stream_read (inp, buf, DIM (buf) - 1);
  if (nread > 0)
    {
      buf[nread] = '\0';
      if (strstr (buf, "-----BEGIN PGP"))
        {
          compress_get_algo (inp, r_zipalgo);
          check = 1;
        }
      cdk_stream_seek (inp, 0);
    }
  return check;
}


static int
is_armored (int ctb)
{
  int pkttype = 0;

  if (!(ctb & 0x80))
    {
      gnutls_assert ();
      return 1;                 /* invalid packet: assume it is armored */
    }
  pkttype = ctb & 0x40 ? (ctb & 0x3f) : ((ctb >> 2) & 0xf);
  switch (pkttype)
    {
    case CDK_PKT_MARKER:
    case CDK_PKT_ONEPASS_SIG:
    case CDK_PKT_PUBLIC_KEY:
    case CDK_PKT_SECRET_KEY:
    case CDK_PKT_PUBKEY_ENC:
    case CDK_PKT_SIGNATURE:
    case CDK_PKT_LITERAL:
    case CDK_PKT_COMPRESSED:
      return 0;                 /* seems to be a regular packet: not armored */
    }
  return 1;
}


static u32
update_crc (u32 crc, const byte * buf, size_t buflen)
{
  unsigned int j;

  if (!crc)
    crc = CRCINIT;

  for (j = 0; j < buflen; j++)
    crc = (crc << 8) ^ crc_table[0xff & ((crc >> 16) ^ buf[j])];
  crc &= 0xffffff;
  return crc;
}


static cdk_error_t
armor_encode (void *data, FILE * in, FILE * out)
{
  armor_filter_t *afx = data;
  struct stat statbuf;
  char crcbuf[5], buf[128], raw[49];
  byte crcbuf2[3];
  size_t nread = 0;
  const char *lf;

  if (!afx)
    {
      gnutls_assert ();
      return CDK_Inv_Value;
    }
  if (afx->idx < 0 || afx->idx > (int) DIM (armor_begin) ||
      afx->idx2 < 0 || afx->idx2 > (int) DIM (armor_end))
    {
      gnutls_assert ();
      return CDK_Inv_Value;
    }

  _gnutls_buffers_log ("armor filter: encode\n");

  memset (crcbuf, 0, sizeof (crcbuf));

  lf = afx->le ? afx->le : LF;
  fprintf (out, "-----%s-----%s", armor_begin[afx->idx], lf);
  fprintf (out, "Version: OpenPrivacy " PACKAGE_VERSION "%s", lf);
  if (afx->hdrlines)
    fwrite (afx->hdrlines, 1, strlen (afx->hdrlines), out);
  fprintf (out, "%s", lf);

  if (fstat (fileno (in), &statbuf))
    {
      gnutls_assert ();
      return CDK_General_Error;
    }

  while (!feof (in))
    {
      nread = fread (raw, 1, DIM (raw) - 1, in);
      if (!nread)
        break;
      if (ferror (in))
        {
          gnutls_assert ();
          return CDK_File_Error;
        }
      afx->crc = update_crc (afx->crc, (byte *) raw, nread);
      base64_encode (buf, (byte *) raw, nread, DIM (buf) - 1);
      fprintf (out, "%s%s", buf, lf);
    }

  crcbuf2[0] = afx->crc >> 16;
  crcbuf2[1] = afx->crc >> 8;
  crcbuf2[2] = afx->crc;
  crcbuf[0] = b64chars[crcbuf2[0] >> 2];
  crcbuf[1] = b64chars[((crcbuf2[0] << 4) & 0x30) | (crcbuf2[1] >> 4)];
  crcbuf[2] = b64chars[((crcbuf2[1] << 2) & 0x3c) | (crcbuf2[2] >> 6)];
  crcbuf[3] = b64chars[crcbuf2[2] & 0x3f];
  fprintf (out, "=%s%s", crcbuf, lf);
  fprintf (out, "-----%s-----%s", armor_end[afx->idx2], lf);

  return 0;
}


/**
 * cdk_armor_filter_use:
 * @inp: the stream to check
 *
 * Check if the stream contains armored data.
 **/
int
cdk_armor_filter_use (cdk_stream_t inp)
{
  int c, check;
  int zipalgo;

  zipalgo = 0;
  c = cdk_stream_getc (inp);
  if (c == EOF)
    return 0;                   /* EOF, doesn't matter whether armored or not */
  cdk_stream_seek (inp, 0);
  check = is_armored (c);
  if (check)
    {
      check = check_armor (inp, &zipalgo);
      if (zipalgo)
        _cdk_stream_set_compress_algo (inp, zipalgo);
    }
  return check;
}


static int
search_header (const char *buf, const char **array)
{
  const char *s;
  int i;

  if (strlen (buf) < 5 || strncmp (buf, "-----", 5))
    {
      gnutls_assert ();
      return -1;
    }
  for (i = 0; (s = array[i]); i++)
    {
      if (!strncmp (s, buf + 5, strlen (s)))
        return i;
    }
  return -1;
}


const char *
_cdk_armor_get_lineend (void)
{
  return LF;
}


static cdk_error_t
armor_decode (void *data, FILE * in, FILE * out)
{
  armor_filter_t *afx = data;
  const char *s;
  char buf[127];
  byte raw[128], crcbuf[4];
  u32 crc2 = 0;
  ssize_t nread = 0;
  int i, pgp_data = 0;
  cdk_error_t rc = 0;
  int len;

  if (!afx)
    {
      gnutls_assert ();
      return CDK_Inv_Value;
    }

  _gnutls_buffers_log ("armor filter: decode\n");

  fseek (in, 0, SEEK_SET);
  /* Search the begin of the message */
  while (!feof (in) && !pgp_data)
    {
      s = fgets (buf, DIM (buf) - 1, in);
      if (!s)
        break;
      afx->idx = search_header (buf, armor_begin);
      if (afx->idx >= 0)
        pgp_data = 1;
    }

  if (feof (in) || !pgp_data)
    {
      gnutls_assert ();
      return CDK_Armor_Error;   /* no data found */
    }

  /* Parse header until the empty line is reached */
  while (!feof (in))
    {
      s = fgets (buf, DIM (buf) - 1, in);
      if (!s)
        return CDK_EOF;
      if (strcmp (s, LF) == 0 || strcmp (s, ALTLF) == 0)
        {
          rc = 0;
          break;                /* empty line */
        }
      /* From RFC2440: OpenPGP should consider improperly formatted Armor
         Headers to be corruption of the ASCII Armor. A colon and a single
         space separate the key and value. */
      if (!strstr (buf, ": "))
        {
          gnutls_assert ();
          return CDK_Armor_Error;
        }
      rc = CDK_General_Error;
      for (i = 0; (s = valid_headers[i]); i++)
        {
          if (!strncmp (s, buf, strlen (s)))
            rc = 0;
        }
      if (rc)
        {
          /* From RFC2440: Unknown keys should be reported to the user,
             but OpenPGP should continue to process the message. */
          _cdk_log_info ("unknown header: `%s'\n", buf);
          rc = 0;
        }
    }

  /* Read the data body */
  while (!feof (in))
    {
      s = fgets (buf, DIM (buf) - 1, in);
      if (!s)
        break;
        
      len = strlen(buf);

      if (buf[len - 1] == '\n')
        buf[len - 1] = '\0';
      if (buf[len - 1] == '\r')
        buf[len - 1] = '\0';
      if (buf[0] == '=' && strlen (s) == 5)
        {                       /* CRC */
          memset (crcbuf, 0, sizeof (crcbuf));
          base64_decode (crcbuf, buf + 1);
          crc2 = (crcbuf[0] << 16) | (crcbuf[1] << 8) | crcbuf[2];
          break;                /* stop here */
        }
      else
        {
          nread = base64_decode (raw, buf);
          if (nread == -1 || nread == 0)
            break;
          afx->crc = update_crc (afx->crc, raw, nread);
          fwrite (raw, 1, nread, out);
        }
    }

  /* Search the tail of the message */
  s = fgets (buf, DIM (buf) - 1, in);
  if (s)
    {
      int len = strlen(buf);
      if (buf[len - 1] == '\n')
        buf[len - 1] = '\0';
      if (buf[len - 1] == '\r')
        buf[len - 1] = '\0';
      rc = CDK_General_Error;
      afx->idx2 = search_header (buf, armor_end);
      if (afx->idx2 >= 0)
        rc = 0;
    }

  /* This catches error when no tail was found or the header is
     different then the tail line. */
  if (rc || afx->idx != afx->idx2)
    rc = CDK_Armor_Error;

  afx->crc_okay = (afx->crc == crc2) ? 1 : 0;
  if (!afx->crc_okay && !rc)
    {
      _gnutls_buffers_log ("file crc=%08X afx_crc=%08X\n",
                           (unsigned int) crc2, (unsigned int) afx->crc);
      rc = CDK_Armor_CRC_Error;
    }

  return rc;
}


/**
 * cdk_file_armor:
 * @hd: Handle
 * @file: Name of the file to protect.
 * @output: Output filename.
 *
 * Protect a file with ASCII armor.
 **/
cdk_error_t
cdk_file_armor (cdk_ctx_t hd, const char *file, const char *output)
{
  cdk_stream_t inp, out;
  cdk_error_t rc;

  rc = _cdk_check_args (hd->opt.overwrite, file, output);
  if (rc)
    return rc;

  rc = cdk_stream_open (file, &inp);
  if (rc)
    {
      gnutls_assert ();
      return rc;
    }

  rc = cdk_stream_new (output, &out);
  if (rc)
    {
      cdk_stream_close (inp);
      gnutls_assert ();
      return rc;
    }

  cdk_stream_set_armor_flag (out, CDK_ARMOR_MESSAGE);
  if (hd->opt.compress)
    rc = cdk_stream_set_compress_flag (out, hd->compress.algo,
                                       hd->compress.level);
  if (!rc)
    rc = cdk_stream_set_literal_flag (out, 0, file);
  if (!rc)
    rc = cdk_stream_kick_off (inp, out);
  if (!rc)
    rc = _cdk_stream_get_errno (out);

  cdk_stream_close (out);
  cdk_stream_close (inp);
  return rc;
}


/**
 * cdk_file_dearmor:
 * @file: Name of the file to unprotect.
 * @output: Output filename.
 *
 * Remove ASCII armor from a file.
 **/
cdk_error_t
cdk_file_dearmor (const char *file, const char *output)
{
  cdk_stream_t inp, out;
  cdk_error_t rc;
  int zipalgo;

  rc = _cdk_check_args (1, file, output);
  if (rc)
    {
      gnutls_assert ();
      return rc;
    }

  rc = cdk_stream_open (file, &inp);
  if (rc)
    {
      gnutls_assert ();
      return rc;
    }

  rc = cdk_stream_create (output, &out);
  if (rc)
    {
      cdk_stream_close (inp);
      gnutls_assert ();
      return rc;
    }

  if (cdk_armor_filter_use (inp))
    {
      rc = cdk_stream_set_literal_flag (inp, 0, NULL);
      zipalgo = cdk_stream_is_compressed (inp);
      if (zipalgo)
        rc = cdk_stream_set_compress_flag (inp, zipalgo, 0);
      if (!rc)
        rc = cdk_stream_set_armor_flag (inp, 0);
      if (!rc)
        rc = cdk_stream_kick_off (inp, out);
      if (!rc)
        rc = _cdk_stream_get_errno (inp);
    }

  cdk_stream_close (inp);
  cdk_stream_close (out);
  gnutls_assert ();
  return rc;
}


int
_cdk_filter_armor (void *data, int ctl, FILE * in, FILE * out)
{
  if (ctl == STREAMCTL_READ)
    return armor_decode (data, in, out);
  else if (ctl == STREAMCTL_WRITE)
    return armor_encode (data, in, out);
  else if (ctl == STREAMCTL_FREE)
    {
      armor_filter_t *afx = data;
      if (afx)
        {
          _gnutls_buffers_log ("free armor filter\n");
          afx->idx = afx->idx2 = 0;
          afx->crc = afx->crc_okay = 0;
          return 0;
        }
    }

  gnutls_assert ();
  return CDK_Inv_Mode;
}


/**
 * cdk_armor_encode_buffer:
 * @inbuf: the raw input buffer
 * @inlen: raw buffer len
 * @outbuf: the destination buffer for the base64 output
 * @outlen: destination buffer len
 * @nwritten: actual length of the base64 data
 * @type: the base64 file type.
 * 
 * Encode the given buffer into base64 format. The base64
 * string will be null terminated but the null will
 * not be contained in the size.
 **/
cdk_error_t
cdk_armor_encode_buffer (const byte * inbuf, size_t inlen,
                         char *outbuf, size_t outlen,
                         size_t * nwritten, int type)
{
  const char *head, *tail, *le;
  byte tempbuf[48];
  char tempout[128];
  size_t pos, off, len, rest;

  if (!inbuf || !nwritten)
    {
      gnutls_assert ();
      return CDK_Inv_Value;
    }
  if (type > CDK_ARMOR_SIGNATURE)
    {
      gnutls_assert ();
      return CDK_Inv_Mode;
    }

  head = armor_begin[type];
  tail = armor_end[type];
  le = _cdk_armor_get_lineend ();
  pos = strlen (head) + 10 + 2 + 2 + strlen (tail) + 10 + 2 + 5 + 2 + 1;
  /* The output data is 4/3 times larger, plus a line end for each line. */
  pos += (4 * inlen / 3) + 2 * (4 * inlen / 3 / 64) + 1;

  if (outbuf && outlen < pos)
    {
      gnutls_assert ();
      *nwritten = pos;
      return CDK_Too_Short;
    }

  /* Only return the size of the output. */
  if (!outbuf)
    {
      *nwritten = pos;
      return 0;
    }

  pos = 0;
  memset (outbuf, 0, outlen);
  memcpy (outbuf + pos, "-----", 5);
  pos += 5;
  memcpy (outbuf + pos, head, strlen (head));
  pos += strlen (head);
  memcpy (outbuf + pos, "-----", 5);
  pos += 5;
  memcpy (outbuf + pos, le, strlen (le));
  pos += strlen (le);
  memcpy (outbuf + pos, le, strlen (le));
  pos += strlen (le);
  rest = inlen;
  for (off = 0; off < inlen;)
    {
      if (rest > 48)
        {
          memcpy (tempbuf, inbuf + off, 48);
          off += 48;
          len = 48;
        }
      else
        {
          memcpy (tempbuf, inbuf + off, rest);
          off += rest;
          len = rest;
        }
      rest -= len;
      base64_encode (tempout, tempbuf, len, DIM (tempout) - 1);
      memcpy (outbuf + pos, tempout, strlen (tempout));
      pos += strlen (tempout);
      memcpy (outbuf + pos, le, strlen (le));
      pos += strlen (le);
    }

  memcpy (outbuf + pos, "-----", 5);
  pos += 5;
  memcpy (outbuf + pos, tail, strlen (tail));
  pos += strlen (tail);
  memcpy (outbuf + pos, "-----", 5);
  pos += 5;
  memcpy (outbuf + pos, le, strlen (le));
  pos += strlen (le);
  outbuf[pos] = 0;
  *nwritten = pos - 1;
  return 0;
}
