
/***************************************************************************
 * nbase_misc.c -- Some small miscelaneous utility/compatability           *
 * functions.                                                              *
 *                                                                         *
 ***********************IMPORTANT NMAP LICENSE TERMS************************
 *                                                                         *
 * The Nmap Security Scanner is (C) 1996-2008 Insecure.Com LLC. Nmap is    *
 * also a registered trademark of Insecure.Com LLC.  This program is free  *
 * software; you may redistribute and/or modify it under the terms of the  *
 * GNU General Public License as published by the Free Software            *
 * Foundation; Version 2 with the clarifications and exceptions described  *
 * below.  This guarantees your right to use, modify, and redistribute     *
 * this software under certain conditions.  If you wish to embed Nmap      *
 * technology into proprietary software, we sell alternative licenses      *
 * (contact sales@insecure.com).  Dozens of software vendors already       *
 * license Nmap technology such as host discovery, port scanning, OS       *
 * detection, and version detection.                                       *
 *                                                                         *
 * Note that the GPL places important restrictions on "derived works", yet *
 * it does not provide a detailed definition of that term.  To avoid       *
 * misunderstandings, we consider an application to constitute a           *
 * "derivative work" for the purpose of this license if it does any of the *
 * following:                                                              *
 * o Integrates source code from Nmap                                      *
 * o Reads or includes Nmap copyrighted data files, such as                *
 *   nmap-os-db or nmap-service-probes.                                    *
 * o Executes Nmap and parses the results (as opposed to typical shell or  *
 *   execution-menu apps, which simply display raw Nmap output and so are  *
 *   not derivative works.)                                                * 
 * o Integrates/includes/aggregates Nmap into a proprietary executable     *
 *   installer, such as those produced by InstallShield.                   *
 * o Links to a library or executes a program that does any of the above   *
 *                                                                         *
 * The term "Nmap" should be taken to also include any portions or derived *
 * works of Nmap.  This list is not exclusive, but is just meant to        *
 * clarify our interpretation of derived works with some common examples.  *
 * These restrictions only apply when you actually redistribute Nmap.  For *
 * example, nothing stops you from writing and selling a proprietary       *
 * front-end to Nmap.  Just distribute it by itself, and point people to   *
 * http://nmap.org to download Nmap.                                       *
 *                                                                         *
 * We don't consider these to be added restrictions on top of the GPL, but *
 * just a clarification of how we interpret "derived works" as it applies  *
 * to our GPL-licensed Nmap product.  This is similar to the way Linus     *
 * Torvalds has announced his interpretation of how "derived works"        *
 * applies to Linux kernel modules.  Our interpretation refers only to     *
 * Nmap - we don't speak for any other GPL products.                       *
 *                                                                         *
 * If you have any questions about the GPL licensing restrictions on using *
 * Nmap in non-GPL works, we would be happy to help.  As mentioned above,  *
 * we also offer alternative license to integrate Nmap into proprietary    *
 * applications and appliances.  These contracts have been sold to dozens  *
 * of software vendors, and generally include a perpetual license as well  *
 * as providing for priority support and updates as well as helping to     *
 * fund the continued development of Nmap technology.  Please email        *
 * sales@insecure.com for further information.                             *
 *                                                                         *
 * As a special exception to the GPL terms, Insecure.Com LLC grants        *
 * permission to link the code of this program with any version of the     *
 * OpenSSL library which is distributed under a license identical to that  *
 * listed in the included COPYING.OpenSSL file, and distribute linked      *
 * combinations including the two. You must obey the GNU GPL in all        *
 * respects for all of the code used other than OpenSSL.  If you modify    *
 * this file, you may extend this exception to your version of the file,   *
 * but you are not obligated to do so.                                     *
 *                                                                         *
 * If you received these files with a written license agreement or         *
 * contract stating terms other than the terms above, then that            *
 * alternative license agreement takes precedence over these comments.     *
 *                                                                         *
 * Source is provided to this software because we believe users have a     *
 * right to know exactly what a program is going to do before they run it. *
 * This also allows you to audit the software for security holes (none     *
 * have been found so far).                                                *
 *                                                                         *
 * Source code also allows you to port Nmap to new platforms, fix bugs,    *
 * and add new features.  You are highly encouraged to send your changes   *
 * to fyodor@insecure.org for possible incorporation into the main         *
 * distribution.  By sending these changes to Fyodor or one of the         *
 * Insecure.Org development mailing lists, it is assumed that you are      *
 * offering Fyodor and Insecure.Com LLC the unlimited, non-exclusive right *
 * to reuse, modify, and relicense the code.  Nmap will always be          *
 * available Open Source, but this is important because the inability to   *
 * relicense code has caused devastating problems for other Free Software  *
 * projects (such as KDE and NASM).  We also occasionally relicense the    *
 * code to third parties as discussed above.  If you wish to specify       *
 * special license conditions of your contributions, just say so when you  *
 * send them.                                                              *
 *                                                                         *
 * This program is distributed in the hope that it will be useful, but     *
 * WITHOUT ANY WARRANTY; without even the implied warranty of              *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       *
 * General Public License v2.0 for more details at                         *
 * http://www.gnu.org/licenses/gpl-2.0.html , or in the COPYING file       *
 * included with Nmap.                                                     *
 *                                                                         *
 ***************************************************************************/

/* $Id: nbase_misc.c 9700 2008-08-23 18:45:30Z david $ */

#include "nbase.h"

#ifndef WIN32
#include <errno.h>
#ifndef errno
extern int errno;
#endif
#else
#include <winsock2.h>
#endif

#include "nbase_ipv6.h"

#include <assert.h>
#include <fcntl.h>

#ifndef INET6_ADDRSTRLEN
#define INET6_ADDRSTRLEN 46
#endif

/* Returns the UNIX/Windows errno-equivalent.  Note that the Windows
   call is socket/networking specific.  The windows error number
   returned is like WSAMSGSIZE, but nbase.h includes #defines to
   correlate many of the common UNIX errors with their closest Windows
   equivalents.  So you can use EMSGSIZE or EINTR. */
int socket_errno() {
#ifdef WIN32
	return WSAGetLastError();
#else
	return errno;
#endif
}

/* This function is an easier version of inet_ntop because you don't
   need to pass a dest buffer.  Instead, it returns a static buffer that
   you can use until the function is called again (by the same or another
   thread in the process).  If there is a wierd error (like sslen being
   too short) then NULL will be returned. */
const char *inet_ntop_ez(struct sockaddr_storage *ss, size_t sslen) {

  struct sockaddr_in *sin = (struct sockaddr_in *) ss;
  static char str[INET6_ADDRSTRLEN];
#if HAVE_IPV6
  struct sockaddr_in6 *sin6 = (struct sockaddr_in6 *) ss;
#endif

  str[0] = '\0';

  if (sin->sin_family == AF_INET) {
    if (sslen < sizeof(struct sockaddr_in))
      return NULL;
    return inet_ntop(AF_INET, &sin->sin_addr, str, sizeof(str));
  } 
#if HAVE_IPV6
  else if(sin->sin_family == AF_INET6) {
    if (sslen < sizeof(struct sockaddr_in6))
      return NULL;
    return inet_ntop(AF_INET6, &sin6->sin6_addr, str, sizeof(str));
  } 
#endif
  //Some laptops report the ip and address family of disabled wifi cards as null
  //so yes, we will hit this sometimes.
  return NULL;
}

int unblock_socket(int sd) {
#ifdef WIN32
u_long one = 1;
if(sd != 501) // Hack related to WinIP Raw Socket support
  ioctlsocket (sd, FIONBIO, &one);
#else
int options;
/*Unblock our socket to prevent recvfrom from blocking forever
  on certain target ports. */
options = O_NONBLOCK | fcntl(sd, F_GETFL);
fcntl(sd, F_SETFL, options);
#endif //WIN32
return 1;
}

/* Convert a socket to blocking mode */
int block_socket(int sd) {
#ifdef WIN32
  unsigned long options=0;
  if(sd == 501) return 1;
  ioctlsocket(sd, FIONBIO, &options);
#else
  int options;
  options = (~O_NONBLOCK) & fcntl(sd, F_GETFL);
  fcntl(sd, F_SETFL, options);
#endif

  return 1;
}

/* Converts a time specification string into milliseconds.  If the string
 * is a plain non-negative number, it is considered to already be in
 * milliseconds and is returned.  If it is a number followed by 's' (for
 * seconds), 'm' (minutes), or 'h' (hours), the number is converted to
 * milliseconds and returned.  If it cannot parse the string, -1 is
 * returned instead.
 */
long tval2msecs(char *tspec) {
  long l;
  char *endptr = NULL;
  l = strtol(tspec, &endptr, 10);
  if (l < 0 || !endptr) return -1;
  if (*endptr == '\0') return l;
  if (*endptr == 's' || *endptr == 'S') return l * 1000;
  if ((*endptr == 'm' || *endptr == 'M')) {
    if (*(endptr + 1) == 's' || *(endptr + 1) == 'S') 
      return l;
    return l * 60000;
  }
  if (*endptr == 'h' || *endptr == 'H') return l * 3600000;
  return -1;
}


/*
 * CRC16 Cyclic Redundancy Check
 * simply copied from http://www.ptb.de/de/org/1/11/112/infos/crc16.htm
 */

/* Table of CRCs of all 8-bit messages. */
static unsigned long crc_table[256];

/* Flag: has the table been computed? Initially false. */
static int crc_table_computed = 0;

/* Make the table for a fast CRC. */
static void make_crc_table(void)
{
  unsigned long c;
  int n, k;

  for (n = 0; n < 256; n++) {
    c = (unsigned long) n;
    for (k = 0; k < 8; k++) {
      if (c & 1) {
        c = 0xedb88320L ^ (c >> 1);
      } else {
        c = c >> 1;
      }
    }
    crc_table[n] = c;
  }
  crc_table_computed = 1;
}

/*
   Update a running crc with the bytes buf[0..len-1] and return
 the updated crc. The crc should be initialized to zero. Pre- and
 post-conditioning (one's complement) is performed within this
 function so it shouldn't be done by the caller. Usage example:

   unsigned long crc = 0L;

   while (read_buffer(buffer, length) != EOF) {
     crc = update_crc(crc, buffer, length);
   }
   if (crc != original_crc) error();
*/
static unsigned long update_crc(unsigned long crc,
                unsigned char *buf, int len)
{
  unsigned long c = crc ^ 0xffffffffL;
  int n;

  if (!crc_table_computed)
    make_crc_table();
  for (n = 0; n < len; n++) {
    c = crc_table[(c ^ buf[n]) & 0xff] ^ (c >> 8);
  }
  return c ^ 0xffffffffL;
}

/* Return the CRC of the bytes buf[0..len-1]. */
unsigned long crc16(unsigned char *buf, int len)
{
  return update_crc(0L, buf, len);
}

