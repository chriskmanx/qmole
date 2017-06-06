/* 
 * $smu-mark$ 
 * $name: getlhs.c$ 
 * $author: Salvatore Sanfilippo <antirez@invece.org>$ 
 * $copyright: Copyright (C) 1999 by Salvatore Sanfilippo$ 
 * $license: This software is under GPL version 2 of license$ 
 * $date: Fri Nov  5 11:55:47 MET 1999$ 
 * $rev: 8$ 
 */ 

/* $Id: getlhs.c,v 1.10 2003/07/25 12:11:24 njombart Exp $ */

#include <string.h>

#include "hping2.h"
#include "globals.h"


#if (!defined OSTYPE_LINUX) || (defined FORCE_LIBPCAP)
int get_linkhdr_size(char *ifname)
{
	int dltype = pcap_datalink(pcapfp);

	if (opt_debug)
		printf("DEBUG: dltype is %d\n", dltype);

	switch(dltype) {
	case DLT_EN10MB:
	case DLT_IEEE802:
		linkhdr_size = 14;
		break;
	case DLT_SLIP:
	case DLT_SLIP_BSDOS:
		linkhdr_size = 16;
		break;
	case DLT_PPP:
	case DLT_NULL:
#ifdef DLT_PPP_SERIAL
	case DLT_PPP_SERIAL:
#endif
#ifdef DLT_LOOP
	case DLT_LOOP:
#endif
		linkhdr_size = 4;
		break;
	case DLT_PPP_BSDOS:
		linkhdr_size = 24;
		break;
	case DLT_FDDI:
		linkhdr_size = 13;
		break;
	case DLT_RAW:
		linkhdr_size = 0;
		break;
	case DLT_IEEE802_11:
		linkhdr_size = 14;
		break;
	case DLT_ATM_RFC1483:
#ifdef DLT_CIP
	case DLT_CIP:
#endif
#ifdef DLT_ATM_CLIP
	case DLT_ATM_CLIP:
#endif
		linkhdr_size = 8;
		break;
#ifdef DLT_C_HDLC
	case DLT_C_HDLC:
		linkhdr_size = 4;
		break;
#endif
#ifdef DLT_LINUX_SLL
	case DLT_LINUX_SLL:
#endif
#ifdef DLT_LANE8023
	case DLT_LANE8023:
#endif
		linkhdr_size = 16;
		break;
	default:
		return -1;
		break;
	}
	return 0;
}
#else /* Linux... */
int get_linkhdr_size(char *ifname)
{

	if ( strstr(ifname, "ppp") ) { /* also works for ippp (ISDN) */
		linkhdr_size = PPPHDR_SIZE_LINUX;
		return 0;
	} else if ( strstr(ifname, "eth") ) {
		linkhdr_size = ETHHDR_SIZE;
		return 0;
	} else if (strstr(ifname, "ets")) {
		linkhdr_size = 0;
		return 0;
	} else if ( strstr(ifname, "lo") ) {
		linkhdr_size = LOHDR_SIZE;
		return 0;
	} else if (strstr(ifname, "atm")) {
		linkhdr_size = 0;
		return 0;
	} else if ( strstr(ifname, "wlan") ) {
		linkhdr_size = WLANHDR_SIZE;
		return 0;
	}
	else
		return -1;
}
#endif /* (!defined OSTYPE_LINUX) || (defined FORCE_LIBPCAP) */
