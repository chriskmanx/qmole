///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#define VERSIONID 0
#define SEQUENCE_MAX 255
#define SEQUENCE_SHIFT 24
#define VERSION_SHIFT 20
#define OIDSIZE 32		// number characters in hex string rep of oid

#include <unistd.h>
#include <sys/time.h>
#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif
#include <MSTypes/MSOid.H>
#include <MSTypes/MSError.H>
#include <MSTypes/MSString.H>
#include <MSTypes/MSDefines.H>

#if defined(MS_GETPID_IN_PROCESS)
#include <process.h>
#endif

#if defined(MS_HAS_SYSTEMINFO)
#include <sys/systeminfo.h>
#elif ! defined(MS_HAS_GETHOSTID_DECLARATION)
// does not appear to exist in a standard include file on all platforms
extern "C" long int gethostid();
#endif

// Class instances:

MSOid const MSOid::null(MSOid::MSOidNull);
unsigned MSOid::_hid = 0;
unsigned MSOid::_pid = 0;
unsigned MSOid::_seq = SEQUENCE_MAX;
unsigned long MSOid::_sec = 0;

// fast conversion to printable hex:
inline char
toHex(unsigned char b)
{
    b &= 0x0F; // Chop off top nybble
    return (b <= 9) ? b+'0' : (b-10)+'A';
}

// and reverse:
inline unsigned char
fromHex(char ch)
{
    return (ch >= '0' && ch <= '9') ? ch-'0' : (toupper(ch)-'A')+10;
}

void
MSOid::newPid()
{
    _pid = getpid();
}

void
MSOid::newOid()
{
    if (*this != MSOid::null)
	return;

    if (!_hid)
    {
#if defined(MS_HAS_SYSTEMINFO)
      char buf[128];
      if (sysinfo(SI_HW_SERIAL, buf, 128) == -1)
       {
         cerr<<"sysinfo: Can't get serial number."<<endl;
         _hid=1;
       }
      else _hid=strtoul(buf,NULL,0);
#else
        _hid = gethostid();
#endif
	newPid();
    }

    // Get current time for each generated OID when the sequence number
    // wraps. As we may be able to cycle through all the sequence numbers 
    // in less than a second, be sure to insure that the time is not the same 
    // if it is just increment it, idle time will adjust it later
    // Note that PIDs get reused in some versions of UNIX
    // (e.g. AIX), so time is needed to differentiate
    // between objects.

    struct timeval tval;

    if (MSOid::_seq == SEQUENCE_MAX)
    {
        gettimeofday(&tval, (struct timezone *)NULL);
        if (tval.tv_sec > MSOid::_sec)
            MSOid::_sec = tval.tv_sec;
        else 
            MSOid::_sec += 1; 
        MSOid::_seq = 0;
    }
    else
        MSOid::_seq += 1;


// first byte-sequence number and second byte bits 0-3-version
// the reset of second byte and bytes 3 and 4 are reserved for the class id

    _oid[0] = MSOid::_seq << SEQUENCE_SHIFT;
    _oid[0] += VERSIONID << VERSION_SHIFT;
    _oid[1] = _hid;
    _oid[2] = MSOid::_sec;
    _oid[3] = _pid;
}

int
MSOid::parse(register const char *str)
{
    if ((!str) || (strlen(str) != OIDSIZE))
	return MSError::BadString;
      
    register unsigned char* memptr = (unsigned char *)_oid;
    while(str[0] && str[1])
    {
	*memptr++ = (fromHex(str[0]) << 4) | (fromHex(str[1]));
	str += 2;
    }

    return MSError::MSSuccess;
}

istream &
operator>>(istream& s, MSOid& target)
{
    const int expectedLength = sizeof(target._oid) * 2;
    char buf[expectedLength + 1], *bp = buf, *end = bp + expectedLength;

    while (bp < end)
    {
	char c;
	if (s >> c)
	{
	    if (isxdigit(c))
	    {
		*bp++ = c;
	    }
	    else
	    {
		s.clear(s.rdstate() | ios::failbit); // set translation error
		return s;               
	    }
	}
	else
	{
	    return s;                                // eof error already set
	}
    }

    // Got all hex chars.
    *bp = 0;
    target.parse(buf);
    return s;
}

ostream &
operator<<(ostream& s, const MSOid& source)
{
    if (!s)
	return s;
    const char *memptr = (const char*)source._oid;
    const char *end = memptr + sizeof(source._oid);
    while(memptr < end)
    {
	s << toHex(*memptr >> 4) << toHex(*memptr);
	memptr++;
    }
    return s;
}

const MSString
MSOid::asString() const
{
    static char chBuff[(2*sizeof(_oid))+1];

    char* str = chBuff;
    const unsigned char * memptr = (unsigned char *)_oid;

    while(memptr < (unsigned char *)(_oid+NPARTS)) {
        *str++ = toHex(*memptr >> 4); // Do the high nibble
        *str++ = toHex(*memptr);      // And the low one
        memptr++;
    }
    *str = '\0';
    return MSString(chBuff);
}

