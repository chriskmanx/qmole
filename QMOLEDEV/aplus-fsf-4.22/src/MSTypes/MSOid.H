#ifndef MSOid_HEADER
#define MSOid_HEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <memory.h>
#include <MSTypes/MSSymbol.H>

#if HAVE_IOSTREAM
#include <iostream>
// #include <fstream>
using namespace std;
#else
class ostream;
class istream;
#endif

class MSString;

#define NPARTS  4

class MSTypesExport MSOid 
{

  public:

    enum MSOidFormat { MSOidNull=0 };

    MSOid();
    // Create computed OID

    MSOid(const MSOid& o);
    // Copy other OID

    MSOid(const MSOidFormat);
    // use a "null oid"

    int set(const char *);
    // Create from 32 character hex representation.

    void setData(void *);
    // Create from 16 byte binary representation.
  
    MSOid& operator=(const MSOid& o_);
    // Fast copying

    int operator==(const MSOid& o_) const;
    // == comparison

    int operator!=(const MSOid& o_) const;
    // != comparison

    int operator!() const;
    // Is this OID null?  Returns 1 of so, else 0.
    // Used as a boolean.
    // E.g.
    // MSOid oid;
    // if (!oid) ...

    operator const void *() const;
    // Is this OID null?  Returns (non-zero) address of object
    // if the OID's value is not null, else 0.  Used as a boolean.
    // E.g.
    // MSOid oid;
    // if (oid) ...

    static void newPid();
    // Call MSOid::newPid() in the child after a fork(). E.g.
    // if (fork() == 0) {
    //   MSOid::newPid();
    //   ...
    // }

    void newOid();
    // If the current OID is null, generates a new non-null value.

    friend MSTypesExport ostream &operator<<(ostream&, const MSOid&);
    // Insert in ostream.

    friend MSTypesExport istream &operator>>(istream&, MSOid&);
    // Extract from istream.

    ~MSOid();

    const MSString asString() const;

    // comparisons needed for collections
    long int compare(const MSOid& c) const;
    friend MSTypesExport long int compare(const MSOid& o1_, const MSOid& o2_);

  private:

    static unsigned _pid;
    static unsigned _hid;
    static unsigned _seq;
    static unsigned long _sec;
    static const MSOid null;

    int parse(const char *hexRepresentation);

    // Oid definition of broken down into 4, 4 byte pieces, the
    // first 4 bytes are further broken down into 3 pieces
    //    first byte-cyclic sequence number;
    //    second byte bits 0-3-version, bits 4-7-class id;
    //    third/fourth byte-rest of class id
    // second 4 bytes is processor id,
    // third 4 bytes is a time stamp in seconds since 1/1/1970
    // last 4 bytes is a process id

    unsigned _oid[NPARTS];

};

inline
MSOid::MSOid()
{
    memset((void*)&_oid, 0, sizeof(_oid));
    newOid();
}

inline
MSOid::MSOid(const MSOidFormat)
{
    memset((void*)&_oid, 0, sizeof(_oid));
}

inline
MSOid::MSOid(const MSOid& o_)
{
    memcpy((void*)&_oid, (void*)&o_._oid, sizeof(_oid));
}

inline MSOid &
MSOid::operator=(const MSOid& o_)
{
    memcpy(_oid,o_._oid,sizeof(_oid));
    return *this;
}

inline int
MSOid::operator==(const MSOid& o_) const
{
    return memcmp((void*)&_oid, (void*)&o_._oid, sizeof(_oid)) ? 0 : 1;
}

inline int
MSOid::operator!=(const MSOid& o_) const
{
    return !(*this == o_);
}

inline int
MSOid::set(const char *hexStr_)
{
    return parse(hexStr_);
}

inline void
MSOid::setData(void * data_)
{
  // copy whatever number of bytes from data_ to fill oid
  memcpy((void*)&_oid, data_, sizeof(_oid));
}

inline
MSOid::operator const void *() const
{
    return *this == MSOid::null ? 0 : this;
}

inline int
MSOid::operator!() const
{
    return *this == MSOid::null ? 1 : 0;
}

inline
MSOid::~MSOid()
{
}

inline
long int compare(const MSOid& o1_, const MSOid& o2_)
{
    return memcmp(o1_._oid, o2_._oid, sizeof(o2_._oid));
}

inline 
long int MSOid::compare(const MSOid& o_) const
{
    return ::compare(*this, o_);
}


const MSSymbol MSOID("msoid");

#endif // MSOid_HEADER
