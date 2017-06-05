////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implements CRC32 hashing/checksum algorithm
////////////////////////////////////////////////////////////////////////////

#ifndef _CRC32_H_
#define _CRC32_H_

class CCrc32
{
public:
    CCrc32 ();
    virtual ~CCrc32 ();

    void Init ();
    void Update (unsigned char *input, int nLen);
    void Final ();
    int GetDigest () {return m_dwCrc32;}

protected:
    static inline void CalcCrc32 (unsigned char byte, unsigned int &dwCrc32);

    unsigned int m_dwCrc32;
    static int s_arrdwCrc32Table[256];
};

#endif
