/* crc32.h

   header file for crc32 checksum

   C implementation of CRC-32 checksums for NAACCR records.  Code is based
   upon and utilizes algorithm published by Ross Williams.

   This file contains:
      CRC lookup table
      function CalcCRC32 for calculating CRC-32 checksum
      function AssignCRC32 for assigning CRC-32 in NAACCR record
      function CheckCRC32 for checking CRC-32 in NAACCR record

   Provided by:
      Eric Durbin
      Kentucky Cancer Registry
      University of Kentucky
      October 14, 1998

   Status:
      Public Domain
*/

#define CRC32_XINIT 0xFFFFFFFFL	/* initial value */
#define CRC32_XOROT 0xFFFFFFFFL	/* final xor value */

#define MINIMUM_CHECKSUM_LEN     8
#define MAXIMUM_CHECKSUM_LEN    99

/* NAACCR 6.0 Specifications */
#define NAACCR_60_CHECKSUM_POS  942
#define NAACCR_60_CHECKSUM_LEN  10

/* function prototypes */
unsigned long CalcCRC32 (unsigned char *, unsigned long, unsigned long,
			 unsigned long);
int AssignCRC32 (unsigned char *, unsigned long, unsigned long, unsigned long);
int CompareCRC32 (unsigned char *, unsigned long, unsigned long, unsigned long);
