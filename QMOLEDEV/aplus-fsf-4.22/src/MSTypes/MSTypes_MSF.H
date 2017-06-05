///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

/* file: MSTypes_MSF.H

   Delimiters needed to delimit MSF message data elements and fields
   fields are composed of a field name and its data elements.

   There are two cases that need to have delimiters:
     strings: A space is placed between elements of multi-component scalar
              MSTypes, such as MSMoney.
     arrays:  Elements in arrays must be flattened in such a way that the
	      are can be reconstructed from the flattened object. The 
              <US>, \37, character was chosen as this character is not
              printable and can be used as separated in  string arrays as
	      well as numeric one.
*/

#ifndef MSTypes_MSFHEADER
#define MSTypes_MSFHEADER

const char MSMSF_US='\37';      // <US> unit separate-delimit data elements 
const char MSMSF_SPACE=' ';     // space separator between components.

#endif // MSTypes_MSFHEADER
