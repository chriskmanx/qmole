#ifndef MSStringEnumHEADER
#define MSStringEnumHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////
#include <MSTypes/MSDefines.H>

/*******************************************************************************
* DESCRIPTION:                                                                 *
*   This file contains the declaration(s) of the class(es):                    *
*     MSStringEnum - Placeholder for enumerations related to class MSString.   *
*******************************************************************************/

class MSStringEnum 
{
/*******************************************************************************
  The MSStringEnum class serves as a repository for enumeration types related
  to the MSString class.  These enumeration types are placed here so they can
  easily be shared between code that implements the MSString class and the
  MSStringBuffer and MSMBStringBuffer classes.
*******************************************************************************/
public:
/*------------------------------ Related Types ---------------------------------
  This class defines the following enumeration types:
    StripMode - Enumeration that defines the mode of operation of various
                functions that strip leading characters, trailing characters,
                or both from MSStrings.
    CharType  - Enumeration that defines the various types of characters that
                comprise an MSString.
------------------------------------------------------------------------------*/
enum StripMode {Leading,Trailing,Both};

/*---------------------------------- Note: -------------------------------------
  Code for mbcs must be the same as the length of the corresponding characters.
------------------------------------------------------------------------------*/

enum CharType {SBCS,
               DBCS1=1,MBCS1=1,
               DBCS2=2,MBCS2=2,
               MBCS3=3,
               MBCS4=4};

}; // class MSStringEnum

#endif // MSStringEnumHEADER
