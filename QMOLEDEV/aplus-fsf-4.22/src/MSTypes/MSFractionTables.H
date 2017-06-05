#ifndef MSFractionTablesHEADER
#define MSFractionTablesHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


typedef struct 
{
  int length;
  struct 
  {
    double  value;
    char   *fraction;
  } table[512];
} FractionTable;

extern FractionTable EightsTable;
extern FractionTable SixteenthsTable;
extern FractionTable ThirtySecondsTable;
extern FractionTable SixtyForthsTable;
extern FractionTable OneTwentyEightsTable;
extern FractionTable TwoFiftySixthsTable;

#endif
