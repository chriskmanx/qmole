#ifndef MSWidgetStateHEADER
#define MSWidgetStateHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSHashTable.H>

#if HAVE_IOSTREAM
#include <iostream>
#else
class istream;
#endif


class MSGUIExport MSWidgetState : public MSHashTable
{
public:
  MSWidgetState (istream &is_);
  ~MSWidgetState(void);
};

#endif
