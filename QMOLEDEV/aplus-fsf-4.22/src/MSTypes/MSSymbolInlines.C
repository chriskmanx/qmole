#ifndef MSSymbolINLINES
#define MSSymbolINLINES

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#ifndef MS_NO_INLINES
#define INLINELINKAGE inline
#else
#define INLINELINKAGE
#endif

// we use MSNullAtom here instead of MSNameSpace::nullAtom()
// for efficiency and compile performance - i.e. we do not
// need to include MSNameSpace.H

INLINELINKAGE MSAtom MSSymbol::atom(void) const 
{ return _atom; } 

#endif
