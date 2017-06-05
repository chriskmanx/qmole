///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <stdarg.h> // varargs 
#include <MSTypes/MSError.H>

int MSError::error(MSError::ErrorStatus,const char *,const char *, ...) { return 0;}
