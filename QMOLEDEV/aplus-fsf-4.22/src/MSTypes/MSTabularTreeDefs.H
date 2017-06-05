#ifndef MSTabularTreeDefsHEADER
#define MSTabularTreeDefsHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSDefines.H>
#include <MSTypes/MSException.H>

enum MSTabularTreeIterationOrder {MSPreorder,MSPostorder};

#define MSTabularTreeCursorNotForThisText      "cursor not for this tree"
#define MSTabularTreeCursorNotForGivenText     "cursor not for given tree"
#define MSTabularTreeCursorNotContainedText    "cursor not contained"
#define MSTabularTreeInvalidCursorText         "invalid cursor"
#define MSTabularTreeInvalidPositionText       "invalid position"
#define MSTabularTreeRootAlreadyExistsText     "root already exists"
#define MSTabularTreeChildAlreadyExistsText    "child already exists"
#define MSTabularTreeOutOfCollectionMemoryText "out of memory"


class MSTabularTreeCursorInvalid : public MSExceptionError
{
  public:
    MSTabularTreeCursorInvalid(const char* what_arg) : MSExceptionError(what_arg) {;}
};

class MSTabularTreePositionInvalid : public MSExceptionError
{
  public:
    MSTabularTreePositionInvalid(const char* what_arg) : MSExceptionError(what_arg) {;}
};

class MSTabularTreeRootAlreadyExists : public MSExceptionError
{
  public:
    MSTabularTreeRootAlreadyExists(const char* what_arg) : MSExceptionError(what_arg) {;}
};

class MSTabularTreeChildAlreadyExists : public MSExceptionError
{
  public:
    MSTabularTreeChildAlreadyExists(const char* what_arg) : MSExceptionError(what_arg) {;}
};

class MSTabularTreeOutOfMemory : public MSOutOfMemory
{
  public:
    MSTabularTreeOutOfMemory(const char* what_arg) : MSOutOfMemory(what_arg) {;}
};

#endif
