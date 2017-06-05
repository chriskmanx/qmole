/*=========================================================================

  Program:   CMake - Cross-Platform Makefile Generator
  Module:    $RCSfile: cmGraphAdjacencyList.h,v $
  Language:  C++
  Date:      $Date: 2008-02-07 21:14:05 $
  Version:   $Revision: 1.1 $

  Copyright (c) 2002 Kitware, Inc., Insight Consortium.  All rights reserved.
  See Copyright.txt or http://www.cmake.org/HTML/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef cmGraphAdjacencyList_h
#define cmGraphAdjacencyList_h

#include "cmStandardIncludes.h"

struct cmGraphNodeList: public std::vector<int> {};
struct cmGraphAdjacencyList: public std::vector<cmGraphNodeList> {};

#endif
