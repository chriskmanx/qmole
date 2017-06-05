/*=========================================================================

  Program:   CMake - Cross-Platform Makefile Generator
  Module:    $RCSfile: cmXCode21Object.h,v $
  Language:  C++
  Date:      $Date: 2007-08-14 15:45:14 $
  Version:   $Revision: 1.4 $

  Copyright (c) 2002 Kitware, Inc., Insight Consortium.  All rights reserved.
  See Copyright.txt or http://www.cmake.org/HTML/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef cmXCode21Object_h
#define cmXCode21Object_h

#include "cmXCodeObject.h"

class cmXCode21Object : public cmXCodeObject
{
public:
  cmXCode21Object(PBXType ptype, Type type);
  virtual void PrintComment(std::ostream&);
  static void PrintList(std::vector<cmXCodeObject*> const&,
                        std::ostream& out,
                        PBXType t);
  static void PrintList(std::vector<cmXCodeObject*> const&,
                        std::ostream& out);
};
#endif
