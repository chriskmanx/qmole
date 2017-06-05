/*=========================================================================

  Program:   CMake - Cross-Platform Makefile Generator
  Module:    $RCSfile: cmDocumentVariables.h,v $
  Language:  C++
  Date:      $Date: 2007-10-18 13:10:42 $
  Version:   $Revision: 1.1 $

  Copyright (c) 2002 Kitware, Inc., Insight Consortium.  All rights reserved.
  See Copyright.txt or http://www.cmake.org/HTML/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef cmDocumentVariables_h
#define cmDocumentVariables_h
class cmake;
class cmDocumentVariables
{
public:
  static void DefineVariables(cmake* cm);
};

#endif
