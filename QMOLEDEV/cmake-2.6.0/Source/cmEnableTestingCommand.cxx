/*=========================================================================

  Program:   CMake - Cross-Platform Makefile Generator
  Module:    $RCSfile: cmEnableTestingCommand.cxx,v $
  Language:  C++
  Date:      $Date: 2008-01-23 15:27:59 $
  Version:   $Revision: 1.19 $

  Copyright (c) 2002 Kitware, Inc., Insight Consortium.  All rights reserved.
  See Copyright.txt or http://www.cmake.org/HTML/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "cmEnableTestingCommand.h"
#include "cmLocalGenerator.h"

// we do this in the final pass so that we now the subdirs have all 
// been defined
bool cmEnableTestingCommand::InitialPass(std::vector<std::string> const&,
                                         cmExecutionStatus &)
{
  this->Makefile->AddDefinition("CMAKE_TESTING_ENABLED","1");
  return true;
}
