/*=========================================================================

  Program:   CMake - Cross-Platform Makefile Generator
  Module:    $RCSfile: cmReturnCommand.cxx,v $
  Language:  C++
  Date:      $Date: 2008-01-23 15:28:26 $
  Version:   $Revision: 1.1 $

  Copyright (c) 2002 Kitware, Inc., Insight Consortium.  All rights reserved.
  See Copyright.txt or http://www.cmake.org/HTML/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "cmReturnCommand.h"

// cmReturnCommand
bool cmReturnCommand::InitialPass(std::vector<std::string> const&,
                                  cmExecutionStatus &status)
{
  status.SetReturnInvoked(true);
  return true;
}

