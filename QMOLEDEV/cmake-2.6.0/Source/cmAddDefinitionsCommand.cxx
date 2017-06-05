/*=========================================================================

  Program:   CMake - Cross-Platform Makefile Generator
  Module:    $RCSfile: cmAddDefinitionsCommand.cxx,v $
  Language:  C++
  Date:      $Date: 2008-01-23 15:27:59 $
  Version:   $Revision: 1.14 $

  Copyright (c) 2002 Kitware, Inc., Insight Consortium.  All rights reserved.
  See Copyright.txt or http://www.cmake.org/HTML/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "cmAddDefinitionsCommand.h"

// cmAddDefinitionsCommand
bool cmAddDefinitionsCommand
::InitialPass(std::vector<std::string> const& args, cmExecutionStatus &)
{
  // it is OK to have no arguments
  if(args.size() < 1 )
    {
    return true;
    }

  for(std::vector<std::string>::const_iterator i = args.begin();
      i != args.end(); ++i)
    {
    this->Makefile->AddDefineFlag(i->c_str());
    }
  return true;
}

