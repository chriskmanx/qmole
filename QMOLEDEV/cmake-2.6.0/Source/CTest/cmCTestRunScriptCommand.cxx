/*=========================================================================

  Program:   CMake - Cross-Platform Makefile Generator
  Module:    $RCSfile: cmCTestRunScriptCommand.cxx,v $
  Language:  C++
  Date:      $Date: 2008-01-23 15:28:01 $
  Version:   $Revision: 1.7 $

  Copyright (c) 2002 Kitware, Inc., Insight Consortium.  All rights reserved.
  See Copyright.txt or http://www.cmake.org/HTML/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "cmCTestRunScriptCommand.h"

#include "cmCTestScriptHandler.h"

bool cmCTestRunScriptCommand
::InitialPass(std::vector<std::string> const& args, cmExecutionStatus &)
{
  if(args.size() < 1 )
    {
    this->CTestScriptHandler->RunCurrentScript();
    return true;
    }

  bool np = false;
  unsigned int i = 0;
  if (args[i] == "NEW_PROCESS")
    {
    np = true;
    i++;
    }
  // run each script
  for (; i < args.size(); ++i)
    {
    cmCTestScriptHandler::RunScript(this->CTest, args[i].c_str(), !np);
    }
  return true;
}


