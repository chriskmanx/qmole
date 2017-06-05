/*=========================================================================

  Program:   CMake - Cross-Platform Makefile Generator
  Module:    $RCSfile: cmCTestEmptyBinaryDirectoryCommand.cxx,v $
  Language:  C++
  Date:      $Date: 2008-01-23 15:28:01 $
  Version:   $Revision: 1.4 $

  Copyright (c) 2002 Kitware, Inc., Insight Consortium.  All rights reserved.
  See Copyright.txt or http://www.cmake.org/HTML/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "cmCTestEmptyBinaryDirectoryCommand.h"

#include "cmCTestScriptHandler.h"

bool cmCTestEmptyBinaryDirectoryCommand
::InitialPass(std::vector<std::string> const& args, cmExecutionStatus &)
{
  if(args.size() != 1 )
    {
    this->SetError("called with incorrect number of arguments");
    return false;
    }

  if ( !cmCTestScriptHandler::EmptyBinaryDirectory(args[0].c_str()) )
    {
    cmOStringStream ostr;
    ostr << "problem removing the binary directory: " << args[0].c_str();
    this->SetError(ostr.str().c_str());
    return false;
    }  
  
  return true;
}


