/*=========================================================================

  Program:   CMake - Cross-Platform Makefile Generator
  Module:    $RCSfile: cmEnableLanguageCommand.cxx,v $
  Language:  C++
  Date:      $Date: 2008-01-23 15:27:59 $
  Version:   $Revision: 1.5 $

  Copyright (c) 2002 Kitware, Inc., Insight Consortium.  All rights reserved.
  See Copyright.txt or http://www.cmake.org/HTML/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "cmEnableLanguageCommand.h"

// cmEnableLanguageCommand
bool cmEnableLanguageCommand
::InitialPass(std::vector<std::string> const& args, cmExecutionStatus &)
{
  bool optional = false;
  std::vector<std::string> languages;
  if(args.size() < 1 )
    {
    this->SetError
      ("ENABLE_LANGUAGE called with incorrect number of arguments");
    return false;
    } 
  for (std::vector<std::string>::const_iterator it = args.begin();
       it != args.end();
       ++it)
    {
    if ((*it) == "OPTIONAL")
      {
      optional = true;
      }
    else
      {
      languages.push_back(*it);
      }
    }

  this->Makefile->EnableLanguage(languages, optional);
  return true;
}

