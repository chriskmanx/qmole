/*=========================================================================

  Program:   CMake - Cross-Platform Makefile Generator
  Module:    $RCSfile: cmDocumentationFormatterHTML.h,v $
  Language:  C++
  Date:      $Date: 2008-03-05 16:05:22 $
  Version:   $Revision: 1.3 $

  Copyright (c) 2002 Kitware, Inc., Insight Consortium.  All rights reserved.
  See Copyright.txt or http://www.cmake.org/HTML/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _cmDocumentationFormatterHTML_h
#define _cmDocumentationFormatterHTML_h

#include "cmStandardIncludes.h"

#include "cmDocumentationFormatter.h"

/** Class to print the documentation as HTML.  */
class cmDocumentationFormatterHTML : public cmDocumentationFormatter
{
public:
  cmDocumentationFormatterHTML();

  virtual cmDocumentationEnums::Form GetForm() const
                                      { return cmDocumentationEnums::HTMLForm;}

  virtual void PrintHeader(const char* name, std::ostream& os);
  virtual void PrintFooter(std::ostream& os);
  virtual void PrintSection(std::ostream& os,
                    const cmDocumentationSection& section,
                    const char* name);
  virtual void PrintPreformatted(std::ostream& os, const char* text);
  virtual void PrintParagraph(std::ostream& os, const char* text); 
  virtual void PrintIndex(std::ostream& ,
                          std::vector<const cmDocumentationSection *>&);
private:
  void PrintHTMLEscapes(std::ostream& os, const char* text);
};

#endif
