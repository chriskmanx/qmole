/********************************************************************************
*                                                                               *
*                     H i g h l i g h t   E n g i n e                           *
*                                                                               *
*********************************************************************************
* Copyright (C) 2002,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
*********************************************************************************
* This program is free software; you can redistribute it and/or modify          *
* it under the terms of the GNU General Public License as published by          *
* the Free Software Foundation; either version 2 of the License, or             *
* (at your option) any later version.                                           *
*                                                                               *
* This program is distributed in the hope that it will be useful,               *
* but WITHOUT ANY WARRANTY; without even the implied warranty of                *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                 *
* GNU General Public License for more details.                                  *
*                                                                               *
* You should have received a copy of the GNU General Public License             *
* along with this program; if not, write to the Free Software                   *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.    *
*********************************************************************************
* $Id: Hilite.h,v 1.35 2006/01/22 18:01:10 fox Exp $                            *
********************************************************************************/
#ifndef HILITE_H
#define HILITE_H

#include "FXRex.h"

class FXRule;
class FXSyntax;


// List of highlight nodes
typedef FXObjectListOf<FXRule> FXRuleList;


// List of syntaxes
typedef FXObjectListOf<FXSyntax> FXSyntaxList;


// Highlight node
class FXRule : public FXObject {
  FXDECLARE(FXRule)
  friend class FXSyntax;
protected:
  FXString     name;    // Name of rule
  FXRuleList   rules;   // Subrules
  FXint        parent;  // Parent style index
  FXint        style;   // Own style index
protected:
  FXRule(){}
private:
  FXRule(const FXRule&);
  FXRule &operator=(const FXRule&);
public:

  // Construct node
  FXRule(const FXString& nm,FXint p,FXint s):name(nm),parent(p),style(s){ }

  // Get number of child rules
  FXint getNumRules() const { return rules.no(); }

  // Get child rule
  FXRule* getRule(FXint index) const { return rules[index]; }

  // Rule name
  const FXString& getName() const { return name; }
  void setName(const FXString& nm){ name=nm; }

  // Get parent
  FXint getParent() const { return parent; }

  // Get style
  FXint getStyle() const { return style; }

  // Stylize text
  virtual FXbool stylize(const FXchar* text,FXchar *textstyle,FXint fm,FXint to,FXint& start,FXint& stop) const;

  // Stylize body, i.e. after begin pattern has been seen
  virtual FXbool stylizeBody(const FXchar* text,FXchar *textstyle,FXint fm,FXint to,FXint& start,FXint& stop) const;
  };


// Simple highlight node
class FXSimpleRule : public FXRule {
  FXDECLARE(FXSimpleRule)
protected:
  FXRex pat;            // Pattern to match
protected:
  FXSimpleRule(){ }
private:
  FXSimpleRule(const FXSimpleRule&);
  FXSimpleRule &operator=(const FXSimpleRule&);
public:

  // Construct node
  FXSimpleRule(const FXString& nm,const FXString& rex,FXint p,FXint s):FXRule(nm,p,s),pat(rex,REX_NEWLINE){ }

  // Stylize text
  virtual FXbool stylize(const FXchar* text,FXchar *textstyle,FXint fm,FXint to,FXint& start,FXint& stop) const;
  };


// Bracketed highlight node
class FXBracketRule : public FXRule {
  FXDECLARE(FXBracketRule)
protected:
  FXRex beg;            // Beginning pattern
  FXRex end;            // Ending pattern
protected:
  FXBracketRule(){ }
private:
  FXBracketRule(const FXBracketRule&);
  FXBracketRule &operator=(const FXBracketRule&);
public:

  // Construct node
  FXBracketRule(const FXString& nm,const FXString& brex,const FXString& erex,FXint p,FXint s):FXRule(nm,p,s),beg(brex,REX_NEWLINE),end(erex,REX_NEWLINE){ }

  // Stylize text
  virtual FXbool stylize(const FXchar* text,FXchar *textstyle,FXint fm,FXint to,FXint& start,FXint& stop) const;

  // Stylize body, i.e. after begin pattern has been seen
  virtual FXbool stylizeBody(const FXchar* text,FXchar *textstyle,FXint fm,FXint to,FXint& start,FXint& stop) const;
  };



// Bracketed highlight node with termination
class FXSafeBracketRule : public FXBracketRule {
  FXDECLARE(FXSafeBracketRule)
protected:
  FXRex esc;           // Termination pattern
protected:
  FXSafeBracketRule(){ }
private:
  FXSafeBracketRule(const FXSafeBracketRule&);
  FXSafeBracketRule &operator=(const FXSafeBracketRule&);
public:

  // Construct node
  FXSafeBracketRule(const FXString& nm,const FXString& brex,const FXString& erex,const FXString& srex,FXint p,FXint s):FXBracketRule(nm,brex,erex,p,s),esc(srex,REX_NEWLINE){ }

  // Stylize text
  virtual FXbool stylize(const FXchar* text,FXchar *textstyle,FXint fm,FXint to,FXint& start,FXint& stop) const;

  // Stylize body, i.e. after begin pattern has been seen
  virtual FXbool stylizeBody(const FXchar* text,FXchar *textstyle,FXint fm,FXint to,FXint& start,FXint& stop) const;
  };



// Master highlight node
class FXMasterRule : public FXRule {
  FXDECLARE(FXMasterRule)
protected:
  FXMasterRule(){ }
private:
  FXMasterRule(const FXMasterRule&);
  FXMasterRule &operator=(const FXMasterRule&);
public:

  // Construct node
  FXMasterRule(const FXString& nm,FXint p,FXint s):FXRule(nm,p,s){ }

  // Stylize text
  virtual FXbool stylize(const FXchar* text,FXchar *textstyle,FXint fm,FXint to,FXint& start,FXint& stop) const;

  // Stylize body, i.e. after begin pattern has been seen
  virtual FXbool stylizeBody(const FXchar* text,FXchar *textstyle,FXint fm,FXint to,FXint& start,FXint& stop) const;
  };


// Syntax for a language
class FXSyntax : public FXObject {
  FXDECLARE(FXSyntax)
protected:
  FXRuleList    rules;          // Highlight rules
  FXString      language;       // Language name
  FXString      extensions;     // File extensions to recognize language
  FXString      contents;       // Contents to recognize language
  FXString      delimiters;     // Word delimiters in this language
  FXint         contextLines;   // Context lines needed for restyle
  FXint         contextChars;   // Context characters needed for restyle
protected:
  static const FXHiliteStyle defaultStyle;
protected:
  FXSyntax(){}
private:
  FXSyntax(const FXSyntax&);
  FXSyntax &operator=(const FXSyntax&);
public:

  // New language
  FXSyntax(const FXString& lang);

  // Get number of child rules
  FXint getNumRules() const { return rules.no(); }

  // Get rule
  FXRule* getRule(FXint rule) const { return rules[rule]; }

  // Return true if toplevel rule
  FXbool isRoot(FXint rule) const;

  // Return true if p is ancestor of c
  FXbool isAncestor(FXint p,FXint c) const;

  // Language name
  const FXString& getName() const { return language; }
  void setName(const FXString& lang){ language=lang; }

  // Extensions
  const FXString& getExtensions() const { return extensions; }
  void setExtensions(const FXString& exts){ extensions=exts; }

  // Contents
  const FXString& getContents() const { return contents; }
  void setContents(const FXString& cont){ contents=cont; }

  // Delimiters
  const FXString& getDelimiters() const { return delimiters; }
  void setDelimiters(const FXString& delims){ delimiters=delims; }

  // Context lines
  FXint getContextLines() const { return contextLines; }
  void setContextLines(FXint num){ contextLines=num; }

  // Context characters
  FXint getContextChars() const { return contextChars; }
  void setContextChars(FXint num){ contextChars=num; }

  // Match filename against wildcards
  FXbool matchFilename(const FXString& name) const;

  // Match contents against regular expression
  FXbool matchContents(const FXString& text) const;

  // Append simple rule
  FXint append(const FXString& name,const FXString& rex,FXint parent=0);

  // Append bracket rule
  FXint append(const FXString& name,const FXString& brex,const FXString& erex,FXint parent=0);

  // Append safe bracket rule
  FXint append(const FXString& name,const FXString& brex,const FXString& erex,const FXString& srex,FXint parent=0);

  // Wipes the rules
  virtual ~FXSyntax();
  };


#endif

