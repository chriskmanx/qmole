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
* $Id: Hilite.cpp,v 1.44 2006/01/22 18:01:10 fox Exp $                          *
********************************************************************************/
#include "fx.h"
#include <new>
#include "FXRex.h"
#include "FXArray.h"
#include "Hilite.h"


/*
  Notes:
  - Restart position: place in text which is default style, a few
    lines of context before the change.
  - Language mode: use wildcard on filename, or forced explicitly.
  - Either simple pattern, or begin/end pattern. Special stop pattern
    to prevent scanning indefinitely.  Patterns may have sub-patterns.
  - Simple pattern must be non-empty; begin/end patterns of a complex
    pattern may be zero-width assertions.
  - Capturing parenthesis are disabled, for speed reasons.
  - Sample text in FXSyntax is for displaying inside interactive
    style setup dialog; it is supposed to contain on instance of
    each pattern matched by the rule base
*/

/*******************************************************************************/


// Default style is all zeroes
const FXHiliteStyle FXSyntax::defaultStyle={
  FXRGBA(0,0,0,0),
  FXRGBA(0,0,0,0),
  FXRGBA(0,0,0,0),
  FXRGBA(0,0,0,0),
  FXRGBA(0,0,0,0),
  FXRGBA(0,0,0,0),
  FXRGBA(0,0,0,0),
  0
  };


FXIMPLEMENT(FXRule,FXObject,NULL,0)


// Fill textstyle with style, returns position of last change+1
static inline void fillstyle(FXchar* textstyle,FXchar style,FXint f,FXint t){
  while(f<t) textstyle[f++]=style;
  }


// Stylize text
FXbool FXRule::stylize(const FXchar*,FXchar*,FXint,FXint,FXint&,FXint&) const {
  return FALSE;
  }


// Stylize body, i.e. after begin pattern has been seen
FXbool FXRule::stylizeBody(const FXchar*,FXchar*,FXint,FXint,FXint&,FXint&) const {
  return FALSE;
  }


FXIMPLEMENT(FXSimpleRule,FXRule,NULL,0)

// Stylize simple expression
FXbool FXSimpleRule::stylize(const FXchar* text,FXchar *textstyle,FXint fm,FXint to,FXint& start,FXint& stop) const {
  if(pat.match(text,to,&start,&stop,REX_NOT_EMPTY|REX_FORWARD,1,fm,fm)){
    fillstyle(textstyle,style,start,stop);
    return TRUE;
    }
  return FALSE;
  }


FXIMPLEMENT(FXBracketRule,FXRule,NULL,0)

// Stylize complex recursive expression
FXbool FXBracketRule::stylizeBody(const FXchar* text,FXchar *textstyle,FXint fm,FXint to,FXint& start,FXint& stop) const {
  FXint head,tail,node;
  start=fm;
  while(fm<to){
    for(node=0; node<rules.no(); node++){
      if(rules[node]->stylize(text,textstyle,fm,to,head,tail)){
        fm=tail;
        goto nxt;
        }
      }
    if(end.match(text,to,&head,&stop,REX_FORWARD,1,fm,fm)){
      fillstyle(textstyle,style,head,stop);
      return TRUE;
      }
    textstyle[fm++]=style;
nxt:continue;
    }
  stop=fm;
  return TRUE;
  }


// Stylize complex recursive expression
FXbool FXBracketRule::stylize(const FXchar* text,FXchar *textstyle,FXint fm,FXint to,FXint& start,FXint& stop) const {
  FXint head,tail;
  if(beg.match(text,to,&start,&tail,REX_FORWARD,1,fm,fm)){
    fillstyle(textstyle,style,start,tail);
    FXBracketRule::stylizeBody(text,textstyle,tail,to,head,stop);
    return TRUE;
    }
  return FALSE;
  }


FXIMPLEMENT(FXSafeBracketRule,FXBracketRule,NULL,0)

// Stylize complex recursive expression with termination pattern
FXbool FXSafeBracketRule::stylizeBody(const FXchar* text,FXchar *textstyle,FXint fm,FXint to,FXint& start,FXint& stop) const {
  FXint head,tail,node;
  start=fm;
  while(fm<to){
    for(node=0; node<rules.no(); node++){
      if(rules[node]->stylize(text,textstyle,fm,to,head,tail)){
        fm=tail;
        goto nxt;
        }
      }
    if(end.match(text,to,&head,&stop,REX_FORWARD,1,fm,fm)){
      fillstyle(textstyle,style,head,stop);
      return TRUE;
      }
    if(esc.match(text,to,&head,&stop,REX_FORWARD,1,fm,fm)){
      fillstyle(textstyle,style,head,stop);
      return TRUE;
      }
    textstyle[fm++]=style;
nxt:continue;
    }
  stop=fm;
  return TRUE;
  }


// Stylize complex recursive expression with termination pattern
FXbool FXSafeBracketRule::stylize(const FXchar* text,FXchar *textstyle,FXint fm,FXint to,FXint& start,FXint& stop) const {
  FXint head,tail;
  if(beg.match(text,to,&start,&tail,REX_FORWARD,1,fm,fm)){
    fillstyle(textstyle,style,start,tail);
    FXSafeBracketRule::stylizeBody(text,textstyle,tail,to,head,stop);
    return TRUE;
    }
  return FALSE;
  }


FXIMPLEMENT(FXMasterRule,FXRule,NULL,0)

// Stylize body
FXbool FXMasterRule::stylizeBody(const FXchar* text,FXchar *textstyle,FXint fm,FXint to,FXint& start,FXint& stop) const {
  FXint head,tail,node;
  start=fm;
  while(fm<to){
    for(node=0; node<rules.no(); node++){
      if(rules[node]->stylize(text,textstyle,fm,to,head,tail)){
        fm=tail;
        goto nxt;
        }
      }
    textstyle[fm++]=style;
nxt:continue;
    }
  stop=to;
  return TRUE;
  }


// Stylize text
FXbool FXMasterRule::stylize(const FXchar* text,FXchar *textstyle,FXint fm,FXint to,FXint& start,FXint& stop) const {
  return FXMasterRule::stylizeBody(text,textstyle,fm,to,start,stop);
  }


FXIMPLEMENT(FXSyntax,FXObject,NULL,0)


// Construct syntax object; needs at least one master rule
FXSyntax::FXSyntax(const FXString& lang):language(lang){
  rules.append(new FXMasterRule("Master",-1,0));
  delimiters=FXText::textDelimiters;
  contextLines=1;
  contextChars=1;
  }


// Match filename against wildcards
FXbool FXSyntax::matchFilename(const FXString& name) const {
  return FXPath::match(extensions,name);
  }


// Match contents against regular expression
FXbool FXSyntax::matchContents(const FXString& text) const {
  return FXRex(contents).match(text);
  }


// Append simple rule
FXint FXSyntax::append(const FXString& name,const FXString& rex,FXint parent){
  register FXint index=rules.no();
  FXASSERT(0<=parent && parent<rules.no());
  FXSimpleRule *rule=new FXSimpleRule(name,rex,parent,index);
  rules.append(rule);
  rules[parent]->rules.append(rule);
  return index;
  }


// Append bracket rule
FXint FXSyntax::append(const FXString& name,const FXString& brex,const FXString& erex,FXint parent){
  register FXint index=rules.no();
  FXASSERT(0<=parent && parent<rules.no());
  FXBracketRule *rule=new FXBracketRule(name,brex,erex,parent,index);
  rules.append(rule);
  rules[parent]->rules.append(rule);
  return index;
  }


// Append safe bracket rule
FXint FXSyntax::append(const FXString& name,const FXString& brex,const FXString& erex,const FXString& srex,FXint parent){
  register FXint index=rules.no();
  FXASSERT(0<=parent && parent<rules.no());
  FXSafeBracketRule *rule=new FXSafeBracketRule(name,brex,erex,srex,parent,index);
  rules.append(rule);
  rules[parent]->rules.append(rule);
  return index;
  }


// Return true if toplevel rule
FXbool FXSyntax::isRoot(FXint rule) const {
  FXASSERT(0<=rule && rule<rules.no());
  return rule==0 || rules[rule]->parent==0;
  }


// Return true if p is ancestor of c
FXbool FXSyntax::isAncestor(FXint p,FXint c) const {
  FXASSERT(0<=p && 0<=c);
  while(c>0){
    c=rules[c]->getParent();
    if(c==p) return TRUE;
    }
  return FALSE;
  }


// Clean up
FXSyntax::~FXSyntax(){
  for(int i=0; i<rules.no(); i++) delete rules[i];
  }



