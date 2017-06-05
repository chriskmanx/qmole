/********************************************************************************
*                                                                               *
*                           S e t t i n g s   C l a s s                         *
*                                                                               *
*********************************************************************************
* Copyright (C) 1998,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
*********************************************************************************
* This library is free software; you can redistribute it and/or                 *
* modify it under the terms of the GNU Lesser General Public                    *
* License as published by the Free Software Foundation; either                  *
* version 2.1 of the License, or (at your option) any later version.            *
*                                                                               *
* This library is distributed in the hope that it will be useful,               *
* but WITHOUT ANY WARRANTY; without even the implied warranty of                *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU             *
* Lesser General Public License for more details.                               *
*                                                                               *
* You should have received a copy of the GNU Lesser General Public              *
* License along with this library; if not, write to the Free Software           *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.    *
*********************************************************************************
* $Id: FXSettings.cpp,v 1.54.2.1 2008/07/28 18:24:09 fox Exp $                      *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "fxascii.h"
#include "FXHash.h"
#include "FXStream.h"
#include "FXString.h"
#include "FXStringDict.h"
#include "FXFile.h"
#include "FXSettings.h"

/*
  Notes:

  - Format for settings database file:

    [Section Key]
    EntryKey=string-with-no-spaces
    EntryKey="string\nwith a\nnewline in it\n"
    EntryKey=" string with leading and trailing spaces and \"embedded\" in it  "
    EntryKey=string with no leading or trailing spaces

  - EntryKey may is of the form "ali baba", "ali-baba", "ali_baba", or "ali.baba".

  - Leading/trailing spaces are NOT part of the EntryKey.

  - FXSectionDict should go; FXSettings should simply derive from FXDict.

  - Escape sequences now allow octal (\377) as well as hex (\xff) codes.

  - EntryKey format should be like values.

  - Extensive error checking in unparseFile() to ensure no settings data is
    lost when disk is full.

*/

#define MAXBUFFER 2000
#define MAXNAME   200
#define MAXVALUE  2000

using namespace FX;

/*******************************************************************************/

namespace FX {

// Object implementation
FXIMPLEMENT(FXSettings,FXDict,NULL,0)


// Construct settings database
FXSettings::FXSettings(){
  modified=false;
  }


// Construct copy of existing database
FXSettings::FXSettings(const FXSettings& orig):FXDict(orig){
  register FXint i;
  modified=orig.modified;
  for(i=0; i<orig.total; i++){
    if(0<=dict[i].hash){
      dict[i].data=new FXStringDict(*((FXStringDict*)orig.dict[i].data));
      }
    }
  }


// Assignment operator
FXSettings& FXSettings::operator=(const FXSettings& orig){
  register FXint i;
  if(&orig!=this){
    FXDict::operator=(orig);
    for(i=0; i<orig.total; i++){
      if(0<=orig.dict[i].hash){
        dict[i].data=new FXStringDict(*((FXStringDict*)orig.dict[i].data));
        }
      }
    }
  return *this;
  }


// Create data
void *FXSettings::createData(const void*){
  return new FXStringDict;
  }


// Delete data
void FXSettings::deleteData(void* ptr){
  delete ((FXStringDict*)ptr);
  }


// Read string
static bool readString(FXFile& file,FXchar *buffer,FXint& bol,FXint& eol,FXint& end){
  register FXint n;
  do{
    if(eol>=end){
      if(bol<end){ memmove(buffer,buffer+bol,end-bol); }
      end=end-bol;
      bol=0;
      eol=end;
      n=file.readBlock(buffer+end,MAXBUFFER-end);
      if(n<0) return false;
      end+=n;
      }
    }
  while(eol<end && buffer[eol++]!='\n');
  return bol<eol;
  }


// Parse filename
bool FXSettings::parseFile(const FXString& filename,bool mark){
  FXFile file(filename,FXIO::Reading);
  if(file.isOpen()){
    FXchar line[MAXBUFFER];
    FXint bol,eol,end,section,name,value,p,lineno;
    FXStringDict *group=NULL;

    lineno=bol=eol=end=0;

    // Read lines
    while(readString(file,line,bol,eol,end)){
      lineno++;

      // Skip leading spaces
      while(bol<eol && Ascii::isBlank(line[bol])) bol++;

      // Skip comment lines and empty lines
      if(bol>=eol || line[bol]=='#' || line[bol]==';' || line[bol]=='\n' || line[bol]=='\r') goto next;

      // Parse section name
      if(line[bol]=='['){

        // Scan section name
        for(section=++bol; bol<eol && line[bol]!=']' && !Ascii::isControl(line[bol]); bol++);

        // Check errors
        if(bol>=eol || line[bol]!=']'){ fxwarning("%s:%d: illegal section name.\n",filename.text(),lineno); goto next; }

        // Terminate name
        line[bol]='\0';

        // Add new section dict
        group=insert(line+section);
        }

      // Parse name-value pair
      else{

        // Should have a group
        if(!group){ fxwarning("%s:%d: settings entry should follow a section.\n",filename.text(),lineno); goto next; }

        // Scan key name
        for(name=bol; bol<eol && line[bol]!='=' && !Ascii::isControl(line[bol]); bol++);

        // Check errors
        if(bol>=eol || line[bol]!='='){ fxwarning("%s:%d: expected '=' to follow key.\n",filename.text(),lineno); goto next; }

        // Remove trailing spaces after name
        for(p=bol; name<p && Ascii::isBlank(line[p-1]); p--);

        // Terminate name
        line[p]='\0';

        // Skip leading spaces
        for(bol++; bol<eol && Ascii::isBlank(line[bol]); bol++);

        // Scan value
        for(value=bol; bol<eol && !Ascii::isControl(line[bol]); bol++);

        // Remove trailing spaces after value
        for(p=bol; value<p && Ascii::isBlank(line[p-1]); p--);

        // Terminate value
        line[p]='\0';

        // Add entry to current section
        group->replace(line+name,dequote(line+value),mark);
        }
next: bol=eol;
      }

    // Done
    return true;
    }
  return false;
  }


// Write string
static bool writeString(FXFile& file,const FXchar* string){
  register FXint len=strlen(string);
  return file.writeBlock(string,len)==len;
  }


// Unparse registry file
bool FXSettings::unparseFile(const FXString& filename){
  FXFile file(filename,FXIO::Writing);
  FXchar line[MAXVALUE];
  if(file.isOpen()){

    // Loop over all sections
    for(FXint s=first(); s<size(); s=next(s)){

      // Get group
      FXStringDict* group=data(s);
      bool sec=false;

      // Loop over all entries
      for(FXint e=group->first(); e<group->size(); e=group->next(e)){

        // Is key-value pair marked?
        if(group->mark(e)){

          // Write section name if not written yet
          if(!sec){
            if(!writeString(file,"[")) goto x;
            if(!writeString(file,key(s))) goto x;
            if(!writeString(file,"]" ENDLINE)) goto x;
            sec=true;
            }

          // Write marked key-value pairs only
          if(!writeString(file,group->key(e))) goto x;
          if(!writeString(file,"=")) goto x;
          if(!writeString(file,enquote(line,group->data(e)))) goto x;
          if(!writeString(file,ENDLINE)) goto x;
          }
        }

      // Blank line after end
      if(sec){
        if(!writeString(file,ENDLINE)) goto x;
        }
      }
    return true;
    }
x:return false;
  }


// Dequote a value, in situ
FXchar* FXSettings::dequote(FXchar* text) const {
  register FXchar *result=text;
  register FXchar *ptr=text;
  register FXuint v;
  if(*text=='"'){
    text++;
    while((v=*text++)!='\0' && v!='\n' && v!='"'){
      if(v=='\\'){
        v=*text++;
        switch(v){
          case 'n':
            v='\n';
            break;
          case 'r':
            v='\r';
            break;
          case 'b':
            v='\b';
            break;
          case 'v':
            v='\v';
            break;
          case 'a':
            v='\a';
            break;
          case 'f':
            v='\f';
            break;
          case 't':
            v='\t';
            break;
          case '\\':
            v='\\';
            break;
          case '"':
            v='"';
            break;
          case '\'':
            v='\'';
            break;
          case 'x':
            v='x';
            if(Ascii::isHexDigit(*text)){
              v=Ascii::digitValue(*text++);
              if(Ascii::isHexDigit(*text)){
                v=(v<<4)+Ascii::digitValue(*text++);
                }
              }
            break;
          case '0':
          case '1':
          case '2':
          case '3':
          case '4':
          case '5':
          case '6':
          case '7':
            v=v-'0';
            if('0'<=*text && *text<='7'){
              v=(v<<3)+*text++-'0';
              if('0'<=*text && *text<='7'){
                v=(v<<3)+*text++-'0';
                }
              }
            break;
          }
        }
      *ptr++=v;
      }
    *ptr='\0';
    }
  return result;
  }


// Check if quotes are needed
static bool needquotes(const FXchar* text){
  register const FXchar *ptr=text;
  register FXuchar c;
  while((c=*ptr++)!='\0'){
    if(0x7f<=c || c<0x20 || c=='"' || c=='\'' || c=='\\' || (c==' ' && (ptr==(text+1) || *ptr=='\0'))) return true;
    }
  return false;
  }


// Enquote a value
FXchar* FXSettings::enquote(FXchar* result,const FXchar* text){
  register FXchar *end=result+MAXVALUE-6;
  register FXchar *ptr=result;
  register FXuchar c;
  if(needquotes(text)){
    *ptr++='"';
    while((c=*text++)!='\0' && ptr<end){
      switch(c){
        case '\n':
          *ptr++='\\';
          *ptr++='n';
          break;
        case '\r':
          *ptr++='\\';
          *ptr++='r';
          break;
        case '\b':
          *ptr++='\\';
          *ptr++='b';
          break;
        case '\v':
          *ptr++='\\';
          *ptr++='v';
          break;
        case '\a':
          *ptr++='\\';
          *ptr++='a';
          break;
        case '\f':
          *ptr++='\\';
          *ptr++='f';
          break;
        case '\t':
          *ptr++='\\';
          *ptr++='t';
          break;
        case '\\':
          *ptr++='\\';
          *ptr++='\\';
          break;
        case '"':
          *ptr++='\\';
          *ptr++='"';
          break;
        case '\'':
          *ptr++='\\';
          *ptr++='\'';
          break;
        default:
          if(c<0x20 || 0x7f<c){
            *ptr++='\\';
            *ptr++='x';
            *ptr++=FXString::HEX[c>>4];
            *ptr++=FXString::HEX[c&15];
            }
          else{
            *ptr++=c;
            }
          break;
        }
      }
    *ptr++='"';
    }
  else{
    while((c=*text++)!='\0' && ptr<end){
      *ptr++=c;
      }
    }
  *ptr='\0';
  return result;
  }


// Furnish our own version if we have to
#ifndef HAVE_VSSCANF
extern "C" int vsscanf(const char* str, const char* format, va_list arg_ptr);
#endif


// Read a formatted registry entry
FXint FXSettings::readFormatEntry(const FXchar *section,const FXchar *key,const FXchar *fmt,...){
  if(!section || !section[0]){ fxerror("FXSettings::readFormatEntry: bad section argument.\n"); }
  if(!key || !key[0]){ fxerror("FXSettings::readFormatEntry: bad key argument.\n"); }
  if(!fmt){ fxerror("FXSettings::readFormatEntry: bad fmt argument.\n"); }
  FXStringDict *group=find(section);
  va_list args;
  va_start(args,fmt);
  FXint result=0;
  if(group){
    const char *value=group->find(key);
    if(value){
      result=vsscanf((char*)value,fmt,args);    // Cast needed for HP-UX 11, which has wrong prototype for vsscanf
      }
    }
  va_end(args);
  return result;
  }


// Read a string-valued registry entry
const FXchar *FXSettings::readStringEntry(const FXchar *section,const FXchar *key,const FXchar *def){
  if(!section || !section[0]){ fxerror("FXSettings::readStringEntry: bad section argument.\n"); }
  if(!key || !key[0]){ fxerror("FXSettings::readStringEntry: bad key argument.\n"); }
  FXStringDict *group=find(section);
  if(group){
    const char *value=group->find(key);
    if(value) return value;
    }
  return def;
  }


// Read a int-valued registry entry
FXint FXSettings::readIntEntry(const FXchar *section,const FXchar *key,FXint def){
  if(!section || !section[0]){ fxerror("FXSettings::readIntEntry: bad section argument.\n"); }
  if(!key || !key[0]){ fxerror("FXSettings::readIntEntry: bad key argument.\n"); }
  FXStringDict *group=find(section);
  if(group){
    const char *value=group->find(key);
    if(value){
      FXint ivalue;
      if(value[0]=='0' && (value[1]=='x' || value[1]=='X')){
        if(sscanf(value+2,"%x",&ivalue)) return ivalue;
        }
      else{
        if(sscanf(value,"%d",&ivalue)==1) return ivalue;
        }
      }
    }
  return def;
  }


// Read a unsigned int-valued registry entry
FXuint FXSettings::readUnsignedEntry(const FXchar *section,const FXchar *key,FXuint def){
  if(!section || !section[0]){ fxerror("FXSettings::readUnsignedEntry: bad section argument.\n"); }
  if(!key || !key[0]){ fxerror("FXSettings::readUnsignedEntry: bad key argument.\n"); }
  FXStringDict *group=find(section);
  if(group){
    const char *value=group->find(key);
    if(value){
      FXuint ivalue;
      if(value[0]=='0' && (value[1]=='x' || value[1]=='X')){
        if(sscanf(value+2,"%x",&ivalue)) return ivalue;
        }
      else{
        if(sscanf(value,"%u",&ivalue)==1) return ivalue;
        }
      }
    }
  return def;
  }


// Read a double-valued registry entry
FXdouble FXSettings::readRealEntry(const FXchar *section,const FXchar *key,FXdouble def){
  if(!section || !section[0]){ fxerror("FXSettings::readRealEntry: bad section argument.\n"); }
  if(!key || !key[0]){ fxerror("FXSettings::readRealEntry: bad key argument.\n"); }
  FXStringDict *group=find(section);
  if(group){
    const char *value=group->find(key);
    if(value){
      FXdouble dvalue;
      if(sscanf(value,"%lf",&dvalue)==1) return dvalue;
      }
    }
  return def;
  }


// Read a color registry entry
FXColor FXSettings::readColorEntry(const FXchar *section,const FXchar *key,FXColor def){
  if(!section || !section[0]){ fxerror("FXSettings::readColorEntry: bad section argument.\n"); }
  if(!key || !key[0]){ fxerror("FXSettings::readColorEntry: bad key argument.\n"); }
  FXStringDict *group=find(section);
  if(group){
    const char *value=group->find(key);
    if(value){
      return fxcolorfromname(value);
      }
    }
  return def;
  }


// Read a boolean registry entry
FXbool FXSettings::readBoolEntry(const FXchar *section,const FXchar *key,FXbool def){
  if(!section || !section[0]){ fxerror("FXSettings::readBoolEntry: bad section argument.\n"); }
  if(!key || !key[0]){ fxerror("FXSettings::readBoolEntry: bad key argument.\n"); }
  FXStringDict *group=find(section);
  if(group){
    const char *value=group->find(key);
    if(value){
      if(comparecase(value,"true")==0) return TRUE;
      else if(comparecase(value,"false")==0) return FALSE;
      else if(comparecase(value,"yes")==0) return TRUE;
      else if(comparecase(value,"no")==0) return FALSE;
      else if(comparecase(value,"on")==0) return TRUE;
      else if(comparecase(value,"off")==0) return FALSE;
      else if(comparecase(value,"1")==0) return TRUE;
      else if(comparecase(value,"0")==0) return FALSE;
      else if(comparecase(value,"maybe")==0) return MAYBE;
      }
    }
  return def;
  }


// Write a formatted registry entry
FXint FXSettings::writeFormatEntry(const FXchar *section,const FXchar *key,const FXchar *fmt,...){
  if(!section || !section[0]){ fxerror("FXSettings::writeFormatEntry: bad section argument.\n"); }
  if(!key || !key[0]){ fxerror("FXSettings::writeFormatEntry: bad key argument.\n"); }
  if(!fmt){ fxerror("FXSettings::writeFormatEntry: bad fmt argument.\n"); }
  FXStringDict *group=insert(section);
  va_list args;
  va_start(args,fmt);
  FXint result=0;
  if(group){
    FXchar buffer[2048];
#if defined(WIN32) || defined(HAVE_VSNPRINTF)
    result=vsnprintf(buffer,sizeof(buffer),fmt,args);
#else
    result=vsprintf(buffer,fmt,args);
#endif
    group->replace(key,buffer,TRUE);
    modified=true;
    }
  va_end(args);
  return result;
  }


// Write a string-valued registry entry
bool FXSettings::writeStringEntry(const FXchar *section,const FXchar *key,const FXchar *val){
  if(!section || !section[0]){ fxerror("FXSettings::writeStringEntry: bad section argument.\n"); }
  if(!key || !key[0]){ fxerror("FXSettings::writeStringEntry: bad key argument.\n"); }
  FXStringDict *group=insert(section);
  if(group){
    group->replace(key,val,true);
    modified=true;
    return true;
    }
  return false;
  }


// Write a int-valued registry entry
bool FXSettings::writeIntEntry(const FXchar *section,const FXchar *key,FXint val){
  if(!section || !section[0]){ fxerror("FXSettings::writeIntEntry: bad section argument.\n"); }
  if(!key || !key[0]){ fxerror("FXSettings::writeIntEntry: bad key argument.\n"); }
  FXStringDict *group=insert(section);
  if(group){
    FXchar buffer[32];
    sprintf(buffer,"%d",val);
    group->replace(key,buffer,true);
    modified=true;
    return true;
    }
  return false;
  }


// Write a unsigned int-valued registry entry
bool FXSettings::writeUnsignedEntry(const FXchar *section,const FXchar *key,FXuint val){
  if(!section || !section[0]){ fxerror("FXSettings::writeUnsignedEntry: bad section argument.\n"); }
  if(!key || !key[0]){ fxerror("FXSettings::writeUnsignedEntry: bad key argument.\n"); }
  FXStringDict *group=insert(section);
  if(group){
    FXchar buffer[32];
    sprintf(buffer,"%u",val);
    group->replace(key,buffer,TRUE);
    modified=true;
    return true;
    }
  return false;
  }


// Write a double-valued registry entry
bool FXSettings::writeRealEntry(const FXchar *section,const FXchar *key,FXdouble val){
  if(!section || !section[0]){ fxerror("FXSettings::writeRealEntry: bad section argument.\n"); }
  if(!key || !key[0]){ fxerror("FXSettings::writeRealEntry: bad key argument.\n"); }
  FXStringDict *group=insert(section);
  if(group){
    FXchar buffer[64];
    sprintf(buffer,"%.16g",val);
    group->replace(key,buffer,TRUE);
    modified=true;
    return true;
    }
  return false;
  }


// Write a color registry entry
bool FXSettings::writeColorEntry(const FXchar *section,const FXchar *key,FXColor val){
  if(!section || !section[0]){ fxerror("FXSettings::writeColorEntry: bad section argument.\n"); }
  if(!key || !key[0]){ fxerror("FXSettings::writeColorEntry: bad key argument.\n"); }
  FXStringDict *group=insert(section);
  if(group){
    FXchar buffer[64];
    group->replace(key,fxnamefromcolor(buffer,val),TRUE);
    modified=true;
    return true;
    }
  return false;
  }


// Write a boolean registry entry
bool FXSettings::writeBoolEntry(const FXchar *section,const FXchar *key,FXbool val){
  if(!section || !section[0]){ fxerror("FXSettings::writeBoolEntry: bad section argument.\n"); }
  if(!key || !key[0]){ fxerror("FXSettings::writeBoolEntry: bad key argument.\n"); }
  FXStringDict *group=insert(section);
  if(group){
    group->replace(key,(val==FALSE) ? "false" : (val==TRUE) ? "true" : "maybe",TRUE);
    modified=true;
    return true;
    }
  return false;
  }


// Delete a registry entry
bool FXSettings::deleteEntry(const FXchar *section,const FXchar *key){
  if(!section || !section[0]){ fxerror("FXSettings::deleteEntry: bad section argument.\n"); }
  if(!key || !key[0]){ fxerror("FXSettings::deleteEntry: bad key argument.\n"); }
  FXStringDict *group=insert(section);
  if(group){
    group->remove(key);
    modified=true;
    return true;
    }
  return false;
  }


// Delete section
bool FXSettings::deleteSection(const FXchar *section){
  if(!section || !section[0]){ fxerror("FXSettings::deleteSection: bad section argument.\n"); }
  remove(section);
  modified=true;
  return true;
  }


// Clear all sections
bool FXSettings::clear(){
  FXDict::clear();
  modified=true;
  return true;
  }


// See if section exists
bool FXSettings::existingSection(const FXchar *section){
  if(!section || !section[0]){ fxerror("FXSettings::existingSection: bad section argument.\n"); }
  return find(section)!=NULL;
  }


// See if entry exists
bool FXSettings::existingEntry(const FXchar *section,const FXchar *key){
  if(!section || !section[0]){ fxerror("FXSettings::existingEntry: bad section argument.\n"); }
  if(!key || !key[0]){ fxerror("FXSettings::existingEntry: bad key argument.\n"); }
  FXStringDict *group=find(section);
  return group && group->find(key)!=NULL;
  }


// Clean up
FXSettings::~FXSettings(){
  clear();
  }

}
