/********************************************************************************
*                                                                               *
*                          D i c t i o n a r y    C l a s s                     *
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
* $Id: FXDict.cpp,v 1.33 2006/01/22 17:58:22 fox Exp $                          *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXStream.h"
#include "FXDict.h"


/*
  Notes:
  - The hash algorithm should yield a number in the range [0...FXDict::EMPTY)
    We need FXDict::EMPTY and FXDict::UNUSED for flag purposes.
  - Since the algorithm doubles the table size when exceeding MAX_LOAD,
    it would be prudent to keep MIN_LOAD less than .5*MAX_LOAD;
    otherwise, the algorithm might hip-hop between halving and doubling,
    which would be quite expensive!!
  - Not many people seem to know that hash tables don't have to be prime
    numbers; in fact, a table size of 2**n and odd probe distance are very
    easy to arrange, and this works just as well!
  - We store the hash key, so that 99.999% of the time we can compare hash numbers;
    only when hash numbers match do we need to compare keys.
    Thus, with a good hash function, the number of calls to strcmp() should be
    roughly the same as the number of successful lookups.
  - The hash table should NEVER get full, or stuff will loop forever!!
*/


#define DEF_HASH_SIZE      4                // Initial table size (MUST be power of 2)
#define MAX_LOAD           80               // Maximum hash table load factor (%)
#define MIN_LOAD           10               // Minimum hash table load factor (%)
#define HASH1(x,n) (((unsigned int)(x))%(n))              // Probe Position [0..n-1]
#define HASH2(x,n) (1|(((unsigned int)(x)*17)%((n)-1)))   // Probe Distance [1..n-2]

using namespace FX;

/*******************************************************************************/

namespace FX {


// Hash function for string
FXint FXDict::hash(const FXchar* str){
  register const FXuchar *s=(const FXuchar*)str;
  register FXint h=0;
  register FXint c;
  while((c=*s++)!='\0'){
    h = ((h << 5) + h) ^ c;
    }
  return h&0x7fffffff;
  }


// Object implementation
FXIMPLEMENT(FXDict,FXObject,NULL,0)


// Construct empty dictionary
FXDict::FXDict(){
  register FXint i;
  FXMALLOC(&dict,FXDictEntry,DEF_HASH_SIZE);
  for(i=0; i<DEF_HASH_SIZE; i++){
    dict[i].key=NULL;
    dict[i].data=NULL;
    dict[i].hash=-1;
    dict[i].mark=false;
    }
  total=DEF_HASH_SIZE;
  number=0;
  }


// Copy constructor
FXDict::FXDict(const FXDict& orig):FXObject(orig){
  register FXint i;
  FXMALLOC(&dict,FXDictEntry,orig.total);
  for(i=0; i<orig.total; i++){
    if(0<=orig.dict[i].hash){
      dict[i].key=strdup(orig.dict[i].key);
      dict[i].data=orig.dict[i].data;
      dict[i].hash=orig.dict[i].hash;
      dict[i].mark=orig.dict[i].mark;
      continue;
      }
    dict[i].key=NULL;
    dict[i].data=NULL;
    dict[i].hash=-1;
    dict[i].mark=false;
    }
  total=orig.total;
  number=orig.number;
  }


// Assignment operator
FXDict& FXDict::operator=(const FXDict& orig){
  register FXint i;
  if(&orig!=this){
    clear();
    FXRESIZE(&dict,FXDictEntry,orig.total);
    for(i=0; i<orig.total; i++){
      if(0<=orig.dict[i].hash){
        dict[i].key=strdup(orig.dict[i].key);
        dict[i].data=orig.dict[i].data;
        dict[i].hash=orig.dict[i].hash;
        dict[i].mark=orig.dict[i].mark;
        continue;
        }
      dict[i].key=NULL;
      dict[i].data=NULL;
      dict[i].hash=-1;
      dict[i].mark=false;
      }
    total=orig.total;
    number=orig.number;
    }
  return *this;
  }


// Defaul implementation
void *FXDict::createData(const void* ptr){ return (void*)ptr; }


// Defaul implementation
void FXDict::deleteData(void*){ }


// Resize table
void FXDict::size(FXint m){
  register FXint i,n,p,x,h;
  FXDictEntry *k;
  FXASSERT(number<=total);
  if(m<DEF_HASH_SIZE) m=DEF_HASH_SIZE;
  n=total;
  while((n>>2)>m) n>>=1;            // Shrink until n/4 <= m
  while((n>>1)<m) n<<=1;            // Grow until m <= n/2
  FXASSERT(m<=(n>>1));
  FXASSERT(DEF_HASH_SIZE<=n);
  if(n!=total){
    FXTRACE((200,"FXDict::size: %p: resizing from %d to %d\n",this,total,n));
    FXASSERT(m<=n);
    FXCALLOC(&k,FXDictEntry,n);
    for(i=0; i<n; i++) k[i].hash=-1;
    for(i=0; i<total; i++){
      h=dict[i].hash;
      if(0<=h){
        p=HASH1(h,n);
        FXASSERT(0<=p && p<n);
        x=HASH2(h,n);
        FXASSERT(1<=x && x<n);
        while(k[p].hash!=-1) p=(p+x)%n;
        FXASSERT(k[p].hash<0);
        k[p]=dict[i];
        }
      }
    FXFREE(&dict);
    dict=k;
    total=n;
    }
  }


// Insert a new entry, leave it alone if already existing
void* FXDict::insert(const FXchar* ky,const void* pdata,bool mrk){
  register FXint p,i,x,h,n;
  register void *ptr;
  if(!ky){ fxerror("FXDict::insert: NULL key argument.\n"); }
  FXASSERT(number<total);
  h=hash(ky);
  FXASSERT(0<=h);
  p=HASH1(h,total);
  FXASSERT(0<=p && p<total);
  x=HASH2(h,total);
  FXASSERT(1<=x && x<total);
  i=-1;
  n=total;
  while(n && dict[p].hash!=-1){
    if((i==-1)&&(dict[p].hash==-2)) i=p;
    if(dict[p].hash==h && strcmp(dict[p].key,ky)==0){
      return dict[p].data;
      }
    p=(p+x)%total;
    n--;
    }
  if(i==-1) i=p;
  FXTRACE((200,"FXDict::insert: %p: inserting: \"%s\"\n",this,ky));
  FXASSERT(0<=i && i<total);
  FXASSERT(dict[i].hash<0);
  ptr=createData(pdata);
  dict[i].hash=h;
  dict[i].mark=mrk;
  dict[i].key=strdup(ky);
  dict[i].data=ptr;
  number++;
  if((100*number)>=(MAX_LOAD*total)) size(number);
  FXASSERT(number<total);
  return ptr;
  }


// Add or replace entry
void* FXDict::replace(const FXchar* ky,const void* pdata,bool mrk){
  register FXint p,i,x,h,n;
  register void *ptr;
  if(!ky){ fxerror("FXDict::replace: NULL key argument.\n"); }
  FXASSERT(number<total);
  h=hash(ky);
  FXASSERT(0<=h);
  p=HASH1(h,total);
  FXASSERT(0<=p && p<total);
  x=HASH2(h,total);
  FXASSERT(1<=x && x<total);
  i=-1;
  n=total;
  while(n && dict[p].hash!=-1){
    if((i==-1)&&(dict[p].hash==-2)) i=p;
    if(dict[p].hash==h && strcmp(dict[p].key,ky)==0){
      if(dict[p].mark<=mrk){
        FXTRACE((200,"FXDict::replace: %p: replacing: \"%s\"\n",this,ky));
        deleteData(dict[p].data);
        dict[p].mark=mrk;
        dict[p].data=createData(pdata);
        }
      return dict[p].data;
      }
    p=(p+x)%total;
    n--;
    }
  if(i==-1) i=p;
  FXTRACE((200,"FXDict::replace: %p: inserting: \"%s\"\n",this,ky));
  FXASSERT(0<=i && i<total);
  FXASSERT(dict[i].hash<0);
  ptr=createData(pdata);
  dict[i].hash=h;
  dict[i].mark=mrk;
  dict[i].key=strdup(ky);
  dict[i].data=ptr;
  number++;
  if((100*number)>=(MAX_LOAD*total)) size(number);
  FXASSERT(number<total);
  return ptr;
  }


// Remove entry
void* FXDict::remove(const FXchar* ky){
  register FXint p,x,h,n;
  if(!ky){ fxerror("FXDict::remove: NULL key argument.\n"); }
  if(0<number){
    h=hash(ky);
    FXASSERT(0<=h);
    p=HASH1(h,total);
    FXASSERT(0<=p && p<total);
    x=HASH2(h,total);
    FXASSERT(1<=x && x<total);
    FXASSERT(number<total);
    n=total;
    while(n && dict[p].hash!=-1){
      if(dict[p].hash==h && strcmp(dict[p].key,ky)==0){
        FXTRACE((120,"FXDict::remove: %p removing: \"%s\"\n",this,ky));
        dict[p].hash=-2;
        dict[p].mark=false;
        free(dict[p].key);
        deleteData(dict[p].data);
        dict[p].key=NULL;
        dict[p].data=NULL;
        number--;
        if((100*number)<=(MIN_LOAD*total)) size(number);
        FXASSERT(number<total);
        return NULL;
        }
      p=(p+x)%total;
      n--;
      }
    }
  return NULL;
  }


// Find entry
void* FXDict::find(const FXchar* ky) const {
  register FXint p,x,h,n;
  if(!ky){ fxerror("FXDict::find: NULL key argument.\n"); }
  if(0<number){
    h=hash(ky);
    FXASSERT(0<=h);
    p=HASH1(h,total);
    FXASSERT(0<=p && p<total);
    x=HASH2(h,total);
    FXASSERT(1<=x && x<total);
    FXASSERT(number<total);
    n=total;
    while(n && dict[p].hash!=-1){
      if(dict[p].hash==h && strcmp(dict[p].key,ky)==0){
        return dict[p].data;
        }
      p=(p+x)%total;
      n--;
      }
    }
  return NULL;
  }


// Get first non-empty entry
FXint FXDict::first() const {
  register FXint pos=0;
  while(pos<total){ if(0<=dict[pos].hash) break; pos++; }
  FXASSERT(total<=pos || 0<=dict[pos].hash);
  return pos;
  }


// Get last non-empty entry
FXint FXDict::last() const {
  register FXint pos=total-1;
  while(0<=pos){ if(0<=dict[pos].hash) break; pos--; }
  FXASSERT(pos<0 || 0<=dict[pos].hash);
  return pos;
  }


// Find next entry
FXint FXDict::next(FXint pos) const {
  FXASSERT(0<=pos && pos<total);
  while(++pos <= total-1){ if(0<=dict[pos].hash) break; }
  FXASSERT(total<=pos || 0<=dict[pos].hash);
  return pos;
  }


// Find previous entry
FXint FXDict::prev(FXint pos) const {
  FXASSERT(0<=pos && pos<total);
  while(--pos >= 0){ if(0<=dict[pos].hash) break; }
  FXASSERT(pos<0 || 0<=dict[pos].hash);
  return pos;
  }


// Remove all
void FXDict::clear(){
  register FXint i;
  for(i=0; i<total; i++){
    if(dict[i].hash>=0){
      dict[i].hash=-1;
      free(dict[i].key);
      deleteData(dict[i].data);
      }
    }
  number=0;
  }


// Destroy table
FXDict::~FXDict(){
  clear();
  FXFREE(&dict);
  dict=(FXDictEntry*)-1L;
  }

}

