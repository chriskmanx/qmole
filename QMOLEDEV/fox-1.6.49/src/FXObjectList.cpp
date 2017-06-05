/********************************************************************************
*                                                                               *
*                            O b j e c t   L i s t                              *
*                                                                               *
*********************************************************************************
* Copyright (C) 1997,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXObjectList.cpp,v 1.39 2006/02/07 01:17:26 fox Exp $                    *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXStream.h"
#include "FXObject.h"
#include "FXStream.h"
#include "FXObjectList.h"

/*
  Notes:
  - FXObjectList now stores only the number of items in the list, and
    stores it in front of the list.  The benefit is that an empty list
    is now only as big as a pointer; also, initialization is faster.
  - We need to be a little bit careful accessing the number field,
    it is located in front of the array; since a pointer may be
    larger than an int, there may be some unused space in between the
    first data value and the number field.
  - When growing the array, newly allocated data are zeroed out.
*/


// Size rounded up to nearest multiple of ROUNDVAL
#define ROUNDVAL    16

// Round up to nearest ROUNDVAL
#define ROUNDUP(n)  (((n)+ROUNDVAL-1)&-ROUNDVAL)

// Empty list
#define EMPTY       ((FXObject**)(emptylist+1))

using namespace FX;

/*******************************************************************************/

namespace FX {


// Empty object list
static const FXObject* emptylist[2]={0,0};


// Change number of items in list
void FXObjectList::no(FXint num){
  register FXint old=*((FXint*)(ptr-1));
  if(old!=num){
    if(0<num){
      if(ptr==EMPTY){
        ptr=1+((FXObject**)malloc(ROUNDUP(num)*sizeof(FXObject*)+sizeof(FXObject*)));
        }
      else{
        ptr=1+((FXObject**)realloc(ptr-1,ROUNDUP(num)*sizeof(FXObject*)+sizeof(FXObject*)));
        }
      if(num>old){memset(ptr+old,0,(num-old)*sizeof(FXObject*));}
      *((FXint*)(ptr-1))=num;
      }
    else if(ptr!=EMPTY){
      free(ptr-1);
      ptr=EMPTY;
      }
    }
  }


// Default constructor
FXObjectList::FXObjectList():ptr(EMPTY){
  }


// Copy constructor
FXObjectList::FXObjectList(const FXObjectList& orig):ptr(EMPTY){
  register FXint num=orig.no();
  if(0<num){
    no(num);
    memcpy(ptr,orig.ptr,num*sizeof(FXObject*));
    }
  }


// Construct and init with single object
FXObjectList::FXObjectList(FXObject* object):ptr(EMPTY){
  no(1);
  ptr[0]=object;
  }


// Construct and init with list of objects
FXObjectList::FXObjectList(FXObject** objects,FXint n):ptr(EMPTY){
  if(0<n){
    no(n);
    memcpy(ptr,objects,n*sizeof(FXObject*));
    }
  }


// Assignment operator
FXObjectList& FXObjectList::operator=(const FXObjectList& orig){
  if(ptr!=orig.ptr){
    register FXint num=orig.no();
    if(0<num){
      no(num);
      memcpy(ptr,orig.ptr,num*sizeof(FXObject*));
      }
    else{
      no(0);
      }
    }
  return *this;
  }


// Assign object p to list
FXObjectList& FXObjectList::assign(FXObject* object){
  no(1);
  ptr[0]=object;
  return *this;
  }


// Assign n objects to list
FXObjectList& FXObjectList::assign(FXObject** objects,FXint n){
  no(n);
  if(0<n){memmove(ptr,objects,n*sizeof(FXObject*));}
  return *this;
  }


// Assign input string to this string
FXObjectList& FXObjectList::assign(FXObjectList& objects){
  assign(objects.ptr,objects.no());
  return *this;
  }


// Insert an object
FXObjectList& FXObjectList::insert(FXint pos,FXObject* object){
  register FXint num=no();
  no(num+1);
  if(pos<=0){
    memmove(&ptr[1],ptr,num*sizeof(FXObject*));
    ptr[0]=object;
    }
  else if(pos>=num){
    ptr[num]=object;
    }
  else{
    memmove(&ptr[pos+1],&ptr[pos],(num-pos)*sizeof(FXObject*));
    ptr[pos]=object;
    }
  return *this;
  }


// Insert n objects at specified position
FXObjectList& FXObjectList::insert(FXint pos,FXObject** objects,FXint n){
  if(0<n){
    register FXint num=no();
    no(num+n);
    if(pos<=0){
      memmove(&ptr[n],ptr,num*sizeof(FXObject*));
      memcpy(ptr,objects,n*sizeof(FXObject*));
      }
    else if(pos>=num){
      memcpy(&ptr[num],objects,n*sizeof(FXObject*));
      }
    else{
      memmove(&ptr[pos+n],&ptr[pos],(num-pos)*sizeof(FXObject*));
      memcpy(&ptr[pos],objects,n*sizeof(FXObject*));
      }
    }
  return *this;
  }


// Insert objects at specified position
FXObjectList& FXObjectList::insert(FXint pos,FXObjectList& objects){
  insert(pos,objects.ptr,objects.no());
  return *this;
  }


// Prepend an object
FXObjectList& FXObjectList::prepend(FXObject* object){
  register FXint num=no();
  no(num+1);
  memmove(&ptr[1],ptr,num*sizeof(FXObject*));
  ptr[0]=object;
  return *this;
  }


// Prepend n objects
FXObjectList& FXObjectList::prepend(FXObject** objects,FXint n){
  if(0<n){
    register FXint num=no();
    no(num+n);
    memmove(&ptr[n],ptr,num*sizeof(FXObject*));
    memcpy(ptr,objects,n*sizeof(FXObject*));
    }
  return *this;
  }


// Prepend objects
FXObjectList& FXObjectList::prepend(FXObjectList& objects){
  prepend(objects.ptr,objects.no());
  return *this;
  }


// Append an object
FXObjectList& FXObjectList::append(FXObject* object){
  register FXint num=no();
  no(num+1);
  ptr[num]=object;
  return *this;
  }


// Add string to the end
FXObjectList& FXObjectList::append(FXObject** objects,FXint n){
  if(0<n){
    register FXint num=no();
    no(num+n);
    memcpy(&ptr[num],objects,n*sizeof(FXObject*));
    }
  return *this;
  }


// Add string to the end
FXObjectList& FXObjectList::append(FXObjectList& objects){
  append(objects.ptr,objects.no());
  return *this;
  }


// Replace element
FXObjectList& FXObjectList::replace(FXint pos,FXObject* object){
  register FXint num=no();
  if(pos<0){
    no(num+1);
    memmove(&ptr[1],ptr,num*sizeof(FXObject*));
    ptr[0]=object;
    }
  else if(pos>=num){
    no(num+1);
    ptr[num]=object;
    }
  else{
    ptr[pos]=object;
    }
  return *this;
  }


// Replaces the m objects at pos with n objects
FXObjectList& FXObjectList::replace(FXint pos,FXint m,FXObject** objects,FXint n){
  register FXint num=no();
  if(pos+m<=0){
    if(0<n){
      no(num+n);
      memmove(&ptr[pos+n],ptr,num*sizeof(FXObject*));
      memcpy(ptr,objects,n*sizeof(FXObject*));
      }
    }
  else if(num<=pos){
    if(0<n){
      no(num+n);
      memcpy(&ptr[num],objects,n*sizeof(FXObject*));
      }
    }
  else{
    if(pos<0){m+=pos;pos=0;}
    if(pos+m>num){m=num-pos;}
    if(m<n){
      no(num-m+n);
      memmove(&ptr[pos+n],&ptr[pos+m],(num-pos-m)*sizeof(FXObject*));
      }
    else if(m>n){
      memmove(&ptr[pos+n],&ptr[pos+m],(num-pos-m)*sizeof(FXObject*));
      no(num-m+n);
      }
    if(0<n){
      memcpy(&ptr[pos],objects,n*sizeof(FXObject*));
      }
    }
  return *this;
  }


// Replace the m objects at pos with objects
FXObjectList& FXObjectList::replace(FXint pos,FXint m,FXObjectList& objects){
  replace(pos,m,objects.ptr,objects.no());
  return *this;
  }


// Remove object at pos
FXObjectList& FXObjectList::erase(FXint pos){
  register FXint num=no();
  if(0<=pos && pos<num){
    memmove(&ptr[pos],&ptr[pos+1],(num-pos-1)*sizeof(FXObject*));
    no(num-1);
    }
  return *this;
  }


// Remove n objects at pos
FXObjectList& FXObjectList::erase(FXint pos,FXint n){
  if(0<n){
    register FXint num=no();
    if(pos<num && pos+n>0){
      if(pos<0){n+=pos;pos=0;}
      if(pos+n>num){n=num-pos;}
      memmove(&ptr[pos],&ptr[pos+n],(num-n-pos)*sizeof(FXObject*));
      no(num-n);
      }
    }
  return *this;
  }


// Remove element p
FXObjectList& FXObjectList::remove(const FXObject* object){
  register FXint num=no();
  register FXint pos;
  for(pos=0; pos<num; pos++){
    if(ptr[pos]==object){
      memmove(&ptr[pos],&ptr[pos+1],(num-pos-1)*sizeof(FXObject*));
      no(num-1);
      break;
      }
    }
  return *this;
  }


// Find object in list, searching forward; return position or -1
FXint FXObjectList::find(const FXObject* object,FXint pos) const {
  register FXint num=no();
  register FXint p=pos;
  if(p<0) return -1;
  while(p<num){ if(ptr[p]==object){ return p; } ++p; }
  return -1;
  }


// Find object in list, searching backward; return position or -1
FXint FXObjectList::rfind(const FXObject* object,FXint pos) const {
  register FXint num=no();
  register FXint p=pos;
  if(p>=num) p=num-1;
  while(0<=p){ if(ptr[p]==object){ return p; } --p; }
  return -1;
  }


// Clear the list
FXObjectList& FXObjectList::clear(){
  no(0);
  return *this;
  }


// Save to stream; children may be NULL
void FXObjectList::save(FXStream& store) const {
  FXint num=no();
  store << num;
  for(FXint i=0; i<num; i++){
    store << ptr[i];
    }
  }


// Load from stream; children may be NULL
void FXObjectList::load(FXStream& store){
  FXint num;
  store >> num;
  no(num);
  for(FXint i=0; i<num; i++){
    store >> ptr[i];
    }
  }


// Free up nicely
FXObjectList::~FXObjectList(){
  no(0);
  }

}
