/********************************************************************************
*                                                                               *
*                       H a s h   T a b l e   C l a s s                         *
*                                                                               *
*********************************************************************************
* Copyright (C) 2003,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXHash.cpp,v 1.25 2006/01/22 17:58:31 fox Exp $                          *
********************************************************************************/
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"


/*
  Notes:
  - The members used and free keep track of the number of slots
    in the table which are used and which are free.
  - When an item is inserted, used is incremented if the item isn't in the table
    yet, and free is decremented if a free slot is used; if an empty slot is
    used, free stays the same.  If the table exceeds the load factor, its
    size is doubled.
  - When an item is removed, used is decremented but free stays the same
    because the slot remains marked as empty instead of free; when the
    number of used items drops below some minimum, the table's size is
    halved.
  - If the table is resized, the empty slots all become free slots since
    the empty holes are not copied into the table; only used items will
    be rehashed into the new table.
*/

#define HASH1(x,m) (((FXuint)((FXuval)(x)^(((FXuval)(x))>>13)))&((m)-1))
#define HASH2(x,m) (((FXuint)((FXuval)(x)^(((FXuval)(x))>>17)|1))&((m)-1))


using namespace FX;

/*******************************************************************************/

namespace FX {

// Make empty table
FXHash::FXHash(){
  FXMALLOC(&table,FXEntry,2);
  table[0].key=NULL;
  table[0].value=NULL;
  table[1].key=NULL;
  table[1].value=NULL;
  total=2;
  used=0;
  free=2;
  }


// Resize hash table, and rehash old stuff into it
void FXHash::size(FXuint m){
  register void *key,*value;
  register FXuint q,x,i;
  FXEntry *newtable;
  FXCALLOC(&newtable,FXEntry,m);
  for(i=0; i<total; i++){
    key=table[i].key;
    value=table[i].value;
    if(key==NULL || key==(void*)-1L) continue;
    q=HASH1(key,m);
    x=HASH2(key,m);
    while(newtable[q].key) q=(q+x)&(m-1);
    newtable[q].key=key;
    newtable[q].value=value;
    }
  FXFREE(&table);
  table=newtable;
  total=m;
  free=m-used;
  }


// Insert key into the table
void* FXHash::insert(void* key,void* value){
  register FXuint p,q,x;
  if(key){
    if((free<<1)<=total) size(total<<1);
    p=HASH1(key,total);
    x=HASH2(key,total);
    q=p;
    while(table[q].key){
      if(table[q].key==key) goto y;             // Return existing
      q=(q+x)&(total-1);
      }
    q=p;
    while(table[q].key){
      if(table[q].key==(void*)-1L) goto x;      // Put it in empty slot
      q=(q+x)&(total-1);
      }
    free--;
x:  used++;
    table[q].key=key;
    table[q].value=value;
y:  return table[q].value;
    }
  return NULL;
  }


// Replace key in the table
void* FXHash::replace(void* key,void* value){
  register FXuint p,q,x;
  if(key){
    if((free<<1)<=total) size(total<<1);
    p=HASH1(key,total);
    x=HASH2(key,total);
    q=p;
    while(table[q].key){
      if(table[q].key==key) goto y;             // Replace existing
      q=(q+x)&(total-1);
      }
    q=p;
    while(table[q].key){
      if(table[q].key==(void*)-1L) goto x;      // Put it in empty slot
      q=(q+x)&(total-1);
      }
    free--;
x:  used++;
    table[q].key=key;
y:  table[q].value=value;
    return table[q].value;
    }
  return NULL;
  }


// Remove association from the table
void* FXHash::remove(void* key){
  register FXuint q,x;
  register void* val;
  if(key){
    q=HASH1(key,total);
    x=HASH2(key,total);
    while(table[q].key!=key){
      if(table[q].key==NULL) goto x;
      q=(q+x)&(total-1);
      }
    val=table[q].value;
    table[q].key=(void*)-1L;                    // Empty but not free
    table[q].value=NULL;
    used--;
    if(used<(total>>2)) size(total>>1);
    return val;
    }
x:return NULL;
  }


// Return true if association in table
void* FXHash::find(void* key) const {
  register FXuint q,x;
  if(key){
    q=HASH1(key,total);
    x=HASH2(key,total);
    while(table[q].key!=key){
      if(table[q].key==NULL) goto x;
      q=(q+x)&(total-1);
      }
    return table[q].value;
    }
x:return NULL;
  }


// Clear hash table
void FXHash::clear(){
  FXRESIZE(&table,FXEntry,2);
  table[0].key=NULL;
  table[0].value=NULL;
  table[1].key=NULL;
  table[1].value=NULL;
  total=2;
  used=0;
  free=2;
  }


// Destroy table
FXHash::~FXHash(){
  FXFREE(&table);
  table=(FXEntry*)-1L;
  }

}

