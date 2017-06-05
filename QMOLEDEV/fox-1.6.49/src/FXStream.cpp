/********************************************************************************
*                                                                               *
*       P e r s i s t e n t   S t o r a g e   S t r e a m   C l a s s e s       *
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
* $Id: FXStream.cpp,v 1.66.2.3 2006/09/13 18:18:52 fox Exp $                        *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXStream.h"
#include "FXString.h"
#include "FXObject.h"


/*
  Notes:
  - Defer malloc in FXStream::open till object is actually being saved/loaded!
  - Programming errors are punished by calling fxerror(); for example, saving
    into a stream which is set for loading, NULL buffer pointers, and so on.
  - Status codes are set when a correct program encounters errors such as
    writing to full disk, running out of memory, and so on.
  - Single character insert/extract operators are virtual so subclasses can
    overload these specific cases for greater speed.
  - Copy byte at a time because don't know about alignment of stream buffer;
    unaligned accesses are disallowed on some RISC cpus.
  - Buffer can not be written till after first call to writeBuffer().
  - Need to haul some memory stream stuff up (buffer mgmt).
  - Need to have load() and save() API's return number of elements ACTUALLY
    transferred (not bytes but whole numbers of elements!).
*/


#define MAXCLASSNAME       256          // Maximum class name length
#define DEF_HASH_SIZE      32           // Initial table size (MUST be power of 2)
#define MAX_LOAD           80           // Maximum hash table load factor (%)
#define FUDGE              5            // Fudge for hash table size
#define UNUSEDSLOT         0xffffffff   // Unsused slot marker
#define CLASSIDFLAG        0x80000000   // Marks it as a class ID

#define HASH1(x,n) (((FXuint)(FXuval)(x)*13)%(n))         // Number [0..n-1]
#define HASH2(x,n) (1|(((FXuint)(FXuval)(x)*17)%((n)-1))) // Number [1..n-2]


using namespace FX;


/*******************************************************************************/

namespace FX {


// Create PersistentStore object
FXStream::FXStream(const FXObject *cont){
  parent=cont;
  begptr=NULL;
  endptr=NULL;
  wrptr=NULL;
  rdptr=NULL;
  pos=0L;
  dir=FXStreamDead;
  code=FXStreamOK;
  seq=0x80000000;
  swap=false;
  owns=false;
  }



// Set stream to big endian mode if true
void FXStream::setBigEndian(bool big){
  swap=(big^FOX_BIGENDIAN);
  }


// Return true if big endian mode.
bool FXStream::isBigEndian() const {
  return (swap^FOX_BIGENDIAN);
  }


// Destroy PersistentStore object
FXStream::~FXStream(){
  if(owns){FXFREE(&begptr);}
  parent=(FXObject*)-1L;
  begptr=(FXuchar*)-1L;
  endptr=(FXuchar*)-1L;
  wrptr=(FXuchar*)-1L;
  rdptr=(FXuchar*)-1L;
  }


// Write at least count bytes from the buffer; the default
// implementation simply discards all data in the buffer.
// It returns the amount of room available to be written.
FXuval FXStream::writeBuffer(FXuval){
  rdptr=begptr;
  wrptr=begptr;
  return endptr-wrptr;
  }


// Read at least count bytes into the buffer; the default
// implementation reads an endless stream of zero's.
// It returns the amount of data available to be read.
FXuval FXStream::readBuffer(FXuval){
  rdptr=begptr;
  wrptr=endptr;
  return wrptr-rdptr;
  }


// Set status code
void FXStream::setError(FXStreamStatus err){
  code=err;
  }


// Get available space
FXuval FXStream::getSpace() const {
  return endptr-begptr;
  }


// Set available space
void FXStream::setSpace(FXuval size){
  if(code==FXStreamOK){

    // Changed size?
    if(begptr+size!=endptr){

      // Old buffer location
      register FXuchar *oldbegptr=begptr;

      // Only resize if owned
      if(!owns){ fxerror("FXStream::setSpace: cannot resize external data buffer.\n"); }

      // Resize the buffer
      if(!FXRESIZE(&begptr,FXuchar,size)){ code=FXStreamAlloc; return; }

      // Adjust pointers, buffer may have moved
      endptr=begptr+size;
      wrptr=begptr+(wrptr-oldbegptr);
      rdptr=begptr+(rdptr-oldbegptr);
      if(wrptr>endptr) wrptr=endptr;
      if(rdptr>endptr) rdptr=endptr;
      }
    }
  }


// Open for save or load
bool FXStream::open(FXStreamDirection save_or_load,FXuval size,FXuchar* data){
  if(save_or_load!=FXStreamSave && save_or_load!=FXStreamLoad){fxerror("FXStream::open: illegal stream direction.\n");}
  if(!dir){

    // Use external buffer space
    if(data){
      begptr=data;
      if(size==ULONG_MAX)
        endptr=(FXuchar*)(FXival)-1L;
      else
        endptr=begptr+size;
      wrptr=begptr;
      rdptr=begptr;
      owns=false;
      }

    // Use internal buffer space
    else{
      if(!FXCALLOC(&begptr,FXuchar,size)){ code=FXStreamAlloc; return false; }
      endptr=begptr+size;
      wrptr=begptr;
      rdptr=begptr;
      owns=true;
      }

    // Reset variables
    hash.clear();
    dir=save_or_load;
    seq=0x80000000;
    pos=0;

    // Append container object to hash table
    if(parent){
      addObject(parent);
      }

    // So far, so good
    code=FXStreamOK;

    return true;
    }
  return false;
  }


// Flush buffer
bool FXStream::flush(){
  writeBuffer(0);
  return code==FXStreamOK;
  }


// Close store; return true if no errors have been encountered
bool FXStream::close(){
  if(dir){
    hash.clear();
    dir=FXStreamDead;
    if(owns){FXFREE(&begptr);}
    begptr=NULL;
    wrptr=NULL;
    rdptr=NULL;
    endptr=NULL;
    owns=false;
    return code==FXStreamOK;
    }
  return false;
  }


// Move to position
bool FXStream::position(FXlong offset,FXWhence whence){
  if(dir==FXStreamDead){fxerror("FXStream::position: stream is not open.\n");}
  if(code==FXStreamOK){
    if(whence==FXFromCurrent) offset=offset+pos;
    else if(whence==FXFromEnd) offset=offset+endptr-begptr;
    pos=offset;
    return true;
    }
  return false;
  }


/******************************  Save Basic Types  *****************************/

// Write one byte
FXStream& FXStream::operator<<(const FXuchar& v){
  if(code==FXStreamOK){
    FXASSERT(begptr<=rdptr);
    FXASSERT(rdptr<=wrptr);
    FXASSERT(wrptr<=endptr);
    if(wrptr+1>endptr && writeBuffer(1)<1){ code=FXStreamFull; return *this; }
    FXASSERT(wrptr+1<=endptr);
    *wrptr++ = v;
    pos++;
    }
  return *this;
  }


// Write one short
FXStream& FXStream::operator<<(const FXushort& v){
  if(code==FXStreamOK){
    FXASSERT(begptr<=rdptr);
    FXASSERT(rdptr<=wrptr);
    FXASSERT(wrptr<=endptr);
    if(wrptr+2>endptr && writeBuffer((wrptr-endptr)+2)<2){ code=FXStreamFull; return *this; }
    FXASSERT(wrptr+2<=endptr);
    if(swap){
      wrptr[0]=((const FXuchar*)&v)[1];
      wrptr[1]=((const FXuchar*)&v)[0];
      }
    else{
      wrptr[0]=((const FXuchar*)&v)[0];
      wrptr[1]=((const FXuchar*)&v)[1];
      }
    wrptr+=2;
    pos+=2;
    }
  return *this;
  }


// Write one int
FXStream& FXStream::operator<<(const FXuint& v){
  if(code==FXStreamOK){
    FXASSERT(begptr<=rdptr);
    FXASSERT(rdptr<=wrptr);
    FXASSERT(wrptr<=endptr);
    if(wrptr+4>endptr && writeBuffer((wrptr-endptr)+4)<4){ code=FXStreamFull; return *this; }
    FXASSERT(wrptr+4<=endptr);
    if(swap){
      wrptr[0]=((const FXuchar*)&v)[3];
      wrptr[1]=((const FXuchar*)&v)[2];
      wrptr[2]=((const FXuchar*)&v)[1];
      wrptr[3]=((const FXuchar*)&v)[0];
      }
    else{
      wrptr[0]=((const FXuchar*)&v)[0];
      wrptr[1]=((const FXuchar*)&v)[1];
      wrptr[2]=((const FXuchar*)&v)[2];
      wrptr[3]=((const FXuchar*)&v)[3];
      }
    wrptr+=4;
    pos+=4;
    }
  return *this;
  }


// Write one double
FXStream& FXStream::operator<<(const FXdouble& v){
  if(code==FXStreamOK){
    FXASSERT(begptr<=rdptr);
    FXASSERT(rdptr<=wrptr);
    FXASSERT(wrptr<=endptr);
    if(wrptr+8>endptr && writeBuffer((wrptr-endptr)+8)<8){ code=FXStreamFull; return *this; }
    FXASSERT(wrptr+8<=endptr);
    if(swap){
      wrptr[0]=((const FXuchar*)&v)[7];
      wrptr[1]=((const FXuchar*)&v)[6];
      wrptr[2]=((const FXuchar*)&v)[5];
      wrptr[3]=((const FXuchar*)&v)[4];
      wrptr[4]=((const FXuchar*)&v)[3];
      wrptr[5]=((const FXuchar*)&v)[2];
      wrptr[6]=((const FXuchar*)&v)[1];
      wrptr[7]=((const FXuchar*)&v)[0];
      }
    else{
      wrptr[0]=((const FXuchar*)&v)[0];
      wrptr[1]=((const FXuchar*)&v)[1];
      wrptr[2]=((const FXuchar*)&v)[2];
      wrptr[3]=((const FXuchar*)&v)[3];
      wrptr[4]=((const FXuchar*)&v)[4];
      wrptr[5]=((const FXuchar*)&v)[5];
      wrptr[6]=((const FXuchar*)&v)[6];
      wrptr[7]=((const FXuchar*)&v)[7];
      }
    wrptr+=8;
    pos+=8;
    }
  return *this;
  }


/************************  Save Blocks of Basic Types  *************************/

// Write array of bytes
FXStream& FXStream::save(const FXuchar* p,FXuval n){
  if(code==FXStreamOK){
    FXASSERT(begptr<=rdptr);
    FXASSERT(rdptr<=wrptr);
    FXASSERT(wrptr<=endptr);
    while(0<n){
      if(wrptr+n>endptr && writeBuffer((wrptr-endptr)+n)<1){ code=FXStreamFull; return *this; }
      FXASSERT(wrptr<endptr);
      do{
        *wrptr++=*p++;
        pos++;
        n--;
        }
      while(0<n && wrptr<endptr);
      }
    }
  return *this;
  }


// Write array of shorts
FXStream& FXStream::save(const FXushort* p,FXuval n){
  register const FXuchar *q=(const FXuchar*)p;
  if(code==FXStreamOK){
    n<<=1;
    FXASSERT(begptr<=rdptr);
    FXASSERT(rdptr<=wrptr);
    FXASSERT(wrptr<=endptr);
    if(swap){
      while(0<n){
        if(wrptr+n>endptr && writeBuffer((wrptr-endptr)+n)<2){ code=FXStreamFull; return *this; }
        FXASSERT(wrptr+2<=endptr);
        do{
          wrptr[0]=q[1];
          wrptr[1]=q[0];
          wrptr+=2;
          pos+=2;
          q+=2;
          n-=2;
          }
        while(0<n && wrptr+2<=endptr);
        }
      }
    else{
      while(0<n){
        if(wrptr+n>endptr && writeBuffer((wrptr-endptr)+n)<2){ code=FXStreamFull; return *this; }
        FXASSERT(wrptr+2<=endptr);
        do{
          wrptr[0]=q[0];
          wrptr[1]=q[1];
          wrptr+=2;
          pos+=2;
          q+=2;
          n-=2;
          }
        while(0<n && wrptr+2<=endptr);
        }
      }
    }
  return *this;
  }


// Write array of ints
FXStream& FXStream::save(const FXuint* p,FXuval n){
  register const FXuchar *q=(const FXuchar*)p;
  if(code==FXStreamOK){
    n<<=2;
    FXASSERT(begptr<=rdptr);
    FXASSERT(rdptr<=wrptr);
    FXASSERT(wrptr<=endptr);
    if(swap){
      while(0<n){
        if(wrptr+n>endptr && writeBuffer((wrptr-endptr)+n)<4){ code=FXStreamFull; return *this; }
        FXASSERT(wrptr+4<=endptr);
        do{
          wrptr[0]=q[3];
          wrptr[1]=q[2];
          wrptr[2]=q[1];
          wrptr[3]=q[0];
          wrptr+=4;
          pos+=4;
          q+=4;
          n-=4;
          }
        while(0<n && wrptr+4<=endptr);
        }
      }
    else{
      while(0<n){
        if(wrptr+n>endptr && writeBuffer((wrptr-endptr)+n)<4){ code=FXStreamFull; return *this; }
        FXASSERT(wrptr+4<=endptr);
        do{
          wrptr[0]=q[0];
          wrptr[1]=q[1];
          wrptr[2]=q[2];
          wrptr[3]=q[3];
          wrptr+=4;
          pos+=4;
          q+=4;
          n-=4;
          }
        while(0<n && wrptr+4<=endptr);
        }
      }
    }
  return *this;
  }


// Write array of doubles
FXStream& FXStream::save(const FXdouble* p,FXuval n){
  register const FXuchar *q=(const FXuchar*)p;
  if(code==FXStreamOK){
    n<<=3;
    FXASSERT(begptr<=rdptr);
    FXASSERT(rdptr<=wrptr);
    FXASSERT(wrptr<=endptr);
    if(swap){
      while(0<n){
        if(wrptr+n>endptr && writeBuffer((wrptr-endptr)+n)<8){ code=FXStreamFull; return *this; }
        FXASSERT(wrptr+8<=endptr);
        do{
          wrptr[0]=q[7];
          wrptr[1]=q[6];
          wrptr[2]=q[5];
          wrptr[3]=q[4];
          wrptr[4]=q[3];
          wrptr[5]=q[2];
          wrptr[6]=q[1];
          wrptr[7]=q[0];
          wrptr+=8;
          pos+=8;
          q+=8;
          n-=8;
          }
        while(0<n && wrptr+8<=endptr);
        }
      }
    else{
      while(0<n){
        if(wrptr+n>endptr && writeBuffer((wrptr-endptr)+n)<8){ code=FXStreamFull; return *this; }
        FXASSERT(wrptr+8<=endptr);
        do{
          wrptr[0]=q[0];
          wrptr[1]=q[1];
          wrptr[2]=q[2];
          wrptr[3]=q[3];
          wrptr[4]=q[4];
          wrptr[5]=q[5];
          wrptr[6]=q[6];
          wrptr[7]=q[7];
          wrptr+=8;
          pos+=8;
          q+=8;
          n-=8;
          }
        while(0<n && wrptr+8<=endptr);
        }
      }
    }
  return *this;
  }


/*****************************  Load Basic Types  ******************************/

// Read one byte
FXStream& FXStream::operator>>(FXuchar& v){
  if(code==FXStreamOK){
    FXASSERT(begptr<=rdptr);
    FXASSERT(rdptr<=wrptr);
    FXASSERT(wrptr<=endptr);
    if(rdptr+1>wrptr && readBuffer(1)<1){ code=FXStreamEnd; return *this; }
    FXASSERT(rdptr+1<=wrptr);
    v=*rdptr++;
    pos++;
    }
  return *this;
  }


// Read one short
FXStream& FXStream::operator>>(FXushort& v){
  if(code==FXStreamOK){
    FXASSERT(begptr<=rdptr);
    FXASSERT(rdptr<=wrptr);
    FXASSERT(wrptr<=endptr);
    if(rdptr+2>wrptr && readBuffer((rdptr-wrptr)+2)<2){ code=FXStreamEnd; return *this; }
    FXASSERT(rdptr+2<=wrptr);
    if(swap){
      ((FXuchar*)&v)[1]=rdptr[0];
      ((FXuchar*)&v)[0]=rdptr[1];
      }
    else{
      ((FXuchar*)&v)[0]=rdptr[0];
      ((FXuchar*)&v)[1]=rdptr[1];
      }
    rdptr+=2;
    pos+=2;
    }
  return *this;
  }


// Read one int
FXStream& FXStream::operator>>(FXuint& v){
  if(code==FXStreamOK){
    FXASSERT(begptr<=rdptr);
    FXASSERT(rdptr<=wrptr);
    FXASSERT(wrptr<=endptr);
    if(rdptr+4>wrptr && readBuffer((rdptr-wrptr)+4)<4){ code=FXStreamEnd; return *this; }
    FXASSERT(rdptr+4<=wrptr);
    if(swap){
      ((FXuchar*)&v)[3]=rdptr[0];
      ((FXuchar*)&v)[2]=rdptr[1];
      ((FXuchar*)&v)[1]=rdptr[2];
      ((FXuchar*)&v)[0]=rdptr[3];
      }
    else{
      ((FXuchar*)&v)[0]=rdptr[0];
      ((FXuchar*)&v)[1]=rdptr[1];
      ((FXuchar*)&v)[2]=rdptr[2];
      ((FXuchar*)&v)[3]=rdptr[3];
      }
    rdptr+=4;
    pos+=4;
    }
  return *this;
  }


// Read one double
FXStream& FXStream::operator>>(FXdouble& v){
  if(code==FXStreamOK){
    FXASSERT(begptr<=rdptr);
    FXASSERT(rdptr<=wrptr);
    FXASSERT(wrptr<=endptr);
    if(rdptr+8>wrptr && readBuffer((rdptr-wrptr)+8)<8){ code=FXStreamEnd; return *this; }
    FXASSERT(rdptr+8<=wrptr);
    if(swap){
      ((FXuchar*)&v)[7]=rdptr[0];
      ((FXuchar*)&v)[6]=rdptr[1];
      ((FXuchar*)&v)[5]=rdptr[2];
      ((FXuchar*)&v)[4]=rdptr[3];
      ((FXuchar*)&v)[3]=rdptr[4];
      ((FXuchar*)&v)[2]=rdptr[5];
      ((FXuchar*)&v)[1]=rdptr[6];
      ((FXuchar*)&v)[0]=rdptr[7];
      }
    else{
      ((FXuchar*)&v)[0]=rdptr[0];
      ((FXuchar*)&v)[1]=rdptr[1];
      ((FXuchar*)&v)[2]=rdptr[2];
      ((FXuchar*)&v)[3]=rdptr[3];
      ((FXuchar*)&v)[4]=rdptr[4];
      ((FXuchar*)&v)[5]=rdptr[5];
      ((FXuchar*)&v)[6]=rdptr[6];
      ((FXuchar*)&v)[7]=rdptr[7];
      }
    rdptr+=8;
    pos+=8;
    }
  return *this;
  }



/************************  Load Blocks of Basic Types  *************************/

// Read array of bytes
FXStream& FXStream::load(FXuchar* p,FXuval n){
  if(code==FXStreamOK){
    FXASSERT(begptr<=rdptr);
    FXASSERT(rdptr<=wrptr);
    FXASSERT(wrptr<=endptr);
    while(0<n){
      if(rdptr+n>wrptr && readBuffer((rdptr-wrptr)+n)<1){ code=FXStreamEnd; return *this; }
      FXASSERT(rdptr<wrptr);
      do{
        *p++=*rdptr++;
        pos++;
        n--;
        }
      while(0<n && rdptr<wrptr);
      }
    }
  return *this;
  }


// Read array of shorts
FXStream& FXStream::load(FXushort* p,FXuval n){
  register FXuchar *q=(FXuchar*)p;
  if(code==FXStreamOK){
    n<<=1;
    FXASSERT(begptr<=rdptr);
    FXASSERT(rdptr<=wrptr);
    FXASSERT(wrptr<=endptr);
    if(swap){
      while(0<n){
        if(rdptr+n>wrptr && readBuffer((rdptr-wrptr)+n)<2){ code=FXStreamEnd; return *this; }
        FXASSERT(rdptr+2<=wrptr);
        do{
          q[1]=rdptr[0];
          q[0]=rdptr[1];
          rdptr+=2;
          pos+=2;
          q+=2;
          n-=2;
          }
        while(0<n && rdptr+2<=wrptr);
        }
      }
    else{
      while(0<n){
        if(rdptr+n>wrptr && readBuffer((rdptr-wrptr)+n)<2){ code=FXStreamEnd; return *this; }
        FXASSERT(rdptr+2<=wrptr);
        do{
          q[0]=rdptr[0];
          q[1]=rdptr[1];
          rdptr+=2;
          pos+=2;
          q+=2;
          n-=2;
          }
        while(0<n && rdptr+2<=wrptr);
        }
      }
    }
  return *this;
  }


// Read array of ints
FXStream& FXStream::load(FXuint* p,FXuval n){
  register FXuchar *q=(FXuchar*)p;
  if(code==FXStreamOK){
    n<<=2;
    FXASSERT(begptr<=rdptr);
    FXASSERT(rdptr<=wrptr);
    FXASSERT(wrptr<=endptr);
    if(swap){
      while(0<n){
        if(rdptr+n>wrptr && readBuffer((rdptr-wrptr)+n)<4){ code=FXStreamEnd; return *this; }
        FXASSERT(rdptr+4<=wrptr);
        do{
          q[3]=rdptr[0];
          q[2]=rdptr[1];
          q[1]=rdptr[2];
          q[0]=rdptr[3];
          rdptr+=4;
          pos+=4;
          q+=4;
          n-=4;
          }
        while(0<n && rdptr+4<=wrptr);
        }
      }
    else{
      while(0<n){
        if(rdptr+n>wrptr && readBuffer((rdptr-wrptr)+n)<4){ code=FXStreamEnd; return *this; }
        FXASSERT(rdptr+4<=wrptr);
        do{
          q[0]=rdptr[0];
          q[1]=rdptr[1];
          q[2]=rdptr[2];
          q[3]=rdptr[3];
          rdptr+=4;
          pos+=4;
          q+=4;
          n-=4;
          }
        while(0<n && rdptr+4<=wrptr);
        }
      }
    }
  return *this;
  }


// Read array of doubles
FXStream& FXStream::load(FXdouble* p,FXuval n){
  register FXuchar *q=(FXuchar*)p;
  if(code==FXStreamOK){
    n<<=3;
    FXASSERT(begptr<=rdptr);
    FXASSERT(rdptr<=wrptr);
    FXASSERT(wrptr<=endptr);
    if(swap){
      while(0<n){
        if(rdptr+n>wrptr && readBuffer((rdptr-wrptr)+n)<8){ code=FXStreamEnd; return *this; }
        FXASSERT(rdptr+4<=wrptr);
        do{
          q[7]=rdptr[0];
          q[6]=rdptr[1];
          q[5]=rdptr[2];
          q[4]=rdptr[3];
          q[3]=rdptr[4];
          q[2]=rdptr[5];
          q[1]=rdptr[6];
          q[0]=rdptr[7];
          rdptr+=8;
          pos+=8;
          q+=8;
          n-=8;
          }
        while(0<n && rdptr+8<=wrptr);
        }
      }
    else{
      while(0<n){
        if(rdptr+n>wrptr && readBuffer((rdptr-wrptr)+n)<8){ code=FXStreamEnd; return *this; }
        FXASSERT(rdptr+4<=wrptr);
        do{
          q[0]=rdptr[0];
          q[1]=rdptr[1];
          q[2]=rdptr[2];
          q[3]=rdptr[3];
          q[4]=rdptr[4];
          q[5]=rdptr[5];
          q[6]=rdptr[6];
          q[7]=rdptr[7];
          rdptr+=8;
          pos+=8;
          q+=8;
          n-=8;
          }
        while(0<n && rdptr+8<=wrptr);
        }
      }
    }
  return *this;
  }


/*********************************  Add Object  ********************************/


// Add object without saving or loading
FXStream& FXStream::addObject(const FXObject* v){
  if(v){
    if(dir==FXStreamSave){
      hash.insert((void*)v,(void*)(FXuval)seq++);
      }
    else if(dir==FXStreamLoad){
      hash.insert((void*)(FXuval)seq++,(void*)v);
      }
    }
  return *this;
  }


/********************************  Save Object  ********************************/


// Save object
FXStream& FXStream::saveObject(const FXObject* v){
  register const FXMetaClass *cls;
  register const FXchar *name;
  FXuint tag,zero=0;
  if(dir!=FXStreamSave){ fxerror("FXStream::saveObject: wrong stream direction.\n"); }
  if(code==FXStreamOK){
    if(v==NULL){                                // Its a NULL
      *this << zero;
      return *this;
      }
    tag=(FXuint)(FXuval)hash.find((void*)v);    // Already in table
    if(tag){
      *this << tag;
      return *this;
      }
    hash.insert((void*)v,(void*)(FXuval)seq++); // Add to table
    cls=v->getMetaClass();
    name=cls->getClassName();
    tag=strlen(name)+1;
    if(tag>MAXCLASSNAME){                       // Class name too long
      code=FXStreamFormat;
      return *this;
      }
    *this << tag;                               // Save tag
    *this << zero;
    save(name,tag);
    FXTRACE((100,"%08ld: saveObject(%s)\n",(FXuval)pos,v->getClassName()));
    v->save(*this);
    }
  return *this;
  }


/*******************************  Load Object  *********************************/

// Load object
FXStream& FXStream::loadObject(FXObject*& v){
  register const FXMetaClass *cls;
  FXchar name[MAXCLASSNAME+1];
  FXuint tag,esc;
  if(dir!=FXStreamLoad){ fxerror("FXStream::loadObject: wrong stream direction.\n"); }
  if(code==FXStreamOK){
    *this >> tag;
    if(tag==0){                                 // Was a NULL
      v=NULL;
      return *this;
      }
    if(tag>=0x80000000){
      v=(FXObject*)hash.find((void*)(FXuval)tag);
      if(!v){
        code=FXStreamFormat;                    // Bad format in stream
        }
      return *this;
      }
    if(tag>MAXCLASSNAME){                       // Class name too long
      code=FXStreamFormat;                      // Bad format in stream
      return *this;
      }
    *this >> esc;                               // Read escape code
    if(esc!=0){                                 // Escape code is wrong
      code=FXStreamFormat;                      // Bad format in stream
      return *this;
      }
    load(name,tag);                             // Load name
    cls=FXMetaClass::getMetaClassFromName(name);
    if(cls==NULL){                              // No FXMetaClass with this class name
      code=FXStreamUnknown;                     // Unknown class
      return *this;
      }
    v=cls->makeInstance();                      // Build some object!!
    hash.insert((void*)(FXuval)seq++,(void*)v); // Add to table
    FXTRACE((100,"%08ld: loadObject(%s)\n",(FXuval)pos,v->getClassName()));
    v->load(*this);
    }
  return *this;
  }

}
