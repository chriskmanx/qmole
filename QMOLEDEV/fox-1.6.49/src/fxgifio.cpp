/********************************************************************************
*                                                                               *
*                        G I F   I n p u t / O u t p u t                        *
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
* $Id: fxgifio.cpp,v 1.79 2006/01/22 17:58:52 fox Exp $                         *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXStream.h"
#include "fxpriv.h"


/*
  Notes:

  - "The Graphics Interchange Format(c) is the Copyright property of
    CompuServe Incorporated. GIF(sm) is a Service Mark property of
    CompuServe Incorporated."

  - Make sure reading and writing GIF transfers same number of bytes
    from/to the stream.

  - For a transparent pixel, we zero out the alpha channel but leave
    the RGB intact; this way, we can maintain the original RGB data.

  - The interleaving works as follows:

                Pass1  Pass2  Pass3  Pass4
        Start     0      4      3      1
        Step      8      8      4      2

  - LZW Patent has expired 6/20/2003; so compression now implemented.
*/


using namespace FX;

/*******************************************************************************/

namespace FX {


extern FXAPI bool fxcheckGIF(FXStream& store);
extern FXAPI bool fxloadGIF(FXStream& store,FXColor*& data,FXint& width,FXint& height);
extern FXAPI bool fxsaveGIF(FXStream& store,const FXColor *data,FXint width,FXint height,bool fast=true);


// Codes found in the GIF specification
const FXuchar TAG_EXTENSION   = 0x21;   // Extension block
const FXuchar TAG_GRAPHIC     = 0xF9;   // Graphic control block
const FXuchar TAG_IMAGE       = 0x2c;   // Image separator
const FXuchar TAG_TERMINATOR  = 0x00;   // Block terminator
const FXuchar TAG_GRAPHICSIZE = 0x04;   // Graphic block size
const FXuchar TAG_IMAGEFLAGS  = 0x00;   // Image flags
const FXuchar TAG_ZERO        = 0x00;   // Just a zero
const FXuchar TAG_ENDFILE     = 0x3B;   // End of file
const FXuchar TAG_TRANSPARENT = 0x01;   // Transparent flag
const FXuchar TAG_SIG1        = 0x47;   // Signature G
const FXuchar TAG_SIG2        = 0x49;   // Signature I
const FXuchar TAG_SIG3        = 0x46;   // Signature F
const FXuchar TAG_VER         = 0x38;   // Version byte
const FXuchar TAG_NEW         = 0x39;   // New version
const FXuchar TAG_OLD         = 0x37;   // Old version
const FXuchar TAG_SUF         = 0x61;   // Version suffix


// Check if stream contains a GIF
bool fxcheckGIF(FXStream& store){
  FXuchar signature[3];
  store.load(signature,3);
  store.position(-3,FXFromCurrent);
  return signature[0]==TAG_SIG1 && signature[1]==TAG_SIG2 && signature[2]==TAG_SIG3;
  }


// Load image from stream
bool fxloadGIF(FXStream& store,FXColor*& data,FXint& width,FXint& height){
  const   FXint Yinit[4]={0,4,2,1};
  const   FXint Yinc[4]={8,8,4,2};
  FXint   imwidth,imheight,interlace,ncolors,npixels,maxpixels,i;
  FXuchar c1,c2,c3,sbsize,flags,alpha,*ptr,*buf,*pix;
  FXColor colormap[256];
  FXint   BitOffset;                  // Bit Offset of next code
  FXint   ByteOffset;                 // Byte offset of next code
  FXint   XC,YC;                      // Output X and Y coords of current pixel
  FXint   Pass;                       // Used by output routine if interlaced pic
  FXint   OutCount;                   // Decompressor output 'stack count'
  FXint   CodeSize;                   // Code size, read from GIF header
  FXint   InitCodeSize;               // Starting code size, used during Clear
  FXint   Code;                       // Value returned by ReadCode
  FXint   MaxCode;                    // limiting value for current code size
  FXint   ClearCode;                  // GIF clear code
  FXint   EOFCode;                    // GIF end-of-information code
  FXint   CurCode,OldCode,InCode;     // Decompressor variables
  FXint   FirstFree;                  // First free code, generated per GIF spec
  FXint   FreeCode;                   // Decompressor,next free slot in hash table
  FXint   FinChar;                    // Decompressor variable
  FXint   BitMask;                    // AND mask for data size
  FXint   ReadMask;                   // Code AND mask for current code size
  FXint   Prefix[4096];               // The hash table used by the decompressor
  FXint   Suffix[4096];               // The hash table used by the decompressor
  FXint   OutCode[4097];              // An output array used by the decompressor

  // Null out
  data=NULL;
  width=0;
  height=0;

  // Load signature
  store >> c1;
  store >> c2;
  store >> c3;

  // Check signature
  if(c1!=TAG_SIG1 || c2!=TAG_SIG2 || c3!=TAG_SIG3) return false;

  // Load version
  store >> c1;
  store >> c2;
  store >> c3;

  // Check version
  if(c1!=TAG_VER || (c2!=TAG_OLD && c2!=TAG_NEW) || c3!=TAG_SUF) return false;

  // Get screen descriptor
  store >> c1 >> c2;    // Skip screen width
  store >> c1 >> c2;    // Skip screen height
  store >> flags;       // Get flags
  store >> alpha;       // Background
  store >> c2;          // Skip aspect ratio

  // Determine number of colors
  ncolors=2<<(flags&7);
  BitMask=ncolors-1;

  // If no colormap, spec says first 2 colors are black and white
  colormap[0]=FXRGB(0,0,0);
  colormap[1]=FXRGB(255,255,255);

  // Read global map if there is one
  if(flags&0x80){
    for(i=0; i<ncolors; i++){
      store >> ((FXuchar*)(colormap+i))[0];     // Blue
      store >> ((FXuchar*)(colormap+i))[1];     // Green
      store >> ((FXuchar*)(colormap+i))[2];     // Red
      ((FXuchar*)(colormap+i))[3]=255;          // Alpha
      }
    }

  // Process it
  while(1){
    store >> c1;
    if(c1==TAG_EXTENSION){

      // Read extension code
      store >> c2;

      // Graphic Control Extension
      if(c2==TAG_GRAPHIC){
        store >> sbsize;
        if(sbsize!=TAG_GRAPHICSIZE) return false;
        store >> flags;         // Flags
        store >> c3 >> c3;      // Delay time
        store >> alpha;         // Alpha color index; we suspect alpha<ncolors not always true...
        store >> c3;
        if(flags&1){            // Clear alpha channel of alpha color
          colormap[alpha]&=FXRGBA(255,255,255,0);       // Clear the alpha channel but keep the RGB
          }
        continue;
        }

      // Other extension
      do{
        store >> sbsize;
        store.position(store.position()+sbsize);
        }
      while(sbsize>0 && !store.eof());    // FIXME this logic still flawed
      continue;
      }

    // Image separator
    if(c1==TAG_IMAGE){
      store >> c1 >> c2;
      store >> c1 >> c2;

      // Get image width
      store >> c1 >> c2;
      imwidth=(c2<<8)+c1;

      // Get image height
      store >> c1 >> c2;
      imheight=(c2<<8)+c1;

      // Get image flags
      store >> flags;

      // Read local map if there is one
      if(flags&0x80){
        ncolors=2<<(flags&7);
        for(i=0; i<ncolors; i++){
          store >> ((FXuchar*)(colormap+i))[0]; // Red
          store >> ((FXuchar*)(colormap+i))[1]; // Green
          store >> ((FXuchar*)(colormap+i))[2]; // Blue
          ((FXuchar*)(colormap+i))[3]=255;      // Alpha
          }
        }

      // Interlaced image
      interlace=(flags&0x40);

      // Total pixels expected
      maxpixels=imwidth*imheight;

      // Allocate memory
      if(!FXMALLOC(&data,FXColor,maxpixels)) return false;

      // Set up pointers; we're using the first 3/4 of the
      // data array for the compressed data, and the latter 1/4 for
      // the 8-bit pixel data.  At the end of the decompression, we
      // overwrite the data array with the 32-bit RGBA data.
      // Note that the unGIF "compressed" data may be larger than
      // the uncompressed data, hence the large safety factor...
      buf=(FXuchar*)data;
      pix=buf+maxpixels+maxpixels+maxpixels;

      // Start reading the raster data. First we get the intial code size
      // and compute decompressor constant values, based on this code size.
      store >> c1;
      CodeSize=c1;

      ClearCode=1<<CodeSize;
      EOFCode=ClearCode+1;
      FreeCode=FirstFree=ClearCode+2;

      // The GIF spec has it that the code size is the code size used to
      // compute the above values is the code size given in the file, but the
      // code size used in compression/decompression is the code size given in
      // the file plus one.
      CodeSize++;
      InitCodeSize=CodeSize;
      MaxCode=1<<CodeSize;
      ReadMask=MaxCode-1;

      // Maximum code should not exceed 4096
      if(MaxCode>=4096){ FXFREE(&data); return false; }

      // Read all blocks of compressed data into one single buffer.
      // We have an extra test to make sure we don't write past 3/4
      // of the buffer:- this could happen in malicious GIF images!
      ptr=buf;
      do{
        store >> sbsize;
        if(ptr+sbsize>pix){ FXFREE(&data); return false; }
        store.load(ptr,sbsize);
        ptr+=sbsize;
        }
      while(sbsize>0 && !store.eof());    // FIXME this logic still flawed

      // Initialize
      BitOffset=XC=YC=Pass=OutCount=OldCode=FinChar=npixels=0;

      // Drop 8-bit pixels in the upper part
      ptr=pix;

      // Decompress the file, continuing until you see the GIF EOF code.
      // One obvious enhancement is to add checking for corrupt files here.
      while(1){

        // Fetch the next code from the raster data stream.  The codes can be
        // any length from 3 to 12 bits, packed into 8-bit bytes, so we have to
        // maintain our location in the source array as a BIT Offset.  We compute
        // the byte Offset into the raster array by dividing this by 8, pick up
        // three bytes, compute the bit Offset into our 24-bit chunk, shift to
        // bring the desired code to the bottom, then mask it off and return it.
        ByteOffset=BitOffset>>3;
        Code=(FXuint)buf[ByteOffset]+(((FXuint)buf[ByteOffset+1])<<8)+(((FXuint)buf[ByteOffset+2])<<16);
        Code>>=(BitOffset&7);
        BitOffset+=CodeSize;
        Code&=ReadMask;

        // Are we done?
        if(Code==EOFCode || npixels>=maxpixels) break;

        // Clear code sets everything back to its initial value, then reads the
        // immediately subsequent code as uncompressed data.
        if(Code==ClearCode){
          CodeSize=InitCodeSize;
          MaxCode=1<<CodeSize;
          ReadMask=MaxCode-1;
          FreeCode=FirstFree;

          // Get next code
          ByteOffset=BitOffset>>3;
          Code=(FXuint)buf[ByteOffset]+(((FXuint)buf[ByteOffset+1])<<8)+(((FXuint)buf[ByteOffset+2])<<16);
          Code>>=(BitOffset&7);
          BitOffset+=CodeSize;
          Code&=ReadMask;

          CurCode=OldCode=Code;
          FinChar=CurCode&BitMask;

          if(!interlace){
            *ptr++=FinChar;
            }
          else{
            FXASSERT(0<=YC && YC<imheight);
            FXASSERT(0<=XC && XC<imwidth);
            ptr[YC*imwidth+XC]=FinChar;
            XC+=1;
            if(XC>=imwidth){
              XC=0;
              YC+=Yinc[Pass];
              if(YC>=imheight){
                Pass++;
                YC=Yinit[Pass&3];
                }
              }
            }
          npixels++;
          }

        // If not a clear code, must be data: save same as CurCode and InCode
        else{

          // If we're at maxcode and didn't get a clear, stop loading
          if(FreeCode>=4096){ FXFREE(&data); return false; }

          CurCode=InCode=Code;

          // If greater or equal to FreeCode, not in the hash table yet; repeat the last character decoded
          if(CurCode>=FreeCode){
            CurCode=OldCode;
            if(OutCount>4096){ FXFREE(&data); return false; }
            OutCode[OutCount++]=FinChar;
            }

          // Unless this code is raw data, pursue the chain pointed to by CurCode
          // through the hash table to its end; each code in the chain puts its
          // associated output code on the output queue.
          while(CurCode>=ClearCode){
            if(OutCount>4096 || CurCode>=FreeCode){ FXFREE(&data); return false; }
            OutCode[OutCount++]=Suffix[CurCode];
            CurCode=Prefix[CurCode];
            }

          if(OutCount>4096){ FXFREE(&data); return false; }

          // The last code in the chain is treated as raw data
          FinChar=CurCode&BitMask;
          OutCode[OutCount++]=FinChar;

          // Now we put the data out to the Output routine.
          // It's been stacked LIFO, so deal with it that way...

          // safety thing: prevent exceeding range
          if(npixels+OutCount>maxpixels) OutCount=maxpixels-npixels;

          npixels+=OutCount;
          if(!interlace){
            for(i=OutCount-1; i>=0; i--){
              *ptr++=OutCode[i];
              }
            }
          else{
            for(i=OutCount-1; i>=0; i--){
              FXASSERT(0<=YC && YC<imheight);
              FXASSERT(0<=XC && XC<imwidth);
              ptr[YC*imwidth+XC]=OutCode[i];
              XC+=1;
              if(XC>=imwidth){
                XC=0;
                YC+=Yinc[Pass];
                if(YC>=imheight){
                  Pass++;
                  YC=Yinit[Pass&3];
                  }
                }
              }
            }
          OutCount=0;

          // Build the hash table on-the-fly. No table is stored in the file
          Prefix[FreeCode]=OldCode;
          Suffix[FreeCode]=FinChar;
          OldCode=InCode;

          // Point to the next slot in the table.  If we exceed the current
          // MaxCode value, increment the code size unless it's already 12.  If it
          // is, do nothing: the next code decompressed better be CLEAR
          FreeCode++;
          if(FreeCode>=MaxCode){
            if(CodeSize<12){
              CodeSize++;
              MaxCode*=2;
              ReadMask=(1<<CodeSize)-1;
              }
            }
          }
        }

      // Did the stream stop prematurely?
      if(npixels!=maxpixels){
        fxwarning("fxloadGIF: image truncated\n");
        }

      width=imwidth;
      height=imheight;

      // Technically, this is incorrect; but we have so
      // many GIF87a's that we have to keep doing this!
      colormap[alpha]&=FXRGBA(255,255,255,0);

      // Apply colormap
      for(i=0; i<maxpixels; i++){
        data[i]=colormap[pix[i]];
        }

      // Skip image terminator to fully read all bytes
      store >> c1;

      return true;
      }

    // Non of the above, we fail!
    return false;
    }

  // Shouldn't get here, but to satisfy compiler
  return false;
  }


/*******************************************************************************/


// Save a gif file to a stream
bool fxsaveGIF(FXStream& store,const FXColor *data,FXint width,FXint height,bool fast){
  FXuint   clearcode,endcode,freecode,findcode,prefix,current,outaccu,initcodesize,codesize,hash,step;
  FXint    maxpixels,ncolors,bitsperpixel,colormapsize,outbits,src,dst,i;
  FXuchar  c1,c2,alpha,*pixels,*output;
  FXColor  colormap[256];
  FXuint   hashtab[5003];
  FXushort codetab[5003];

  // Must make sense
  if(!data || width<=0 || height<=0) return false;

  // How many pixels
  maxpixels=width*height;

  // Allocate temp buffer for pixels
  if(!FXMALLOC(&output,FXuchar,(maxpixels<<1))) return false;
  pixels=output+maxpixels;

  // First, try EZ quantization, because it is exact; a previously
  // loaded GIF will be re-saved with exactly the same colors.
  if(!fxezquantize(pixels,data,colormap,ncolors,width,height,256)){
    if(fast){
      fxfsquantize(pixels,data,colormap,ncolors,width,height,256);
      }
    else{
      fxwuquantize(pixels,data,colormap,ncolors,width,height,256);
      }
    }

  // File signature
  store << TAG_SIG1;
  store << TAG_SIG2;
  store << TAG_SIG3;

  // File version
  store << TAG_VER;
  store << TAG_NEW;
  store << TAG_SUF;

  // Figure out bits per pixel
  for(bitsperpixel=1; ncolors>(1<<bitsperpixel); bitsperpixel++);

  // Colormap size
  colormapsize=1<<bitsperpixel;

  // Screen header
  c1=width;
  c2=width>>8;
  store << c1 << c2;            // Width
  c1=height;
  c2=height>>8;
  store << c1 << c2;            // Height
  c1=0x80;                      // There is a color map
  c1|=(bitsperpixel-1)<<4;      // Number of bits of color resolution
  c1|=(bitsperpixel-1);         // The size (in bits) of the colormap
  store << c1;                  // Flags
  store << TAG_ZERO;            // Background color
  store << TAG_ZERO;            // Aspect Ratio is none

  // Output colormap
  for(i=0; i<colormapsize; i++){
    store << ((FXuchar*)(colormap+i))[0]; // Blue
    store << ((FXuchar*)(colormap+i))[1]; // Green
    store << ((FXuchar*)(colormap+i))[2]; // Red
    }

  // Output Graphics Control Extension, if alpha is present
  for(i=0,alpha=0; i<ncolors; i++){
    if(((FXuchar*)(colormap+i))[3]==0){
      alpha=i;
      store << TAG_EXTENSION;   // Extension Introducer
      store << TAG_GRAPHIC;     // Graphic Control Label
      store << TAG_GRAPHICSIZE; // Block Size
      store << TAG_TRANSPARENT; // Disposal Method
      store << TAG_ZERO;        // Delay Time
      store << TAG_ZERO;
      store << alpha;           // Transparent color index
      store << TAG_TERMINATOR;  // Block Terminator
      break;
      }
    }

  // Image descriptor
  store << TAG_IMAGE;           // Image separator
  store << TAG_ZERO;            // Image offset X
  store << TAG_ZERO;
  store << TAG_ZERO;            // Image offset Y
  store << TAG_ZERO;
  c1=width;
  c2=width>>8;
  store << c1 << c2;            // Width
  c1=height;
  c2=height>>8;
  store << c1 << c2;            // Height
  store << TAG_IMAGEFLAGS;      // Flags: no local map, no interlace

  // Figure out code size and stuff
  initcodesize=(bitsperpixel<=1)?2:bitsperpixel;
  codesize=initcodesize+1;
  clearcode=1<<(codesize-1);
  endcode=clearcode+1;

  // Now for the beef...
  c1=initcodesize;
  store << c1;                          // Write the Code size

  // Clear hash table
  memset(hashtab,0xff,sizeof(hashtab));
  freecode=clearcode+2;

  // Output clear code
  FXASSERT(clearcode<(1u<<codesize));
  outaccu=clearcode;
  outbits=codesize;

  // Compress image
  src=dst=0;
  prefix=pixels[src++];
  while(1){

    // Flush filled out bytes
    while(outbits>=8){
      output[dst++]=(FXuchar)outaccu;
      outaccu>>=8;
      outbits-=8;
      }

    // Done yet
    if(src>=maxpixels) break;

    // Get next pixel
    current=pixels[src++];

    // Check if in hash table
    findcode=(current<<12)+prefix;
    hash=findcode%5003;                 // 0<=hash<=5002
    step=findcode%4999+1;               // 1<=step<=4999
    while(hashtab[hash]!=0xffffffff){   // Occupied slot?
      if(hashtab[hash]==findcode){      // Existing prefix
        prefix=codetab[hash];           // Code for prefix
        goto nxt;
        }
      hash=(hash+step)%5003;
      }

    // Output prefix code
    FXASSERT(prefix<(1u<<codesize));
    FXASSERT(outbits+codesize<=32);
    outaccu|=prefix<<outbits;
    outbits+=codesize;

    // New prefix code
    prefix=current;

    // If still room, enter into hash table
    if(freecode<4096){                  // Add to hash table
      if(freecode>=(1u<<codesize) && codesize<12u) codesize++;
      codetab[hash]=freecode++;
      hashtab[hash]=findcode;
      }

    // Else issue clear code
    else{
      FXASSERT(clearcode<(1u<<codesize));
      FXASSERT(outbits+codesize<=32);
      outaccu|=clearcode<<outbits;
      outbits+=codesize;

      // Clear hash table
      memset(hashtab,0xff,sizeof(hashtab));
      freecode=clearcode+2;
      codesize=initcodesize+1;
      }

    // Next pixel
nxt:continue;
    }

  // Output final prefix code
  FXASSERT(prefix<(1u<<codesize));
  FXASSERT(outbits+codesize<=32);
  outaccu|=prefix<<outbits;
  outbits+=codesize;

  // Output end code
  FXASSERT(endcode<(1u<<codesize));
  FXASSERT(outbits+codesize<=32);
  outaccu|=endcode<<outbits;
  outbits+=codesize;

  // FLush remaining bits out
  while(outbits>0){
    output[dst++]=(FXuchar)outaccu;
    outaccu>>=8;
    outbits-=8;
    }

  // Write blocks
  for(src=0; src<dst; src+=c1){
    c1=FXMIN(255,(dst-src));
    store << c1;
    store.save(&output[src],c1);
    }

  // Trailer
  store << TAG_TERMINATOR;      // Block terminator
  store << TAG_ENDFILE;         // File terminator

  // Free storage
  FXFREE(&output);
  return true;
  }


}

