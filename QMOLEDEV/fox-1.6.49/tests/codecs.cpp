/********************************************************************************
*                                                                               *
*                                 Codecs Tests                                  *
*                                                                               *
*********************************************************************************
* Copyright (C) 2005,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
*********************************************************************************
* $Id: codecs.cpp,v 1.23 2006/01/22 17:58:59 fox Exp $                          *
********************************************************************************/
#include "fx.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "FXKOI8RCodec.h"
#include "FX88592Codec.h"
#include "FXUTF16Codec.h"
#include "FXUTF32Codec.h"
#include "FXUTF8Codec.h"
/*


*/

/*******************************************************************************/

FXKOI8RCodec   koi8_r;
FX88592Codec   iso8859_2;

FXUTF16Codec   utf16;
FXUTF16BECodec utf16be;
FXUTF16LECodec utf16le;

FXUTF8Codec    utf8;

FXUTF32Codec   utf32;
FXUTF32BECodec utf32be;
FXUTF32LECodec utf32le;



// UTF8 string of 1, 2, 3, 4, 5, and 6 bytes
const FXchar utfteststring[]="\x7f\xdf\xbf\xef\xbf\xbf\xf7\xbf\xbf\xbf\xfb\xbf\xbf\xbf\xbf\xfd\xbf\xbf\xbf\xbf\xbf";


// Test roundtrip for codec
void roundtriptest(FXTextCodec *codec){
  FXchar dst[32],src[32];
  FXint c,n,i,j;
  FXwchar wc;
  printf("Roundtrip test for: %s..\n",codec->name());
  for(i=0; i<256; i++){
    src[0]=i;
    codec->mb2wc(wc,src,1);
    codec->wc2mb(dst,32,wc);
    j=(FXuchar)dst[0];
    printf("0x%02x -> 0x%04x -> 0x%02x %s\n",i,wc,j,(i!=j)?"BAD":"");
    }
  printf("Roundtrip test done!\n");
  }


// Test utf8 roundtrip for codec
void utf8roundtriptest(FXTextCodec *codec){
  FXchar dst[32],src[32];
  FXint c,n,i,j;
  FXwchar wc;
  printf("UTF8 Roundtrip test for: %s..\n",codec->name());
  for(i=0; i<256; i++){
    src[0]=i;
    memset(dst,0,sizeof(dst));
    n=codec->mb2utf(dst,32,src,1);
    if(n<=0) printf("mb2utf(0x%02x) gave error %d\n",i,n);
    memset(src,0,sizeof(src));
    n=codec->utf2mb(src,32,dst,n);
    if(n<=0) printf("utf2mb(0x%02x) gave error %d\n",i,n);
    j=(FXuchar)src[0];
    if(i!=j) printf("0x%02x -> utf8 -> 0x%02x\n",i,j);
    }
  printf("UTF8 Roundtrip test done!\n");
  }


// Start the whole thing
int main(int,char**){
  const FXwchar *wcdec;
  FXwchar wc,ww;
  FXint i,n,dec,cc,num1,num2,numt;
  FXString value,norm;
  FXuchar buf[128];
  FXchar input[256];
//utf8codingtest();
//utf16codingtest();
  // Test codecs
//  roundtriptest(&koi8_r);
//  roundtriptest(&iso8859_2);

//  utf8roundtriptest(&koi8_r);
//  utf8roundtriptest(&iso8859_2);

/*
  wc=0x7fffffff;
  wc=0x3ffffff;
  wc=0x1fffff;
  wc=0xffff;
  wc=0x7ff;
  wc=0x7f;
  value.assign(&wc,1);
  switch(value.length()){
    case 1: printf("%06X: \\x%02x\n",wc,(FXuchar)value[0]); break;
    case 2: printf("%06X: \\x%02x\\x%02x\n",wc,(FXuchar)value[0],(FXuchar)value[1]); break;
    case 3: printf("%06X: \\x%02x\\x%02x\\x%02x\n",wc,(FXuchar)value[0],(FXuchar)value[1],(FXuchar)value[2]); break;
    case 4: printf("%06X: \\x%02x\\x%02x\\x%02x\\x%02x\n",wc,(FXuchar)value[0],(FXuchar)value[1],(FXuchar)value[2],(FXuchar)value[3]); break;
    case 5: printf("%06X: \\x%02x\\x%02x\\x%02x\\x%02x\\x%02x\n",wc,(FXuchar)value[0],(FXuchar)value[1],(FXuchar)value[2],(FXuchar)value[3],(FXuchar)value[4]); break;
    case 6: printf("%06X: \\x%02x\\x%02x\\x%02x\\x%02x\\x%02x\\x%02x\n",wc,(FXuchar)value[0],(FXuchar)value[1],(FXuchar)value[2],(FXuchar)value[3],(FXuchar)value[4],(FXuchar)value[5]); break;
    }
  value=utfteststring;
  i=0;
  printf("inc(%d)=",i); printf("%d\n",value.inc(i));
  printf("inc(%d)=",i); printf("%d\n",value.inc(i));
  printf("inc(%d)=",i); printf("%d\n",value.inc(i));
  printf("inc(%d)=",i); printf("%d\n",value.inc(i));
  printf("inc(%d)=",i); printf("%d\n",value.inc(i));
  printf("inc(%d)=",i); printf("%d\n",value.inc(i));
  printf("inc(%d)=",i); printf("%d\n",value.inc(i));
  exit(0);
*/

  while(fgets(input,sizeof(input),stdin)){
    input[strlen(input)-1]=0;
//    value=unescape(input);
    value=input;
    printf("in : \"%s\"\n",escape(value).text());
    value=fromAscii(value);
    printf("org: \"%s\"\n",escape(value).text());
    value=decompose(value,DecCanonical);
    printf("dec: \"%s\"\n",escape(value).text());
    value=toAscii(value);
    printf("out: \"%s\"\n",value.text());
    }

  exit(0);

  // Test utf
  for(wc=0; wc<0x7fffffff; wc++){
    n=wc2utfs((FXchar*)buf,&wc,1);
    utf2wcs(&ww,(const FXchar*)buf,n);
    if(wc!=ww) printf("%06X: Problem: %06X\n",wc,ww);
    }

/*
  // Test decompose table
  for(wc=num1=num2=numt=0; wc<=0x110000; wc++){
    dec=Unicode::decomposeType(wc);
    cc=Unicode::charCombining(wc);
    if(dec==DecCanonical && cc==0){
      wcdec=Unicode::charDecompose(wc);
      if(1<wcdec[-1]){
        if(wcdec[-1]==1) num1++;
        if(wcdec[-1]==2) num2++;
        numt++;
        printf("%04X: cc=%2d (%d): ",wc,cc,wcdec[-1]);
        for(i=0; i<wcdec[-1]; i++){
          printf("%04X ",wcdec[i]);
          }
        printf(" comp=%04X ",Unicode::charCompose(wcdec[0],wcdec[1]));
        printf("\n");
        }
      }
    }
  printf("num1=%d num2=%d numt=%d",num1,num2,numt);
*/
/*
  for(wc=0; wc<0x110000; wc++){
    value.assign(&wc,1);
    norm=decompose(value,1);
    printf("%04X: (%2d): ",wc,norm.length());
    for(i=0; i<value.length(); i++){
      printf("%02X ",(FXuchar)value[i]);
      //printf("%c",(FXuchar)value[i]);
      }
    printf(" -> ");
    for(i=0; i<norm.length(); i++){
      printf("%02X ",(FXuchar)norm[i]);
      //printf("%c",(FXuchar)norm[i]);
      }
    printf("\n");
    }
*/

  return 1;
  }
