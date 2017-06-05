/********************************************************************************
*                                                                               *
*                          U t i l i t y   F u n c t i o n s                    *
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
* $Id: fxutils.cpp,v 1.129.2.4 2008/05/20 16:21:12 fox Exp $                        *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "fxascii.h"
#include "fxkeys.h"
#include "FXHash.h"
#include "FXStream.h"
#include "FXString.h"


/*
  Notes:
  - Handy global utility functions.
*/


// Make it larger if you need
#ifndef MAXMESSAGESIZE
#define MAXMESSAGESIZE 1024
#endif


using namespace FX;

// Allows GNU autoconfigure to find FOX
extern "C" FXAPI void fxfindfox(void){ }


/*******************************************************************************/

namespace FX {

// Global flag which controls tracing level
unsigned int fxTraceLevel=0;


// Version number that the library has been compiled with
const FXuchar fxversion[3]={FOX_MAJOR,FOX_MINOR,FOX_LEVEL};


// Thread-safe, linear congruential random number generator from Knuth & Lewis.
FXuint fxrandom(FXuint& seed){
  seed=1664525UL*seed+1013904223UL;
  return seed;
  }


#ifdef WIN32

// Return TRUE if console application
FXbool fxisconsole(const FXchar *path){
  IMAGE_OPTIONAL_HEADER optional_header;
  IMAGE_FILE_HEADER     file_header;
  IMAGE_DOS_HEADER      dos_header;
  DWORD                 dwCoffHeaderOffset;
  DWORD                 dwNewOffset;
  DWORD                 dwMoreDosHeader[16];
  ULONG                 ulNTSignature;
  HANDLE                hImage;
  DWORD                 dwBytes;
  FXbool                flag=MAYBE;

  // Open the application file.
  hImage=CreateFileA(path,GENERIC_READ,FILE_SHARE_READ,NULL,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,NULL);
  if(hImage!=INVALID_HANDLE_VALUE){

    // Read MS-Dos image header.
    if(ReadFile(hImage,&dos_header,sizeof(IMAGE_DOS_HEADER),&dwBytes,NULL)==0) goto x;

    // Test bytes read
    if(dwBytes!=sizeof(IMAGE_DOS_HEADER)) goto x;

    // Test signature
    if(dos_header.e_magic!=IMAGE_DOS_SIGNATURE) goto x;

    // Read more MS-Dos header.
    if(ReadFile(hImage,dwMoreDosHeader,sizeof(dwMoreDosHeader),&dwBytes,NULL)==0) goto x;

    // Test bytes read
    if(dwBytes!=sizeof(dwMoreDosHeader)) goto x;

    // Move the file pointer to get the actual COFF header.
    dwNewOffset=SetFilePointer(hImage,dos_header.e_lfanew,NULL,FILE_BEGIN);
    dwCoffHeaderOffset=dwNewOffset+sizeof(ULONG);
    if(dwCoffHeaderOffset==0xFFFFFFFF) goto x;

    // Read NT signature of the file.
    if(ReadFile(hImage,&ulNTSignature,sizeof(ULONG),&dwBytes,NULL)==0) goto x;

    // Test bytes read
    if(dwBytes!=sizeof(ULONG)) goto x;

    // Test NT signature
    if(ulNTSignature!=IMAGE_NT_SIGNATURE) goto x;

    if(ReadFile(hImage,&file_header,IMAGE_SIZEOF_FILE_HEADER,&dwBytes,NULL)==0) goto x;

    // Test bytes read
    if(dwBytes!=IMAGE_SIZEOF_FILE_HEADER) goto x;

    // Read the optional header of file.
    if(ReadFile(hImage,&optional_header,sizeof(IMAGE_OPTIONAL_HEADER),&dwBytes,NULL)==0) goto x;

    // Test bytes read
    if(dwBytes!=sizeof(IMAGE_OPTIONAL_HEADER)) goto x;

    // Switch on systems
    switch(optional_header.Subsystem){
      case IMAGE_SUBSYSTEM_WINDOWS_GUI:     // Windows GUI (2)
      case IMAGE_SUBSYSTEM_WINDOWS_CE_GUI:  // Windows CE GUI (9)
        flag=FALSE;
        break;
      case IMAGE_SUBSYSTEM_WINDOWS_CUI:     // Windows Console (3)
      case IMAGE_SUBSYSTEM_OS2_CUI:         // OS/2 Console (5)
      case IMAGE_SUBSYSTEM_POSIX_CUI:       // Posix Console (7)
        flag=TRUE;
        break;
      case IMAGE_SUBSYSTEM_NATIVE:          // Native (1)
      case IMAGE_SUBSYSTEM_NATIVE_WINDOWS:  // Native Win9x (8)
      case IMAGE_SUBSYSTEM_UNKNOWN:         // Unknown (0)
      default:
        break;
      }
x:  CloseHandle(hImage);
    }
  return flag;
  }


#else

// Return TRUE if console application
FXbool fxisconsole(const FXchar*){
  return TRUE;
  }

#endif


// Assert failed routine
void fxassert(const char* expression,const char* filename,unsigned int lineno){
#ifndef WIN32
  fprintf(stderr,"%s:%d: FXASSERT(%s) failed.\n",filename,lineno,expression);
  fflush(stderr);
#else
#ifdef _WINDOWS
  char msg[MAXMESSAGESIZE];
  sprintf(msg,"%s(%d): FXASSERT(%s) failed.\n",filename,lineno,expression);
  OutputDebugStringA(msg);
  fprintf(stderr,"%s",msg); // if a console is available
  fflush(stderr);
#else
  fprintf(stderr,"%s(%d): FXASSERT(%s) failed.\n",filename,lineno,expression);
  fflush(stderr);
#endif
#endif
  }


// Log message to [typically] stderr
void fxmessage(const char* format,...){
#ifndef WIN32
  va_list arguments;
  va_start(arguments,format);
  vfprintf(stderr,format,arguments);
  fflush(stderr);
  va_end(arguments);
#else
#ifdef _WINDOWS
  char msg[MAXMESSAGESIZE];
  va_list arguments;
  va_start(arguments,format);
  vsnprintf(msg,sizeof(msg),format,arguments);
  va_end(arguments);
  OutputDebugStringA(msg);
  fprintf(stderr,"%s",msg); // if a console is available
  fflush(stderr);
#else
  va_list arguments;
  va_start(arguments,format);
  vfprintf(stderr,format,arguments);
  fflush(stderr);
  va_end(arguments);
#endif
#endif
  }


// Error routine
void fxerror(const char* format,...){
#ifndef WIN32
  va_list arguments;
  va_start(arguments,format);
  vfprintf(stderr,format,arguments);
  fflush(stderr);
  va_end(arguments);
  abort();
#else
#ifdef _WINDOWS
  char msg[MAXMESSAGESIZE];
  va_list arguments;
  va_start(arguments,format);
  vsnprintf(msg,sizeof(msg),format,arguments);
  va_end(arguments);
  OutputDebugStringA(msg);
  fprintf(stderr,"%s",msg); // if a console is available
  fflush(stderr);
  MessageBoxA(NULL,msg,NULL,MB_OK|MB_ICONEXCLAMATION|MB_APPLMODAL);
  DebugBreak();
#else
  va_list arguments;
  va_start(arguments,format);
  vfprintf(stderr,format,arguments);
  fflush(stderr);
  va_end(arguments);
  abort();
#endif
#endif
  }


// Warning routine
void fxwarning(const char* format,...){
#ifndef WIN32
  va_list arguments;
  va_start(arguments,format);
  vfprintf(stderr,format,arguments);
  fflush(stderr);
  va_end(arguments);
#else
#ifdef _WINDOWS
  char msg[MAXMESSAGESIZE];
  va_list arguments;
  va_start(arguments,format);
  vsnprintf(msg,sizeof(msg),format,arguments);
  va_end(arguments);
  OutputDebugStringA(msg);
  fprintf(stderr,"%s",msg); // if a console is available
  fflush(stderr);
  MessageBoxA(NULL,msg,NULL,MB_OK|MB_ICONINFORMATION|MB_APPLMODAL);
#else
  va_list arguments;
  va_start(arguments,format);
  vfprintf(stderr,format,arguments);
  fflush(stderr);
  va_end(arguments);
#endif
#endif
  }


// Trace printout routine
void fxtrace(unsigned int level,const char* format,...){
  if(fxTraceLevel>level){
#ifndef WIN32
    va_list arguments;
    va_start(arguments,format);
    vfprintf(stderr,format,arguments);
    fflush(stderr);
    va_end(arguments);
#else
#ifdef _WINDOWS
    char msg[MAXMESSAGESIZE];
    va_list arguments;
    va_start(arguments,format);
    vsnprintf(msg,sizeof(msg),format,arguments);
    OutputDebugStringA(msg);
    fprintf(stderr,"%s",msg); // if a console is available
    fflush(stderr);
    va_end(arguments);
#else
    va_list arguments;
    va_start(arguments,format);
    vfprintf(stderr,format,arguments);
    fflush(stderr);
    va_end(arguments);
#endif
#endif
    }
  }


// Sleep n microseconds
void fxsleep(unsigned int n){
#ifdef WIN32
  unsigned int zzz=n/1000;
  if(zzz==0) zzz=1;
  Sleep(zzz);
#else
#ifdef __USE_POSIX199309
  struct timespec value;
  value.tv_nsec = 1000 * (n%1000000);
  value.tv_sec = n/1000000;
  nanosleep(&value,NULL);
#else
#ifndef BROKEN_SELECT
  struct timeval value;
  value.tv_usec = n % 1000000;
  value.tv_sec = n / 1000000;
  select(1,0,0,0,&value);
#else
  unsigned int zzz=n/1000000;
  if(zzz==0) zzz=1;
  if(zzz){
    while((zzz=sleep(zzz))>0) ;
    }
#endif
#endif
#endif
  }


// Get highlight color
FXColor makeHiliteColor(FXColor clr){
  FXuint r,g,b;
  r=FXREDVAL(clr);
  g=FXGREENVAL(clr);
  b=FXBLUEVAL(clr);
  r=FXMAX(31,r);
  g=FXMAX(31,g);
  b=FXMAX(31,b);
  r=(133*r)/100;
  g=(133*g)/100;
  b=(133*b)/100;
  r=FXMIN(255,r);
  g=FXMIN(255,g);
  b=FXMIN(255,b);
  return FXRGB(r,g,b);
  }


// Get shadow color
FXColor makeShadowColor(FXColor clr){
  FXuint r,g,b;
  r=FXREDVAL(clr);
  g=FXGREENVAL(clr);
  b=FXBLUEVAL(clr);
  r=(66*r)/100;
  g=(66*g)/100;
  b=(66*b)/100;
  return FXRGB(r,g,b);
  }


/*******************************************************************************/

// Convert string of length len to MSDOS; return new string and new length
bool fxtoDOS(FXchar*& string,FXint& len){
  register FXint f=0,t=0;
  while(f<len && string[f]!='\0'){
    if(string[f++]=='\n') t++; t++;
    }
  len=t;
  if(!FXRESIZE(&string,FXchar,len+1)) return false;
  while(0<t){
    if((string[--t]=string[--f])=='\n') string[--t]='\r';
    }
  string[len]='\0';
  return true;
  }


// Convert string of length len from MSDOS; return new string and new length
bool fxfromDOS(FXchar*& string,FXint& len){
  register FXint f=0,t=0,c;
  while(f<len && string[f]!='\0'){
    if((c=string[f++])!='\r') string[t++]=c;
    }
  len=t;
  if(!FXRESIZE(&string,FXchar,len+1)) return false;
  string[len]='\0';
  return true;
  }


// Get process id
FXint fxgetpid(){
#ifndef WIN32
  return getpid();
#else
  return (int)GetCurrentProcessId();
#endif
  }


/*******************************************************************************/

// Convert RGB to HSV
void fxrgb_to_hsv(FXfloat& h,FXfloat& s,FXfloat& v,FXfloat r,FXfloat g,FXfloat b){
  FXfloat t,delta;
  v=FXMAX3(r,g,b);
  t=FXMIN3(r,g,b);
  delta=v-t;
  if(v!=0.0f)
    s=delta/v;
  else
    s=0.0f;
  if(s==0.0f){
    h=0.0f;
    }
  else{
    if(r==v)
      h=(g-b)/delta;
    else if(g==v)
      h=2.0f+(b-r)/delta;
    else if(b==v)
      h=4.0f+(r-g)/delta;
    h=h*60.0f;
    if(h<0.0f) h=h+360.0f;
    }
  }


// Convert to RGB
void fxhsv_to_rgb(FXfloat& r,FXfloat& g,FXfloat& b,FXfloat h,FXfloat s,FXfloat v){
  FXfloat f,w,q,t;
  FXint i;
  if(s==0.0f){
    r=v;
    g=v;
    b=v;
    }
  else{
    if(h==360.0f) h=0.0f;
    h=h/60.0f;
    i=(FXint)h;
    f=h-i;
    w=v*(1.0f-s);
    q=v*(1.0f-(s*f));
    t=v*(1.0f-(s*(1.0f-f)));
    switch(i){
      case 0: r=v; g=t; b=w; break;
      case 1: r=q; g=v; b=w; break;
      case 2: r=w; g=v; b=t; break;
      case 3: r=w; g=q; b=v; break;
      case 4: r=t; g=w; b=v; break;
      case 5: r=v; g=w; b=q; break;
      }
    }
  }


// Calculate a hash value from a string; algorithm same as in perl
FXuint fxstrhash(const FXchar* str){
  register const FXuchar *s=(const FXuchar*)str;
  register FXuint h=0;
  register FXuint c;
  while((c=*s++)!='\0'){
    h = ((h << 5) + h) ^ c;
    }
  return h;
  }


/*******************************************************************************/


// Classify IEEE 754 floating point number
//
//            31 30           23 22             0
// +------------+---------------+---------------+
// | s[31] 1bit | e[30:23] 8bit | f[22:0] 23bit |
// +------------+---------------+---------------+
//
FXint fxieeefloatclass(FXfloat number){
  FXfloat num=number;
  FXASSERT(sizeof(FXfloat)==sizeof(FXuint));
  FXuint s=(*((FXuint*)&num)&0x80000000);        // Sign
  FXuint e=(*((FXuint*)&num)&0x7f800000);        // Exponent
  FXuint m=(*((FXuint*)&num)&0x007fffff);        // Mantissa
  FXint result=0;
  if(e==0x7f800000){
    if(m==0)
      result=1;     // Inf
    else
      result=2;     // NaN
    if(s)
      result=-result;
    }
  return result;
  }


// Classify IEEE 754 floating point number
//
//            63 62            52 51            32 31             0
// +------------+----------------+----------------+---------------+
// | s[63] 1bit | e[62:52] 11bit | f[51:32] 20bit | f[31:0] 32bit |
// +------------+----------------+----------------+---------------+
//
FXint fxieeedoubleclass(FXdouble number){
  FXdouble num=number;
  FXASSERT(sizeof(FXdouble)==2*sizeof(FXuint));
#if FOX_BIGENDIAN
  FXuint s=(((FXuint*)&num)[0]&0x80000000);     // Sign
  FXuint e=(((FXuint*)&num)[0]&0x7ff00000);     // Exponent
  FXuint h=(((FXuint*)&num)[0]&0x000fffff);     // Mantissa high
  FXuint l=(((FXuint*)&num)[1]);                // Mantissa low
#else
  FXuint s=(((FXuint*)&num)[1]&0x80000000);     // Sign
  FXuint e=(((FXuint*)&num)[1]&0x7ff00000);     // Exponent
  FXuint h=(((FXuint*)&num)[1]&0x000fffff);     // Mantissa high
  FXuint l=(((FXuint*)&num)[0]);                // Mantissa low
#endif
  FXint result=0;
  if(e==0x7ff00000){
    if(h==0 && l==0)
      result=1;     // Inf
    else
      result=2;     // NaN
    if(s)
      result=-result;
    }
  return result;
  }


/*******************************************************************************/




#if defined(__GNUC__) && defined(__linux__) && defined(__i386__)

extern FXAPI FXuint fxcpuid();

// Capabilities
#define CPU_HAS_TSC             0x001
#define CPU_HAS_MMX             0x002
#define CPU_HAS_MMXEX           0x004
#define CPU_HAS_SSE             0x008
#define CPU_HAS_SSE2            0x010
#define CPU_HAS_3DNOW           0x020
#define CPU_HAS_3DNOWEXT        0x040
#define CPU_HAS_SSE3            0x080
#define CPU_HAS_HT              0x100


// The CPUID instruction returns stuff in eax, ecx, edx.
#define cpuid(op,eax,ecx,edx)	\
  asm volatile ("pushl %%ebx \n\t"    	\
                "cpuid       \n\t"    	\
                "popl  %%ebx \n\t"      \
                : "=a" (eax),		\
                  "=c" (ecx),           \
                  "=d" (edx)            \
                : "a" (op)              \
                : "cc")


/*
* Find out some useful stuff about the CPU we're running on.
* We don't care about everything, but just MMX, XMMS, SSE, SSE2, 3DNOW, 3DNOWEXT,
* for the obvious reasons.
* If we're generating for Pentium or above then assume CPUID is present; otherwise,
* test if CPUID is present first using the recommended code...
*/
FXuint fxcpuid(){
  FXuint eax, ecx, edx, caps;

  // Generating code for pentium or better :- don't bother checking for CPUID presence.
#if !(defined(__i586__) || defined(__i686__) || defined(__athlon__) || defined(__pentium4__) || defined(__x86_64__))

  // If EFLAGS bit 21 can be changed, we have CPUID capability.
  asm volatile ("pushfl                 \n\t"
                "popl   %0              \n\t"
                "movl   %0,%1           \n\t"
                "xorl   $0x200000,%0    \n\t"
                "pushl  %0              \n\t"
                "popfl                  \n\t"
                "pushfl                 \n\t"
                "popl   %0              \n\t"
                : "=a" (eax),
                  "=d" (edx)
                :
                : "cc");

  // Yes, we have no CPUID!
  if(eax==edx) return 0;
#endif

  // Capabilities
  caps=0;

  // Get vendor string; this also returns the highest CPUID code in eax.
  // If highest CPUID code is zero, we can't call any other CPUID functions.
  cpuid(0x00000000,eax,ecx,edx);
  if(eax){

    // AMD:   ebx="Auth" edx="enti" ecx="cAMD",
    // Intel: ebx="Genu" edx="ineI" ecx="ntel"
    // VIAC3: ebx="Cent" edx="aurH" ecx="auls"

    // Test for AMD
    if((ecx==0x444d4163) && (edx==0x69746e65)){

      // Any extended capabilities; this returns highest extended CPUID code in eax.
      cpuid(0x80000000,eax,ecx,edx);
      if(eax>0x80000000){

        // Test extended athlon capabilities
        cpuid(0x80000001,eax,ecx,edx);
        if(edx&0x08000000) caps|=CPU_HAS_MMXEX;
        if(edx&0x80000000) caps|=CPU_HAS_3DNOW;
        if(edx&0x40000000) caps|=CPU_HAS_3DNOWEXT;
        }
      }

    // Standard CPUID code 1.
    cpuid(0x00000001,eax,ecx,edx);
    if(edx&0x00000010) caps|=CPU_HAS_TSC;
    if(edx&0x00800000) caps|=CPU_HAS_MMX;
    if(edx&0x02000000) caps|=CPU_HAS_SSE;
    if(edx&0x04000000) caps|=CPU_HAS_SSE2;
    if(edx&0x10000000) caps|=CPU_HAS_HT;
    if(ecx&0x00000001) caps|=CPU_HAS_SSE3;
    }

  // Return capabilities
  return caps;
  }
#endif




/*
* Return clock ticks from x86 TSC register [GCC/ICC x86 version].
*/
#if (defined(__GNUC__) || defined(__ICC)) && defined(__linux__) && defined(__i386__)
extern FXAPI FXlong fxgetticks();
FXlong fxgetticks(){
  FXlong value;
  asm volatile ("rdtsc" : "=A" (value));
  return value;
  }
#endif


/*
* Return clock ticks from performance counter [GCC AMD64 version].
*/
#if defined(__GNUC__) && defined(__linux__) && defined(__x86_64__)
extern FXAPI FXlong fxgetticks();
FXlong fxgetticks(){
  register FXuint a,d;
  asm volatile("rdtsc" : "=a" (a), "=d" (d));
//asm volatile("rdtscp" : "=a" (a), "=d" (d): : "%ecx");        // Serializing version (%ecx has processor id)
  return ((FXulong)a) | (((FXulong)d)<<32);
  }
#endif


/*
* Return clock ticks from performance counter [GCC IA64 version].
*/
#if !defined(__INTEL_COMPILER) && defined(__GNUC__) && defined(__linux__) && defined(__ia64__)
extern FXAPI FXlong fxgetticks();
FXlong fxgetticks(){
  FXlong value;
  asm volatile ("mov %0=ar.itc" : "=r"(value));         // FIXME not tested!
  return value;
  }
#endif


/*
* Return clock ticks from performance counter [GCC PPC version].
*/
#if defined(__GNUC__) && ( defined(__powerpc__) || defined(__ppc__) )
extern FXAPI FXlong fxgetticks();
FXlong fxgetticks(){
  unsigned int tbl, tbu0, tbu1;
  do{
    asm volatile("mftbu %0" : "=r"(tbu0));
    asm volatile("mftb %0" : "=r"(tbl));
    asm volatile("mftbu %0" : "=r"(tbu1));
    }
  while(tbu0!=tbu1);
  return (((unsigned long long)tbu0) << 32) | tbl;
  }
#endif


/*
* Return clock ticks from performance counter [GCC PA-RISC version].
*/
#if defined(__GNUC__) && (defined(__hppa__) || defined(__hppa))
extern FXAPI FXlong fxgetticks();
FXlong fxgetticks(){
  FXlong value;
  asm volatile("mfctl 16, %0": "=r" (value));             // FIXME not tested!
  return value;
  }
#endif


/*
* Return clock ticks from performance counter [GCC SPARC V9 version].
*/
#if defined(__GNUC__) && defined(__sparc_v9__)
extern FXAPI FXlong fxgetticks();
FXlong fxgetticks(){
  FXlong value;
  __asm__ __volatile__("rd %%tick, %0" : "=r" (value)); // FIXME not tested!
  return value;
  }
#endif


/*
* Return clock ticks from performance counter [WIN32 version].
*/
#ifdef WIN32
extern FXAPI FXlong fxgetticks();
FXlong fxgetticks(){
  FXlong value;
  QueryPerformanceCounter((LARGE_INTEGER*)&value);
  return value;
  }
#endif


}

