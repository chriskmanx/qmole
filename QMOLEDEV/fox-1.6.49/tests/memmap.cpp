/********************************************************************************
*                                                                               *
*                          M e m o r y   M a p   T e s t                        *
*                                                                               *
*********************************************************************************
* Copyright (C) 2004,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
*********************************************************************************
* $Id: memmap.cpp,v 1.5 2006/01/22 17:59:02 fox Exp $                           *
********************************************************************************/
#include "fx.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "FXMemMap.h"

/*
  Notes:

  - Test FXMemMap capabilities.

*/

/*******************************************************************************/

enum ACTION {
  TEST_NONE,
  TEST_READ,
  TEST_WRITE
  };


void printusage(){
  fprintf(stderr,"Usage: \n\n");
  fprintf(stderr,"   memmap read <length> <filename>\n\n");
  fprintf(stderr," or\n\n");
  fprintf(stderr,"   memmap write <length> <filename>\n");
  }


// Start the whole thing
int main(int argc,char** argv){
  FXString filename=FXString::null;
  ACTION action=TEST_READ;
  long index;
  long length=0;
  long offset=0;
  char* base;

  // What to test
  FXMemMap map;

  // Show how to use
  if(argc<4){
    printusage();
    exit(0);
    }

  // Get action
  if(strcmp(argv[1],"read")==0)
    action=TEST_READ;
  else if(strcmp(argv[1],"write")==0)
    action=TEST_WRITE;
  else{
    printusage();
    exit(0);
    }

  // Get size
  if(sscanf(argv[2],"%ld",&length)<1){
    printusage();
    exit(0);
    }

  // Get filename
  filename=argv[3];

  // Show process id
  fprintf(stderr,"Process id=%d\n",fxgetpid());

  // Test reading
  if(action==TEST_READ){

    // Wait for return
    fprintf(stderr,"Press return to map:");
    getchar();

    // Map
    base=(char*)map.mapFile(filename,offset,length,FXMemMap::READ,FXMemMap::SHAR);
    if(base==NULL){
      fprintf(stderr,"mapFile returned NULL\n");
      exit(1);
      }

    fprintf(stderr,"actual length = %ld\n",map.length());

    // Touch it
    for(index=0; index<map.length(); index++){
      if(base[index]!='Z') fprintf(stderr,"base[%ld]=%d\n",index,base[index]);
      }

    // Wait for return
    fprintf(stderr,"Press return to unmap:");
    getchar();

    // Unmap
    map.unmap();

    // Wait for return
    fprintf(stderr,"Press return to quit:");
    getchar();
    }

  // Test writing
  else if(action==TEST_WRITE){

    // Wait for return
    fprintf(stderr,"Press return to map:");
    getchar();

    // Map
    base=(char*)map.mapFile(filename,offset,length,FXMemMap::READ|FXMemMap::WRITE|FXMemMap::TRUNC,FXMemMap::SHAR);
    if(base==NULL){
      fprintf(stderr,"mapFile returned NULL\n");
      exit(1);
      }

    fprintf(stderr,"actual length = %ld\n",map.length());

    // Write it
    for(index=0; index<map.length(); index++){
      base[index]='Z';
      }

    // Wait for return
    fprintf(stderr,"Press return to unmap:");
    getchar();

    // Unmap
    map.unmap();

    // Wait for return
    fprintf(stderr,"Press return to quit:");
    getchar();
    }

  return 1;
  }


