/********************************************************************************
*                                                                               *
*                R e s o u r c e   W r a p p i n g   U t i l i t y              *
*                                                                               *
*********************************************************************************
* Copyright (C) 1997,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: reswrap.cpp,v 1.17 2006/01/22 17:59:02 fox Exp $                         *
********************************************************************************/
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "ctype.h"



/*

  Notes:
  - Updated to version 4.0.
  - Can now also generate output as a text string.
  - When ASCII option is used with text string option, it prints human
    readable C-style string, with non-ASCII characters escaped as appropriate.
  - Removed PPM feature as that was never really used in FOX.
  - License changed to GPL from LGPL because this is a standalone program
    that does not need to be linked to anything.
  - Added MSDOS mode, useful for wrapping ASCII text.
  - Added option to keep extension (separated by '_').
  - Need option to place #include "icons.h" or something into icons.cpp
  - Always prepend prefix in front of resource name, even if name was overrided
    because prefix may be namespace name.
*/

/*******************************************************************************/

#define MODE_DECIMAL  0
#define MODE_HEX      1
#define MODE_TEXT     2


const char version[]="4.0.0";


/* Print some help */
void printusage(){
  fprintf(stderr,"Usage: reswrap [options] [-o[a] outfile] files...\n");
  fprintf(stderr,"Convert files containing images, text, or binary data into C/C++ data arrays.\n");
  fprintf(stderr,"\n");
  fprintf(stderr,"Options:\n");
  fprintf(stderr,"  -o[a] outfile  Output [append] to outfile instead of stdout\n");
  fprintf(stderr,"  -h             Print help\n");
  fprintf(stderr,"  -v             Print version number\n");
  fprintf(stderr,"  -d             Output as decimal\n");
  fprintf(stderr,"  -m             Read files with MS-DOS mode (default is binary)\n");
  fprintf(stderr,"  -x             Output as hex (default)\n");
  fprintf(stderr,"  -t[a]          Output as [ascii] text string\n");
  fprintf(stderr,"  -e             Generate external reference declaration\n");
  fprintf(stderr,"  -i             Build an include file\n");
  fprintf(stderr,"  -k             Keep extension, separated by underscore\n");
  fprintf(stderr,"  -s             Suppress header in output file\n");
  fprintf(stderr,"  -p prefix      Place prefix in front of names of declarations and definitions\n");
  fprintf(stderr,"  -n namespace   Place declarations and definitions inside given namespace\n");
  fprintf(stderr,"  -c cols        Change number of columns in output to cols\n");
  fprintf(stderr,"  -u             Force unsigned char even for text mode\n");
  fprintf(stderr,"  -z             Output size in declarations\n");
  fprintf(stderr,"\n");
  fprintf(stderr,"Each file may be preceded by the following extra option:\n");
  fprintf(stderr,"  -r name        Override resource name of following resource file\n");
  fprintf(stderr,"\n");
  }


/* Print version information */
void printversion(){
  fprintf(stderr,"Reswrap %s %s.\n",version,__DATE__);
  fprintf(stderr,"Copyright (C) 1997,2005 Jeroen van der Zijp. All Rights Reserved.\n");
  fprintf(stderr,"Please visit: http://www.fox-toolkit.org for further information.\n");
  fprintf(stderr,"\n");
  fprintf(stderr,"This program is free software; you can redistribute it and/or modify\n");
  fprintf(stderr,"it under the terms of the GNU General Public License as published by\n");
  fprintf(stderr,"the Free Software Foundation; either version 2 of the License, or\n");
  fprintf(stderr,"(at your option) any later version.\n");
  fprintf(stderr,"\n");
  fprintf(stderr,"This program is distributed in the hope that it will be useful,\n");
  fprintf(stderr,"but WITHOUT ANY WARRANTY; without even the implied warranty of\n");
  fprintf(stderr,"MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n");
  fprintf(stderr,"GNU General Public License for more details.\n");
  fprintf(stderr,"\n");
  fprintf(stderr,"You should have received a copy of the GNU General Public License\n");
  fprintf(stderr,"along with this program; if not, write to the Free Software\n");
  fprintf(stderr,"Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.\n");
  }


/* Build resource name */
void resourcename(char *name,const char* prefix,const char* filename,int keepdot){
  const char* begin=name;
  const char* ptr;

  /* Copy prefix */
  while(*prefix){
    *name++=*prefix++;
    }

  /* Get name only; take care of mixed path separator characters on mswindows */
  if((ptr=strrchr(filename,'/'))!=0) filename=ptr;
  if((ptr=strrchr(filename,'\\'))!=0) filename=ptr;

  /* Copy filename */
  while(*filename){

    /* C++ identifier may contain _, alpha, digit (if not first), or namespace separator : */
    if(*filename==':' || *filename=='_' || isalpha(*filename) || (isdigit(*filename) && (begin!=name))){
      *name++=*filename;
      }

    /* We can squash dot extension to _ */
    else if(*filename=='.'){
      if(!keepdot) break;
      *name++='_';
      }

    filename++;
    }
  *name=0;
  }


/* Main */
int main(int argc,char **argv){
  char *outfilename,*resfilename;
  FILE *resfile,*outfile;
  int i,b,first,col,append,maxcols,external,header,include,mode,colsset,ascii,msdos,keepext,hex,forceunsigned,declaresize,resfilesize;
  const char *thenamespace,*theprefix;
  char resname[1000];

  /* Initialize */
  outfilename=0;
  outfile=stdout;
  maxcols=16;
  colsset=0;
  external=0;
  header=1;
  include=0;
  ascii=0;
  mode=MODE_HEX;
  msdos=0;
  keepext=0;
  thenamespace=0;
  theprefix="";
  append=0;
  forceunsigned=0;
  declaresize=0;

  /* Check arguments */
  if(argc<2){
    printusage();
    exit(1);
    }

  /* Process all options first, except for the -r option */
  for(i=1; i<argc && argv[i][0]=='-' && argv[i][1]!='r'; i++){

    /* Change output file */
    if(argv[i][1]=='o'){
      if(argv[i][2]=='a') append=1;
      i++;
      if(i>=argc){
        fprintf(stderr,"reswrap: missing argument for -o option\n");
        exit(1);
        }
      outfilename=argv[i];
      }

    /* Print help */
    else if(argv[i][1]=='h'){
      printusage();
      exit(0);
      }

    /* Print version */
    else if(argv[i][1]=='v'){
      printversion();
      exit(0);
      }

    /* Switch to decimal */
    else if(argv[i][1]=='d'){
      mode=MODE_DECIMAL;
      if(!colsset) maxcols=10;
      }

    /* Switch to hex */
    else if(argv[i][1]=='x'){
      mode=MODE_HEX;
      if(!colsset) maxcols=16;
      }

    /* Switch to text */
    else if(argv[i][1]=='t'){
      if(argv[i][2]=='a') ascii=1;
      mode=MODE_TEXT;
      if(!colsset) maxcols=80;
      }

    /* Suppress header */
    else if(argv[i][1]=='s'){
      header=0;
      }

    /* Force unsigned */
    else if(argv[i][1]=='u'){
      forceunsigned=1;
      }

    /* Declare size */
    else if(argv[i][1]=='z'){
      declaresize=1;
      }

    /* Generate as external reference */
    else if(argv[i][1]=='e'){
      external=1;
      }

    /* Building include file implies also extern */
    else if(argv[i][1]=='i'){
      include=1;
      external=1;
      }

    /* Read resource with MS-DOS mode */
    else if(argv[i][1]=='m'){
      msdos=1;
      }

    /* Keep extension */
    else if(argv[i][1]=='k'){
      keepext=1;
      }

    /* Change number of columns */
    else if(argv[i][1]=='c'){
      i++;
      if(i>=argc){
        fprintf(stderr,"reswrap: missing argument for -c option\n");
        exit(1);
        }
      if(sscanf(argv[i],"%d",&maxcols)==1 && maxcols<1){
        fprintf(stderr,"reswrap: illegal argument for number of columns\n");
        exit(1);
        }
      colsset=1;
      }

    /* Embed in namespace */
    else if(argv[i][1]=='n'){
      i++;
      if(i>=argc){
        fprintf(stderr,"reswrap: missing argument for -n option\n");
        exit(1);
        }
      thenamespace=argv[i];
      }

    /* Prefix in front of declarations */
    else if(argv[i][1]=='p'){
      i++;
      if(i>=argc){
        fprintf(stderr,"reswrap: missing argument for -p option\n");
        exit(1);
        }
      theprefix=argv[i];
      }
    }

  /* To file instead out stdout */
  if(outfilename){
    outfile=fopen(outfilename,append?"a":"w");
    if(!outfile){
      fprintf(stderr,"reswrap: unable to open output file %s\n",outfilename);
      exit(1);
      }
    }

  /* Output header */
  if(header){
    fprintf(outfile,"/* Generated by reswrap version %s */\n\n",version);
    }

  /* Output namespace begin */
  if(thenamespace){
    fprintf(outfile,"namespace %s {\n\n",thenamespace);
    }

  /* Process resource files next */
  for(; i<argc; i++){

    /* Resource name override */
    if(argv[i][0]=='-' && argv[i][1]=='r'){

      /* Must have extra argument */
      if(++i>=argc){
        fprintf(stderr,"reswrap: missing name argument for -r option\n");
        exit(1);
        }

      /* Get resource name */
      resourcename(resname,theprefix,argv[i],keepext);

      /* Must have following file name */
      if(++i>=argc){
        fprintf(stderr,"reswrap: missing resource file name\n");
        exit(1);
        }

      /* Get resource file name */
      resfilename=argv[i];
      }

    else{

      /* Get resource file name */
      resfilename=argv[i];

      /* Get resource name */
      resourcename(resname,theprefix,resfilename,keepext);
      }


    /* Check resource name not empty */
    if(*resname==0){
      fprintf(stderr,"reswrap: empty resource name for %s\n",resfilename);
      exit(1);
      }

    /* Open resource file; always open as binary */
    resfile=fopen(resfilename,"rb");
    if(!resfile){
      fprintf(stderr,"reswrap: unable to open input file %s\n",resfilename);
      exit(1);
      }

    /* Get the size */
    fseek(resfile,0,SEEK_END);
    resfilesize=ftell(resfile);
    fseek(resfile,0,SEEK_SET);

    /* Add one if text mode, for end of string */
    if(mode==MODE_TEXT){
      resfilesize++;
      }

    /* Output header */
    if(header){
      fprintf(outfile,"/* created by reswrap from file %s */\n",resfilename);
      }

    /* Generate external reference for #include's */
    if(external){ fprintf(outfile,"extern "); }

    /* In text mode, output a 'char' declaration */
    if((mode==MODE_TEXT) && !forceunsigned){
      if(declaresize){
        fprintf(outfile,"const char %s[%d]",resname,resfilesize);
        }
      else{
        fprintf(outfile,"const char %s[]",resname);
        }
      }

    /* In binary mode, output a 'unsigned char' declaration */
    else{
      if(declaresize){
        fprintf(outfile,"const unsigned char %s[%d]",resname,resfilesize);
        }
      else{
        fprintf(outfile,"const unsigned char %s[]",resname);
        }
      }

    /* Generate resource array */
    if(!include){
      if(mode==MODE_TEXT){
        col=0;
        hex=0;
        fprintf(outfile,"=\n  \"");
        while((b=fgetc(resfile))!=EOF){
          if(msdos && (b=='\r')) continue;
          if(col>=maxcols){
            fprintf(outfile,"\"\n  \"");
            col=0;
            }
          if(ascii){
            if(b=='\\'){ fprintf(outfile,"\\\\"); col+=2; hex=0; }
            else if(b=='\a'){ fprintf(outfile,"\\a"); col+=2; hex=0; }
            else if(b=='\t'){ fprintf(outfile,"\\t"); col+=2; hex=0; }
            else if(b=='\r'){ fprintf(outfile,"\\r"); col+=2; hex=0; }
            else if(b=='\f'){ fprintf(outfile,"\\f"); col+=2; hex=0; }
            else if(b=='\v'){ fprintf(outfile,"\\v"); col+=2; hex=0; }
            else if(b=='\"'){ fprintf(outfile,"\\\""); col+=2; hex=0; }
            else if(b=='\n'){ fprintf(outfile,"\\n\"\n  \""); col=0; hex=0; }
            else if(b<32 || b>=127){ fprintf(outfile,"\\x%02x",b); col+=4; hex=1; }
            else if(hex && isxdigit(b)){ fprintf(outfile,"\\x%02x",b); col+=4; hex=1; }
            else{ fprintf(outfile,"%c",b); col+=1; hex=0; }
            }
          else{
            fprintf(outfile,"\\x%02x",b); col+=4;
            }
          }
        fprintf(outfile,"\"\n  ");
        }
      else{
        col=0;
        first=1;
        fprintf(outfile,"={\n  ");
        while((b=fgetc(resfile))!=EOF){
          if(msdos && (b=='\r')) continue;
          if(!first){
            fprintf(outfile,",");
            }
          if(col>=maxcols){
            fprintf(outfile,"\n  ");
            col=0;
            }
          if(mode==MODE_HEX)
            fprintf(outfile,"0x%02x",b);
          else
            fprintf(outfile,"%3d",b);
          first=0;
          col++;
          }
        fprintf(outfile,"\n  }");
        }
      }

    fprintf(outfile,";\n\n");

    /* Close resource file */
    fclose(resfile);
    }

  /* Output namespace end */
  if(thenamespace){
    fprintf(outfile,"}\n");
    }

  /* To file instead out stdout */
  if(outfilename){
    fclose(outfile);
    }

  return 0;
  }



