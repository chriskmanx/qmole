/********************************************************************************
*                                                                               *
*                  F O X   D e s k t o p   C a l c u l a t o r                  *
*                                                                               *
*********************************************************************************
* Copyright (C) 2001,2005 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: main.cpp,v 1.8 2005/01/16 16:06:06 fox Exp $                             *
********************************************************************************/
#include "fx.h"
#include "fxkeys.h"
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <signal.h>
#ifndef WIN32
#include <unistd.h>
#endif
#include <ctype.h>
#include "Calculator.h"


// Start the whole thing
int main(int argc,char *argv[]){

  // Make application
  FXApp application("Calculator",FXString::null);

  // Open display
  application.init(argc,argv);

  // Main window
  Calculator* calculator=new Calculator(&application);

  // Handle interrupt to save stuff nicely
  application.addSignal(SIGINT,calculator,Calculator::ID_CLOSE);

  // Create app
  application.create();

  // Run
  return application.run();
  }


