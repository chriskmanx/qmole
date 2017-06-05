///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <MSTypes/MSStringVector.H>
#include <AplusGUI/AplusDisplayServer.H>
#include <X11/Xresource.h>

static const char *_XA_DT_WORKSPACE_LIST="_DT_WORKSPACE_LIST";

Atom *AplusDisplayServer::workspaceAtoms(void) const
{
  Atom          *xAtoms=0, *atoms=0;
  Atom           actualType;
  int            actualFormat;
  unsigned long  lcount;
  unsigned long  leftover;
  
  Atom wsListAtom = XInternAtom(display(),_XA_DT_WORKSPACE_LIST,False);

  int status=XGetWindowProperty(display(),_mwmWindow,
				wsListAtom,0L,(long)BUFSIZ,
				False,XA_ATOM,
				&actualType,&actualFormat,
				&lcount,&leftover,
				(unsigned char**)&xAtoms);
      
  if (status==Success && actualType==XA_ATOM && lcount>0)
    {
      atoms = new Atom[lcount];
      memcpy(atoms,xAtoms,(int)lcount*sizeof(Atom)); // copy the new atoms
    }

  if (xAtoms!=0)
    {
      XFree((char*)xAtoms);
    }

  return atoms;
}


int AplusDisplayServer::workspaceNumber(const char *wsName_) const
{
  MSStringVector wsNames(workspaceNames());
  int n=numberOfWorkspaces();

  for (int i=0; i<n; ++i)
    {
      if (wsName_==wsNames(i))
	{
	  return i+1;	// workspace numbers should be 1-based
	}
    }

  return 0;
} 


int AplusDisplayServer::virtualScreen(Window win_)
{
  if (isCDERunning()==MSTrue)
    {
      // This method should not be called for CDE to find out the presence of
      // a window; CDE presence is handled in virtualScreen() method of AplusShell or AplusPopup
      //
      return -1;
    }
  else	// CDE is not running
    {
      Atom actualTarget;
      int actualFormat;
      int status;
      int screen=0;
      unsigned long numItems;
      unsigned long bytesRemaining;
      unsigned char *actData;
      int *data=0;
      long len=(sizeof(int)+3)/4;
      Atom atom = XInternAtom(display(), "WM_VIRTUAL_SCREEN", False);

      status=XGetWindowProperty(display(),win_,atom,
				0L,len,False,XA_INTEGER,
				&actualTarget,&actualFormat,&numItems,
				&bytesRemaining,&actData);
      
      if (status==Success&&actualTarget==XA_INTEGER&&actualFormat==32&&numItems>0)
	{
	  data=(int *)actData;
	  screen=(int)data[0];
	}
      if (data!=0) XFree((char *)actData);
      return screen;
    }
}


int AplusDisplayServer::virtualScreen(void)
{
  if (isCDERunning()==MSTrue)
    {
      return workspaceNumber(currentWorkspaceName());
    }
  else	// CDE is not running
    {
      return virtualScreen(root());
    }
}


void AplusDisplayServer::virtualGeometry(int& x_,int& y_)
{
  if (isCDERunning()==MSTrue)
    {
      // With TED's current implementation, changes in the virtual geometry are not written to
      // the resource database (where the number of rows is stored) until auto-save takes place.
      // Hopefully, in the future, TED will have a property that we can query for the number of
      // rows.  Trying to force the database update manually proved to be too time-consuming.
      //
      char coverStr[256];
      char *coverClass = NULL;
      char *coverType[256];
      XrmValue value;
      char *serverString = XResourceManagerString(_dpy);
      if (serverString==0)
	{
	  x_=1;
	  y_=1;
	  return;
	}

      XrmDatabase serverDB = XrmGetStringDatabase(serverString);

      sprintf(coverStr,"Dtwm*%d*gwmRows",screenNum());
      Bool rc = XrmGetResource(serverDB, coverStr, coverClass, coverType, &value);
      if (rc==True)
	{
	  if( 0== (x_ = atoi(MSString(value.addr,value.size).string()) ))
	    {
	      x_=1;
	      y_=1;
	    }
	  else
	    y_ = numberOfWorkspaces()/x_;
	}
      else
	{
 	  x_=1;
	  y_=1;
	}
    }
  else	// CDE is not running
    {
      Atom actualTarget;
      int actualFormat;
      int status;
      unsigned long numItems;
      unsigned long bytesRemaining;
      unsigned char *actData;
      int *data=0;
      long len=2*((sizeof(int)+3)/4);
      Atom atom = XInternAtom(display(),"WM_VIRTUAL_GEOMETRY",False);

      status=XGetWindowProperty(display(),root(),atom,
				0L,len,False,XA_INTEGER,
				&actualTarget,&actualFormat,&numItems,
				&bytesRemaining,&actData);
      
      if (status==Success&&actualTarget==XA_INTEGER&&actualFormat==32&&numItems>0)
	{
	  data=(int *)actData;
	  x_=(int)data[0];
	  y_=(int)data[1];	
	}  
      else
	{
	  x_=1;
	  y_=1;
	}
      if (data!=0) XFree((char *)actData);
    }
}

