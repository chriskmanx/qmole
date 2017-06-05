///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <MSGUI/MSTextField.H>
#include <MSGUI/MSWidget.H>
#include <MSIPC/MSTv.H>
#include <MSIPC/MSFds.H>
#include <MSIPC/MSChannel.H>
#include <MSTypes/MSMessageLog.H>
#include <a/ik.h>
#include <a/fncdcls.h>
#include "AplusApplication.H"
#include <unistd.h>

#ifdef __VISUAL_C_2_0__
#define select(a1, a2, a3, a4, a5) __MSTK_select(a1,a2,a3,a4,a5)
static int __winx_timer_ID = -1;
extern int SetWindowsTimer(int nmilliseconds);
extern void KillWindowsTimer(int id);
static int __MSTK_select(unsigned long,void *,void *,void *, 
					struct timeval *timeout)  
{
    int nmsecs = timeout ? (1000*timeout->tv_sec + timeout->tv_usec/1000) : 0; 
    if (nmsecs) {
	if (__winx_timer_ID != -1) KillWindowsTimer(__winx_timer_ID);
	__winx_timer_ID = SetWindowsTimer(nmsecs);
	printf("Timer %d = %d msecs\n", __winx_timer_ID, nmsecs);
    }
    return 1;
}
#endif


extern "C" void AplusLoop(long, char**, int);
extern "C" int chanproc(void);
extern "C" int timerproc(void);
extern "C" int sgnlproc(void);
extern fd_set *fds_r;
extern fd_set *fds_w;
extern fd_set *fds_x;
extern fd_set *fds_ra;
extern fd_set *fds_wa;
extern fd_set *fds_xa;
extern int fds_size;
extern "C" void pr(void);
extern "C" struct timeval *timernext();

extern MSBoolean processAVariables(void);
extern MSBoolean processDeleteQueue(void);

// AplusApplication has a different main loop than
// MSApplication.  It deletes the *mainLoop created
// by the constructor of MSApplication and make it
// Point to a AplusMainLoop

AplusApplication::AplusApplication(void)
{
  if (mainLoop()!=0)
    {
      delete mainLoop();
    }

  _mainLoop=new AplusMainLoop(this);
 
}


AplusApplication::AplusApplication(int argc_,char **argv_) : MSApplication(argc_, argv_)
{
  if (mainLoop()!=0)
    {
      delete mainLoop();
    }

  _mainLoop=new AplusMainLoop(this);
}

AplusApplication::~AplusApplication(void)
{}

 
AplusMainLoop::AplusMainLoop(MSApplication *pApplication_) : MSApplicationMainLoop(pApplication_)
{}

AplusMainLoop::~AplusMainLoop(void)
{}

MSBoolean AplusMainLoop::processTimers(void)
{
  (void)timerproc();
  MSBoolean rc=MSMainLoop::processTimers();
  return rc;
}

void AplusMainLoop::innerLoopFlush(MSBoolean)
{
  if (flush()==MSTrue||processAVariables()==MSTrue) zeroTimeOut(MSTrue);
  else zeroTimeOut(MSFalse);
}    

void AplusMainLoop::selectAndProcess(void)
{
  struct timeval timeout;
  struct timeval *tvpnext;
  struct timeval *tvp;
  int rc;
  
  // Merging MSTK and Dap File Descriptors
  // Copy Enabled to Abled
  MSChannel::fds()->fdsor(fds_r,MSChannel::fds()->r(),MSChannel::fds()->ra());
  MSChannel::fds()->fdsor(fds_w,MSChannel::fds()->w(),MSChannel::fds()->wa());
  MSChannel::fds()->fdsor(fds_x,MSChannel::fds()->x(),MSChannel::fds()->xa());

  // set up timeout 
  if (zeroTimeOut()==MSTrue) 
   {
     tvp=&timeout;
     tvp->tv_sec=tvp->tv_usec=(long)(0);
   }
  // Have to check both MSTK timers and Dap timers to determine a proper timeout value  
  else if ((tvpnext=MSTimer::nextTimeVal())==(struct timeval *)(0)&& 
           (tvpnext=timernext())==(struct timeval *)(0))
   {
     tvp=(struct timeval *)(0);
   }
  else
   {
     tvp=&timeout;
     (void) tvdiff(tvpnext,tod(),tvp);
     if (tvp->tv_sec<0) tvp->tv_sec=tvp->tv_usec=(long)(0);
   }

  rc=MSChannel::select(tvp);

  // Need to copy the result to fds_*a so dap can know which descriptor popped.
  MSChannel::fds()->fdscopy(MSChannel::fds()->ra(),fds_ra);
  MSChannel::fds()->fdscopy(MSChannel::fds()->wa(),fds_wa);
  MSChannel::fds()->fdscopy(MSChannel::fds()->xa(),fds_xa);
  if ((rc<0)&&(errno!=EINTR)) 
   {
     if (MSMessageLog::quietMode()!=MSTrue)
      {
        perror("MSMainLoop: error: select() exiting...");
        fprintf(stderr,"MSMainLoop: error: select()\n");
        exit(1);
      }
   }
  if (rc<=0)
   {	
     MSChannel::fds()->fdszero(fds_ra);
     MSChannel::fds()->fdszero(fds_wa);
     MSChannel::fds()->fdszero(fds_xa);
     MSChannel::fdszero();
   }
  (void) MSChannel::processChannels();  // Handle MStk Events
  (void) chanproc();           		// Process Dap Channels
  (void) sgnlproc();           		// Process Dap Signals
}

extern const char *MSAltDefaultFont;

const char *aplKeyTranslationFunction(const XEvent *);

// Settings to mimic the Aplus "btk" look instead of MStk
void setAplusAppDefaults(void)
{
  MSDefaultBackgroundColorSpec="grey";
  MSDefaultForegroundColorSpec="black";
  MSDefaultFont="kaplgallant-19";
  MSAltDefaultFont="fixed";
}

// Add KeyTranslation Function
void loadAplusTextKeyTranslations(void)
{ MSTextField::keyTranslationFunction(aplKeyTranslationFunction); }
  
void AplusLoop(long argc_,char **argv_, int i_)
{
  setAplusAppDefaults();
  loadAplusTextKeyTranslations();
  
  AplusApplication ap(argc_, argv_);

  if (i_ < argc_ && argv_[i_] && *argv_[i_])
    loadafile(argv_[i_],0);                  /* load script */
  if (Tf) pr();                            /* initial prompt */

  ap.loop();
}
  
  
