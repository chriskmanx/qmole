/*
	gcc -fPIC -c -g -O2 testload.c
	gcc -shared tclWinkill.o -ltclstub -o winkill.dll
	and then load winkill.dll
	winkill -pid 23423 -signal INT
        # should rewrite this all to make more general utilitity:
        sharedmem init name length
        sharedmem set  name ind value
	sharedmem get  name ind length

    
    

	or
	to just init the shared memory..
	winkill -pid 23423
	
*/
#define USE_TCL_STUBS

#include "windows.h"

#include "tcl.h"


#undef TCL_STORAGE_CLASS
#define TCL_STORAGE_CLASS DLLEXPORT

#include <stdlib.h>
#include <signal.h>

#define signal_mask(n)  (1 << (n))


typedef struct _sharedMemory sharedMemory;
struct  _sharedMemory {
  HANDLE handle;
  LPVOID address;
  DWORD length ;
  char name[20] ;
  int pid;
  sharedMemory *next;
};

static sharedMemory *sharedMemoryPtr;

#define MEMSIZE    0x10000
    


typedef struct {int signumber; char *name ;} sigNameStruct;
sigNameStruct sigNames[]=
{
#ifdef 	SIGHUP
{	SIGHUP, "HUP" },	/* Hangup (POSIX).  */
#endif
#ifdef 	SIGINT
{	SIGINT, "INT" },	/* Interrupt (ANSI).  */
#endif
#ifdef 	SIGQUIT
{	SIGQUIT, "QUIT" },	/* Quit (POSIX).  */
#endif
#ifdef 	SIGILL
{	SIGILL, "ILL" },	/* Illegal instruction (ANSI).  */
#endif
#ifdef 	SIGTRAP
{	SIGTRAP, "TRAP" },	/* Trace trap (POSIX).  */
#endif
#ifdef 	SIGABRT
{	SIGABRT, "ABRT" },	/* Abort (ANSI).  */
#endif
#ifdef 	SIGIOT
{	SIGIOT, "IOT" },	/* IOT trap (4.2 BSD).  */
#endif
#ifdef 	SIGBUS
{	SIGBUS, "BUS" },	/* BUS error (4.2 BSD).  */
#endif
#ifdef 	SIGFPE
{	SIGFPE, "FPE" },	/* Floating-point exception (ANSI).  */
#endif
#ifdef 	SIGKILL
{	SIGKILL, "KILL" },	/* Kill, unblockable (POSIX).  */
#endif
#ifdef 	SIGUSR1
{	SIGUSR1, "USR1" },	/* User-defined signal 1 (POSIX).  */
#endif
#ifdef 	SIGSEGV
{	SIGSEGV, "SEGV" },	/* Segmentation violation (ANSI).  */
#endif
#ifdef 	SIGUSR2
{	SIGUSR2, "USR2" },	/* User-defined signal 2 (POSIX).  */
#endif
#ifdef 	SIGPIPE
{	SIGPIPE, "PIPE" },	/* Broken pipe (POSIX).  */
#endif
#ifdef 	SIGALRM
{	SIGALRM, "ALRM" },	/* Alarm clock (POSIX).  */
#endif
#ifdef 	SIGTERM
{	SIGTERM, "TERM" },	/* Termination (ANSI).  */
#endif
#ifdef 	SIGSTKFLT
{	SIGSTKFLT, "STKFLT" },	/* Stack fault.  */
#endif
#ifdef 	SIGCLD
{	SIGCLD, "CLD" },	/* Same as SIGCHLD (System V).  */
#endif
#ifdef 	SIGCHLD
{	SIGCHLD, "CHLD" },	/* Child status has changed (POSIX).  */
#endif
#ifdef 	SIGCONT
{	SIGCONT, "CONT" },	/* Continue (POSIX).  */
#endif
#ifdef 	SIGSTOP
{	SIGSTOP, "STOP" },	/* Stop, unblockable (POSIX).  */
#endif
#ifdef 	SIGTSTP
{	SIGTSTP, "TSTP" },	/* Keyboard stop (POSIX).  */
#endif
#ifdef 	SIGTTIN
{	SIGTTIN, "TTIN" },	/* Background read from tty (POSIX).  */
#endif
#ifdef 	SIGTTOU
{	SIGTTOU, "TTOU" },	/* Background write to tty (POSIX).  */
#endif
#ifdef 	SIGURG
{	SIGURG, "URG" },	/* Urgent condition on socket (4.2 BSD).  */
#endif
#ifdef 	SIGXCPU
{	SIGXCPU, "XCPU" },	/* CPU limit exceeded (4.2 BSD).  */
#endif
#ifdef 	SIGXFSZ
{	SIGXFSZ, "XFSZ" },	/* File size limit exceeded (4.2 BSD).  */
#endif
#ifdef 	SIGVTALRM
{	SIGVTALRM, "VTALRM" },	/* Virtual alarm clock (4.2 BSD).  */
#endif
#ifdef 	SIGPROF
{	SIGPROF, "PROF" },	/* Profiling alarm clock (4.2 BSD).  */
#endif
#ifdef 	SIGWINCH
{	SIGWINCH, "WINCH" },	/* Window size change (4.3 BSD, Sun).  */
#endif
#ifdef 	SIGPOLL
{	SIGPOLL, "POLL" },	/* Pollable event occurred (System V).  */
#endif
#ifdef 	SIGIO
{	SIGIO, "IO" },	/* I/O now possible (4.2 BSD).  */
#endif
#ifdef 	SIGPWR
{	SIGPWR, "PWR" },	/* Power failure restart (System V).  */
#endif
#ifdef  SIGSYS
{ SIGSYS, "SYS" },
#endif
{ 0,0}
};

int
Tcl_WinKillCmd(ClientData clientData,
                Tcl_Interp *interp,
                int argc,
	       char *argv[]);


void close_shared_memory1();
void close_shared_memory(ClientData clientData);

EXTERN int Tclwinkill_Init(   Tcl_Interp *interp);

static int refcount;


int
Tclwinkill_Init(   Tcl_Interp *interp)
{
    if (!Tcl_InitStubs(interp, "8.0", 0)) {
	return TCL_ERROR;
    }
  if (refcount==0)
    atexit(close_shared_memory1);
  refcount++;
  Tcl_CreateCommand(interp, "winkill" ,Tcl_WinKillCmd ,NULL,close_shared_memory);
  return TCL_OK;
}




void close_shared_memory(ClientData data)
{
  if (--refcount <= 0) {
  sharedMemory *p,*old = sharedMemoryPtr;
  p = old;
  while (p) {
    if (p->handle)  CloseHandle(p->handle);
    p->handle = NULL;
    if (p->address)  UnmapViewOfFile(p->address);
    p->address = NULL;
    old = p;
    p = p->next;
    free(old);
    }
  }
  sharedMemoryPtr=NULL;
}

#define ErrorHandler(x) do {Tcl_AppendResult(interp,x,0); return NULL;} while(0)

sharedMemory *
getSharedMemoryPtr(Tcl_Interp *interp,int pid)
{
  sharedMemory shm;
  sharedMemory *  shmPtr = sharedMemoryPtr;
  while (shmPtr) {
    if (shmPtr->pid == pid) {
      return shmPtr;
    }
    shmPtr=shmPtr->next;
    
  }
  shmPtr = &shm;
  memset(&shm,0,sizeof(sharedMemory));
  shmPtr->pid = pid;
  shmPtr->next = NULL;
  shmPtr->handle = NULL;
  sprintf(shmPtr->name,"gcl-%d",pid);
  { 
    int value;
    int *at;

    shmPtr->handle =
      OpenFileMapping(FILE_MAP_WRITE, /*  Read/write permission.   */
		      FALSE,                     /*  Do not inherit the name  */
		      shmPtr->name);           /*  of the mapping object.   */
    
    if (shmPtr->handle == NULL) 
      { 
	ErrorHandler("winkill: Could not open file-mapping object."); 
      } 
 
    shmPtr->address =
      MapViewOfFile(shmPtr->handle, /* Handle to mapping object.  */
		    FILE_MAP_WRITE,       /* Read/write permission.  */
		    0,                   /* Max.  object size.  */
		    0,                   /* Size of hFile.  */
		    0);                  /* Map entire file.  */
 
    if (shmPtr->address == NULL) 
      { 
	ErrorHandler("winkill: Could not map view of file.");
      }
    { sharedMemory *newPtr = malloc(sizeof(sharedMemory));
    *newPtr = *shmPtr;
    newPtr->next = sharedMemoryPtr;
    sharedMemoryPtr = newPtr;
    }
  }
}


int
Tcl_WinKillCmd(ClientData clientData,
                Tcl_Interp *interp,
                int argc,
                char *argv[])
{int sig=-1;
 int pid=-1;
 int i = 0;
 int value = 0;
 char *dosig = NULL;
 char *in;
 char *pidPtr = NULL;
 sharedMemory *shmPtr;
   sigNameStruct *sigNamePtr = sigNames;
   
  if  (argc < 3 || argv[1][0] != '-') {
     {
     USAGE:
       Tcl_AppendResult(interp,"winkill -pid pid -signal SIG",0);
       return TCL_ERROR;
     }
  }
  for (i = 1 ; i < argc ; i+=2) {  
       if (argv[i][0]!='-') { goto USAGE;}
       if (argv[i][1]=='s' && strcmp(&argv[i][1],"signal")==0) {
	 in = &(argv[i+1][1]);
	 if (sscanf(in,"%d",&sig)==0) {
	   while(sigNamePtr->name) {
	     if (strcmp(sigNamePtr->name,in)==0) {
	       sig = sigNamePtr->signumber;
	       break;
	     }
	     sigNamePtr++;
	   }
	 }
	 if (sig<0)
	   {
	     Tcl_AppendResult(interp,"Bad Signal",0);
	     goto USAGE;
	   }
	 value |= signal_mask(sig);
       } else
	 if (argv[i][1]=='p' && strcmp(&argv[i][1],"pid")==0) {
	   pidPtr = argv[i+1];
	   if (1 != sscanf(argv[i+1],"%d",&pid)) {
	     Tcl_AppendResult(interp,"Bad pid arg:",argv[2],".",0);
	     goto USAGE;
	   }
       }
	 else 	     goto USAGE;
     }
  if (pidPtr== NULL || (shmPtr = getSharedMemoryPtr(interp,pid))==NULL) {
       Tcl_AppendResult(interp,"Could not open shared memory for pid ",
			pidPtr,0);
       return TCL_ERROR;
     }
  { int *at;
    at = (int *)(shmPtr->address);
    *at |= value;
  }
  return TCL_OK;
}


void close_shared_memory1()
{
  refcount=0;
  close_shared_memory(NULL);
}
