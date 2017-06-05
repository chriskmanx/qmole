/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley Dean Witter & Co. All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/

#include <a/development.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <syslog.h>
#include <pwd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <dap/dap.h>
#include <dap/args.h>

#include <a/k.h>
#include <a/fncdcls.h>
#include <a/fir.h>
#include <a/arthur.h>
extern I Tf;


/* internal macro declarations */
#define ACKNOWLEDGE_LICENSE_FILE  ".apluslicok"
#define BANNER "A+"
#define COPYRIGHT \
        "Copyright (c) 1990-2008 Morgan Stanley.  All rights reserved."
#define POR_UNSET       (0)
#define POR_DEVWORK	(1)
#define POR_DEV		(2)
#define POR_MAINTWORK	(3)
#define POR_MAINT	(4)
#define POR_ALPHA	(5)
#define POR_BETA	(6)
#define POR_PROD	(7)
#define POR_OFFTRACK    (8)
#include <version.h>		/* per tree definitions */

/* internal function declarations */
static void initReleaseData(C*);
static void initVersion(void);
static void initDefaultATREE(C*);
static void startupSyslog(C*);
void initCallouts(void);
static void printId(void);
static void envinit(void);
static void xi(C*);
static I parseargs(I,C**);
static void ignore_dup(I);
static void argvInstall(I,C**,I);
static void atmpinit(void);

/* internal data definitions */
static I _majorRelease = 0;
static I _minorRelease = 0;
static I _phaseOfRelease = POR_UNSET;
static C *_workingCopy = OWNER_FULLNAME;
static C *_releaseCode = RELEASE_CODE;
static C *_whoseTree = "";
static C *_banner = BANNER;
static C *_copyright = COPYRIGHT;
static C *_defaultATREE = (char *)0;
static C *_version = (char *)0;
static C *usage = "usage: a+ [-d display] [-s] [-w workarea] [-q] [-h megsforheap] [-m atmpMode] [script [args]]\n";
static I _enable_coredump;
static I _backing_store;
static C *_display;
static I _load_s;
static I _workarea;
static I _megsforheap;
static I _quiet = 0;
static I _atmp_mode = WS_ATMP_SHARED;

#ifdef _INTERPRETER_ONLY
#include <sys/select.h>
static void getm()
{
  fd_set read_fd;
  FD_ZERO(&read_fd);
  FD_SET(fileno(stdin),&read_fd);
  if(-1==select(fileno(stdin)+1, &read_fd,0,0,0) && Tf)
    NL, sbi(), pr();
  else
    if(FD_ISSET(fileno(stdin), &read_fd))  tf();
}
#endif

void aplus_main(long argc, char** argv)
{
#if !defined(_INTERPRETER_ONLY)
        extern void AplusLoop();
#endif  
	I i; /* the number of arguments parsed */

#if !defined(_INTERPRETER_ONLY)
	dapinit();
#endif
	initReleaseData(_releaseCode);
	initVersion();
	initDefaultATREE(argv[0]);
	initCallouts();
	i = parseargs(argc, argv);
	if( !_quiet )
	  printId();
	atmpinit();
	envinit();
	if(0!=_megsforheap)setk1(_megsforheap);  /* must be before mem init! */
	ai(_workarea);                           /* initialize */
        if(_enable_coredump)
          {
            setSigv(1);
            setSigb(1);
            coreLimSet(aplusInfinity);
          }
	versSet(_version);
	releaseCodeSet(_releaseCode);
	phaseOfReleaseSet(_phaseOfRelease);
	majorReleaseSet(_majorRelease);
	minorReleaseSet(_minorRelease);
	startupSyslog(_version);
	xi(argv[0]);                             /* installation */
	argvInstall(argc, argv, i);              /* set up _argv */
	uextInstall();                           /* user lib install */
#if !defined(_INTERPRETER_ONLY)
	AplusLoop(argc, argv, i);
#else
        if (i < argc && argv[i] && *argv[i])
          loadafile(argv[i],0);
	if (Tf) pr(); 
	while(1) getm();
#endif
/**********************************************************************
    These functions are moved to AplusLoop 

	if (i < argc && argv[i] && *argv[i])
		loadafile(argv[i],0);                / * load script * /
	if (Tf) pr();                                / * initial prompt * /

**********************************************************************/
}

static void startupSyslog(C* version)
{
  C *s=bnstring("<A+ ",version,">",0);
  struct passwd *pwd;
  uid_t uid=getuid();

  openlog(s,LOG_PID,LOG_LOCAL1);
  if ((pwd = getpwuid(uid)) && pwd->pw_name) syslog(LOG_INFO, pwd->pw_name);
  else syslog(LOG_INFO, "user == %d",uid);
  closelog();bfree(s);
}

static void initReleaseData(C* rcode)
{
  if ('m'==*rcode) {	
    _minorRelease = 0;
    ++rcode;
    if (!ISdigit(*rcode)) goto breakout;
    if (0>=(_majorRelease = (I)strtol(rcode,&rcode,10))) goto breakout;
    if ('.'==*rcode) {
      _phaseOfRelease = POR_MAINTWORK;
      _whoseTree = strdup(1+rcode);
    }
    else if (!*rcode) _phaseOfRelease = POR_MAINT;
  }
  else if (!strncmp(rcode,"dev",3)) { 
    _minorRelease = 0; _majorRelease = 3;
    rcode += 3;
    if ( *rcode && '.'!=*rcode) goto breakout;
    if ( *rcode) {
      _phaseOfRelease = POR_DEVWORK;
      _whoseTree = strdup(1+rcode);
    }
    else _phaseOfRelease = POR_DEV;
  }
  else if (ISdigit(*rcode)) {	
    if (0>=(_majorRelease = (I)strtol(rcode,&rcode,10))) goto breakout;
    if ('a'==*rcode) _phaseOfRelease = POR_ALPHA;
    else if ('b'==*rcode) _phaseOfRelease = POR_BETA;
    else if ('.'==*rcode) _phaseOfRelease = POR_PROD;
    else goto breakout;
    ++rcode;
    if (POR_ALPHA == _phaseOfRelease && !*rcode) goto breakout;
    else {
      if (!ISdigit(*rcode)) {  _phaseOfRelease = POR_UNSET; goto breakout; }
      if (0>(_minorRelease = (I)strtol(rcode,&rcode,10))) goto breakout;
    }
    if (*rcode) {  _phaseOfRelease = POR_UNSET; goto breakout; }
  }
  else if (!strncmp(rcode,"off",3)) {
    _minorRelease = _majorRelease = 0;
    rcode +=3;
    if ( *rcode && '.'!=*rcode) goto breakout;
    _phaseOfRelease = POR_OFFTRACK;
    if (*rcode) _whoseTree = strdup(1+rcode);
  }
 breakout:
  if (POR_UNSET == _phaseOfRelease) _majorRelease = _minorRelease = 0;
  return;
}  


static void initVersion(void)
{
  if (_version != (char *)(0)) return;
  
  switch (_phaseOfRelease)
    {
    case POR_DEVWORK:
      _version = bgprintf(strlen((DEV_STRARG)_workingCopy) + 64,
			  "%s's latest development",
			  _workingCopy);
      break;
    case POR_DEV:
      _version = bstring("Development");
      break;
    case POR_MAINTWORK:
      _version = bgprintf(strlen((DEV_STRARG)_workingCopy) + 64,
			  "%s's latest maintenance of Release %d",
			  _workingCopy, _majorRelease);
      break;
    case POR_MAINT:
      _version = bgprintf(64, "Maintenance of Release %d",
			  _majorRelease);
      break;
    case POR_ALPHA:
      _version = bgprintf(64, "Release %d alpha", _majorRelease);
      break;
    case POR_BETA:
      _version = bgprintf(64, "Release %d beta %0*d",
			  _majorRelease, 100>_minorRelease?2:4, _minorRelease);
      break;
    case POR_PROD:
      if( strlen(_workingCopy) )
	_version = bgprintf(64, "%s %d.%0*d",_workingCopy,
			    _majorRelease, 
			    100>_minorRelease?2:4, _minorRelease);
      
      else
	_version = bgprintf(64, "Release %d.%0*d",
			    _majorRelease, 
			    100>_minorRelease?2:4, _minorRelease);

      break;
    case POR_OFFTRACK:
      if (_majorRelease) 
	_version = bgprintf(strlen((DEV_STRARG)_workingCopy) + 64,
			    "%s's off-track version of Release %d",
			    _workingCopy, _majorRelease);
      else
	_version = bgprintf(strlen((DEV_STRARG)_workingCopy) + 64,
			    "%s's off-track version of Development Release",
			    _workingCopy);
      break;
    default:
      Exit(1, "PHASE_OF_RELEASE == %d, which is invalid\n",
	   _phaseOfRelease);
    }
  return;
}

static void initDefaultATREE(C *argv0)
{
  if (_defaultATREE != (char *)(0)) return;
 
  {				/* Try to set ATREE by argv[0] */
    int i;
    C *s, *st, ss[]={"/lib/s.+"};
    if(0 == strchr(argv0,'/')) st=(C *)searchPATH(argv0);
    else st=strdup(argv0);
    s=(C *)malloc( (strlen(st)+strlen(ss)+1) * sizeof(C) );
    strcpy(s,st); free(st);
    if ( s && s[0]=='/' ){	/* should be fully qualified  */
      for(i=strlen(s)-1;i;i--) {if(s[i]=='/') break;}
      if(0==strncmp(&s[MAX(0,i-4)],"/bin",4)) {
	s[MAX(0,i-4)]='\0';
	strcat(s,ss);	
	if (!unloadable(s,R_OK)){	/* Look for ATREE/lib/s.+ */
	  s[MAX(0,i-4)]='\0';
	  _defaultATREE = strdup(s);
	  free(s);
	  return;
	}
      }
    } 
    if(s) free(s);
  }

  switch (_phaseOfRelease)
  {
  case POR_DEVWORK:
  case POR_DEV:
  case POR_MAINTWORK:
  case POR_MAINT:
  case POR_OFFTRACK:
  case POR_ALPHA:
  case POR_BETA:
  case POR_PROD:
    _defaultATREE = bstring(IMDIR);
    break;
  default:
    Exit(1, "PHASE_OF_RELEASE == %d, which is invalid\n",
	 _phaseOfRelease);
  }
  return;
}

static void printId(void)
{
	if (_banner != (char *)(0))
		fprintf(stderr, "     %s\n", _banner);
	if (_copyright != (char *)(0))
		fprintf(stderr, "     %s\n", _copyright);
	if (_version != (char *)(0))
		fprintf(stderr, "     This version is %s\n", _version);
	fflush(stderr);
}

#ifdef BSTUB
extern void setAplusMemStatsMode(int mode_); /* a/bstub.c */
static void atmpinit() 
{
  if ( _atmp_mode==WS_MEM_STATS )
    setAplusMemStatsMode(1);
}
#else

static void atmpinit()
{
  if ( (_atmp_mode==WS_MALLOC) || (atmpMissing() && _atmp_mode!=WS_ATMP_HEAP) )
    {
      pf_tmp = tmp_malloc;
      pf_ma  = ma_malloc;
      pf_mf  = mf_malloc;
    }
  else
    setAtmpMmapFlags(_atmp_mode);
}

#endif

static void envinit(void)
{
	char *display;
	char *atree;
	char *apath;

	if (_display != (char *)(0))
	{
		display = bnstring("DISPLAY=", _display, (char *)(0));
		putenv(display);
	}
	if ((atree = getenv("ATREE")) == (char *)(0))
	{
		atree = bnstring("ATREE=", _defaultATREE, (char *)(0));
		putenv(atree);
		atree = getenv("ATREE");
	}
	if ((apath = getenv("APATH")) == (char *)(0))
	{
		apath = bnstring("APATH=.:", atree,
                                 "/acore",(char *)(0));

		putenv(apath);
	}

	return;
}

static void xi(C* argv0)
{
	ovliInstall();
	dyldSlowInstall(argv0);	/* both dyme 0 and dyme 2 */
	esfInstall();
#if !defined(_INTERPRETER_ONLY)
	CppInstall();
#endif
	cInstall();
	sysInstall();
	binaryInstall();
#if !defined(_INTERPRETER_ONLY)
	/* TK set backing store */
	s_backingStoreOption(_backing_store);
	XaInstall();
	if (_load_s) sInstall();
	ipcInstall();
#endif

	return;
}

static I parseargs(argc, argv)
register I argc;
register C *argv[];
{
  I isinvalid = 0;
  C *optlist = "bcd:w:sh:qm:";
  I bflag = 0,  cflag=0, dflag = 0, wflag = 0, sflag = 0, hflag = 0;
  I qflag = 0, mflag = 0;
  I c;
  C *ep;       /* points to end of option argument */
  
  if (argsfirst(argc, argv) != 0)
  {
    Warn("%t usage: argument list is empty\n");
    isinvalid = 1;
  }
  
  while ((c = argsgetopt(argc, argv, optlist)) != -1)
  {
    switch (c)
    {
    case 'b':
      if (bflag == 0)
      {
	bflag = 1;
	_backing_store = 0;
      }
      else ignore_dup(c);
      break;
    case 'c':
      if (cflag == 0)
	{
	  cflag = 1;
	  _enable_coredump = 1;
	}
      else ignore_dup(c);
      break;
    case 'd':
      if (dflag == 0)
      {
	dflag = 1;
	_display = bstring(args_value);
      }
      else ignore_dup(c);
      break;
    case 'h':
      if (hflag == 0)
      {
	hflag = 1;
	_megsforheap = (I)strtol(args_value, &ep, 10);
	if ((*ep != '\0') || (_megsforheap < 1))
	{
	  Warn("%t usage: '%s' is an invalid heap size\n",args_value);
	  isinvalid = 1;
	}
      }
      else ignore_dup(c);
      break;
    case 's':
      if (sflag == 0)
      {
	sflag = 1;
	_load_s = 0;
      }
      else ignore_dup(c);
      break;
    case 'w':
      if (wflag == 0)
      {
	wflag = 1;
	_workarea = (I)strtol(args_value, &ep, 10);
	if ((*ep != '\0') || (_workarea < 1))
	{
	  Warn("%t usage: '%s' is an invalid workarea size\n",args_value);
	  isinvalid = 1;
	}
      }
      else ignore_dup(c);
      break;
    case 'q':
      if (qflag == 0)
	{
	  qflag = 1;
	  _quiet = 1;
	}
      else ignore_dup(c);
      break;
    case 'm':
      if (mflag == 0)
	{
	  mflag = 1;
	  if( !strcmp(args_value,"ws_atmp_noreserve") )
	    _atmp_mode = WS_ATMP_NORESERVE;
	  else if( !strcmp(args_value,"ws_malloc") )
	    _atmp_mode = WS_MALLOC;
	  else if( !strcmp(args_value,"ws_atmp_heap") )
	    _atmp_mode = WS_ATMP_HEAP;
	  else if( !strcmp(args_value,"ws_atmp_private") )
	    _atmp_mode = WS_ATMP_PRIVATE;
	  else if( !strcmp(args_value,"ws_atmp_shared") )
	    _atmp_mode = WS_ATMP_SHARED;
          else if( !strcmp(args_value,"ws_mem_stats") )
            _atmp_mode = WS_MEM_STATS;
	  else 
	    {
	      Warn("%t usage: '%s' is an invalid memory mode\n", args_value);
	      isinvalid = 1;
	    }
	}
      else ignore_dup(c);
      break;
    default:
      Warn("%t usage: -%c is an unknown option\n", c);
      isinvalid = 1;
      break;
    }
  }
  
  /* check for presence of required options */
  
  if (isinvalid)
  {
    Exit(1, usage);
  }
  
  /* set up defaults as necessary */
  if (cflag == 0)
    {
      if( getenv("APLUS_ENABLE_COREDUMP"))
        _enable_coredump = 1;
    }
  if (bflag == 0) _backing_store = 1;
  if (dflag == 0) _display = (char *)(0);
  if (sflag == 0) _load_s = 0;
  if (wflag == 0) _workarea = 4;
  if (hflag == 0) _megsforheap = 0;
  if (qflag == 0) _quiet = 0;
  if (mflag == 0) 
    {
      char *aplus_atmp_model;
      
      /* Set ATMP Platform Defaults */
#if defined(_WS_ATMP_NORESERVE)
      _atmp_mode = WS_ATMP_NORESERVE;
#else
      _atmp_mode = WS_ATMP_SHARED;
#endif
      /* Check for environment override */
      if( aplus_atmp_model=getenv("APLUS_ATMP_MODEL") )
	{
	  if( !strcmp(aplus_atmp_model,"ws_atmp_noreserve") )
	    _atmp_mode = WS_ATMP_NORESERVE;
	  else if( !strcmp(aplus_atmp_model,"ws_malloc") )
	    _atmp_mode = WS_MALLOC;
	  else if( !strcmp(aplus_atmp_model,"ws_atmp_heap") )
	    _atmp_mode = WS_ATMP_HEAP;
	  else if( !strcmp(aplus_atmp_model,"ws_atmp_private") )
	    _atmp_mode = WS_ATMP_PRIVATE;
	  else if( !strcmp(aplus_atmp_model,"ws_atmp_shared") )
	    _atmp_mode = WS_ATMP_SHARED;
	  else 
	    Warn("Invalid Environment Variable APLUS_ATMP_MODEL=%s is ignored...\n", aplus_atmp_model);
	}
    }

  return args_index;
}

static void ignore_dup(I c)
{
	Warn("%t usage: duplicate -%c option ignored\n", c);
	return;
}

static A gst(I x,C* s){A r=(A)gsv(x,s);r->r=1;return r;}
static void argvInstall(I argc, C** argv, I offset)
{
  A aobj; V v; I i=0;

  if (argc < offset) { argv += argc; argc = 0; }
  else { argv += offset; argc -= offset; }
  aobj = gv(Et, argc);
  while (argc--) aobj->p[i++] = (I)gst(0, *argv++);
  v = vi(si("_argv"), Rx);
  if (v->a) dc((A)v->a);
  v->a = (I)aobj; v->t=0;
  return;
}

#if defined(_INTERPRETER_ONLY)
void no_op(void){};
void initCallouts(void)
{
  extern void stdinDisable(), stdinEnable();

  setfunc_dst(no_op);
  setfunc_xf(no_op);
  setfunc_xup(no_op);
  setfunc_vfy(no_op);
  setfunc_disable(no_op);
  setfunc_enable(no_op);
}
#else
void initCallouts(void)
{
  extern void destroyVar(), flush(), update(), stdinDisable(), stdinEnable();
  extern I verify();

  setfunc_dst(destroyVar);
  setfunc_xf(flush);
  setfunc_xup(update);
  setfunc_vfy(verify);
  setfunc_disable(stdinDisable);
  setfunc_enable(stdinEnable);
}
#endif
