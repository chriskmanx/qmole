/*  Copyright 1992-94, 1997 John Bovey, University of Kent at Canterbury.
 *
 *  Redistribution and use in source code and/or executable forms, with
 *  or without modification, are permitted provided that the following
 *  condition is met:
 *
 *  Any redistribution must retain the above copyright notice, this
 *  condition and the following disclaimer, either as part of the
 *  program source code included in the redistribution or in human-
 *  readable materials provided with the redistribution.
 *
 *  THIS SOFTWARE IS PROVIDED "AS IS".  Any express or implied
 *  warranties concerning this software are disclaimed by the copyright
 *  holder to the fullest extent permitted by applicable law.  In no
 *  event shall the copyright-holder be liable for any damages of any
 *  kind, however caused and on any theory of liability, arising in any
 *  way out of the use of, or inability to use, this software.
 *
 *  -------------------------------------------------------------------
 *
 *  In other words, do not misrepresent my work as your own work, and
 *  do not sue me if it causes problems.  Feel free to do anything else
 *  you wish with it.
 */


#include <stdarg.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <termios.h>
#include <unistd.h>
#include <signal.h>
#include <fcntl.h>
#include <grp.h>
#include <pwd.h>
#include <errno.h>
#include <string.h>
#include <time.h>
#include "xvt.h"
#include "token.h"
#include "command.h"
#include "ttyinit.h"
#include "screen.h"
#include "xsetup.h"

/*  NOTES ON PORTING
 *
 * Almost all the non-portable parts of xvt are concerned with setting up
 * and configuring the pseudo-teletype (pty) link that connects xvt itself
 * to the commands running in its window.  In practice, there are four
 * different tasks that need to be done to get a pty up and running and
 * each of these tasks can be done in different ways, depending on the
 * flavour of UNIX.  The four tasks are:
 *
 * Obtaining a free pty
 * --------------------
 * This is handled in the function get_pseudo_teletype.  On BSD flavours
 * of UNIX, the directory /dev contains a block of pty master devices,
 * with names pty??, and a matching set of slave teletype devices with the
 * initial 'p' replaced by a 't'.  The way to find a free pseudoteletype
 * pair is to try opening pty devices, one by one, until one is opened
 * successfully.  Then the matching tty device is opened to form the other
 * end of the pair.  The alternative (SVR4?) approach is to open /dev/ptmx
 * and then use a sequence of system calls to request and initialise the
 * pty pair.  Either of the two approaches are selected by defining one of
 * BSD_PTY and SVR4_PTY, but the only system I have found which supports
 * SVR4_PTY is SunOS 5.  Some systems have a function called openpty()
 * which "does the right thing" for that system.  Defining LIBC_OPENPTY
 * selects this approach.  This is the case for glibc2-based systems.
 *
 * Creation of a controlling teletype
 * ----------------------------------
 * After the fork(), the slave tty needs to be made into the controlling
 * teletype for the child process (which will eventually exec the command
 * program, usually a shell).  On some systems this is done with a
 * TIOCSCTTY ioctl after calling setsid() to create a new session.  On
 * most other systems, the procedure is to call setsid() and then re-open
 * the tty.  This latter approach is the default - if your system uses
 * TIOCSCTTY then #define SCTTY_IOCTL.
 *
 * Configure the teletype
 * ----------------------
 * This is handled in set_ttymodes().  SVR4 systems use the termios
 * structure and the tcsetattr() system call whereas BSD systems use the
 * sgttyb structure and TIOC ioctls.  In the systems I have ported to,
 * termios is prevalent and so this is the default.  If your system does
 * not have termios then you will need to #define BSD_TTY to enable the
 * use of sgttyb instead.
 *
 * Adding an entry to utmp
 * -----------------------
 * Xvt needs to add an entry to /etc/utmp if the session is to be visible
 * to programs like finger and who.  On BSD systems this involved finding
 * the correct position in the utmp file and then using write(2) to insert
 * an entry.  On SVR4 systems the work is done by pututline(3) and
 * family.  SunOS 5 has a utmp extension which stores additional
 * information about each session.  The relevent #defines are SVR4_UTMP,
 * BSD_UTMP and SVR4_UTMPX.  If you don't need utmp entries then you don't
 * need to define any of these.
 */

/*  Definitions that enable machine dependent parts of the code.
 */

/* Unix SUNOS5 */
#if defined (__sun__)
#include <sys/stropts.h>
#include <sys/file.h>
#include <sys/stat.h>
#define SVR4_PTY
#endif /* SUNOS5 */

#define LIBC_OPENPTY


/* Unix HPUX */
#if defined (__hpux__)
#endif /* HPUX */

/* GNU LINUX */
#if defined (__linux__)
#include <sys/ioctl.h>
#include <pty.h>
#define LIBC_OPENPTY
#endif /* LINUX */

/* GNU KFREEBSD */
#if defined (__FreeBSD_kernel__)
#include <sys/ioctl.h>
#include <pty.h>
#endif /* KFREEBSD */

/* GNU HURD */
#if defined (__hurd__) || defined (__gnu_hurd__) || defined (__GNU__)
#include <sys/ioctl.h>
#include <pty.h>
#endif /* HURD */

#ifdef UKC_LOCATIONS
#include <loc.h>
#define LOCTMPFILE	"/etc/loctmp"
#endif /* UKC_LOCATIONS */

static void catch_child(int);
static void catch_sig(int);
static void write_utmp(void);
static void tidy_utmp(void);
static void set_ttymodes(void);
#ifdef BSD_UTMP
static int get_tslot(char *);
#endif /* BSD_UTMP */
static char *get_pseudo_tty(int *,int *);

/*  Global variables that are set up at the beginning and then not changed
 */
extern Display		*display;
extern int 		comm_fd;	/* file descriptor connected to the command */
extern int		fd_width;	/* Number of available file descriptors */

static int comm_pid = -1;		/* process id of child */
static char *tty_name = NULL;	/* name of the slave teletype */
#ifdef BSD_UTMP
static int tslot = -1;		/* index to our slot in the utmp file */
#endif /* BSD_UTMP */
/*static struct utmp utent;*/	/* our current utmp entry */

/*  Catch a SIGCHLD signal and exit if the direct child has died.
 */
static void catch_child(int sig)
{
    int status;

    if (wait(&status) == comm_pid)
        quit(0);

    signal(SIGCHLD,catch_child);
}

/*  Catch a fatal signal and tidy up before quitting
 */
static void catch_sig(int sig)
{
    tidy_utmp();
    signal(sig,SIG_DFL);
    setuid(getuid());
    kill(getpid(),sig);
}

/*  Attempt to create and write an entry to the utmp file
 */
static void write_utmp()
{
    struct passwd *pw;

#ifdef SVR4_UTMP
    memset(&utent,0,sizeof(utent));
    utent.ut_type = USER_PROCESS;
    strncpy(utent.ut_id,tty_name + 8,sizeof(utent.ut_id));
    strncpy(utent.ut_line,tty_name + 5,sizeof(utent.ut_line));
    pw = getpwuid(getuid());
    if (pw != NULL)
        strncpy(utent.ut_name,pw->pw_name,sizeof(utent.ut_name));
    strncpy(utent.ut_host,XDisplayString(display),sizeof(utent.ut_host));
    time((time_t*)&utent.ut_time);
    pututline(&utent);
    endutent();
#endif /* SVR4_UTMP */

#ifdef SVR4_UTMPX
    struct utmpx utentx;
    int n;

    memset(&utentx,0,sizeof(utentx));
    utentx.ut_type = USER_PROCESS;
    utentx.ut_pid = comm_pid;
    if (sscanf(tty_name,"/dev/pts/%d",&n) != 1)
    {
        error("Can's parse tty name %s",tty_name);
        return;
    }
    sprintf(utentx.ut_id,"vt%02x",n);
    sprintf(utentx.ut_line,"pts/%d",n);
    pw = getpwuid(getuid());
    if (pw != NULL)
        strncpy(utentx.ut_name,pw->pw_name,sizeof(utent.ut_name));
    strncpy(utentx.ut_host,XDisplayString(display),sizeof(utentx.ut_host));
    utentx.ut_syslen = strlen(utentx.ut_host) + 1;
    time(&utentx.ut_xtime);
    getutmp(&utentx,&utent);
    pututline(&utent);
    pututxline(&utentx);
    updwtmpx(WTMPX_FILE,&utentx);
    endutent();
#endif /* SVR4_UTMPX */

#ifdef BSD_UTMP
#ifndef UTMP_FILE
#define UTMP_FILE	"/etc/utmp"
#endif /* !UTMP_FILE */

    int ut_fd;

    if ((tslot = get_tslot(tty_name + 5)) < 0)
        error("can't locate tty %s in %s",tty_name,TTYTAB);

    /*  Attempt to write an entry into the utmp file.
     */
    if (tslot > 0)
    {

        if ((ut_fd = open(UTMP_FILE, O_WRONLY)) >= 0)
        {
            memset(&utent,0,sizeof(utent));
            strncpy(utent.ut_line,tty_name + 5,sizeof(utent.ut_line));
            pw = getpwuid(getuid());
            if (pw != NULL)
                strncpy(utent.ut_name,pw->pw_name,sizeof(utent.ut_name));
            strncpy(utent.ut_host,XDisplayString(display),sizeof(utent.ut_host));
            time(&utent.ut_time);
            lseek(ut_fd,(long)(tslot * sizeof(struct utmp)),0);
            write(ut_fd,(char *)&utent,sizeof(struct utmp));
            close(ut_fd);
        }
    }
#endif /* BSD_UTMP */

#ifdef UKC_LOCATIONS
    if (tslot > 0)
    {
        struct ttyb locent;
        int loc_fd = open(LOCTMPFILE, O_WRONLY);

        if (loc_fd < 0 && errno == ENOENT)
        {
            /*  Need to create the file.
             */
            loc_fd = creat(LOCTMPFILE, 0644);
        }
        if (loc_fd >= 0)
        {
            memset(&locent,0,sizeof(struct ttyb));
            strncpy(locent.t_name,XDisplayString (display),LN_NAMEL);
            locent.t_name[LN_NAMEL - 1] = '\0';
            strcpy(locent.t_extra, "(xvt)");
            lseek(loc_fd, (long)(tslot*sizeof(locent)), 0);
            write(loc_fd, (char *)&locent, sizeof(locent));
            close(loc_fd);
        }
    }
#endif /* UKC_LOCATIONS */
}

/*  Tidy up the utmp entry etc prior to exiting.
 */
static void tidy_utmp()
{
#if defined(SVR4_UTMP) || defined(SVR4_UTMPX)
    setutent();
    if (getutid(&utent) == NULL)
        return;
    utent.ut_type = DEAD_PROCESS;
    time((time_t*)&utent.ut_time);
    pututline(&utent);
#endif /* SVR4_UTMP || SVR4_UTMPX */
#ifdef SVR4_UTMPX
    updwtmp(WTMP_FILE,&utent);
#endif /* SVR4_UTMPX */

#ifdef BSD_UTMP
    int ut_fd;

    if (tslot < 0)
        return;
    if ((ut_fd = open(UTMP_FILE,O_WRONLY)) < 0)
        return;

    memset(&utent,0,sizeof(utent));
    lseek(ut_fd,(long)(tslot * sizeof(struct utmp)),0);
    write(ut_fd,(char *)&utent,sizeof(struct utmp));
    close(ut_fd);
#endif /* BSD_UTMP */

#ifdef BSD_PTY
    chmod(tty_name,0666);
#endif /* BSD_PTY */
}

/*  Quit with the status after first removing our entry from the utmp file.
 */
void quit(int status)
{
    tidy_utmp();
    exit(status);
}

#ifdef BSD_UTMP
/*  Look up the tty name in the etc/ttytab file and return a slot number
 *  that can be used to access the utmp file.  We cannot use ttyslot()
 *  because the tty name is not that of fd 0.
 */
static int get_tslot(char* ttynam)
{
    FILE *fs;
    char buf[200], name[200];
    int i;

    if ((fs = fopen(TTYTAB,"r")) == NULL)
        return(-1);
    i = 1;
    while (fgets(buf,200,fs) != NULL)
    {
        if (*buf == '#')
            continue;
        if (sscanf(buf,"%s",name) != 1)
            continue;
        if (strcmp(ttynam,name) == 0)
        {
            fclose(fs);
            return(i);
        }
        i++;
    }
    fclose(fs);
    return(-1);
}
#endif /* BSD_UTMP */

/*  Acquire a pseudo teletype from the system.  The return value is the
 *  name of the slave part of the pair or NULL if unsucsessful.  If
 *  successful then the master and slave file descriptors are returned
 *  via the arguments.
 */
static char* get_pseudo_tty(int* pmaster, int* pslave)
{
#ifdef BSD_PTY
    int mfd, sfd;
    char *s3, *s4;
    static char ptyc3[] = "pqrstuvwxyz";
    static char ptyc4[] = "0123456789abcdef";
    static char ptynam[] = "/dev/ptyxx";
    static char ttynam[] = "/dev/ttyxx";

    /*  First find a master pty that we can open.
     */
    mfd = -1;
    for (s3 = ptyc3; *s3 != 0; s3++)
    {
        for (s4 = ptyc4; *s4 != 0; s4++)
        {
            ptynam[8] = ttynam[8] = *s3;
            ptynam[9] = ttynam[9] = *s4;
            if ((mfd = open(ptynam,O_RDWR)) >= 0)
            {
                if (geteuid() == 0 || access(ttynam,R_OK|W_OK) == 0)
                    break;
                else
                {
                    close(mfd);
                    mfd = -1;
                }
            }
        }
        if (mfd >= 0)
            break;
    }
    if (mfd < 0)
    {
        error("Can't open a pseudo teletype");
        return(NULL);
    }
    if ((sfd = open(ttynam,O_RDWR)) < 0)
    {
        error("could not open slave tty %s",ttynam);
        return(NULL);
    }
#endif /* BSD_PTY */

#ifdef SVR4_PTY
    char *ttynam;
    int mfd, sfd;

    if ((mfd = open("/dev/ptmx",O_RDWR)) < 0)
    {
        error("Can't open a pseudo teletype");
        return(NULL);
    }
    grantpt(mfd);
    unlockpt(mfd);
    ttynam = ptsname(mfd);
    if ((sfd = open(ttynam,O_RDWR)) < 0)
    {
        error("could not open slave tty %s",ttynam);
        return(NULL);
    }
    ioctl(sfd,I_PUSH,"ptem");
    ioctl(sfd,I_PUSH,"ldterm");
#endif /* SVR4_PTY */

#ifdef LIBC_OPENPTY
    char *ttynam;
    int mfd, sfd;

    if (openpty(&mfd, &sfd, NULL, NULL, NULL))
    {
        error("Cannot allocate a pseudo teletype");
        perror("");
        return(NULL);
    }
    ttynam = ttyname(sfd);
#endif /* LIBC_OPENPTY */

    *pslave = sfd;
    *pmaster = mfd;
    return(ttynam);
}

/*  Initialise the terminal attributes.
 */
static void set_ttymodes()
{
    int width, height;

#ifndef BSD_TTY

    /*  Set the terminal using the standard System V termios interface
     */
    static struct termios term;

    memset((char *)&term,0,sizeof(term));
    term.c_iflag = BRKINT | IGNPAR | ICRNL | IXON;
#ifdef IMAXBEL
    term.c_iflag |= IMAXBEL;
#endif /* IMAXBEL */
    if (!is_eightbit())
        term.c_iflag |= ISTRIP;
    term.c_oflag = OPOST | ONLCR;
    term.c_cflag = B9600 | CREAD;
    if (!is_eightbit())
        term.c_cflag |=  PARENB | CS7;
    else
        term.c_cflag |= CS8;
    term.c_lflag = ISIG | IEXTEN | ICANON | ECHO | ECHOE | ECHOK;
#ifdef ECHOCTL
    term.c_lflag |= ECHOCTL;
#endif /* ECHOCTL */
#ifdef ECHOKE
    term.c_lflag |= ECHOKE;
#endif /* ECHOKE */
    term.c_cc[VINTR] = 003;		/* ^C */
    term.c_cc[VQUIT] = 034;		/* ^\ */
    term.c_cc[VERASE] = 0177;	/* DEL */
    term.c_cc[VKILL] = 025;		/* ^U */
    term.c_cc[VEOF] = 004;		/* ^D */
    term.c_cc[VSTART] = 021;	/* ^Q */
    term.c_cc[VSTOP] = 023;		/* ^S */
    term.c_cc[VSUSP] = 032;		/* ^Z */
#ifdef VREPRINT
    term.c_cc[VREPRINT] = 022;	/* ^R */
#endif /* VREPRINT */
#ifdef VWERASE
    term.c_cc[VWERASE] = 027;	/* ^W */
#endif /* VWERASE */
#ifdef VLNEXT
    term.c_cc[VLNEXT] = 026;	/* ^V */
#endif /* VLNEXT */
#ifdef VDSUSP
    term.c_cc[VDSUSP] = 031;	/* ^Y */
#endif /* VDSUSP */
#ifdef VDISCARD
    term.c_cc[VDISCARD] = 017;	/* ^O */
#endif /* VDISCARD */

    tcsetattr(0,TCSANOW,&term);

#else /* BSD_TTY */
    /* Use sgtty rather than termios interface to configure the terminal
     */
    int ldisc, lmode;
    struct sgttyb tty;
    struct tchars tc;
    struct ltchars ltc;

#ifdef NTTYDISC
    ldisc = NTTYDISC;
    (void)ioctl(0,TIOCSETD,&ldisc);
#endif /* NTTYDISC */
    tty.sg_ispeed = B9600;
    tty.sg_ospeed = B9600;
    tty.sg_erase = 0177;
    tty.sg_kill = 025;		/* ^U */
    tty.sg_flags = CRMOD | ECHO | EVENP | ODDP;
    (void)ioctl(0,TIOCSETP,&tty);

    tc.t_intrc = 003;		/* ^C */
    tc.t_quitc = 034;		/* ^\ */
    tc.t_startc = 021;		/* ^Q */
    tc.t_stopc = 023;		/* ^S */
    tc.t_eofc =  004;		/* ^D */
    tc.t_brkc = -1;
    (void)ioctl(0,TIOCSETC,&tc);

    ltc.t_suspc = 032;		/* ^Z */
    ltc.t_dsuspc = 031;		/* ^Y */
    ltc.t_rprntc = 022;		/* ^R */
    ltc.t_flushc = 017;		/* ^O */
    ltc.t_werasc = 027;		/* ^W */
    ltc.t_lnextc = 026;		/* ^V */
    (void)ioctl(0,TIOCSLTC,&ltc);

    lmode = LCRTBS | LCRTERA | LCTLECH | LPASS8 | LCRTKIL;
    (void)ioctl(0,TIOCLSET,&lmode);
#endif /* BSD_TTY */

    scr_get_size(&width,&height);
    tty_set_size(width,height);
#ifdef TIOCCONS
    if (is_console())
        if (ioctl(0,TIOCCONS,0) != 0)
        {
            error("Could not set console");
            perror("");
        }
#endif /* TIOCCONS */
}

/*  Run the command in a subprocess and return a file descriptor for the
 *  master end of the pseudo-teletype pair with the command talking to
 *  the slave.
 */
int run_command(char* command, char** argv)
{
    int ptyfd, ttyfd;
    int uid, gid;
    int i;
    extern int messages;

    if ((tty_name = get_pseudo_tty(&ptyfd,&ttyfd)) == NULL)
        return(-1);

    fcntl(ptyfd,F_SETFL,O_NDELAY);

    fd_width = sysconf(_SC_OPEN_MAX);
    for (i = 1; i <= 15; i++)
        signal(i,catch_sig);
    comm_pid = fork();
    if (comm_pid < 0)
    {
        error("Can't fork");
        return(-1);
    }
    if (comm_pid == 0)
    {
        struct group *gr;
        pid_t pgid;

        if ((pgid = setsid()) < 0)
            error("failed to start session");

        /*  Having started a new session, we need to establish
         *  a controlling teletype for it.  On some systems
         *  this can be done with an ioctl but on others
         *  we need to re-open the slave tty.
         */
#ifdef SCTTY_IOCTL
        (void)ioctl(ttyfd,TIOCSCTTY,0);
#else /* !SCTTY_IOCTL */
        i = ttyfd;
        if ((ttyfd = open(tty_name,O_RDWR)) < 0)
        {
            error("Can't open teletype %s\n",tty_name);
            return(-1);
        }
        close(i);
#endif /* !SCTTY_IOCTL */
        uid = getuid();
        if ((gr = getgrnam("tty")) != NULL)
            gid = gr->gr_gid;
        else
            gid = -1;
        fchown(ttyfd,uid,gid);
        fchmod(ttyfd,messages ? 0620 : 0600);
        for (i = 0; i < fd_width; i++)
            if (i != ttyfd)
                close(i);
        dup(ttyfd);
        dup(ttyfd);
        dup(ttyfd);
        if (ttyfd > 2)
            close(ttyfd);
#ifdef BSD_TTY
        ioctl(0, TIOCSPGRP, (char *)&pgid);
        setpgrp (0, pgid);
#endif /* BSD_TTY */
        set_ttymodes();
        setgid(getgid());
        setuid(uid);
        execvp(command,argv);
        error("Couldn't execute %s",command);
        quit(1);
    }
    signal(SIGCHLD,catch_child);
    write_utmp();
    return(ptyfd);
}

/*  Tell the teletype handler what size the window is.  Called initially from
 *  the child and after a window size change from the parent.
 */
void tty_set_size(int width, int height)
{
#ifdef TIOCSWINSZ
    struct winsize wsize;

    if (comm_pid < 0)
        return;
    wsize.ws_row = height;
    wsize.ws_col = width;
    wsize.ws_xpixel = 0;
    wsize.ws_ypixel = 0;
    ioctl((comm_pid == 0) ? 0 : comm_fd,TIOCSWINSZ,(char *)&wsize);
#endif /* TIOCSWINSZ */
}
