/*
 * Copyright (C) 1997-2009, Michael Jennings
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies of the Software, its documentation and marketing & publicity
 * materials, and acknowledgment shall be given in the documentation, materials
 * and software packages that this Software was used.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

static const char cvs_ident[] = "$Id: utmp.c 51650 2010-08-26 01:34:13Z lucas $";

#include "config.h"
#include "feature.h"

# include "eterm_utmp.h"
# include "command.h"
# include "screen.h"

#if defined(UTMP_SUPPORT) && !defined(HAVE_LIBUTEMPTER)

/* screen.h includes config.h again, so re-fix these.  Pointed out by Sung-Hyun Nam <namsh@lgic.co.kr> */
# if defined(_HPUX_SOURCE) || defined(_AIX) || ((__GLIBC__ >= 2) && (__GLIBC_MINOR__ >= 1))
#   undef HAVE_UTMPX_H
# endif

/* C.K. */
#undef HAVE_UTMPX_H

/* don't go off end of ut_id & remember if an entry has been made */
#  if defined(USE_SYSV_UTMP) || defined(__OpenBSD__)
static char ut_id[5];           /* remember if entry to utmp made */
#  elif !defined(NEW_BSD_UTMP)
static int utmp_pos;            /* BSD position of utmp-stamp */
#  endif

# ifdef USE_SYSV_UTMP

#  ifdef HAVE_UTMPX_H
#   undef WTMP_FILENAME
#   define WTMP_FILENAME WTMPX_FILE
#   define update_wtmp updwtmpx
#  else /* HAVE_UTMPX_H */

static void
update_wtmp(char *fname, struct utmp *putmp)
{

    int fd, retry = 10;         /* 10 attempts at locking */
    struct flock lck;           /* fcntl locking scheme */

    if ((fd = open(fname, O_WRONLY | O_APPEND, 0)) < 0) {
        D_UTMP(("Warning:  Unable to open \"%s\" for writing -- %s\n", fname, strerror(errno)));
        return;
    }
    lck.l_whence = SEEK_END;    /* start lock at current eof */
    lck.l_len = 0;              /* end at ``largest possible eof'' */
    lck.l_start = 0;
    lck.l_type = F_WRLCK;       /* we want a write lock */

    /* attempt lock with F_SETLK - F_SETLKW would cause a deadlock! */
    while (retry--) {
        if ((fcntl(fd, F_SETLK, &lck) < 0) && errno != EACCESS) {
            D_UTMP(("Warning:  Unable to establish file lock on \"%s\" -- %s\n", fname, strerror(errno)));
            close(fd);
            return;             /* failed for unknown reason: give up */
        } else if (errno == EACCESS) {
            D_UTMP(("Warning:  Unable to establish file lock on \"%s\" -- %s\n", fname, strerror(errno)));
        }
    }

    write(fd, putmp, sizeof(struct utmp));

    /* unlocking the file */
    lck.l_type = F_UNLCK;
    fcntl(fd, F_SETLK, &lck);

    close(fd);
}
#  endif /* HAVE_UTMPX_H */

void
add_utmp_entry(const char *pty, const char *hostname, int fd)
{
    struct passwd *pwent = getpwuid(my_ruid);

#   ifdef HAVE_UTMPX_H
    struct utmpx utmp;
    struct utmp utmp2;
    MEMSET(&utmp, 0, sizeof(struct utmpx));
#   else
    struct utmp utmp;
    MEMSET(&utmp, 0, sizeof(struct utmp));
#   endif

#   ifdef WITH_DMALLOC
    return;
#   endif

    if (!strncmp(pty, "/dev/", 5))
        pty += 5;               /* skip /dev/ prefix */
    if (!strncmp(pty, "pty", 3) || !strncmp(pty, "tty", 3))
        strncpy(ut_id, (pty + 3), sizeof(ut_id));       /* bsd naming */
    else {
        int n;

        if (sscanf(pty, "pts/%d", &n) == 1)
            sprintf(ut_id, "vt%02x", n);        /* sysv naming */
        else {
            libast_print_error("can't parse tty name \"%s\"\n", pty);
            ut_id[0] = '\0';    /* entry not made */
            return;
        }
    }
    strncpy(utmp.ut_id, ut_id, sizeof(utmp.ut_id));
    utmp.ut_type = DEAD_PROCESS;

    privileges(INVOKE);
#   ifdef HAVE_UTMPX_H
    getutmp(&utmp, &utmp2);
    getutid(&utmp2);            /* position to entry in utmp file */
#   else
    getutid(&utmp);             /* position to entry in utmp file */
#   endif

    /* set up the new entry */
    strncpy(utmp.ut_id, ut_id, sizeof(utmp.ut_id));
    strncpy(utmp.ut_line, pty, sizeof(utmp.ut_line));
    strncpy(utmp.ut_name, pwent->pw_name, sizeof(utmp.ut_name));
    strncpy(utmp.ut_user, pwent->pw_name, sizeof(utmp.ut_user));
    strncpy(utmp.ut_host, hostname, sizeof(utmp.ut_host));
    utmp.ut_type = USER_PROCESS;
    utmp.ut_pid = getpid();
#   ifdef HAVE_UTMPX_H
    utmp.ut_session = getsid(0);
    utmp.ut_xtime = time(NULL);
    utmp.ut_tv.tv_usec = 0;
#   else
    utmp.ut_time = time(NULL);
#   endif

    /*
     * write a utmp entry to the utmp file
     */
    utmpname(UTMP_FILENAME);
#   ifdef HAVE_UTMPX_H
    getutmp(&utmp, &utmp2);
    pututline(&utmp2);
    pututxline(&utmp);
#   else
    pututline(&utmp);
#   endif
    update_wtmp(WTMP_FILENAME, &utmp);
    endutent();                 /* close the file */
    privileges(REVERT);
    return;
    fd = 0;
}

void
remove_utmp_entry(void)
{
#   ifdef HAVE_UTMPX_H
    struct utmp utmp;
    struct utmpx utmpx;

    if (!ut_id[0])
        return;                 /* entry not made */

    utmpname(UTMP_FILENAME);
    setutent();
    strncpy(utmp.ut_id, ut_id, sizeof(utmp.ut_id));
    utmp.ut_type = USER_PROCESS;
    if (!getutid(&utmp)) {
        return;
    }
    utmp.ut_type = DEAD_PROCESS;
    utmp.ut_time = time(NULL);
    pututline(&utmp);
    getutmpx(&utmp, &utmpx);
    update_wtmp(WTMP_FILENAME, &utmpx);
    endutent();

#   else /* HAVE_UTMPX_H */
    struct utmp *putmp;
    pid_t pid = getpid();

    if (!ut_id[0])
        return;                 /* entry not made */

    utmpname(UTMP_FILENAME);
    setutent();
    /*
     * The following code waw copied from the poeigl-1.20 login/init package.
     * Special thanks to poe for the code examples.
     */
    while ((putmp = getutent())) {
        if (putmp->ut_pid == pid) {
            putmp->ut_type = DEAD_PROCESS;
            putmp->ut_pid = 0;
            putmp->ut_user[0] = '\0';
            putmp->ut_time = time(NULL);
            pututline(putmp);
            update_wtmp(WTMP_FILENAME, putmp);
            break;
        }
    }
    endutent();
#   endif /* HAVE_UTMPX_H */
}

# else /* USE_SYSV_UTMP */
/* BSD utmp support */

#  ifdef NEW_BSD_UTMP

/* used to hold the line we are using */
static char ut_line[32];

static int
get_tslot(const char *ttyname)
{
    register struct ttyent *ttyp;
    register int slot;

    setttyent();
    for (slot = 1; (ttyp = getttyent()); ++slot)
        if (!strcmp(ttyp->ty_name, ttyname)) {
            endttyent();
            return (slot);
        }
    endttyent();
    return 0;
}

void
b_login(struct utmp *ut)
{
    /*
     ** replacement for freebsd's login(), which uses ttyslot()
     **
     ** like I shouldn't have just KNOWN that from the comment on get_tslot
     ** below...
     **            - brian
     */
    register int fd;
    int tty;

    tty = get_tslot(ut->ut_line);
    if (tty > 0 && (fd = open(_PATH_UTMP, O_WRONLY | O_CREAT, 0644)) >= 0) {
        (void) lseek(fd, (off_t) (tty * sizeof(struct utmp)), L_SET);
        (void) write(fd, ut, sizeof(struct utmp));

        (void) close(fd);
    }
    if ((fd = open(_PATH_WTMP, O_WRONLY | O_APPEND, 0)) >= 0) {
        (void) write(fd, ut, sizeof(struct utmp));

        (void) close(fd);
    }
}

#  else /* NEW_BSD_UTMP */
static int utmp_pos = 0;        /* position of utmp-stamp */

/*----------------------------------------------------------------------*
 * get_tslot() - grabbed from xvt-1.0 - modified by David Perry
 *
 * find ttyname in /etc/ttytab and return a slot number that can be used to
 * access the utmp file.  Can't use ttyslot() because the tty name is not
 * that of fd 0.
 *----------------------------------------------------------------------*/
static int
get_tslot(const char *ttyname)
{
    char buf[256], name[256];
    FILE *fd;

    if ((fd = fopen(UTMP_FILENAME, "r"))) {
        int i;

        for (i = 1; fgets(buf, sizeof(buf), fd); i++) {
            if (*buf == '#' || sscanf(buf, "%s", name) != 1)
                continue;
            if (!strcmp(ttyname, name)) {
                fclose(fd);
                return i;
            }
        }
        fclose(fd);
    }
    return -1;
}

/*
 * write utmp entry to UTMP_FILENAME
 */
static int
write_utmp(struct utmp *putmp)
{
    int rval = 0;
    FILE *fd;

    fprintf(stderr, "Utmp file is %s\n", UTMP_FILENAME);
    privileges(INVOKE);
    if ((fd = fopen(UTMP_FILENAME, "r+"))) {
        utmp_pos = get_tslot(putmp->ut_line) * sizeof(struct utmp);

        if (utmp_pos >= 0) {
            fseek(fd, utmp_pos, 0);
            fwrite(putmp, sizeof(struct utmp), 1, fd);

            rval = 1;
        }
        fclose(fd);
    }
    privileges(REVERT);
    return rval;
}

#  endif /* NEW_BSD_UTMP */

void
add_utmp_entry(const char *pty, const char *hostname, int fd)
{
    struct passwd *pwent = getpwuid(my_ruid);
    struct utmp utmp;

    MEMSET(&utmp, 0, sizeof(struct utmp));

    if (!strncmp(pty, "/dev/", 5))
        pty += 5;               /* skip /dev/ prefix */

#  ifdef NEW_BSD_UTMP
    strncpy(ut_line, pty, 31);

    strncpy(utmp.ut_line, pty, UT_LINESIZE);
    strncpy(utmp.ut_name, pwent->pw_name, UT_NAMESIZE);
    strncpy(utmp.ut_host, hostname, UT_HOSTSIZE);
    utmp.ut_time = time(NULL);

    b_login(&utmp);
#  else /* NEW_BSD_UTMP */
    if (!strncmp(pty, "pty", 3) || !strncmp(pty, "tty", 3))
        strncpy(ut_id, (pty + 3), sizeof(ut_id));       /* bsd naming */
    else {
        libast_print_error("can't parse tty name \"%s\"\n", pty);
        ut_id[0] = '\0';        /* entry not made */
        return;
    }

    strncpy(utmp.ut_line, ut_id, sizeof(utmp.ut_line));
    strncpy(utmp.ut_name, pwent->pw_name, sizeof(utmp.ut_name));
    strncpy(utmp.ut_host, hostname, sizeof(utmp.ut_host));
    utmp.ut_time = time(NULL);

    if (write_utmp(&utmp) < 0)
        ut_id[0] = '\0';        /* entry not made */
#  endif
    return;
    fd = 0;
}

/*
 * remove a utmp entry
 */
void
remove_utmp_entry(void)
{
#  ifdef NEW_BSD_UTMP
    logout(ut_line);
    logwtmp(ut_line, "", "");
#  else /* NEW_BSD_UTMP */
    FILE *fd;

    privileges(INVOKE);
    if (!ut_id[0] && (fd = fopen(UTMP_FILENAME, "r+"))) {
        struct utmp utmp;
        MEMSET(&utmp, 0, sizeof(struct utmp));

        fseek(fd, utmp_pos, 0);
        fwrite(&utmp, sizeof(struct utmp), 1, fd);

        fclose(fd);
    }
    privileges(REVERT);
#  endif /* NEW_BSD_UTMP */
}

# endif /* USE_SYSV_UTMP */

#endif /* UTMP_SUPPORT */
