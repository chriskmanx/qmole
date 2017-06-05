/* $Header: /cvsroot/lesstif/lesstif/test/Xm/filesb/test14.c,v 1.4 2001/06/15 09:17:36 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <pwd.h>

#include <Xm/XmosP.h>


static int ExitStatus = 0;

static void
DoTest(String dir, String filter, String e_dir, String e_filter)
{
    String pd, pf;

    _XmOSQualifyFileSpec(dir, filter, &pd, &pf);
    if (strcmp(pd, e_dir) != 0 || strcmp(pf, e_filter) != 0)
    {
    	ExitStatus = 1;
    }
    printf("%s %s %s %s %s %s %s\n",
    	pd, strcmp(pd, e_dir) == 0 ? "==" : "!=", e_dir,
    	pf, strcmp(pf, e_filter) == 0 ? "==" : "!=", e_filter,
    	strcmp(pd, e_dir) == 0 && strcmp(pf, e_filter) == 0 ? "okay" : "fail");
    XtFree(pd);
    XtFree(pf);
}


int
main(int argc, char *argv[])
{
      String d, f;
      String pd, pf;
      String cwd;
      struct passwd *root_pw, *user_pw;
      String root_dir, root_dir1, user_dir, user_name;

	cwd = getcwd(NULL, 2048);
	cwd = XtRealloc(cwd, strlen(cwd) + 2);
	strcat(cwd, "/");

	root_pw = getpwnam("root");
	root_dir = XtMalloc(strlen(root_pw->pw_dir) + 2);
	strcpy(root_dir, root_pw->pw_dir);
	strcat(root_dir, "/");
	root_dir1 = XtMalloc(strlen(root_pw->pw_dir) + 6);
	strcpy(root_dir1, root_pw->pw_dir);
	strcat(root_dir1, "/tmp/");

	user_pw = getpwuid(getuid());
	user_dir = XtMalloc(strlen(user_pw->pw_dir) + 2);
	strcpy(user_dir, user_pw->pw_dir);
	strcat(user_dir, "/");
	user_name = XtMalloc(strlen(user_pw->pw_name) + 2);
	strcpy(user_name, "~");
	strcat(user_name, user_pw->pw_name);

	DoTest(NULL, NULL, cwd, "*");
	DoTest("", "", cwd, "*");
	DoTest("~", "*", user_dir, "*");
	DoTest(user_name, "*", user_dir, "*");
	DoTest("~root", "*", root_dir, "*");
	DoTest("~unknown_user", "*", "/", "*");
	DoTest("/", "*", "/", "*");
	DoTest("~root/tmp", "*", root_dir1, "*");
	exit(ExitStatus);
}
