/*
 * Post-process CVS diff output to be capable of feeding it into patch.
 * This code is very very strictly tied to CVS output.
 *
 * Danny Backx (u27113@kb.be), 12/6/1997.
 *
 * Input example #1
 * ===================================================================
 * RCS file: /usr/local/hungryCVS/hungry/lesstif/KNOWN_BUGS,v
 * retrieving revision 1.12
 * retrieving revision 1.14
 * diff -c -r1.12 -r1.14
 * *** KNOWN_BUGS       1997/06/10 22:00:51     1.12
 * --- KNOWN_BUGS       1997/06/11 18:38:44     1.14
 * ***************
 *
 * This must become :
 *
 * ===================================================================
 * RCS file: /usr/local/hungryCVS/hungry/lesstif/KNOWN_BUGS,v
 * retrieving revision 1.12
 * retrieving revision 1.14
 * diff -c -r1.12 -r1.14
 * *** /usr/local/hungryCVS/hungry/lesstif/KNOWN_BUGS   1997/06/10 22:00:51     1.12
 * *** /usr/local/hungryCVS/hungry/lesstif/KNOWN_BUGS   1997/06/11 18:38:44     1.14
 * ***************
 *
 * so it can be fed into "patch -p5" if that command is run from the lesstif/
 * subdirectory.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define	LEN	512

int 
main()
{
    char line[LEN], *s, *p;
    char fname[LEN];
    int n;

    while ((s = fgets(line, LEN, stdin)) != NULL)
    {
	if (strncmp(line, "Index: ", 7) == 0)
	{
	char prefix[5];

	    /* Found file */
	    n = strlen(line) - 7 - 1;	/* 1 is strlen("\n") */
	    strncpy(fname, line + 7, n);
	    fname[n] = '\0';

	    printf("%s", line);	/* Print it */
	    /* Just copy lines until you see *** at the beginning */
	    while (1)
	    {
		if ((s = fgets(line, LEN, stdin)) == NULL)
		{
		    perror("fgets a");
		    exit(1);
		}
		if ((strncmp(line, "*** ", 4) == 0) || (strncmp(line, "--- ", 4) == 0))
		    break;
		printf("%s", line);
	    }
	    /* Now process two special lines */
	    if ((p = strchr(line + 4, '\t')) == NULL)
	    {
		perror("strchr");
		fprintf(stderr, "Line was '%s'\n", line);
		exit(1);
	    }
	    strncpy(prefix, line, 3);
	    prefix[3] = '\0';
	    printf("%s %s%s", prefix, fname, p);
	    if ((s = fgets(line, LEN, stdin)) == NULL)
	    {
		perror("fgets b");
		exit(1);
	    }
	    if ((p = strchr(line + 4, '\t')) == NULL)
	    {
		perror("strchr");
		fprintf(stderr, "Line was '%s'\n", line);
		exit(1);
	    }
	    strncpy(prefix, line, 3);
	    prefix[3] = '\0';
	    printf("%s %s%s", prefix, fname, p);
	}
	else
	{
	    printf("%s", line);
	}
    }
    exit(0);
}
