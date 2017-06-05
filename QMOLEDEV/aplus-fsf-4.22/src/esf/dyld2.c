/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/

#include <sys/param.h>
#if !defined(__APPLE__)
#include <dlfcn.h>
#endif
#if defined(__NetBSD__) || defined(__FreeBSD) || defined (__APPLE__)
#include <stdlib.h>
#else
#include <malloc.h>
#endif

#include <stdio.h>
#include <string.h>
#include <a/k.h>
#include <a/x.h>
#include <a/fir.h>
extern void dymeSet();
extern void dyld2Install();
extern I dbg_tdyld;

static int dyld2();

void dyldSlowInstall(argv0)
char *argv0;
 {
  dymeSet(2);
  install(dyld2,"_dyld", IV, 2, CP, A_,0,0,0,0,0,0);

  return;
 }

static int dyld2(sharedobj, bindings)
  char *sharedobj;	/* Name of shared object to load in */
  A bindings;		/* Bindings from c/f77 to A names/types */
{
  A *ap;		/* Points to elements of bindings */
  I nentries;		/* Number of entries */
  I i;		        /* Loop index */
  void *handle;		/* handle for shared object */
  void **locations;	/* locations of symbols in shared object */

  int status;
  char cmd_str[2*MAXPATHLEN + 2048];
  char *library_file;
#if defined(_AIX)
  static int dlopenMode=RTLD_MEMBER;
#else
  static int dlopenMode=1;
#endif

  NDC1(bindings);
  if(dbg_tdyld)dyldtrc(sharedobj);

  /* Open shared object */
  if ((handle = (void *)dlopen(sharedobj, dlopenMode)) == NULL) {
    (void)fprintf(stderr, "dlopen(%s,1):%s\n", sharedobj, dlerror());
    return -1;
  }
  
  /* Resolve symbols */
  nentries = bindings->n / 3;
  if ((locations = (void **)malloc(nentries * sizeof(*locations))) == NULL) {
    (void)fprintf(stderr, "Not enough memory for dynamic load\n");
    (void)dlclose(handle);
    return -1;
  }
  for (ap = (A*)bindings->p, i = 0; i < nentries; i++, ap += 3) {
    if ((locations[i] = (void *)dlsym(handle, (char *)(*ap)->p)) == NULL) {
      (void)fprintf(stderr, "dlsym:%s:%s\n", (char *)(*ap)->p, dlerror());
      (void)free((char *)locations);
      (void)dlclose(handle);
      return -1;
    }
  }
  for (ap = (A*)bindings->p, i = 0; i < nentries; i++, ap += 3) {
    xinstall(locations[i],
	     (char *)ap[1]->p,
	     *ap[2]->p,
	     ap[2]->n-1,
	     ap[2]->p+1,(C *)0);
  }
  (void)free((char *)locations);
  return 0;
}

void MAIN_(){}	/* This is needed to satisfy libF77.so */

