#ifndef included_dap_remprog_h
#define included_dap_remprog_h

/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/


/* external struct, union, typedef and enum declarations */
typedef struct remprog {
	char	*rp_host;
	long	rp_prognum;
	char	*rp_proto;
} remprog_t;

typedef struct {
	int		count;
	int		index;
	char		*name;
	char		*host;
	char		*protocol;
	long		program;
	remprog_t	*rp, *rpp;
} ServEnt;

#if defined(__STDC__) || defined(__cplusplus) || defined(_AIX)
# ifdef __cplusplus
extern "C" {
# endif
  extern int GetService( char * );
  extern int NextService( char **, long *, char ** );
  extern int NextServ( ServEnt * );
  extern int get_service_by_name( char *,char **,long *, char ** );
  extern ServEnt *GetServ( char * );
# ifdef __cplusplus
}
# endif
#else
  extern int GetService();
  extern int NextService();
  extern int NextServ();
  extern int get_service_by_name();
  extern ServEnt *GetServ();
#endif

#endif

