#ifndef included_dap_notsunos4_h
#define included_dap_notsunos4_h

/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/


/* header file inclusions */

#ifndef FD_SET
#	ifdef _AIX
#		include <sys/select.h>
#	else

		/* external macro declarations */
#		define PRESUNOS4				/* Pre-SunOS 4.0 */

		/* The following is taken from <sys/types.h> on SunOS 4.0.
		 * It is necessary because fd_set's are not used.
		 */
#		define	NBBY	8		/* number of bits in a byte */
		/*
		 * Select uses bit masks of file descriptors in longs.
		 * These macros manipulate such bit fields (the filesystem
		 * macros use chars).  FD_SETSIZE may be defined by the user,
		 * but the default here should be >= NOFILE (param.h).
		 */
#		ifndef	FD_SETSIZE
			/* this coincides with SUNOS 3.5 definition of fd_set */
#			define	FD_SETSIZE	(sizeof(int) * NBBY)
#		endif

#		define	NFDBITS	(sizeof(fd_mask) * NBBY)	/* bits per mask */
#		ifndef	howmany
#			ifdef sun386
#				define howmany(x, y) \
						((((unsigned int)(x))+(((unsigned int)(y))-1))/((unsigned int)(y)))
#			else
#				define howmany(x, y) (((x)+((y)-1))/(y))
#			endif
#		endif

#		define	FD_SET(n, p) \
				((p)->fds_bits[(n)/NFDBITS] |= (1 << ((n) % NFDBITS)))
#		define	FD_CLR(n, p) \
				((p)->fds_bits[(n)/NFDBITS] &= ~(1 << ((n) % NFDBITS)))
#		define	FD_ISSET(n, p) \
				((p)->fds_bits[(n)/NFDBITS] & (1 << ((n) % NFDBITS)))
#		define	FD_ZERO(p)	bzero((char *)(p), sizeof(*(p)))

		/* external struct, union, typedef and enum declarations */
		/* this coincides with SUNOS 3.5 usage, SUNOS 4.0 uses long */
		typedef	int	fd_mask;
#	endif

#endif

#endif

