#ifndef __ASM_H
#define __ASM_H

#ifdef __EMX__
/* Due to strange behaviour of as.exe we use this macros */
/* For all OS/2 coders - please use PGCC to compile this code */
# define PR_(sym) ___##sym
#else
# define PR_(sym) __##sym
#endif

#if defined(__GNUC__) && (__GNUC__ >= 4)
# define HIDDEN_(sym) .hidden PR_(sym)
#else
# define HIDDEN_(sym)
#endif

#define FN_(sym) \
	.global PR_(sym); \
	HIDDEN_(sym); \
	.type PR_(sym),@function;
#define SIZE(sym) \
	.size PR_(sym),.-PR_(sym); \
	.align 8;

#endif /* __ASM_H */
