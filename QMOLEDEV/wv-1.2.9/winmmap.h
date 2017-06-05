#ifndef _MMAP_WIN_H
#define _MMAP_WIN_H
#include <gsf/gsf.h>
#include <sys/types.h>
#include <stddef.h>
#include <io.h>
/*mmap section got from imagick sources*/

#if defined _WIN32
#define PROT_READ  1
#define PROT_WRITE  2
#define PROT_READWRITE  3
#define MAP_SHARED  1
#define MAP_PRIVATE  2
#define F_OK 0
#define R_OK 4
#define W_OK 2
#define RW_OK 6

#if !defined(MAP_FAILED)
#define MAP_FAILED      ((void *) -1)
#endif

void *mmap(char *,size_t,int,int,int,gsf_off_t);
int   munmap(void *,size_t);
#endif
#endif
