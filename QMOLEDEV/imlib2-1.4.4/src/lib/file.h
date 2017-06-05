#ifndef __FILE
#define __FILE 1

__hidden char               *__imlib_FileKey(const char *file);
__hidden char               *__imlib_FileRealFile(const char *file);
__hidden char               *__imlib_FileExtension(const char *file);
__hidden int                 __imlib_FileExists(const char *s);
__hidden int                 __imlib_FileIsFile(const char *s);
__hidden int                 __imlib_FileIsDir(const char *s);
__hidden char              **__imlib_FileDir(char *dir, int *num);
__hidden void                __imlib_FileFreeDirList(char **l, int num);
__hidden void                __imlib_FileDel(char *s);
__hidden time_t              __imlib_FileModDate(const char *s);
__hidden char               *__imlib_FileHomeDir(int uid);
__hidden int                 __imlib_FilePermissions(const char *s);
__hidden int                 __imlib_FileCanRead(const char *s);
__hidden int                 __imlib_IsRealFile(const char *s);

#endif
