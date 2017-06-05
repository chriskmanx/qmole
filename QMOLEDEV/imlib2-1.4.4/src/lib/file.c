#include "common.h"
#include <stdio.h>
#include <ctype.h>
#ifdef __EMX__
#include <sys/types.h>
#endif
#include <sys/stat.h>
#include <unistd.h>
#include <sys/types.h>
#include <dirent.h>
#include <string.h>
#include <stdlib.h>
#include <pwd.h>
#include "file.h"

char               *
__imlib_FileKey(const char *file)
{
   char               *newfile;

   newfile = malloc(strlen(file) + 1);
   if (!newfile)
      return NULL;
   newfile[0] = 0;
   {
      char               *p1, *p2;
      int                 go;

      go = 0;
      p1 = (char *)file;
      p2 = newfile;
      while (p1[0])
        {
           if (go)
             {
                p2[0] = p1[0];
                p2++;
             }
           if ((p1[0] == ':') && (p1[1] != ':'))
              go = 1;
           if ((p1[0] == ':') && (p1[1] == ':'))
              p1++;
           p1++;
        }
      p2[0] = p1[0];
   }
   if (newfile[0])
      return newfile;
   else
      free(newfile);
   return NULL;
}

char               *
__imlib_FileRealFile(const char *file)
{
   char               *newfile;

   newfile = malloc(strlen(file) + 1);
   if (!newfile)
      return NULL;
   newfile[0] = 0;
   {
      char               *p1, *p2;

      p1 = (char *)file;
      p2 = newfile;
      while (p1[0])
        {
           if (p1[0] == ':')
             {
                if (p1[1] == ':')
                  {
                     p2[0] = ':';
                     p2++;
                     p1++;
                  }
                else
                  {
                     p2[0] = 0;
                     return newfile;
                  }
             }
           else
             {
                p2[0] = p1[0];
                p2++;
             }
           p1++;
        }
      p2[0] = p1[0];
   }
   return newfile;
}

char               *
__imlib_FileExtension(const char *file)
{
   char               *p;
   char               *fl;

   fl = __imlib_FileRealFile(file);
   if (!fl)
      return strdup("");
   p = strrchr(file, '.');
   if (p)
     {
        char               *ret;

        ret = strdup(p + 1);
        free(fl);
        return ret;
     }
   free(fl);
   return strdup("");
}

int
__imlib_FileExists(const char *s)
{
   struct stat         st;
   char               *fl;

   if ((!s) || (!*s))
      return 0;
   if (__imlib_IsRealFile(s))
      fl = strdup(s);
   else
      fl = __imlib_FileRealFile(s);
   if (!fl)
      return 0;
   if (stat(fl, &st) < 0)
     {
        free(fl);
        return 0;
     }
   free(fl);
   return 1;
}

int
__imlib_FileIsFile(const char *s)
{
   struct stat         st;
   char               *fl;

   if ((!s) || (!*s))
      return 0;
   if (__imlib_IsRealFile(s))
      fl = strdup(s);
   else
      fl = __imlib_FileRealFile(s);
   if (!fl)
      return 0;
   if (stat(fl, &st) < 0)
     {
        free(fl);
        return 0;
     }
   if (S_ISREG(st.st_mode))
     {
        free(fl);
        return 1;
     }
   free(fl);
   return 0;
}

int
__imlib_FileIsDir(const char *s)
{
   struct stat         st;
   char               *fl;

   if ((!s) || (!*s))
      return 0;
   if (__imlib_IsRealFile(s))
      fl = strdup(s);
   else
      fl = __imlib_FileRealFile(s);
   if (!fl)
      return 0;
   if (stat(fl, &st) < 0)
     {
        free(fl);
        return 0;
     }
   if (S_ISDIR(st.st_mode))
     {
        free(fl);
        return 1;
     }
   free(fl);
   return 0;
}

int
__imlib_FilePermissions(const char *s)
{
   struct stat         st;
   char               *fl;

   if ((!s) || (!*s))
      return 0;
   if (__imlib_IsRealFile(s))
      fl = strdup(s);
   else
      fl = __imlib_FileRealFile(s);
   if (!fl)
      return 0;
   if (stat(fl, &st) < 0)
     {
        free(fl);
        return 0;
     }
   free(fl);
   return st.st_mode;
}

int
__imlib_FileCanRead(const char *s)
{
   char               *fl;
   int                 val;

   if (__imlib_IsRealFile(s))
      fl = strdup(s);
   else
      fl = __imlib_FileRealFile(s);
   if (!fl)
      return 0;
   if (!(__imlib_FilePermissions(fl) & (S_IRUSR | S_IRGRP | S_IROTH)))
     {
        free(fl);
        return 0;
     }

   val = (1 + access(fl, R_OK));
   free(fl);
   return val;
}

char              **
__imlib_FileDir(char *dir, int *num)
{
   int                 i, dirlen;
   int                 done = 0;
   DIR                *dirp;
   char              **names;
   struct dirent      *dp;

   if ((!dir) || (!*dir))
      return (0);
   dirp = opendir(dir);
   if (!dirp)
     {
        *num = 0;
        return (NULL);
     }
   /* count # of entries in dir (worst case) */
   for (dirlen = 0; (dp = readdir(dirp)) != NULL; dirlen++);
   if (!dirlen)
     {
        closedir(dirp);
        *num = dirlen;
        return (NULL);
     }
   names = (char **)malloc(dirlen * sizeof(char *));

   if (!names)
      return (NULL);

   rewinddir(dirp);
   for (i = 0; i < dirlen;)
     {
        dp = readdir(dirp);
        if (!dp)
           break;
        if ((strcmp(dp->d_name, ".")) && (strcmp(dp->d_name, "..")))
          {
             names[i] = strdup(dp->d_name);
             i++;
          }
     }

   if (i < dirlen)
      dirlen = i;               /* dir got shorter... */
   closedir(dirp);
   *num = dirlen;
   /* do a simple bubble sort here to alphanumberic it */
   while (!done)
     {
        done = 1;
        for (i = 0; i < dirlen - 1; i++)
          {
             if (strcmp(names[i], names[i + 1]) > 0)
               {
                  char               *tmp;

                  tmp = names[i];
                  names[i] = names[i + 1];
                  names[i + 1] = tmp;
                  done = 0;
               }
          }
     }
   return (names);
}

void
__imlib_FileFreeDirList(char **l, int num)
{
   if (!l)
      return;
   while (num--)
      if (l[num])
         free(l[num]);
   free(l);
   return;
}

void
__imlib_FileDel(char *s)
{
   if ((!s) || (!*s))
      return;
   unlink(s);
   return;
}

int
__imlib_IsRealFile(const char *s)
{
   struct stat         st;

   return ((stat(s, &st) != -1) && (S_ISREG(st.st_mode)));
}

time_t
__imlib_FileModDate(const char *s)
{
   struct stat         st;
   char               *fl;

   if ((!s) || (!*s))
      return 0;
   if (__imlib_IsRealFile(s))
      fl = strdup(s);
   else
      fl = __imlib_FileRealFile(s);
   if (!fl)
      return 0;
   if (stat(fl, &st) < 0)
     {
        free(fl);
        return 0;
     }
   if (st.st_mtime > st.st_ctime)
     {
        free(fl);
        return st.st_mtime;
     }
   free(fl);
   return st.st_ctime;
}

char               *
__imlib_FileHomeDir(int uid)
{
   static int          usr_uid = -1;
   static char        *usr_s = NULL;
   char               *s;
   struct passwd      *pwd;

#ifndef __EMX__
   s = getenv("HOME");
   if (s)
      return strdup(s);
   if (usr_uid < 0)
      usr_uid = getuid();
   if ((uid == usr_uid) && (usr_s))
     {
        return (strdup(usr_s));
     }
   pwd = getpwuid(uid);
   if (pwd)
     {
        s = strdup(pwd->pw_dir);
        if (uid == usr_uid)
           usr_s = strdup(s);
        return (s);
     }
#else
   if ((s = getenv("HOME")) != NULL)
      return strdup(s);
   else if ((s = getenv("TMP")) != NULL)
      return strdup(s);
#endif
   return NULL;
}
