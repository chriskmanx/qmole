#include "loader_common.h"

#define SWAP32(x) (x) = \
((((x) & 0x000000ff ) << 24) |\
 (((x) & 0x0000ff00 ) << 8) |\
 (((x) & 0x00ff0000 ) >> 8) |\
 (((x) & 0xff000000 ) >> 24))

char
load(ImlibImage * im, ImlibProgressFunction progress,
     char progress_granularity, char immediate_load)
{
   int                 w = 0, h = 0, alpha = 0;
   FILE               *f;

   if (im->data)
      return 0;
   f = fopen(im->real_file, "rb");
   if (!f)
      return 0;

   /* header */
   {
      char                buf[256], buf2[256];

      buf[0] = '\0';
      if (!fgets(buf, 255, f))
        {
           fclose(f);
           return 0;
        }
      buf2[0] = '\0';
      sscanf(buf, "%s %i %i %i", buf2, &w, &h, &alpha);
      if (strcmp(buf2, "ARGB"))
        {
           fclose(f);
           return 0;
        }
      if (!IMAGE_DIMENSIONS_OK(w, h))
	{
           fclose(f);
           return 0;
	}
      im->w = w;
      im->h = h;
      if (!im->format)
        {
           if (alpha)
              SET_FLAG(im->flags, F_HAS_ALPHA);
           else
              UNSET_FLAG(im->flags, F_HAS_ALPHA);
           im->format = strdup("argb");
        }
   }
   if (((!im->data) && (im->loader)) || (immediate_load) || (progress))
     {
        DATA32             *ptr;
        int                 y, pl = 0;
        char                pper = 0;

        /* must set the im->data member before callign progress function */
        ptr = im->data = malloc(w * h * sizeof(DATA32));
        if (!im->data)
          {
             fclose(f);
             return 0;
          }
        for (y = 0; y < h; y++)
          {
#ifdef WORDS_BIGENDIAN
             {
                int                 x;

                fread(ptr, im->w, 4, f);
                for (x = 0; x < im->w; x++)
                   SWAP32(ptr[x]);
             }
#else
             fread(ptr, im->w, 4, f);
#endif
             ptr += im->w;
             if (progress)
               {
                  char                per;
                  int                 l;

                  per = (char)((100 * y) / im->h);
                  if (((per - pper) >= progress_granularity) ||
                      (y == (im->h - 1)))
                    {
                       l = y - pl;
                       if (!progress(im, per, 0, (y - l), im->w, l))
                         {
                            fclose(f);
                            return 2;
                         }
                       pper = per;
                       pl = y;
                    }
               }
          }
     }
   fclose(f);
   return 1;
}

char
save(ImlibImage * im, ImlibProgressFunction progress, char progress_granularity)
{
   FILE               *f;
   DATA32             *ptr;
   int                 y, pl = 0, alpha = 0;
   char                pper = 0;

#ifdef WORDS_BIGENDIAN
   DATA32             *buf = (DATA32 *) malloc(im->w * 4);
#endif

   /* no image data? abort */
   if (!im->data)
      return 0;
   f = fopen(im->real_file, "wb");
   if (!f)
      return 0;
   if (im->flags & F_HAS_ALPHA)
      alpha = 1;
   fprintf(f, "ARGB %i %i %i\n", im->w, im->h, alpha);
   ptr = im->data;
   for (y = 0; y < im->h; y++)
     {
#ifdef WORDS_BIGENDIAN
        {
           int                 x;

           memcpy(buf, ptr, im->w * 4);
           for (x = 0; x < im->w; x++)
              SWAP32(buf[x]);
           fwrite(buf, im->w, 4, f);
        }
#else
        fwrite(ptr, im->w, 4, f);
#endif
        ptr += im->w;
        if (progress)
          {
             char                per;
             int                 l;

             per = (char)((100 * y) / im->h);
             if (((per - pper) >= progress_granularity) || (y == (im->h - 1)))
               {
                  l = y - pl;
                  if (!progress(im, per, 0, (y - l), im->w, l))
                    {
#ifdef WORDS_BIGENDIAN
                       if (buf)
                          free(buf);
#endif
                       fclose(f);
                       return 2;
                    }
                  pper = per;
                  pl = y;
               }
          }
     }
   /* finish off */
#ifdef WORDS_BIGENDIAN
   if (buf)
      free(buf);
#endif
   fclose(f);
   return 1;
}

void
formats(ImlibLoader * l)
{
   char               *list_formats[] = { "argb", "arg" };

   {
      int                 i;

      l->num_formats = (sizeof(list_formats) / sizeof(char *));
      l->formats = malloc(sizeof(char *) * l->num_formats);
      for (i = 0; i < l->num_formats; i++)
         l->formats[i] = strdup(list_formats[i]);
   }
}
