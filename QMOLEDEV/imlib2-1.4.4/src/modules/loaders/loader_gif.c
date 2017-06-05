#include "loader_common.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <gif_lib.h>

char
load(ImlibImage * im, ImlibProgressFunction progress, char progress_granularity,
     char immediate_load)
{
   DATA32             *ptr;
   GifFileType        *gif;
   GifRowType         *rows;
   GifRecordType       rec;
   ColorMapObject     *cmap;
   int                 i, j, done, bg, r, g, b, w = 0, h = 0;
   float               per = 0.0, per_inc;
   int                 last_per = 0, last_y = 0;
   int                 intoffset[] = { 0, 4, 2, 1 };
   int                 intjump[] = { 8, 8, 4, 2 };
   int                 transp;
   int                 fd;

   done = 0;
   rows = NULL;
   transp = -1;

   /* if immediate_load is 1, then dont delay image laoding as below, or */
   /* already data in this image - dont load it again */
   if (im->data)
      return 0;
#ifndef __EMX__
   fd = open(im->real_file, O_RDONLY);
#else
   fd = open(im->real_file, O_RDONLY | O_BINARY);
#endif
   if (fd < 0)
      return 0;
   gif = DGifOpenFileHandle(fd);
   if (!gif)
     {
        close(fd);
        return 0;
     }
   do
     {
        if (DGifGetRecordType(gif, &rec) == GIF_ERROR)
          {
             /* PrintGifError(); */
             rec = TERMINATE_RECORD_TYPE;
          }
        if ((rec == IMAGE_DESC_RECORD_TYPE) && (!done))
          {
             if (DGifGetImageDesc(gif) == GIF_ERROR)
               {
                  /* PrintGifError(); */
                  rec = TERMINATE_RECORD_TYPE;
               }
             w = gif->Image.Width;
             h = gif->Image.Height;
	     if (!IMAGE_DIMENSIONS_OK(w, h))
	       {
                  DGifCloseFile(gif);
                  return 0;
	       }
             rows = malloc(h * sizeof(GifRowType *));
             if (!rows)
               {
                  DGifCloseFile(gif);
                  return 0;
               }
             for (i = 0; i < h; i++)
               {
                  rows[i] = NULL;
               }
             for (i = 0; i < h; i++)
               {
                  rows[i] = malloc(w * sizeof(GifPixelType));
                  if (!rows[i])
                    {
                       DGifCloseFile(gif);
                       for (i = 0; i < h; i++)
                         {
                            if (rows[i])
                              {
                                 free(rows[i]);
                              }
                         }
                       free(rows);
                       return 0;
                    }
               }
             if (gif->Image.Interlace)
               {
                  for (i = 0; i < 4; i++)
                    {
                       for (j = intoffset[i]; j < h; j += intjump[i])
                         {
                            DGifGetLine(gif, rows[j], w);
                         }
                    }
               }
             else
               {
                  for (i = 0; i < h; i++)
                    {
                       DGifGetLine(gif, rows[i], w);
                    }
               }
             done = 1;
          }
        else if (rec == EXTENSION_RECORD_TYPE)
          {
             int                 ext_code;
             GifByteType        *ext;

             ext = NULL;
             DGifGetExtension(gif, &ext_code, &ext);
             while (ext)
               {
                  if ((ext_code == 0xf9) && (ext[1] & 1) && (transp < 0))
                    {
                       transp = (int)ext[4];
                    }
                  ext = NULL;
                  DGifGetExtensionNext(gif, &ext);
               }
          }
   } while (rec != TERMINATE_RECORD_TYPE);
   if (transp >= 0)
     {
        SET_FLAG(im->flags, F_HAS_ALPHA);
     }
   else
     {
        UNSET_FLAG(im->flags, F_HAS_ALPHA);
     }
   /* set the format string member to the lower-case full extension */
   /* name for the format - so example names would be: */
   /* "png", "jpeg", "tiff", "ppm", "pgm", "pbm", "gif", "xpm" ... */
   im->w = w;
   im->h = h;
   if (!im->format)
      im->format = strdup("gif");
   if (im->loader || immediate_load || progress)
     {
        bg = gif->SBackGroundColor;
        cmap = (gif->Image.ColorMap ? gif->Image.ColorMap : gif->SColorMap);
        im->data = (DATA32 *) malloc(sizeof(DATA32) * w * h);
        if (!im->data)
          {
             DGifCloseFile(gif);
             free(rows);
             return 0;
          }
        ptr = im->data;
        per_inc = 100.0 / (((float)w) * h);
        for (i = 0; i < h; i++)
          {
             for (j = 0; j < w; j++)
               {
                  if (rows[i][j] == transp)
                    {
                       r = cmap->Colors[bg].Red;
                       g = cmap->Colors[bg].Green;
                       b = cmap->Colors[bg].Blue;
                       *ptr++ = 0x00ffffff & ((r << 16) | (g << 8) | b);
                    }
                  else
                    {
                       r = cmap->Colors[rows[i][j]].Red;
                       g = cmap->Colors[rows[i][j]].Green;
                       b = cmap->Colors[rows[i][j]].Blue;
                       *ptr++ = (0xff << 24) | (r << 16) | (g << 8) | b;
                    }
                  per += per_inc;
                  if (progress && (((int)per) != last_per)
                      && (((int)per) % progress_granularity == 0))
                    {
                       last_per = (int)per;
                       if (!(progress(im, (int)per, 0, last_y, w, i)))
                         {
                            DGifCloseFile(gif);
                            for (i = 0; i < h; i++)
                              {
                                 free(rows[i]);
                              }
                            free(rows);
                            return 2;
                         }
                       last_y = i;
                    }
               }
          }
     }
   if (progress)
     {
        progress(im, 100, 0, last_y, w, h);
     }
   DGifCloseFile(gif);
   for (i = 0; i < h; i++)
     {
        free(rows[i]);
     }
   free(rows);
   return 1;
}

/* fills the ImlibLoader struct with a strign array of format file */
/* extensions this loader can load. eg: */
/* loader->formats = { "jpeg", "jpg"}; */
/* giving permutations is a good idea. case sensitivity is irrelevant */
/* your laoder CAN load more than one format if it likes - like: */
/* loader->formats = { "gif", "png", "jpeg", "jpg"} */
/* if it can load those formats. */
void
formats(ImlibLoader * l)
{
   /* this is the only bit you have to change... */
   char               *list_formats[] = { "gif" };

   /* don't bother changing any of this - it just reads this in and sets */
   /* the struct values and makes copies */
   {
      int                 i;

      l->num_formats = (sizeof(list_formats) / sizeof(char *));
      l->formats = malloc(sizeof(char *) * l->num_formats);
      for (i = 0; i < l->num_formats; i++)
         l->formats[i] = strdup(list_formats[i]);
   }
}
