#include "loader_common.h"
#include <ctype.h>

char
load(ImlibImage * im, ImlibProgressFunction progress,
     char progress_granularity, char immediate_load)
{
   char                p = ' ', numbers = 3, count = 0;
   int                 w = 0, h = 0, v = 255, c = 0;
   char                buf[256];
   FILE               *f = NULL;

   if (im->data)
      return 0;
   f = fopen(im->real_file, "rb");
   if (!f)
      return 0;

   /* can't use fgets(), because there might be
    * binary data after the header and there
    * needn't be a newline before the data, so
    * no chance to distinguish between end of buffer
    * and a binary 0.
    */

   /* read the header info */

   c = fgetc(f);
   if (c != 'P')
     {
        fclose(f);
        return 0;
     }

   p = fgetc(f);
   if (p == '1' || p == '4')
      numbers = 2;              /* bitimages don't have max value */

   if ((p < '1') || (p > '8'))
     {
        fclose(f);
        return 0;
     }
   count = 0;
   while (count < numbers)
     {
        c = fgetc(f);

        if (c == EOF)
          {
             fclose(f);
             return 0;
          }

        /* eat whitespace */
        while (isspace(c))
           c = fgetc(f);
        /* if comment, eat that */
        if (c == '#')
          {
             do
                c = fgetc(f);
             while (c != '\n' && c != EOF);
          }
        /* no comment -> proceed */
        else
          {
             int                 i = 0;

             /* read numbers */
             while (c != EOF && !isspace(c) && (i < 255))
               {
                  buf[i++] = c;
                  c = fgetc(f);
               }
             if (i)
               {
                  buf[i] = 0;
                  count++;
                  switch (count)
                    {
                         /* width */
                      case 1:
                         w = atoi(buf);
                         break;
                         /* height */
                      case 2:
                         h = atoi(buf);
                         break;
                         /* max value, only for color and greyscale */
                      case 3:
                         v = atoi(buf);
                         break;
                    }
               }
          }
     }
   if ((v < 0) || (v > 255))
     {
        fclose(f);
        return 0;
     }

   im->w = w;
   im->h = h;
   if (!IMAGE_DIMENSIONS_OK(w, h))
     {
	fclose(f);
	return 0;
     }
   if (!im->format)
     {
        if (p == '8')
           SET_FLAG(im->flags, F_HAS_ALPHA);
        else
           UNSET_FLAG(im->flags, F_HAS_ALPHA);
        im->format = strdup("pnm");
     }

   if (((!im->data) && (im->loader)) || (immediate_load) || (progress))
     {
        DATA8              *data = NULL;        /* for the binary versions */
        DATA8              *ptr = NULL;
        int                *idata = NULL;       /* for the ASCII versions */
        int                *iptr;
        char                buf2[256];
        DATA32             *ptr2;
        int                 i, j, x, y, pl = 0;
        char                pper = 0;

        /* must set the im->data member before callign progress function */
        ptr2 = im->data = malloc(w * h * sizeof(DATA32));
        if (!im->data)
          {
             fclose(f);
             return 0;
          }
        /* start reading the data */
        switch (p)
          {
            case '1':          /* ASCII monochrome */
               buf[0] = 0;
               i = 0;
               for (y = 0; y < h; y++)
                 {
                    x = 0;
                    while (x < w)
                      {
                         if (!buf[i])   /* fill buffer */
                           {
                              if (!fgets(buf, 255, f))
                                {
                                   fclose(f);
                                   return 0;
                                }
                              i = 0;
                           }
                         while (buf[i] && isspace(buf[i]))
                            i++;
                         if (buf[i])
                           {
                              if (buf[i] == '1')
                                 *ptr2 = 0xff000000;
                              else if (buf[i] == '0')
                                 *ptr2 = 0xffffffff;
                              else
                                {
                                   fclose(f);
                                   return 0;
                                }
                              ptr2++;
                              i++;
                           }
                      }
                    if (progress)
                      {
                         char                per;
                         int                 l;

                         per = (char)((100 * y) / im->h);
                         if (((per - pper) >= progress_granularity)
                             || (y == (im->h - 1)))
                           {
                              l = y - pl;

                              /* fix off by one in case of the last line */
                              if (y == (im->h - 1))
                                 l++;

                              if (!progress(im, per, 0, pl, im->w, l))
                                {
                                   fclose(f);
                                   return 2;
                                }
                              pper = per;
                              pl = y;
                           }
                      }
                 }
               break;
            case '2':          /* ASCII greyscale */
               idata = malloc(sizeof(int) * w);

               if (!idata)
                 {
                    fclose(f);
                    return 0;
                 }
               buf[0] = 0;
               i = 0;
               j = 0;
               for (y = 0; y < h; y++)
                 {
                    iptr = idata;
                    x = 0;
                    while (x < w)
                      {
                         if (!buf[i])   /* fill buffer */
                           {
                              if (!fgets(buf, 255, f))
                                {
                                   free(idata);
                                   fclose(f);
                                   return 0;
                                }
                              i = 0;
                           }
                         while (buf[i] && isspace(buf[i]))
                            i++;
                         while (buf[i] && !isspace(buf[i]))
                            buf2[j++] = buf[i++];
                         if (j)
                           {
                              buf2[j] = 0;
                              *(iptr++) = atoi(buf2);
                              j = 0;
                              x++;
                           }
                      }
                    iptr = idata;
                    if (v == 255)
                      {
                         for (x = 0; x < w; x++)
                           {
                              *ptr2 =
                                  0xff000000 | (iptr[0] << 16) | (iptr[0] << 8)
                                  | iptr[0];
                              ptr2++;
                              iptr++;
                           }
                      }
                    else
                      {
                         for (x = 0; x < w; x++)
                           {
                              *ptr2 =
                                  0xff000000 | (((iptr[0] * 255) / v) << 16) |
                                  (((iptr[0] * 255) / v) << 8) | ((iptr[0] *
                                                                   255) / v);
                              ptr2++;
                              iptr++;
                           }
                      }
                    if (progress)
                      {
                         char                per;
                         int                 l;

                         per = (char)((100 * y) / im->h);
                         if (((per - pper) >= progress_granularity)
                             || (y == (im->h - 1)))
                           {

                              l = y - pl;

                              /* fix off by one in case of the last line */
                              if (y == (im->h - 1))
                                 l++;

                              if (!progress(im, per, 0, pl, im->w, l))
                                {
                                   if (idata)
                                      free(idata);
                                   fclose(f);
                                   return 2;
                                }
                              pper = per;
                              pl = y;
                           }
                      }
                 }
               break;
            case '3':          /* ASCII RGB */
               idata = malloc(3 * sizeof(int) * w);

               if (!idata)
                 {
                    fclose(f);
                    return 0;
                 }
               buf[0] = 0;
               i = 0;
               j = 0;
               for (y = 0; y < h; y++)
                 {
                    int                 w3 = 3 * w;

                    iptr = idata;
                    x = 0;
                    while (x < w3)
                      {
                         if (!buf[i])   /* fill buffer */
                           {
                              if (!fgets(buf, 255, f))
                                {
                                   free(idata);
                                   fclose(f);
                                   return 0;
                                }
                              i = 0;
                           }
                         while (buf[i] && isspace(buf[i]))
                            i++;
                         while (buf[i] && !isspace(buf[i]))
                            buf2[j++] = buf[i++];
                         if (j)
                           {
                              buf2[j] = 0;
                              *(iptr++) = atoi(buf2);
                              j = 0;
                              x++;
                           }
                      }
                    iptr = idata;
                    if (v == 255)
                      {
                         for (x = 0; x < w; x++)
                           {
                              *ptr2 =
                                  0xff000000 | (iptr[0] << 16) | (iptr[1] << 8)
                                  | iptr[2];
                              ptr2++;
                              iptr += 3;
                           }
                      }
                    else
                      {
                         for (x = 0; x < w; x++)
                           {
                              *ptr2 =
                                  0xff000000 | (((iptr[0] * 255) / v) << 16) |
                                  (((iptr[1] * 255) / v) << 8) | ((iptr[2] *
                                                                   255) / v);
                              ptr2++;
                              iptr += 3;
                           }
                      }
                    if (progress)
                      {
                         char                per;
                         int                 l;

                         per = (char)((100 * y) / im->h);
                         if (((per - pper) >= progress_granularity)
                             || (y == (im->h - 1)))
                           {
                              l = y - pl;

                              /* fix off by one in case of the last line */
                              if (y == (im->h - 1))
                                 l++;

                              if (!progress(im, per, 0, pl, im->w, l))
                                {
                                   if (idata)
                                      free(idata);
                                   fclose(f);
                                   return 2;
                                }
                              pper = per;
                              pl = y;
                           }
                      }
                 }
               break;
            case '4':          /* binary 1bit monochrome */
               data = malloc(1 * sizeof(DATA8));
               if (!data)
                 {
                    fclose(f);
                    return 0;
                 }
               ptr2 = im->data;
               j = 0;
               while ((fread(data, 1, 1, f)) && (j < (w * h)))
                 {
                    for (i = 7; i >= 0; i--)
                      {
                         if (j < (w * h))
                           {
                              if (data[0] & (1 << i))
                                 *ptr2 = 0xff000000;
                              else
                                 *ptr2 = 0xffffffff;
                              ptr2++;
                           }
                         j++;
                      }
                 }
               break;
            case '5':          /* binary 8bit grayscale GGGGGGGG */
               data = malloc(1 * sizeof(DATA8) * w);
               if (!data)
                 {
                    fclose(f);
                    return 0;
                 }
               ptr2 = im->data;
               for (y = 0; y < h; y++)
                 {
                    if (!fread(data, w * 1, 1, f))
                      {
                         free(data);
                         fclose(f);
                         return 1;
                      }
                    ptr = data;
                    if (v == 255)
                      {
                         for (x = 0; x < w; x++)
                           {
                              *ptr2 =
                                  0xff000000 | (ptr[0] << 16) | (ptr[0] << 8) |
                                  ptr[0];
                              ptr2++;
                              ptr++;
                           }
                      }
                    else
                      {
                         for (x = 0; x < w; x++)
                           {
                              *ptr2 =
                                  0xff000000 | (((ptr[0] * 255) / v) << 16) |
                                  (((ptr[0] * 255) / v) << 8) | ((ptr[0] *
                                                                  255) / v);
                              ptr2++;
                              ptr++;
                           }
                      }
                    if (progress)
                      {
                         char                per;
                         int                 l;

                         per = (char)((100 * y) / im->h);
                         if (((per - pper) >= progress_granularity)
                             || (y == (im->h - 1)))
                           {
                              l = y - pl;

                              /* fix off by one in case of the last line */
                              if (y == (im->h - 1))
                                 l++;

                              if (!progress(im, per, 0, pl, im->w, l))
                                {
                                   if (data)
                                      free(data);
                                   fclose(f);
                                   return 2;
                                }
                              pper = per;
                              pl = y;
                           }
                      }
                 }
               break;
            case '6':          /* 24bit binary RGBRGBRGB */
               data = malloc(3 * sizeof(DATA8) * w);
               if (!data)
                 {
                    fclose(f);
                    return 0;
                 }
               ptr2 = im->data;
               for (y = 0; y < h; y++)
                 {
                    if (!fread(data, w * 3, 1, f))
                      {
                         free(data);
                         fclose(f);
                         return 1;
                      }
                    ptr = data;
                    if (v == 255)
                      {
                         for (x = 0; x < w; x++)
                           {
                              *ptr2 =
                                  0xff000000 | (ptr[0] << 16) | (ptr[1] << 8) |
                                  ptr[2];
                              ptr2++;
                              ptr += 3;
                           }
                      }
                    else
                      {
                         for (x = 0; x < w; x++)
                           {
                              *ptr2 =
                                  0xff000000 | (((ptr[0] * 255) / v) << 16) |
                                  (((ptr[1] * 255) / v) << 8) | ((ptr[2] *
                                                                  255) / v);
                              ptr2++;
                              ptr += 3;
                           }
                      }
                    if (progress)
                      {
                         char                per;
                         int                 l;

                         per = (char)((100 * y) / im->h);
                         if (((per - pper) >= progress_granularity)
                             || (y == (im->h - 1)))
                           {
                              l = y - pl;

                              /* fix off by one in case of the last line */
                              if (y == (im->h - 1))
                                 l++;

                              if (!progress(im, per, 0, pl, im->w, l))
                                {
                                   if (data)
                                      free(data);
                                   fclose(f);
                                   return 2;
                                }
                              pper = per;
                              pl = y;
                           }
                      }
                 }
               break;
            case '7':          /* XV's 8bit 332 format */
               data = malloc(1 * sizeof(DATA8) * w);
               if (!data)
                 {
                    fclose(f);
                    return 0;
                 }
               ptr2 = im->data;
               for (y = 0; y < h; y++)
                 {
                    if (!fread(data, w * 1, 1, f))
                      {
                         free(data);
                         fclose(f);
                         return 1;
                      }
                    ptr = data;
                    for (x = 0; x < w; x++)
                      {
                         int                 r, g, b;

                         r = (*ptr >> 5) & 0x7;
                         g = (*ptr >> 2) & 0x7;
                         b = (*ptr) & 0x3;
                         *ptr2 =
                             0xff000000 | (((r << 21) | (r << 18) | (r << 15)) &
                                           0xff0000) | (((g << 13) | (g << 10) |
                                                         (g << 7)) & 0xff00) |
                             ((b << 6) | (b << 4) | (b << 2) | (b << 0));
                         ptr2++;
                         ptr++;
                      }
                    if (progress)
                      {
                         char                per;
                         int                 l = 0;

                         per = (char)((100 * y) / im->h);
                         if (((per - pper) >= progress_granularity)
                             || (y == (im->h - 1)))
                           {
                              /* fix off by one in case of the last line */
                              if (y == (im->h - 1))
                                 l++;

                              if (!progress(im, per, 0, pl, im->w, l))
                                {
                                   if (data)
                                      free(data);
                                   fclose(f);
                                   return 2;
                                }
                              pper = per;
                              pl = y;
                           }
                      }
                 }
               break;
            case '8':          /* 24bit binary RGBARGBARGBA */
               data = malloc(4 * sizeof(DATA8) * w);
               if (!data)
                 {
                    fclose(f);
                    return 0;
                 }
               ptr2 = im->data;
               for (y = 0; y < h; y++)
                 {
                    if (!fread(data, w * 4, 1, f))
                      {
                         free(data);
                         fclose(f);
                         return 1;
                      }
                    ptr = data;
                    if (v == 255)
                      {
                         for (x = 0; x < w; x++)
                           {
                              *ptr2 =
                                  (ptr[3] << 24) | (ptr[0] << 16) | (ptr[1] <<
                                                                     8) |
                                  ptr[2];
                              ptr2++;
                              ptr += 4;
                           }
                      }
                    else
                      {
                         for (x = 0; x < w; x++)
                           {
                              *ptr2 =
                                  (((ptr[3] * 255) /
                                    v) << 24) | (((ptr[0] * 255) /
                                                  v) << 16) | (((ptr[1] * 255) /
                                                                v) << 8) |
                                  ((ptr[2] * 255) / v);
                              ptr2++;
                              ptr += 4;
                           }
                      }
                    if (progress)
                      {
                         char                per;
                         int                 l = 0;

                         per = (char)((100 * y) / im->h);
                         if (((per - pper) >= progress_granularity)
                             || (y == (im->h - 1)))
                           {
                              /* fix off by one in case of the last line */
                              if (y == (im->h - 1))
                                 l++;

                              if (!progress(im, per, 0, pl, im->w, l))
                                {
                                   if (data)
                                      free(data);
                                   fclose(f);
                                   return 2;
                                }
                              pper = per;
                              pl = y;
                           }
                      }
                 }
               break;
            default:
               fclose(f);
               return 0;
               break;
          }
        if (idata)
           free(idata);
        if (data)
           free(data);
     }
   fclose(f);
   return 1;
}

char
save(ImlibImage * im, ImlibProgressFunction progress, char progress_granularity)
{
   FILE               *f;
   DATA8              *buf, *bptr;
   DATA32             *ptr;
   int                 x, y, pl = 0;
   char                pper = 0;

   /* no image data? abort */
   if (!im->data)
      return 0;
   f = fopen(im->real_file, "wb");
   if (!f)
      return 0;
   /* if the image has a useful alpha channel */
   if (im->flags & F_HAS_ALPHA)
     {
        /* allocate a small buffer to convert image data */
        buf = malloc(im->w * 4 * sizeof(DATA8));
        if (!buf)
          {
             fclose(f);
             return 0;
          }
        ptr = im->data;
        fprintf(f, "P8\n" "# PNM File written by Imlib2\n" "%i %i\n" "255\n",
                im->w, im->h);
        for (y = 0; y < im->h; y++)
          {
             bptr = buf;
             for (x = 0; x < im->w; x++)
               {
                  bptr[0] = ((*ptr) >> 16) & 0xff;
                  bptr[1] = ((*ptr) >> 8) & 0xff;
                  bptr[2] = ((*ptr)) & 0xff;
                  bptr[3] = ((*ptr) >> 24) & 0xff;
                  bptr += 4;
                  ptr++;
               }
             fwrite(buf, im->w * 4, 1, f);
             if (progress)
               {
                  char                per;
                  int                 l;

                  per = (char)((100 * y) / im->h);
                  if (((per - pper) >= progress_granularity)
                      || (y == (im->h - 1)))
                    {
                       l = y - pl;
                       if (!progress(im, per, 0, (y - l), im->w, l))
                         {
                            free(buf);
                            fclose(f);
                            return 2;
                         }
                       pper = per;
                       pl = y;
                    }
               }
          }
     }
   else
     {
        /* allocate a small buffer to convert image data */
        buf = malloc(im->w * 3 * sizeof(DATA8));
        if (!buf)
          {
             fclose(f);
             return 0;
          }
        ptr = im->data;
        fprintf(f, "P6\n" "# PNM File written by Imlib2\n" "%i %i\n" "255\n",
                im->w, im->h);
        for (y = 0; y < im->h; y++)
          {
             bptr = buf;
             for (x = 0; x < im->w; x++)
               {
                  bptr[0] = ((*ptr) >> 16) & 0xff;
                  bptr[1] = ((*ptr) >> 8) & 0xff;
                  bptr[2] = ((*ptr)) & 0xff;
                  bptr += 3;
                  ptr++;
               }
             fwrite(buf, im->w * 3, 1, f);
             if (progress)
               {
                  char                per;
                  int                 l;

                  per = (char)((100 * y) / im->h);
                  if (((per - pper) >= progress_granularity)
                      || (y == (im->h - 1)))
                    {
                       l = y - pl;
                       if (!progress(im, per, 0, (y - l), im->w, l))
                         {
                            free(buf);
                            fclose(f);
                            return 2;
                         }
                       pper = per;
                       pl = y;
                    }
               }
          }
     }
   /* finish off */
   free(buf);
   fclose(f);
   return 1;
   progress = NULL;
}

void
formats(ImlibLoader * l)
{
   char               *list_formats[] = { "pnm", "ppm", "pgm", "pbm", "pam" };

   {
      int                 i;

      l->num_formats = (sizeof(list_formats) / sizeof(char *));
      l->formats = malloc(sizeof(char *) * l->num_formats);

      for (i = 0; i < l->num_formats; i++)
         l->formats[i] = strdup(list_formats[i]);
   }
}
