/*
 * Based off of Peter Alm's BMP loader from xmms, with additions from
 * imlib's old BMP loader
 */

/*
 * 21.3.2006 - Changes made by Petr Kobalicek
 * - Simplify and make secure RLE encoding
 * - Fix 16 and 32 bit depth (old code was incorrect and it's commented) 
 */
#include "loader_common.h"
#include <sys/stat.h>

typedef struct tagRGBQUAD {
   unsigned char       rgbBlue;
   unsigned char       rgbGreen;
   unsigned char       rgbRed;
   unsigned char       rgbReserved;
} RGBQUAD;

#define BI_RGB       0
#define BI_RLE8      1
#define BI_RLE4      2
#define BI_BITFIELDS 3

/* 21.3.3006 - Use enumeration for RLE encoding. This makes it more readable */
enum {
  RLE_NEXT = 0, /* Next line */
  RLE_END = 1,  /* End of RLE encoding */
  RLE_MOVE = 2  /* Move by X and Y (Offset is stored in two next bytes) */
};

static int
ReadleShort(FILE * file, unsigned short *ret)
{
   unsigned char       b[2];

   if (fread(b, sizeof(unsigned char), 2, file) != 2)
      return 0;

   *ret = (b[1] << 8) | b[0];   
   return 1;
}

static int
ReadleLong(FILE * file, unsigned long *ret)
{
   unsigned char       b[4];

   if (fread(b, sizeof(unsigned char), 4, file) != 4)
      return 0;

   *ret = (b[3] << 24) | (b[2] << 16) | (b[1] << 8) | b[0];
   return 1;
}

static int
WriteleByte(FILE * file, unsigned char val)
{
   int rc;

   rc = fputc ((int) val & 0xff, file);
   if (rc == EOF)
      return 0;

   return 1;
}

static int
WriteleShort(FILE * file, unsigned short val)
{
   int rc;

   rc = fputc ((int) (val & 0xff), file);
   if (rc == EOF)
      return 0;
   rc = fputc ((int) ((val >> 8) & 0xff), file);
   if (rc == EOF)
      return 0;

   return 1;
}

static int
WriteleLong(FILE * file, unsigned long val)
{
   int rc;

   rc = fputc ((int) (val & 0xff), file);
   if (rc == EOF)
      return 0;
   rc = fputc ((int) ((val >> 8) & 0xff), file);
   if (rc == EOF)
      return 0;
   rc = fputc ((int) ((val >> 16) & 0xff), file);
   if (rc == EOF)
      return 0;
   rc = fputc ((int) ((val >> 24) & 0xff), file);
   if (rc == EOF)
      return 0;

   return 1;
}

char
load(ImlibImage * im, ImlibProgressFunction progress,
     char progress_granularity, char immediate_load)
{
   FILE               *f;
   char                pper = 0;
   int                 pl = 0;
   char                type[2];
   unsigned long       size, offset, headSize, comp, imgsize, j, k, l;
   unsigned short      tmpShort, planes, bitcount, ncols, skip;
   unsigned char       byte = 0, g, b, r;
   unsigned long       i, w, h;
   unsigned short      x, y;
   DATA32             *ptr, *data_end;
   unsigned char      *buffer_ptr, *buffer, *buffer_end;
   RGBQUAD             rgbQuads[256];
   unsigned long       rmask = 0xff, gmask = 0xff, bmask = 0xff;
   unsigned long       rshift = 0, gshift = 0, bshift = 0;
   unsigned long       rleftshift = 0, gleftshift = 0, bleftshift = 0;

   /*
    * 21.3.2006:
    * Added these two variables for RLE.
    */
   unsigned char       byte1, byte2;

   if (im->data)
      return 0;
   f = fopen(im->real_file, "rb");
   if (!f)
      return 0;

   /* header */
   {
      struct stat         statbuf;

      if (stat(im->real_file, &statbuf) == -1)
        {
           fclose(f);
           return 0;
        }
      size = statbuf.st_size;

      if (fread(type, 1, 2, f) != 2)
        {
           fclose(f);
           return 0;
        }
      if (strncmp(type, "BM", 2))
        {
           fclose(f);
           return 0;
        }

      fseek(f, 8, SEEK_CUR);
      ReadleLong(f, &offset);
      ReadleLong(f, &headSize);
      if (offset >= size)
        {
           fclose(f);
           return 0;
        }
      if (headSize == 12)
        {
           ReadleShort(f, &tmpShort);
           w = tmpShort;
           ReadleShort(f, &tmpShort);
           h = tmpShort;
           ReadleShort(f, &planes);
           ReadleShort(f, &bitcount);
           imgsize = size - offset;
           comp = BI_RGB;
        }
      else if (headSize == 40)
        {
           ReadleLong(f, &w);
           ReadleLong(f, &h);
           ReadleShort(f, &planes);
           ReadleShort(f, &bitcount);
           ReadleLong(f, &comp);
           ReadleLong(f, &imgsize);
           imgsize = size - offset;

           fseek(f, 16, SEEK_CUR);
        }
      else
        {
           fclose(f);
           return 0;
        }

      if (!IMAGE_DIMENSIONS_OK(w, h))
        {
           fclose(f);
           return 0;
        }

      if (bitcount < 16)
        {
           ncols = (offset - headSize - 14);
           if (headSize == 12)
             {
                ncols /= 3;
                if (ncols > 256) ncols = 256;
                for (i = 0; i < ncols; i++)
                   fread(&rgbQuads[i], 3, 1, f);
             }
           else
             {
                ncols /= 4;
                if (ncols > 256) ncols = 256;
                fread(rgbQuads, 4, ncols, f);
             }
        }
      else if (bitcount == 16 || bitcount == 32)
        {
           if (comp == BI_BITFIELDS)
             {
                int                 bit;

                ReadleLong(f, &rmask);
                ReadleLong(f, &gmask);
                ReadleLong(f, &bmask);
                for (bit = bitcount - 1; bit >= 0; bit--)
                  {
                     if (bmask & (1 << bit)) bshift = bit;
                     if (gmask & (1 << bit)) gshift = bit;
                     if (rmask & (1 << bit)) rshift = bit;
                  }
                while(((((0xffffL & bmask) >> bshift) << bleftshift) & 0x80) == 0)
                  {
                     bleftshift++;
                  }
                while(((((0xffffL & gmask) >> gshift) << gleftshift) & 0x80) == 0)
                  {
                     gleftshift++;
                  }
                while(((((0xffffL & rmask) >> rshift) << rleftshift) & 0x80) == 0)
                  {
                     rleftshift++;
                  }
              }
           else if (bitcount == 16)
             {
                rmask = 0x7C00;
                gmask = 0x03E0;
                bmask = 0x001F;
                rshift = 10;
                gshift = 5;
                bshift = 0;
                rleftshift = gleftshift = bleftshift = 3;
             }
           else if (bitcount == 32)
             {
                rmask = 0x00FF0000;
                gmask = 0x0000FF00;
                bmask = 0x000000FF;
                rshift = 16;
                gshift = 8;
                bshift = 0;
             }
        }

      im->w = w;
      im->h = h;
      if (!im->format)
        {
           UNSET_FLAG(im->flags, F_HAS_ALPHA);
           im->format = strdup("bmp");
        }
   }
   if (((!im->data) && (im->loader)) || (immediate_load) || (progress))
     {
        fseek(f, offset, SEEK_SET);
        buffer = malloc(imgsize);
        if (!buffer)
          {
             fclose(f);
             return 0;
          }
        im->data = malloc(w * h * sizeof(DATA32));
        if (!im->data)
          {
             fclose(f);
             free(buffer);
             return 0;
          }

        fread(buffer, imgsize, 1, f);
        fclose(f);
        buffer_ptr = buffer;
        buffer_end = buffer + imgsize;

        data_end = im->data + w * h;
        ptr = im->data + ((h - 1) * w);

        if (bitcount == 1)
          {
             if (comp == BI_RGB)
               {
                  skip = ((((w + 31) / 32) * 32) - w) / 8;
                  for (y = 0; y < h; y++)
                    {
                       for (x = 0; x < w && buffer_ptr < buffer_end; x++)
                         {
                            if ((x & 7) == 0)
                               byte = *(buffer_ptr++);
                            k = (byte >> 7) & 1;
                            *ptr++ = 0xff000000 |
                                (rgbQuads[k].rgbRed << 16) |
                                (rgbQuads[k].rgbGreen << 8) |
                                rgbQuads[k].rgbBlue;
                            byte <<= 1;
                         }
                       buffer_ptr += skip;
                       ptr -= w * 2;
                       if (progress)
                         {
                            char                per;
                            int                 l;

                            per = (char)((100 * y) / im->h);
                            if (((per - pper) >= progress_granularity) ||
                                (y == (im->h - 1)))
                              {
                                 l = y - pl;
                                 if (!progress
                                     (im, per, 0, im->h - y - 1, im->w,
                                      im->h - y + l))
                                   {
                                      free(buffer);
                                      return 2;
                                   }
                                 pper = per;
                                 pl = y;
                              }
                         }
                    }
               }
          }

        /*
         * 21.3.2006
         * Bug fixes and optimization:
         * 
         * RLE encoding is dangerous and can be used by attackers by creating special files.
         * We has 'buffer_ptr' and 'buffer_end' variables and buffer_end points to first 
         * unaccessible byte in buffer.
         * - If we use 'byte = *(buffer_ptr++) in main loop we must check if 
         *   'buffer_ptr != buffer_end', because special or incomplete bmp file can generate
         *   segfault (I was writing it, because in RLE we need to read depending count of
		 *   bytes that depends on requester operation).
         *   SOLUTION: Don't read one byte, read two bytes and check.
         * - If RLE teels us than single color length will be larger than allowed, we can
         *   stop, because bitmap is corrupted or crawled.
         *   SOLUTION: Check for length ('l' variable in RLE) and break loop if it's invalid
         *   IMPROVEMENTS: We can stop checking if 'x' is out of rangle, because it never be.
         * - In RLE4 use one bigger loop that fills two pixels. This is faster and cleaner.
         *   If one pixel remains (the tail), do it on end of the loop.
         * - If we will check x and y (new line and skipping), we can't go outsize imlib
         *   image buffer.
         */

        if (bitcount == 4)
          {
             if (comp == BI_RLE4)
               {
                  /*
                   * 21.3.2006: This is better than using 'if buffer_ptr + 1 < buffer_end'
                   */
                  unsigned char *buffer_end_minus_1 = buffer_end - 1;
                  x = 0;
                  y = 0;

                  for (i = 0; i < imgsize && buffer_ptr < buffer_end_minus_1; i++)
                    {
                       byte1 = buffer_ptr[0];
                       byte2 = buffer_ptr[1];
                       buffer_ptr += 2;
                       if (byte1)
                         {
                            DATA32 t1, t2;

                            l = byte1;
                            /* Check for invalid length */
                            if (l + x > w) goto _bail;

                            t1 = 0xff000000 | (rgbQuads[byte2 >>  4].rgbRed   << 16) |
                                              (rgbQuads[byte2 >>  4].rgbGreen <<  8) |
                                              (rgbQuads[byte2 >>  4].rgbBlue       ) ;
                            t2 = 0xff000000 | (rgbQuads[byte2 & 0xF].rgbRed   << 16) |
                                              (rgbQuads[byte2 & 0xF].rgbGreen <<  8) |
                                              (rgbQuads[byte2 & 0xF].rgbBlue       ) ;
                            for (j = l/2; j; j--) {
                               ptr[0] = t1;
                               ptr[1] = t2;
                               ptr += 2;
                            }
                            /* tail */
                            if (l & 1) *ptr++ = t1;
                            x += l;
                         }
                       else
                         {
                            switch (byte2)
                              {
                                case RLE_NEXT:
                                   x = 0;
                                   if (++y >= h) goto _bail;
                                   ptr = im->data + (h - y - 1) * w;
                                   break;
                                case RLE_END:
                                   goto _bail;
                                case RLE_MOVE:
                                   /* Need to read two bytes */
                                   if (buffer_ptr >= buffer_end_minus_1) goto _bail; 
                                   x += buffer_ptr[0];
                                   y += buffer_ptr[1];
                                   buffer_ptr += 2;
                                   /* Check for correct coordinates */
                                   if (x >= w) goto _bail;
                                   if (y >= h) goto _bail;
                                   ptr = im->data + (h - y - 1) * w + x;
                                   break;
                                default:
                                   l = byte2;
                                   /* Check for invalid length and valid buffer size */
                                   if (l + x > w) goto _bail;
                                   if (buffer_ptr + (l >> 1) + (l & 1) > buffer_end) goto _bail;

                                   for (j = l/2; j; j--) {
                                     byte = *buffer_ptr++;
                                     ptr[0] = 0xff000000 | (rgbQuads[byte >>  4].rgbRed   << 16) |
                                                           (rgbQuads[byte >>  4].rgbGreen <<  8) |
                                                           (rgbQuads[byte >>  4].rgbBlue       ) ;
                                     ptr[1] = 0xff000000 | (rgbQuads[byte & 0xF].rgbRed   << 16) |
                                                           (rgbQuads[byte & 0xF].rgbGreen <<  8) |
                                                           (rgbQuads[byte & 0xF].rgbBlue       ) ;
                                     ptr += 2;
                                   }
                                   if (l & 1) {
                                     byte = *buffer_ptr++;
                                     *ptr++ = 0xff000000 | (rgbQuads[byte >>  4].rgbRed   << 16) |
                                                           (rgbQuads[byte >>  4].rgbGreen <<  8) |
                                                           (rgbQuads[byte >>  4].rgbBlue       ) ;
                                   }
                                   x += l;

                                   if ((l & 3) == 1)
                                      buffer_ptr += 2;
                                   else if ((l & 3) == 2)
                                      buffer_ptr++;
                                   break;
                              }
                         }
                       if (progress)
                         {
                            char                per;
                            int                 l;

                            per = (char)((100 * y) / im->h);
                            if (((per - pper) >= progress_granularity) ||
                                (y == (im->h - 1)))
                              {
                                 l = y - pl;
                                 if (!progress
                                     (im, per, 0, im->h - y - 1, im->w,
                                      im->h - y + l))
                                   {
                                      free(buffer);
                                      return 2;
                                   }
                                 pper = per;
                                 pl = y;
                              }
                         }

                    }
               }
             else if (comp == BI_RGB)
               {
                  skip = ((((w + 7) / 8) * 8) - w) / 2;
                  for (y = 0; y < h; y++)
                    {
                       for (x = 0; x < w && buffer_ptr < buffer_end; x++)
                         {
                            if ((x & 1) == 0)
                               byte = *(buffer_ptr++);
                            k = (byte & 0xF0) >> 4;
                            *ptr++ = 0xff000000 |
                                (rgbQuads[k].rgbRed << 16) |
                                (rgbQuads[k].rgbGreen << 8) |
                                rgbQuads[k].rgbBlue;
                            byte <<= 4;
                         }
                       buffer_ptr += skip;
                       ptr -= w * 2;
                       if (progress)
                         {
                            char                per;
                            int                 l;

                            per = (char)((100 * y) / im->h);
                            if (((per - pper) >= progress_granularity) ||
                                (y == (im->h - 1)))
                              {
                                 l = y - pl;
                                 if (!progress
                                     (im, per, 0, im->h - y - 1, im->w,
                                      im->h - y + l))
                                   {
                                      free(buffer);
                                      return 2;
                                   }
                                 pper = per;
                                 pl = y;
                              }
                         }
                    }
               }
          }
        if (bitcount == 8)
          {
             if (comp == BI_RLE8)
               {
                  /*
                   * 21.3.2006: This is better than using 'if buffer_ptr + 1 < buffer_end'
                   */
                  unsigned char *buffer_end_minus_1 = buffer_end - 1;
                  x = 0;
                  y = 0;
                  for (i = 0; i < imgsize && buffer_ptr < buffer_end_minus_1; i++)
                    {
                       byte1 = buffer_ptr[0];
                       byte2 = buffer_ptr[1];
                       buffer_ptr += 2;
                       if (byte1)
                         {
                            DATA32 pix = 0xff000000 | (rgbQuads[byte2].rgbRed   << 16) |
                                                      (rgbQuads[byte2].rgbGreen <<  8) |
                                                      (rgbQuads[byte2].rgbBlue       ) ;
                            l = byte1;
                            if (x + l > w) goto _bail;
                            for (j = l; j; j--) *ptr++ = pix;
                            x += l;
                         }
                       else
                         {
                            switch (byte2)
                              {
                                case RLE_NEXT:
                                   x = 0;
                                   if (++y >= h) goto _bail;
                                   ptr = im->data + ((h - y - 1) * w) + x;
                                   break;
                                case RLE_END:
                                   goto _bail;
                                case RLE_MOVE:
                                   /* Need to read two bytes */
                                   if (buffer_ptr >= buffer_end_minus_1) goto _bail; 
                                   x += buffer_ptr[0];
                                   y += buffer_ptr[1];
                                   buffer_ptr += 2;
                                   /* Check for correct coordinates */
                                   if (x >= w) goto _bail;
                                   if (y >= h) goto _bail;
                                   ptr = im->data + ((h - y - 1) * w) + x;
                                   break;
                                default:
                                   l = byte2;
                                   if (x + l > w) goto _bail;
                                   if (buffer_ptr + l > buffer_end) goto _bail;
                                   for (j = 0; j < l; j++)
                                     {
                                        byte = *(buffer_ptr++);

                                        *ptr++ = 0xff000000 |
                                            (rgbQuads[byte].rgbRed << 16) |
                                            (rgbQuads[byte].rgbGreen << 8) |
                                            rgbQuads[byte].rgbBlue;
                                     }
                                   x += l;
                                   if (l & 1)
                                      buffer_ptr++;
                                   break;
                              }
                         }
                    }
                  if (progress)
                    {
                       char                per;
                       int                 l;

                       per = (char)((100 * y) / im->h);
                       if (((per - pper) >= progress_granularity) ||
                           (y == (im->h - 1)))
                         {
                            l = y - pl;
                            if (!progress
                                (im, per, 0, im->h - y - 1, im->w,
                                 im->h - y + l))
                              {
                                 free(buffer);
                                 return 2;
                              }
                            pper = per;
                            pl = y;
                         }
                    }
               }
             else if (comp == BI_RGB)
               {
                  skip = (((w + 3) / 4) * 4) - w;
                  for (y = 0; y < h; y++)
                    {
                       for (x = 0; x < w && buffer_ptr < buffer_end; x++)
                         {
                            byte = *(buffer_ptr++);
                            *ptr++ = 0xff000000 |
                                (rgbQuads[byte].rgbRed << 16) |
                                (rgbQuads[byte].rgbGreen << 8) |
                                rgbQuads[byte].rgbBlue;
                         }
                       ptr -= w * 2;
                       buffer_ptr += skip;
                       if (progress)
                         {
                            char                per;
                            int                 l;

                            per = (char)((100 * y) / im->h);
                            if (((per - pper) >= progress_granularity) ||
                                (y == (im->h - 1)))
                              {
                                 l = y - pl;
                                 if (!progress
                                     (im, per, 0, im->h - y - 1, im->w,
                                      im->h - y + l))
                                   {
                                      free(buffer);
                                      return 2;
                                   }
                                 pper = per;
                                 pl = y;
                              }
                         }
                    }
               }

          }
        else if (bitcount == 16)
          {
             /* 21.3.2006 - Need to check for buffer_ptr + 1 < buffer_end */
             unsigned char *buffer_end_minus_1 = buffer_end - 1;
             skip = (((w * 16 + 31) / 32) * 4) - (w * 2);
             for (y = 0; y < h; y++)
               {
                  for (x = 0; x < w && buffer_ptr < buffer_end_minus_1; x++)
                    {
                       /*
                        * THIS WAS OLD CODE 
                        *
                        * r = ((unsigned short)(*buffer_ptr) & rmask) >> rshift;
                        * g = ((unsigned short)(*buffer_ptr) & gmask) >> gshift;
                        * b = ((unsigned short)(*(buffer_ptr++)) & bmask) >>
                        *   bshift;
                        * *ptr++ = 0xff000000 | (r << 16) | (g << 8) | b;
                        */
                       unsigned short pix = *(unsigned short *)buffer_ptr;
                       *ptr++ = 0xff000000 | ((((pix & rmask) >> rshift) << rleftshift) << 16) |
                                             ((((pix & gmask) >> gshift) << gleftshift) <<  8) |
                                             ((((pix & bmask) >> bshift) << bleftshift)      ) ;
                       buffer_ptr += 2;
                    }
                  ptr -= w * 2;
                  buffer_ptr += skip;
                  if (progress)
                    {
                       char                per;
                       int                 l;

                       per = (char)((100 * y) / im->h);
                       if (((per - pper) >= progress_granularity) ||
                           (y == (im->h - 1)))
                         {
                            l = y - pl;
                            if (!progress
                                (im, per, 0, im->h - y - 1, im->w,
                                 im->h - y + l))
                              {
                                 free(buffer);
                                 return 2;
                              }
                            pper = per;
                            pl = y;
                         }
                    }
               }
          }
        else if (bitcount == 24)
          {
             /* 21.3.2006 - Fix: need to check for buffer_ptr + 2 < buffer_end */
             unsigned char *buffer_end_minus_2 = buffer_end - 2;
             skip = (4 - ((w * 3) % 4)) & 3;
             for (y = 0; y < h; y++)
               {
                  for (x = 0; x < w && buffer_ptr < buffer_end_minus_2; x++)
                    {
                       b = *(buffer_ptr++);
                       g = *(buffer_ptr++);
                       r = *(buffer_ptr++);
                       *ptr++ = 0xff000000 | (r << 16) | (g << 8) | b;
                    }
                  ptr -= w * 2;
                  buffer_ptr += skip;
                  if (progress)
                    {
                       char                per;
                       int                 l;

                       per = (char)((100 * y) / im->h);
                       if (((per - pper) >= progress_granularity) ||
                           (y == (im->h - 1)))
                         {
                            l = y - pl;
                            if (!progress
                                (im, per, 0, im->h - y - 1, im->w,
                                 im->h - y + l))
                              {
                                 free(buffer);
                                 return 2;
                              }
                            pper = per;
                            pl = y;
                         }
                    }
               }
          }
        else if (bitcount == 32)
          {
             /* 21.3.2006 - Need to check buffer_ptr + 3 < buffer_end */
             unsigned char *buffer_end_minus_3 = buffer_end_minus_3;
             skip = (((w * 32 + 31) / 32) * 4) - (w * 4);
             for (y = 0; y < h; y++)
               {
                  for (x = 0; x < w && buffer_ptr < buffer_end_minus_3; x++)
                    {
                       /*
                        * THIS WAS OLD CODE: I don't understand it and it's invalid.
                        *
                        * r = ((unsigned long)(*buffer_ptr) & rmask) >> rshift;
                        * g = ((unsigned long)(*buffer_ptr) & gmask) >> gshift;
                        * b = ((unsigned long)(*buffer_ptr) & bmask) >> bshift;
                        * *ptr++ = 0xff000000 | (r << 16) | (g << 8) | b;
                        * r = *(buffer_ptr++);
                        * r = *(buffer_ptr++);
                        */

                       /* TODO: What about alpha channel...Is used? */
                       DATA32 pix = *(unsigned int *)buffer_ptr;
                       *ptr++ = 0xff000000 | (((pix & rmask) >> rshift) << 16) |
                                             (((pix & gmask) >> gshift) <<  8) |
                                             (((pix & bmask) >> bshift)      ) ; 
                       buffer_ptr += 4;
                    }
                  ptr -= w * 2;
                  buffer_ptr += skip;
                  if (progress)
                    {
                       char                per;
                       int                 l;

                       per = (char)((100 * y) / im->h);
                       if (((per - pper) >= progress_granularity) ||
                           (y == (im->h - 1)))
                         {
                            l = y - pl;
                            if (!progress
                                (im, per, 0, im->h - y - 1, im->w,
                                 im->h - y + l))
                              {
                                 free(buffer);
                                 return 2;
                              }
                            pper = per;
                            pl = y;
                         }
                    }
               }
          }
_bail:
        free(buffer);
     }
   return 1;
}

char
save(ImlibImage * im, ImlibProgressFunction progress, char progress_granularity)
{
   FILE               *f;
   Imlib_Color         pixel_color;
   unsigned long       i, j, pad;

   if (!im->data)
      return 0;

   f = fopen(im->real_file, "wb");
   if (!f)
      return 0;

   /* calculate number of bytes to pad on end of each row */
   pad = (4 - ((im->w * 3) % 4)) & 0x03;

   /* write BMP file header */
   WriteleShort(f, 0x4d42);   /* prefix */
   WriteleLong(f, 54 + 3 * im->w * im->h);  /* filesize */
   WriteleShort(f, 0x0000);   /* reserved #1 */
   WriteleShort(f, 0x0000);   /* reserved #2 */
   WriteleLong(f, 54);        /* offset to image data */

   /* write BMP bitmap header */
   WriteleLong(f, 40);   /* 40-byte header */
   WriteleLong(f, im->w);
   WriteleLong(f, im->h);
   WriteleShort(f, 1);   /* one plane      */
   WriteleShort(f, 24);  /* bits per pixel */
   WriteleLong(f, 0);    /* no compression */
   WriteleLong(f, 3 * im->w * im->h);
   for (i = 0; i < 4; i++)
     WriteleLong(f, 0x0000);  /* pad to end of header */

   /* write actual BMP data */
   for (i = 0; i < im->h; i++)
      {
         for (j = 0; j < im->w; j++)
            {
               imlib_image_query_pixel (j, im->h - i - 1, &pixel_color);
               WriteleByte(f, pixel_color.blue);
               WriteleByte(f, pixel_color.green);
               WriteleByte(f, pixel_color.red);
            }
         for (j = 0; j < pad; j++)
            WriteleByte(f, 0);
      }

   fclose(f);
   return 1;
}

void
formats(ImlibLoader * l)
{
   char               *list_formats[] = { "bmp" };

   {
      int                 i;

      l->num_formats = (sizeof(list_formats) / sizeof(char *));
      l->formats = malloc(sizeof(char *) * l->num_formats);
      for (i = 0; i < l->num_formats; i++)
         l->formats[i] = strdup(list_formats[i]);
   }
}
