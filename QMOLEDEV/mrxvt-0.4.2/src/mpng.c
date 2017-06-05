/*--------------------------------*-C-*---------------------------------*
 * File:	mpng.c
 *----------------------------------------------------------------------*
 *
 * All portions of code are copyright by their respective author/s.
 * Copyright (c) 1999-2002   Toshikaz Hirabayashi
 * Copyright (c) 2004        Jingmin Zhou <jimmyzhou@users.sourceforge.net>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *----------------------------------------------------------------------*/
/*
** $Id: mpng.c,v 1.8 2004/11/11 00:20:28 cvs Exp $
*/

#include "../config.h"
#include "rxvt.h"


#ifdef DEBUG_VERBOSE
#define DEBUG_LEVEL 1
#else 
#define DEBUG_LEVEL 0
#endif

#if DEBUG_LEVEL
#define DBG_MSG(d,x) if(d <= DEBUG_LEVEL) fprintf x
#else
#define DBG_MSG(d,x)
#endif


#ifdef USE_PNG

static void
png_cexcept_error (png_structp png_ptr, png_const_charp msg)
{
	if(png_ptr){
		fprintf(stderr, "png file read error: %s\n", msg);
	}
}


long
PngReadFileToPixmap (Display* display, Window window, GC gc, char* filename, Pixmap* pixmap, long* w, long* h)
{
    int red_mask, green_mask, blue_mask;
    int red_shift, green_shift, blue_shift;
    int start_shift, msb_flag;
    unsigned int start_mask, udat;
    XWindowAttributes win_attr;
    FILE* ifile;
  long display_depth;
      png_byte            sig[8];
  png_infop info_ptr;
  png_structp png_ptr;
      png_uint_32 png_width;
    png_uint_32 png_height;
    int png_depth;
    int png_color_type;
png_uint_32 png_row_bytes;
  png_uint_32 png_channels;
    long rwidth;
    long rheight;
    long components;
    unsigned char* buf;
    png_byte** png_row_ptrs;
    long vwidth;
    long vheight;
    long stretched;
    XImage* image;
    Visual* visual;
    Pixmap pix;
    int i;
    char* data1;
    unsigned char r,g,b;
    long ptr = 0;
    long ptr2 = 0;
    long j;

    red_mask = green_mask = blue_mask = 0;
    red_shift = green_shift = blue_shift = 0;

    ifile = fopen(filename,"r");
    if (ifile == NULL){
      return -1;
    }
    display_depth = XDefaultDepth(display,XDefaultScreen(display));

    fread(sig, 1, 8, ifile);
    if (!png_check_sig(sig, 8)){
      fclose(ifile);
      return -1;
    }
    png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL,
      (png_error_ptr)png_cexcept_error, (png_error_ptr)NULL);
    if (png_ptr == NULL){
      fclose(ifile);
      return -1;
    } 
    info_ptr = png_create_info_struct(png_ptr);
    if (info_ptr == NULL){
      png_destroy_read_struct(&png_ptr, NULL, NULL);
      fclose(ifile);
      return -1;
    }

    png_init_io(png_ptr, ifile);
    png_set_sig_bytes(png_ptr, 8);
    png_read_info(png_ptr, info_ptr);
    png_get_IHDR(png_ptr, info_ptr, &png_width, &png_height, &png_depth,
            &png_color_type, NULL, NULL, NULL);
    if (png_depth == 16){
      png_set_strip_16(png_ptr);
    }
    png_row_bytes = png_get_rowbytes(png_ptr, info_ptr);
    png_channels = png_get_channels(png_ptr, info_ptr);

    if (png_depth < 8){
      if (png_color_type == PNG_COLOR_TYPE_GRAY ){
        png_set_gray_1_2_4_to_8(png_ptr);
        png_row_bytes = png_width;
      }else{
        png_set_expand(png_ptr);
        png_row_bytes = png_width;
        png_row_bytes = png_width * 3;
        png_channels = 3;
      }
    }
    if (png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS)){
      png_set_expand(png_ptr);
      png_row_bytes = png_width;
    }
    if (png_color_type == PNG_COLOR_TYPE_GRAY ||
        png_color_type == PNG_COLOR_TYPE_GRAY_ALPHA){
      png_set_gray_to_rgb(png_ptr);
      png_row_bytes = png_width;
    }

    if (png_color_type == PNG_COLOR_TYPE_PALETTE){
      png_set_palette_to_rgb(png_ptr);
      png_row_bytes = png_width * 3;
      png_channels = 3;
    }

    rwidth = png_width;
    rheight = png_height;
    components = png_channels;

	/* possible integer overflow? */
	assert ((int) png_row_bytes > 0);
	assert ((int) png_height > 0);
	assert (((int)png_row_bytes) * ((int)png_height) * sizeof(png_byte) > 0);
    buf = rxvt_malloc(png_row_bytes * png_height * sizeof(png_byte));
    if (buf == NULL){
fprintf(stderr,"png read error: out of memory..\n");
      png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
      fclose(ifile);
      return -1;
    }
	/* possible integer overflow? */
	assert ((int) png_height > 0);
	assert (sizeof(png_bytep) * ((int)png_height) > 0);
    png_row_ptrs = rxvt_malloc (sizeof(png_bytep)*png_height);
    if (png_row_ptrs == NULL){
fprintf(stderr,"png read error: out of memory..\n");
      png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
      fclose(ifile);
      return -1;
    }

    for(i = 0; i < (int)png_height; i++){
      png_row_ptrs[i] = (png_byte*)(buf + i * png_row_bytes);
    }
    png_read_image(png_ptr, png_row_ptrs);
    png_read_end(png_ptr,NULL);
    free(png_row_ptrs);

    vwidth = *w;
    vheight = *h;
    stretched =0;
    if (*w == 0 || *h == 0){
      *w = rwidth;
      *h = rheight;
    }else{
      if ((long)((double)rwidth * vheight/vwidth) < rheight){
        *w = (long)((double)vheight * rwidth/rheight);
      }else{
        *h = (long)((double)vwidth * rheight/rwidth);
      }
      stretched = 1;
    }
    vwidth = *w;
    vheight = *h;



    image = 0;
    visual = XDefaultVisual(display,XDefaultScreen(display));
    if (display_depth >16){
      image = XCreateImage(display,visual, display_depth,
                           ZPixmap,0,0,vwidth,vheight,32,0);
    }else
    if (display_depth >8){
      image = XCreateImage(display,visual, display_depth,
                           ZPixmap,0,0,vwidth,vheight,16,0);
    }else{
      image = XCreateImage(display,visual, display_depth,
                           ZPixmap,0,0,vwidth,vheight,8,0);
    }

    msb_flag = (ImageByteOrder(display) == MSBFirst)?1:0;

    if (XGetWindowAttributes(display,
                     RootWindow(display, DefaultScreen(display)),
                     &win_attr) == 0) {
        fclose(ifile);
                 return -1;
    }
    //
    if ((win_attr.depth == 24) || (win_attr.depth == 16)) {
      unsigned int n;
      if (win_attr.depth == 24) {
        start_shift = 24;
        start_mask = 0x80000000;
      }else{
        start_shift = 8;
        start_mask = 0x8000;
      }
      red_mask = win_attr.visual->red_mask;
      red_shift = start_shift;
      n = start_mask;
      while (!(n & red_mask)) {
        n >>= 1;
        red_shift--;
      }
      green_mask = win_attr.visual->green_mask;
      green_shift = start_shift;
      n = start_mask;
      while (!(n & green_mask)) {
        n >>= 1;
        green_shift--;
      }
      blue_mask = win_attr.visual->blue_mask;
      blue_shift = start_shift;
      n = start_mask;
      while (!(n & blue_mask)) {
        n >>= 1;
        blue_shift--;
      }
    }

	/* possible integer overflow? */
	assert ((int) image->bytes_per_line > 0);
	assert ((int) vheight > 0);
	assert (((int)image->bytes_per_line) * ((int)vheight) > 0);
    data1 = rxvt_malloc(image->bytes_per_line * vheight);
    if (image->bits_per_pixel ==32){
      if (components == 3 || components == 4){
        for(i=0; i<vheight; i++){
          for(j=0; j<vwidth; j++){
            if (stretched != 0){
              ptr = (long)((double)i*rheight/vheight)*rwidth
                   + (long)((double)j*rwidth/vwidth);
              ptr *= 3;
            }
            r = buf[ptr++];
            g = buf[ptr++];
            b = buf[ptr++];
            if (components == 4){
              ptr++;
            }
            udat = 0;
            if (red_shift >= 0){
              udat |= (((int)r << red_shift) & red_mask);
            }else{
              udat |= (((int)r >> (-red_shift)) & red_mask);
            }
            if (green_shift >= 0){
              udat |= (((int)g << green_shift) & green_mask);
            }else{
              udat |= (((int)g >> (-green_shift)) & green_mask);
            }
            if (blue_shift >= 0){
              udat |= (((int)b << blue_shift) & blue_mask);
            }else{
              udat |= (((int)b >> (-blue_shift)) & blue_mask);
            }
            if (msb_flag){
              ((unsigned char*)data1)[ptr2++] = (udat & 0xff000000)>>24;
              ((unsigned char*)data1)[ptr2++] = (udat & 0xff0000)>>16;
              ((unsigned char*)data1)[ptr2++] = (udat & 0xff00)>>8;
              ((unsigned char*)data1)[ptr2++] = (udat & 0xff);
            }else{
              ((unsigned char*)data1)[ptr2++] = (udat & 0xff);
              ((unsigned char*)data1)[ptr2++] = (udat & 0xff00)>>8;
              ((unsigned char*)data1)[ptr2++] = (udat & 0xff0000)>>16;
              ((unsigned char*)data1)[ptr2++] = (udat & 0xff000000)>>24;
            }
          }
        }
      }else{
        for(i=0; i<vheight; i++){
          for(j=0; j<vwidth; j++){
            if (stretched != 0){
              ptr = (long)((double)i*rheight/vheight)*rwidth
                   + (long)((double)j*rwidth/vwidth);
            }
            r = buf[ptr];
            g = buf[ptr];
            b = buf[ptr++];
            if (msb_flag){
              ((unsigned char*)data1)[ptr2++] =  0;
              ((unsigned char*)data1)[ptr2++] =  b;
              ((unsigned char*)data1)[ptr2++] =  g;
              ((unsigned char*)data1)[ptr2++] =  r;
            }else{
              ((unsigned char*)data1)[ptr2++] =  r;
              ((unsigned char*)data1)[ptr2++] =  g;
              ((unsigned char*)data1)[ptr2++] =  b;
              ((unsigned char*)data1)[ptr2++] =  0;
            }
          }
        }
      }
    }else if (image->bits_per_pixel == 24){
      if (components == 3 || components == 4){
        for(i=0; i<vheight; i++){
          ptr2 = i * image->bytes_per_line;
          for(j=0; j<vwidth; j++){
            if (stretched != 0){
              ptr = (long)((double)i*rheight/vheight)*rwidth
                   + (long)((double)j*rwidth/vwidth);
              ptr *= 3;
            }
            r = buf[ptr++];
            g = buf[ptr++];
            b = buf[ptr++];
            if (components == 4){
              ptr++;
            }

            udat = 0;
            if (red_shift >= 0){
              udat |= (((int)r << red_shift) & red_mask);
            }else{
              udat |= (((int)r >> (-red_shift)) & red_mask);
            }
            if (green_shift >= 0){
              udat |= (((int)g << green_shift) & green_mask);
            }else{
              udat |= (((int)g >> (-green_shift)) & green_mask);
            }
            if (blue_shift >= 0){
              udat |= (((int)b << blue_shift) & blue_mask);
            }else{
              udat |= (((int)b >> (-blue_shift)) & blue_mask);
            }

            if (msb_flag){
              ((unsigned char*)data1)[ptr2++] = (udat & 0xff0000)>>16;
              ((unsigned char*)data1)[ptr2++] = (udat & 0xff00)>>8;
              ((unsigned char*)data1)[ptr2++] = (udat & 0xff);
            }else{
              ((unsigned char*)data1)[ptr2++] = (udat & 0xff);
              ((unsigned char*)data1)[ptr2++] = (udat & 0xff00)>>8;
              ((unsigned char*)data1)[ptr2++] = (udat & 0xff0000)>>16;
            }
          }
        }
      }else{
        for(i=0; i<vheight; i++){
          for(j=0; j<vwidth; j++){
            if (stretched != 0){
              ptr = (long)((double)i*rheight/vheight)*rwidth
                   + (long)((double)j*rwidth/vwidth);
            }
            r = buf[ptr];
            g = buf[ptr];
            b = buf[ptr++];
            if (msb_flag){
              ((unsigned char*)data1)[ptr2++] =  b;
              ((unsigned char*)data1)[ptr2++] =  g;
              ((unsigned char*)data1)[ptr2++] =  r;
            }else{
              ((unsigned char*)data1)[ptr2++] =  r;
              ((unsigned char*)data1)[ptr2++] =  g;
              ((unsigned char*)data1)[ptr2++] =  b;
            }
          }
        }
      }
    }else if (image->bits_per_pixel ==16){
      if (components == 3 || components == 4){
        for(i=0; i<vheight; i++){
          for(j=0; j<vwidth; j++){
            unsigned int rr,gg,bb;
            if (stretched != 0){
              ptr = (long)((double)i*rheight/vheight+0.5)*rwidth
                   + (long)((double)j*rwidth/vwidth + 0.5);
              ptr *= 3;
            }
            rr = buf[ptr++];
            gg = buf[ptr++];
            bb = buf[ptr++];
            if (components == 4){
              ptr++;
            }

            udat = 0;
            if (red_shift >= 0){
              udat |= (((int)rr << red_shift) & red_mask);
            }else{
              udat |= (((int)rr >> (-red_shift)) & red_mask);
            }
            if (green_shift >= 0){
              udat |= (((int)gg << green_shift) & green_mask);
            }else{
              udat |= (((int)gg >> (-green_shift)) & green_mask);
            }
            if (blue_shift >= 0){
              udat |= (((int)bb << blue_shift) & blue_mask);
            }else{
              udat |= (((int)bb >> (-blue_shift)) & blue_mask);
            }
            if (msb_flag){
              ((unsigned char*)data1)[ptr2++] = (udat >> 8) & 0xff;
              ((unsigned char*)data1)[ptr2++] = (udat & 0xff);
            }else{
              ((unsigned char*)data1)[ptr2++] = (udat & 0xff);
              ((unsigned char*)data1)[ptr2++] = (udat & 0xff00)>>8;
            }
          }
        }
      }else{
        for(i=0; i<vheight; i++){
          for(j=0; j<vwidth; j++){
            if (stretched != 0){
              ptr = (long)((double)i*rheight/vheight)*rwidth
                   + (long)((double)j*rwidth/vwidth);
            }
            r = buf[ptr]>>3;
            g = buf[ptr]>>2;
            b = buf[ptr++]>>3;
            ((short*)data1)[ptr2++] =  r <<11 | g<<5 | b;
          }
        }
      }
    }else if (image->bits_per_pixel == 8){
//printf("components=%d\n",components);
      XColor col[5*5*5];
      Colormap cm = DefaultColormap(display,DefaultScreen(display));
      long k;
      long cnt=0;
      long colptr = 0;
      long rr = 0,gg = 0,bb = 0;
      long tr1,tg1,tb1;
      signed char dr1,dg1,db1;
      for(i=0; i<5; i++){
        for(j=0; j<5; j++){
          for(k=0; k<5; k++){
            if (i != 4){
              col[cnt].red = i*64*256;
            }else{
              col[cnt].red = 0xffff;
            }
            if (j != 4){
              col[cnt].green = j*64*256;
            }else{
              col[cnt].green = 0xffff;
            }
            if (k != 4){
              col[cnt].blue = k*64*256;
            }else{
              col[cnt].blue = 0xffff;
            }
            XAllocColor(display,cm,&(col[cnt]));
            cnt++;
          }
        }
      }

      for(i=0; i<vheight; i++){
        dr1 = 0;
        dg1 = 0;
        db1 = 0;
        for(j=0; j<vwidth; j++){

          if (components == 3 || components== 4){
            if (stretched != 0){
              ptr = (long)((double)i*rheight/vheight)*rwidth
                     + (long)((double)j*rwidth/vwidth);
              ptr *= 3;
            }
            tr1 = buf[ptr++];
            tg1 = buf[ptr++];
            tb1 = buf[ptr++];
            if (components == 4){
              ptr++;
            }
          }else{
            ptr = (long)((double)i*rheight/vheight)*rwidth
                     + (long)((double)j*rwidth/vwidth);
            tr1 = buf[ptr];
            tg1 = buf[ptr];
            tb1 = buf[ptr++];
          }
          if ((0 < tr1 + dr1) && (tr1 + dr1) < 256){
            if ((-32 < dr1) && (dr1 < 32)){
              rr = (tr1 + dr1 + 31) & 0x1c0;
            }else if (dr1 > 31){
              rr = (tr1 + 63) & 0x1c0;
            }else{
              rr = tr1  & 0x1c0;
            }
          }else if (tr1 + dr1 > 255){
            rr = 0x100;
          }else{
            rr = 0;
          }
          dr1 += (tr1 - rr);
          if (0 < tg1 + dg1  &&tg1 + dg1 < 256){
            if (-32 < dg1 && dg1 < 32 ){
              gg = (tg1 + dg1 + 31) & 0x1c0;
            }else if (dg1 > 31){
              gg = (tg1 + 63) & 0x1c0;
            }else{
              gg = tg1  & 0x1c0;
            }
          }else if (tg1 + dg1 > 255){
            gg = 0x100;
          }else{
            gg = 0;
          }
          dg1 += (tg1 - gg);

          if (0 < tb1 + db1 && tb1 + db1 < 256){
            if (-32 < db1 && db1 < 32){
              bb = (tb1 + db1 + 31) & 0x1c0;
            }else if (db1 > 31){
              bb = (tb1 + 63) & 0x1c0;
            }else{
              bb = tb1 & 0x1c0;
            }
          }else if (tb1 + db1 > 255){
            bb = 0x100;
          }else{
            bb = 0;
          }
          db1 += (tb1 - bb);
          db1 += (tb1 - bb);
          colptr = (rr>>6)*5*5  + (gg>>6)*5 + (bb>>6);
          data1[ptr2++] =  col[colptr].pixel;
        }
      }
    }
    image->data = data1;
    free(buf);

    pix = XCreatePixmap(display,window, vwidth,vheight,display_depth);
    *pixmap = pix;
    XPutImage(display,pix,gc,image,0,0,0,0,vwidth,vheight);

    if (image->data != NULL){
      free(image->data);
      image->data = NULL;
    }
    XDestroyImage(image);
    png_destroy_read_struct(&png_ptr, &info_ptr, NULL);

    fclose(ifile);
    return 0;
}

#endif	/* USE_PNG */

/*----------------------- end-of-file (C source) -----------------------*/
