/*--------------------------------*-C-*---------------------------------*
 * File:	mjpg.c
 *----------------------------------------------------------------------*
 *
 * All portions of code are copyright by their respective author/s.
 * Copyright (c) 1999-2002    Toshikaz Hirabayashi
 * Copyright (c) 2004         Jingmin Zhou <jimmyzhou@users.sourceforge.net>
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
** $Id: mjpg.c,v 1.9 2004/11/11 00:20:28 cvs Exp $
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


#ifdef USE_JPEG

static void
ehandler (j_common_ptr jinf, int level)
{
}

static void
exhandler (j_common_ptr jinf)
{
}

static int
chandler (j_decompress_ptr jinf)
{
	return TRUE;
}


long JpegReadFileToPixmap (Display* display, Window window, GC gc, char* filename, Pixmap* pixmap, long* w, long* h)
{

    int				red_mask, green_mask, blue_mask;
    int				red_shift, green_shift, blue_shift;
    int				start_shift, msb_flag;
    unsigned int	start_mask, udat;
    FILE*			ifile;
    XWindowAttributes win_attr;
    struct jpeg_decompress_struct    jinf;
    struct jpeg_error_mgr            jerr;
    long			rwidth, rheight;
    long			vwidth = *w;
    long			vheight = *h;
    long			stretched =0;
    long			components;
    unsigned char*	buf;
    JSAMPROW		rowptr[1];
    long			display_depth = XDefaultDepth(display,XDefaultScreen(display));
    XImage*			image = 0;
    Visual*			visual;
    char*			data1;
    unsigned char	r, g, b;
    long			ptr = 0;
    long			ptr2 = 0;
    long			i;
    long			j;
    Pixmap			pix;
    

    red_mask = green_mask = blue_mask = 0;
    red_shift = green_shift = blue_shift = 0;

    ifile = fopen(filename,"r");
    if (ifile == NULL)	{
		return -1;
    }

    jinf.err = jpeg_std_error(&jerr);
    jinf.err->emit_message = ehandler;
    jinf.err->error_exit = exhandler;

    jpeg_create_decompress(&jinf);
    jpeg_set_marker_processor(&jinf,JPEG_COM,chandler);

    jpeg_stdio_src(&jinf,ifile);
    jpeg_read_header(&jinf,TRUE);
    jpeg_calc_output_dimensions(&jinf);


    jpeg_start_decompress(&jinf);
    rwidth = jinf.output_width;
    rheight = jinf.output_height;
    if (*w == 0 || *h == 0){
		*w = rwidth;
		*h = rheight;
    }
	else{
		if ((long)((double)rwidth * vheight/vwidth) < rheight){
			*w = (long)((double)vheight * rwidth/rheight);
		}
		else	{
			*h = (long)((double)vwidth * rheight/rwidth);
		}
		stretched = 1;
	}
    vwidth = *w;
    vheight = *h;
//    long bpp = jinf.data_precision;
    components = jinf.output_components;
	/* possible integer overflow */
	assert ((int) components > 0);
	assert ((int) rheight > 0);
	assert ((int) rwidth > 0);
	assert (components * rheight * rwidth * sizeof (JSAMPLE) > 0);
    buf = (unsigned char*)rxvt_malloc(components * rheight * rwidth*sizeof(JSAMPLE));
//printf("jpg::readjpg w,h=%d,%d  c=%d\n",width,height,components);


    for(i=0; i< rheight; i++){
      rowptr[0] = (JSAMPROW)&(buf[i * rwidth * components]);
      jpeg_read_scanlines(&jinf, rowptr,1);
    }
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
        free(buf);
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
      n = start_mask;
      red_mask = win_attr.visual->red_mask;
      red_shift = start_shift;
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
	assert ((int)image->bytes_per_line > 0);
	assert ((int)vheight > 0);
	assert (((int)image->bytes_per_line) * ((int)vheight) > 0);
    data1 = rxvt_malloc(image->bytes_per_line * vheight);
    ptr = 0;
    ptr2 = 0;

    if (image->bits_per_pixel ==32){
      if (components == 3){
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
      if (components == 3){
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
      if (components == 3){
        for(i=0; i<vheight; i++){
          for(j=0; j<vwidth; j++){
            unsigned int rr = buf[ptr++];
            unsigned int gg = buf[ptr++];
            unsigned int bb = buf[ptr++];
            if (stretched != 0){
              ptr = (long)((double)i*rheight/vheight+0.5)*rwidth
                   + (long)((double)j*rwidth/vwidth + 0.5);
              ptr *= 3;
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

          if (components == 3){
            if (stretched != 0){
              ptr = (long)((double)i*rheight/vheight)*rwidth
                     + (long)((double)j*rwidth/vwidth);
              ptr *= 3;
            }
            tr1 = buf[ptr++];
            tg1 = buf[ptr++];
            tb1 = buf[ptr++];
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

    jpeg_finish_decompress(&jinf);
    jpeg_destroy_decompress(&jinf);

    if (image->data != NULL){
      free(image->data);
      image->data = NULL;
    }
    XDestroyImage(image);

    fclose(ifile);
    return 0;
}

#endif	/* USE_JPEG */
/*----------------------- end-of-file (C source) -----------------------*/
